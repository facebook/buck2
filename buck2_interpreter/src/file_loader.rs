/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::anyhow;
use buck2_core::bzl::ImportPath;
use buck2_core::bzl::ModuleID;
use gazebo::prelude::*;
use indexmap::map::IndexMap;
use starlark::environment::FrozenModule;
use starlark::eval::FileLoader;

use crate::common::OwnedStarlarkModulePath;
use crate::common::StarlarkModulePath;

#[derive(Default, Clone)]
pub struct LoadedModules {
    pub map: IndexMap<ModuleID, LoadedModule>,
}

impl LoadedModules {
    pub fn imports(&self) -> impl Iterator<Item = &ImportPath> {
        self.map.values().map(|module| {
            *module
                .path()
                .unpack_load_file()
                .expect("imports should only be bzl files")
        })
    }
}

pub trait LoadResolver {
    fn resolve_load(&self, path: &str) -> anyhow::Result<OwnedStarlarkModulePath>;
}

pub struct ModuleDeps(pub Vec<LoadedModule>);

impl ModuleDeps {
    pub fn get_loaded_modules(&self) -> LoadedModules {
        let mut map = IndexMap::with_capacity(self.0.len());
        for dep in &*self.0 {
            map.insert(dep.path().id().to_owned(), dep.dupe());
        }
        LoadedModules { map }
    }
}

#[derive(Clone, Dupe)]
pub struct LoadedModule(Arc<LoadedModuleData>);

struct LoadedModuleData {
    path: OwnedStarlarkModulePath,
    loaded_modules: LoadedModules,
    env: FrozenModule,
}

impl LoadedModule {
    pub fn new(
        path: OwnedStarlarkModulePath,
        loaded_modules: LoadedModules,
        env: FrozenModule,
    ) -> Self {
        Self(Arc::new(LoadedModuleData {
            path,
            loaded_modules,
            env,
        }))
    }

    pub fn loaded_modules(&self) -> &LoadedModules {
        &self.0.loaded_modules
    }

    pub fn imports(&self) -> impl Iterator<Item = &ImportPath> {
        self.0.loaded_modules.imports()
    }

    pub fn path(&self) -> StarlarkModulePath<'_> {
        self.0.path.borrow()
    }

    pub fn env(&self) -> &FrozenModule {
        &self.0.env
    }
}

pub struct InterpreterFileLoader {
    loaded_modules: LoadedModules,
    info: Arc<dyn LoadResolver>,
}

impl InterpreterFileLoader {
    pub fn new(loaded_modules: LoadedModules, info: Arc<dyn LoadResolver>) -> Self {
        Self {
            loaded_modules,
            info,
        }
    }
}

fn to_diagnostic(err: &anyhow::Error, id: &str) -> anyhow::Error {
    anyhow!("UnkownError in {}: {}", id, err)
}

impl InterpreterFileLoader {
    /// Used for looking up modules by id.
    fn find_module(&self, id: &ModuleID) -> anyhow::Result<&FrozenModule> {
        match self.loaded_modules.map.get(id) {
            Some(v) => Ok(&v.0.env),
            None => Err(to_diagnostic(
                &anyhow!(
                    "Should have had an env for {}. had <{:?}>",
                    id,
                    self.loaded_modules.map.keys().collect::<Vec<_>>()
                ),
                id.as_str(),
            )),
        }
    }
}

impl FileLoader for InterpreterFileLoader {
    /// The Interpreter will call this to resolve and load imports for load()
    /// statements.
    fn load(&self, path: &str) -> anyhow::Result<FrozenModule> {
        match self.info.resolve_load(path) {
            Ok(import) => Ok(self.find_module(import.borrow().id())?.dupe()),
            Err(e) => Err(to_diagnostic(&e, path)),
        }
    }
}

#[cfg(test)]
mod tests {
    use starlark::environment::Module;

    use super::*;

    struct TestLoadResolver {}

    impl LoadResolver for TestLoadResolver {
        fn resolve_load(&self, path: &str) -> anyhow::Result<OwnedStarlarkModulePath> {
            match path {
                "//some/package:import.bzl" => Ok(OwnedStarlarkModulePath::LoadFile(import(
                    "root",
                    "some/package",
                    "import.bzl",
                ))),
                "cell1//next/package:import.bzl" => Ok(OwnedStarlarkModulePath::LoadFile(import(
                    "cell1",
                    "next/package",
                    "import.bzl",
                ))),
                "alias2//last/package:import.bzl" => Ok(OwnedStarlarkModulePath::LoadFile(import(
                    "cell2",
                    "last/package",
                    "import.bzl",
                ))),
                _ => Err(anyhow!("error")),
            }
        }
    }

    fn resolver() -> Arc<TestLoadResolver> {
        Arc::new(TestLoadResolver {})
    }

    fn env(name: &ModuleID) -> FrozenModule {
        let m = Module::new();
        m.set("name", m.heap().alloc(name.as_str()));
        m.freeze().unwrap()
    }

    fn loaded_modules() -> LoadedModules {
        let mut loaded_modules = LoadedModules::default();
        let resolver = resolver();

        let mut insert = |path| {
            let import_path = resolver.resolve_load(path).unwrap();
            let id = import_path.borrow().id().clone();
            let module = LoadedModule::new(import_path, LoadedModules::default(), env(&id));
            loaded_modules.map.insert(id, module);
        };

        // Insert the same things that the TestLoadResolver supports.
        insert("//some/package:import.bzl");
        insert("cell1//next/package:import.bzl");
        insert("alias2//last/package:import.bzl");

        loaded_modules
    }

    fn import(cell: &str, package: &str, filename: &str) -> ImportPath {
        ImportPath::unchecked_new(cell, package, filename)
    }

    #[test]
    fn no_resolution() -> anyhow::Result<()> {
        let path = "some//random:file.bzl".to_owned();
        let loader = InterpreterFileLoader::new(loaded_modules(), resolver());
        match loader.load(&path) {
            Ok(_) => panic!("Expected load failure for {}", path),
            Err(_) => {
                // TODO: verify the error is correct
            }
        }
        Ok(())
    }

    #[test]
    fn missing_in_loaded_modules() -> anyhow::Result<()> {
        let path = "cell1//next/package:import.bzl".to_owned();
        let resolver = resolver();
        let id = resolver.resolve_load(&path)?.borrow().id().to_owned();

        let mut loaded_modules = loaded_modules();
        loaded_modules.map.remove(&id);
        let loader = InterpreterFileLoader::new(loaded_modules, resolver);
        match loader.load(&path) {
            Ok(_) => panic!("Expected load failure for {}", path),
            Err(_) => {
                // TODO: verify the error is correct
            }
        }
        Ok(())
    }

    #[test]
    fn valid_load() -> anyhow::Result<()> {
        let path = "cell1//next/package:import.bzl".to_owned();
        let resolver = resolver();
        let id = resolver.resolve_load(&path)?.borrow().id().to_owned();

        let loader = InterpreterFileLoader::new(loaded_modules(), resolver);
        let loaded = loader.load(&path)?;

        let v = loaded.get("name").unwrap();
        assert_eq!(v.value().unpack_str(), Some(id.as_str()));

        Ok(())
    }

    #[test]
    fn valid_find() -> anyhow::Result<()> {
        let path = "cell1//next/package:import.bzl".to_owned();
        let resolver = resolver();
        let resolved = resolver.resolve_load(&path)?;
        let borrow = resolved.borrow();
        let id = borrow.id();

        let loader = InterpreterFileLoader::new(loaded_modules(), resolver);
        let found = loader.find_module(id)?;

        let v = found.get("name").unwrap();
        assert_eq!(v.value().unpack_str(), Some(id.as_str()));

        Ok(())
    }
}
