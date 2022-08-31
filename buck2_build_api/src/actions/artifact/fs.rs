use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::path::buck_out_path::BuckOutPath;
use buck2_execute::path::buck_out_path::BuckOutPathResolver;
use buck2_execute::path::buck_out_path::BuckPathResolver;
use buck2_node::execute::config::PathSeparatorKind;
use either::Either;

use crate::actions::artifact::path::ArtifactPath;
use crate::actions::artifact::source_artifact::SourceArtifact;

#[derive(Clone)]
pub struct ArtifactFs {
    buck_path_resolver: BuckPathResolver,
    buck_out_path_resolver: BuckOutPathResolver,
    project_filesystem: ProjectRoot,
}

impl ArtifactFs {
    pub fn new(
        buck_path_resolver: BuckPathResolver,
        buck_out_path_resolver: BuckOutPathResolver,
        project_filesystem: ProjectRoot,
    ) -> Self {
        Self {
            buck_path_resolver,
            buck_out_path_resolver,
            project_filesystem,
        }
    }

    pub fn resolve(&self, artifact: ArtifactPath<'_>) -> anyhow::Result<ProjectRelativePathBuf> {
        let ArtifactPath {
            base_path,
            projected_path,
        } = artifact;

        let base_path = match base_path {
            Either::Left(build) => self.buck_out_path_resolver.resolve_gen(&build),
            Either::Right(source) => self.buck_path_resolver.resolve(source)?,
        };

        Ok(match projected_path {
            Some(projected_path) => base_path.join(projected_path),
            None => base_path,
        })
    }

    pub fn retrieve_unhashed_location(&self, path: &BuckOutPath) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.unhashed_gen(path)
    }

    pub fn resolve_build(&self, path: &BuckOutPath) -> ProjectRelativePathBuf {
        self.buck_out_path_resolver.resolve_gen(path)
    }

    pub fn resolve_cell_path(&self, path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve_cell_path(path)
    }

    pub fn resolve_source(
        &self,
        artifact: &SourceArtifact,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        self.buck_path_resolver.resolve(artifact.get_path())
    }

    /// Writes a file's contents to disk, creating any intermediate directories needed
    pub fn write_file(
        &self,
        dest: &BuckOutPath,
        contents: impl AsRef<[u8]>,
        executable: bool,
    ) -> anyhow::Result<()> {
        let dest_path = self.resolve_build(dest);
        self.project_filesystem
            .write_file(&dest_path, contents, executable)
    }

    pub fn copy(&self, src: ArtifactPath, dest: &BuckOutPath) -> anyhow::Result<()> {
        let src_path = self.resolve(src)?;
        let dest_path = self.resolve_build(dest);
        self.project_filesystem.copy(&src_path, &dest_path)
    }

    pub fn fs(&self) -> &ProjectRoot {
        &self.project_filesystem
    }

    pub fn buck_out_path_resolver(&self) -> &BuckOutPathResolver {
        &self.buck_out_path_resolver
    }
}

pub struct ExecutorFs<'a> {
    fs: &'a ArtifactFs,
    path_separator: PathSeparatorKind,
}

impl<'a> ExecutorFs<'a> {
    pub fn new(fs: &'a ArtifactFs, path_separator: PathSeparatorKind) -> Self {
        Self { fs, path_separator }
    }

    pub fn fs(&self) -> &ArtifactFs {
        self.fs
    }

    pub fn path_separator(&self) -> PathSeparatorKind {
        self.path_separator
    }
}
