use std::collections::HashMap;
use std::io::ErrorKind;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use anyhow::Context;
use buck2_core::fs::fs_util;
use itertools::Itertools;
use walkdir::DirEntry;

use crate::client_ctx::ClientCommandContext;
use crate::common::CommonConsoleOptions;
use crate::exit_result::ExitResult;
use crate::final_console::FinalConsole;

/// Buck2 Init
///
/// This command is intented to be part-tutorial part-convenience
/// for generating buck2 projects. Given a path and optional name
/// (in the case that the folder name is not desirable).
#[derive(Debug, clap::Parser)]
#[clap(name = "install", about = "Initialize a buck2 project")]
pub struct InitCommand {
    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    /// The path to initialize the project in. The folder does not need to exist.
    #[clap(default_value = ".")]
    path: PathBuf,

    /// The name for the project. If not provided will default to the last segment
    /// of the path.
    #[clap(short, long)]
    name: Option<String>,

    /// Don't generate a prelude or a toolchain.
    #[clap(long)]
    no_prelude: bool,

    /// Initialize the project even if the git repo at \[PATH\] has uncommitted changes.
    #[clap(long)]
    allow_dirty: bool,
}

impl InitCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, _ctx: ClientCommandContext) -> ExitResult {
        let console = self.console_opts.final_console();

        match exec_impl(self, &console) {
            Ok(_) => ExitResult::Status(0),
            Err(e) => {
                // include the backtrace with the error output
                // (same behaviour as returning the Error from main)
                console.print_error(&format!("{:?}", e))?;
                ExitResult::Status(1)
            }
        }
    }
}

/// A number of known filetypes that buck2 init recognises.
///
/// This is used to generate a stub build file using rules
/// from the prelude.
#[derive(Debug, PartialEq, Eq, Hash)]
enum FileType {
    Rust,
    Python,
    Starlark,
    Javascript,
    Typescript,
    Go,
    Unknown,
    Directory,
    Cxx,
}

impl FileType {
    fn from_ext(ext: &str) -> Self {
        match ext {
            "rs" => FileType::Rust,
            "py" => FileType::Python,
            "c" | "cpp" | "h" | "hpp" => FileType::Cxx,
            "bzl" => FileType::Starlark,
            "go" => FileType::Go,
            "js" | "jsx" => FileType::Javascript,
            "ts" | "tsx" => FileType::Typescript,
            _ => FileType::Unknown,
        }
    }
}

fn exec_impl(cmd: InitCommand, console: &FinalConsole) -> anyhow::Result<()> {
    fs_util::create_dir_all(&cmd.path)?;
    let absolute = fs_util::canonicalize(&cmd.path)?;

    if absolute.is_file() {
        return Err(anyhow::anyhow!(
            "Target path {} cannot be an existing file",
            absolute.display()
        ));
    }

    let status = match Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(&absolute)
        .output()
    {
        Err(e) if e.kind().eq(&ErrorKind::NotFound) => {
            console.print_error(
                "Warning: no git found on path, can't check for dirty repo. Proceeding anyway.",
            )?;
            None
        }
        r => Some(r.context("Couldn't detect dirty status of folder.")?),
    };

    let changes = status.filter(|o| o.status.success()).map(|o| {
        String::from_utf8_lossy(&o.stdout)
            .trim()
            .lines()
            .any(|l| !l.starts_with("??"))
    });

    if let (Some(true), false) = (changes, cmd.allow_dirty) {
        return Err(anyhow::anyhow!(
            "Refusing to initialize in a dirty repo. Stash your changes or use `--allow-dirty` to override."
        ));
    }

    let name = match cmd
        .name
        .as_deref()
        .or_else(|| absolute.file_name().and_then(|s| s.to_str()))
    {
        Some(x) => x,
        None => {
            return Err(anyhow::anyhow!(
                "Could not set project name `{}`. Is it valid unicode?",
                absolute.display()
            ));
        }
    };

    let discovered_langs = discover_project(&absolute);

    set_up_project(name, &absolute, !cmd.no_prelude, &discovered_langs)
}

fn discover_project(path: &PathBuf) -> HashMap<FileType, Vec<PathBuf>> {
    let mut discovered_langs = HashMap::<FileType, Vec<PathBuf>>::new();

    // ignore common build folders
    fn ignore(e: &DirEntry) -> bool {
        e.file_name().to_str().map_or(true, |s| {
            !s.starts_with('.') && !["node_modules", "target", "buck-out"].contains(&s)
        })
    }

    // if the folder exists and is nonempty, recursive walk to discover the contents
    // limit depth to 5 and take the first 1000 files picking the top match based on ext
    // to prevent taking too much time on huge projects
    for entry in walkdir::WalkDir::new(path)
        .max_depth(5)
        .into_iter()
        .filter_entry(ignore)
        .take(1000)
        .filter_map(Result::ok)
    {
        let ext = entry
            .file_name()
            .to_str()
            .unwrap()
            .split_once('.') // assume anything after the first . is the extension
            .map(|(_, ext)| ext);

        let dir = entry.metadata().map(|m| m.is_dir()).ok();

        let lang = match (ext, dir) {
            (_, Some(true)) => FileType::Directory,
            (Some(ext), _) => FileType::from_ext(ext),
            _ => FileType::Unknown,
        };

        discovered_langs
            .entry(lang)
            .or_default()
            .push(entry.path().to_owned());
    }

    discovered_langs
}

fn set_up_project(
    name: &str,
    path: &PathBuf,
    prelude: bool,
    discovered_langs: &HashMap<FileType, Vec<PathBuf>>,
) -> anyhow::Result<()> {
    if !Command::new("git")
        .arg("init")
        .current_dir(&path)
        .status()?
        .success()
    {
        return Err(anyhow::anyhow!("Failure when running `git init`."));
    };

    let mut buck_config = {
        let mut buck_config = std::fs::File::create(path.join(".buckconfig"))?;
        writeln!(buck_config, "[buildfile]")?;
        writeln!(buck_config, "name = BUCK2")?;
        writeln!(buck_config)?;
        writeln!(buck_config, "[repositories]")?;
        writeln!(buck_config, "root = .")?;
        buck_config
    };

    let mut buck2 = std::fs::File::create(path.join("BUCK2"))?;

    if prelude {
        if !Command::new("git")
            .args([
                "submodule",
                "add",
                "https://github.com/facebookincubator/buck2-prelude.git",
                "prelude",
            ])
            .current_dir(&path)
            .status()?
            .success()
        {
            return Err(anyhow::anyhow!(
                "Unable to clone the prelude. Is the folder in use?"
            ));
        }

        std::fs::create_dir(path.join("toolchains"))?;
        writeln!(buck_config, "prelude = prelude")?;
        writeln!(buck_config, "toolchains = toolchains")?;

        for (lang, files) in discovered_langs.iter() {
            match lang {
                FileType::Cxx => write!(buck2, "{}", generate_cxx_rule(name, path, files))?,
                FileType::Python => write!(buck2, "{}", generate_python_rule(name, path, files))?,
                _ => (),
            }
        }
    } else {
        writeln!(buck2, "# to get started without using the prelude")?;
        writeln!(
            buck2,
            "# please visit buck2.build/docs/quickstart/no-prelude"
        )?;
    }

    Ok(())
}

fn generate_python_rule(name: &str, path: &PathBuf, files: &[PathBuf]) -> String {
    let srcs = files
        .iter()
        .map(|f| {
            format!(
                "\"{}\"",
                f.strip_prefix(path)
                    .expect("always a subpath")
                    .to_string_lossy()
            )
        })
        .join(",\n        ");

    format!(
        "# to learn about how to use this, please visit buck2.build/docs/python
python_binary(
    name = \"{}-py\",
    main = \"\" # todo: set the main entrypoint
)

# to learn about how to use this, please visit buck2.build/docs/python
python_library(
    name = \"{}-lib-py\",
    srcs = [
        {}
    ],
    visibility = [\"PUBLIC\"],
    base_module = \"{}\",
)\n\n",
        name, name, srcs, name
    )
}

fn generate_cxx_rule(name: &str, path: &PathBuf, files: &[PathBuf]) -> String {
    let srcs = files
        .iter()
        .map(|f| {
            format!(
                "\"{}\"",
                f.strip_prefix(path)
                    .expect("always a subpath")
                    .to_string_lossy()
            )
        })
        .join(",\n        ");
    format!(
        "# to learn about how to use this, please visit buck2.build/docs/cxx
cxx_binary(
    name = \"{}-cpp\",
    link_style = \"static\",
    srcs = [
        {}
    ]
)\n\n",
        name, srcs
    )
}
