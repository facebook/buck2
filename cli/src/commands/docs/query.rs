use buck2_build_api::query::{
    cquery::environment::CqueryEnvironment, uquery::environment::UqueryEnvironment,
};
use buck2_core::exit_result::ExitResult;
use gazebo::dupe::Dupe;

use crate::CommandContext;

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum DocsOutputFormatArg {
    Markdown,
}

#[derive(Debug, clap::Parser)]
pub(crate) struct QueryDocsOptions {
    /// How to format the documentation
    #[clap(
        long = "format",
        default_value = "markdown",
        arg_enum,
        ignore_case = true
    )]
    format: DocsOutputFormatArg,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "docs-uquery", about = "Print documentation for uquery")]
pub(crate) struct DocsUqueryCommand {
    #[clap(flatten)]
    docs_options: QueryDocsOptions,
}

#[derive(Debug, clap::Parser)]
#[clap(name = "docs-cquery", about = "Print documentation for cquery")]
pub(crate) struct DocsCqueryCommand {
    #[clap(flatten)]
    docs_options: QueryDocsOptions,
}

impl DocsUqueryCommand {
    pub(crate) fn exec(self, _matches: &clap::ArgMatches, _ctx: CommandContext) -> ExitResult {
        let description = UqueryEnvironment::describe();
        match self.docs_options.format {
            DocsOutputFormatArg::Markdown => {
                let markdown = description.render_markdown();
                crate::println!("{}", markdown)?;
            }
        }
        ExitResult::success()
    }
}

impl DocsCqueryCommand {
    pub(crate) fn exec(self, _matches: &clap::ArgMatches, _ctx: CommandContext) -> ExitResult {
        let description = CqueryEnvironment::describe();
        match self.docs_options.format {
            DocsOutputFormatArg::Markdown => {
                let markdown = description.render_markdown();
                crate::println!("{}", markdown)?;
            }
        }
        ExitResult::success()
    }
}
