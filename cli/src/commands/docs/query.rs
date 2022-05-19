use buck2_build_api::query::{
    cquery::environment::CqueryEnvironment, uquery::environment::UqueryEnvironment,
};
use buck2_core::exit_result::ExitResult;
use clap::arg_enum;
use structopt::{clap, StructOpt};

use crate::CommandContext;

structopt::clap::arg_enum! {
    #[derive(Debug)]
    enum DocsOutputFormatArg {
        Markdown
    }
}

#[derive(Debug, StructOpt)]
pub struct QueryDocsOptions {
    /// How to format the documentation
    #[structopt(long = "format", default_value = "markdown")]
    format: DocsOutputFormatArg,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "docs-uquery", about = "Print documentation for uquery")]
pub struct DocsUqueryCommand {
    #[structopt(flatten)]
    docs_options: QueryDocsOptions,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "docs-cquery", about = "Print documentation for cquery")]
pub struct DocsCqueryCommand {
    #[structopt(flatten)]
    docs_options: QueryDocsOptions,
}

impl DocsUqueryCommand {
    pub fn exec(self, _matches: &clap::ArgMatches<'_>, _ctx: CommandContext) -> ExitResult {
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
    pub fn exec(self, _matches: &clap::ArgMatches<'_>, _ctx: CommandContext) -> ExitResult {
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
