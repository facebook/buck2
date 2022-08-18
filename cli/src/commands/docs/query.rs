use buck2_build_api::query::cquery::environment::CqueryEnvironment;
use buck2_build_api::query::uquery::environment::UqueryEnvironment;
use buck2_query::query::syntax::simple::functions::docs::MarkdownOptions;
use buck2_query::query::syntax::simple::functions::docs::QueryEnvironmentDescription;
use gazebo::dupe::Dupe;

use crate::exit_result::ExitResult;
use crate::ClientCommandContext;

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum DocsOutputFormatArg {
    Markdown,
    Rendered,
}

#[derive(Debug, clap::Parser)]
pub(crate) struct QueryDocsOptions {
    /// How to format the documentation
    #[clap(
        long = "format",
        default_value = "rendered",
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

fn output(options: QueryDocsOptions, description: QueryEnvironmentDescription) -> ExitResult {
    match options.format {
        DocsOutputFormatArg::Markdown => {
            let markdown = description.render_markdown(&MarkdownOptions {
                include_alt_text: true,
            });
            crate::println!("{}", markdown)?;
        }
        DocsOutputFormatArg::Rendered => {
            let markdown = description.render_markdown(&MarkdownOptions {
                include_alt_text: false,
            });
            let skin = termimad::MadSkin::default();
            let area = termimad::Area::full_screen();
            let width = std::cmp::min(100, area.width) as usize;
            let rendered = skin.text(&markdown, Some(width));
            crate::println!("{}", rendered)?;
        }
    }
    ExitResult::success()
}

impl DocsUqueryCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let description = UqueryEnvironment::describe();
        output(self.docs_options, description)
    }
}

impl DocsCqueryCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let description = CqueryEnvironment::describe();
        output(self.docs_options, description)
    }
}
