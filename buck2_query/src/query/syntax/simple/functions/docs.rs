use std::fmt::Write;

use enum_iterator::IntoEnumIterator;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::query::syntax::simple::functions::helpers::QueryArgType;

// Instances created by #[query_module]
pub struct ArgDescription {
    pub name: String,
    pub repr_format: String,
    pub arg_type: QueryArgType,
}

impl ArgDescription {
    pub fn render_markdown(&self) -> String {
        format!(
            "{}: {}",
            &self.name,
            self.repr_format
                .replace("{}", &format!("_\\<{}\\>_", self.arg_type.repr()))
        )
    }
}

// Instances created by #[query_module]
pub struct FunctionDescription {
    pub name: &'static str,
    pub args: Vec<ArgDescription>,

    pub short_help: Option<String>,
    pub details: Option<String>,
}

impl FunctionDescription {
    pub fn render_short_markdown(&self) -> String {
        format!(
            " - {}({}){}",
            self.name,
            self.args.iter().map(|v| v.render_markdown()).join(", "),
            &match &self.short_help {
                None => "".to_owned(),
                Some(v) => format!(": {}", v),
            }
        )
    }

    pub fn render_markdown(&self) -> String {
        let mut rendered = format!(
            "### {}({})\n\n",
            self.name,
            self.args.iter().map(|v| v.render_markdown()).join(", ")
        );
        if let Some(v) = &self.short_help {
            writeln!(rendered, "{}\n", v).unwrap();
        }

        if let Some(v) = &self.details {
            writeln!(rendered, "{}\n", v).unwrap();
        }
        rendered
    }
}

// Instances created by #[query_module]
pub struct ModuleDescription {
    pub functions: IndexMap<&'static str, FunctionDescription>,

    pub short_help: Option<String>,
    pub details: Option<String>,
}

impl ModuleDescription {
    pub fn render_markdown(&self) -> String {
        let mut rendered = format!(
            "## {}\n\n",
            match &self.short_help {
                Some(v) => v,
                None => "Query functions",
            }
        );
        if let Some(v) = &self.details {
            writeln!(rendered, "{}\n", v).unwrap();
        }

        for (_, func) in &self.functions {
            writeln!(rendered, "{}\n", func.render_short_markdown()).unwrap();
        }

        for (_, func) in &self.functions {
            writeln!(rendered, "{}\n", func.render_markdown()).unwrap();
        }

        rendered
    }
}

pub struct QueryEnvironmentDescription {
    pub name: String,
    pub mods: Vec<ModuleDescription>,
}

impl QueryEnvironmentDescription {
    pub fn render_markdown(&self) -> String {
        format!(
            indoc::indoc! {r#"
            # {}

            ## Query Value Types
            {}

            {}
            "#},
            &self.name,
            QueryArgType::into_enum_iter()
                .map(render_arg_type_markdown)
                .join("\n\n"),
            self.mods.iter().map(|v| v.render_markdown()).join("\n\n")
        )
    }
}

fn render_arg_type_markdown(v: QueryArgType) -> String {
    format!(
        indoc::indoc! {r#"
        *[\<{}\>]: {}
        - _{}_: {}
    "#},
        v.repr(),
        v.short_description(),
        v.repr(),
        v.description()
    )
}
