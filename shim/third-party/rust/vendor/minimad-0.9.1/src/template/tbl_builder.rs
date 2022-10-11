use crate::*;

#[derive(Debug, Clone)]
pub struct CellDef {
    /// how the cell will be filled, may be a template
    pub md: String,

    pub align: Alignment,
}

#[derive(Debug, Clone)]
pub struct Col {
    pub header: CellDef,
    pub content: CellDef,
}

/// A facility to build templates for tables
///
/// You can for example build a table this two ways:
///
/// ### with an explicit string:
///
/// ```
/// static MD: &str = r#"
/// |-:|:-:|:-:|:-:|:-:|-:|:-:|:-:|:-|:-
/// |id|dev|filesystem|disk|type|used|use%|free|size|mount point
/// |-:|:-|:-|:-:|:-:|-:|-:|-:|:-
/// ${rows
/// |${id}|${dev-major}:${dev-minor}|${filesystem}|${disk}|${type}|~~${used}~~|~~${use-percents}~~ `${bar}`|*${free}*|**${size}**|${mount-point}
/// }
/// |-:
/// "#;
/// ```
/// ### with a table_builder:
///
/// ```
///  use minimad::{*, Alignment::*};
///
///  let mut tbl = TableBuilder::default();
///  tbl
///      .col(Col::simple("id").align(Right))
///      .col(Col::new("dev", "${dev-major}:${dev-minor}"))
///      .col(Col::simple("filesystem"))
///      .col(Col::simple("disk").align_content(Center))
///      .col(Col::simple("type"))
///      .col(Col::new("used", "~~${used}~~"))
///      .col(Col::new("use%", "~~${use-percents}~~ `${bar}`").align_content(Right))
///      .col(Col::new("free", "*${free}*").align(Right))
///      .col(Col::new("size", "**${size}**"))
///      .col(Col::simple("mount point").align(Left));
/// ```
///
/// Both ways are mostly equivalent but a table builder makes it easier to dynamically
/// define the columns.
///
/// (example taken from [lfs](https://github.com/Canop/lfs))
#[derive(Debug, Clone, Default)]
pub struct TableBuilder {
    pub cols: Vec<Col>,
    /// an optional name for the sub template for the rows.
    /// This is mostly useful when you want to concatenate
    /// table templates and you need to distinguish their
    /// subs
    pub rows_sub_name: Option<String>,
}

impl CellDef {
    pub fn new<S: Into<String>>(md: S) -> Self {
        Self {
            md: md.into(),
            align: Alignment::Unspecified,
        }
    }
    pub fn align(mut self, align: Alignment) -> Self {
        self.align = align;
        self
    }
}

impl Col {
    pub fn simple<S: AsRef<str>>(var_name: S) -> Self {
        Self::new(
            var_name.as_ref().to_string(),
            format!("${{{}}}", var_name.as_ref().replace(' ', "-")),
        )
    }
    pub fn new<H: Into<String>, C: Into<String>>(header_md: H, content_md: C) -> Self {
        Self {
            header: CellDef::new(header_md).align(Alignment::Center),
            content: CellDef::new(content_md),
        }
    }
    pub fn align(mut self, align: Alignment) -> Self {
        self.header.align = align;
        self.content.align = align;
        self
    }
    pub fn align_header(mut self, align: Alignment) -> Self {
        self.header.align = align;
        self
    }
    pub fn align_content(mut self, align: Alignment) -> Self {
        self.content.align = align;
        self
    }
}

impl TableBuilder {
    pub fn col(&mut self, col: Col) -> &mut Self {
        self.cols.push(col);
        self
    }
    /// build the string of a template of the table
    pub fn template_md(&self) -> String {
        let mut md = String::new();
        for col in &self.cols {
            md.push_str(col.header.align.col_spec());
        }
        md.push('\n');
        for col in &self.cols {
            md.push('|');
            md.push_str(&col.header.md);
        }
        md.push('\n');
        for col in &self.cols {
            md.push_str(col.content.align.col_spec());
        }
        md.push_str("\n${");
        if let Some(name) = self.rows_sub_name.as_ref() {
            md.push_str(name);
        } else {
            md.push_str("rows");
        }
        md.push('\n');
        for col in &self.cols {
            md.push('|');
            md.push_str(&col.content.md);
        }
        md.push_str("\n}\n|-\n");
        md
    }
}


impl From<&TableBuilder> for String {
    fn from(tblbl: &TableBuilder) -> String {
        tblbl.template_md()
    }
}
