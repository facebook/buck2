mod align;
mod composite;
mod compound;
mod header;
mod line;
mod tbl;
mod text;

pub use {
    align::Alignment,
    composite::{Composite, CompositeStyle},
    compound::Compound,
    header::header_level,
    line::{Line, MAX_HEADER_DEPTH},
    tbl::{TableRow, TableRule},
    text::Text,
};
