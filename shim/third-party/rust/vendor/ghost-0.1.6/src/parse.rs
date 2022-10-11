use syn::parse::{Parse, ParseStream, Result};
use syn::{Attribute, Generics, Ident, Token, Visibility, WhereClause};

pub struct UnitStruct {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub struct_token: Token![struct],
    pub ident: Ident,
    pub generics: Generics,
}

impl Parse for UnitStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let struct_token: Token![struct] = input.parse()?;
        let ident: Ident = input.parse()?;

        // Require there to be generics.
        input.fork().parse::<Token![<]>()?;
        let generics: Generics = input.parse()?;
        let where_clause: Option<WhereClause> = input.parse()?;

        input.parse::<Token![;]>()?;

        Ok(UnitStruct {
            attrs,
            vis,
            struct_token,
            ident,
            generics: Generics {
                where_clause,
                ..generics
            },
        })
    }
}
