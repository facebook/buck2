use syn::{parse_quote, Visibility};

// If `vis` describes a visibility relative to some module scope, returns the
// same visibility as described from a module contained within that scope.
//
//     quote! {
//         #vis #A;
//         mod #name {
//             // Same visibility as super::A.
//             #vis_super #B;
//         }
//     }
//
// Note if you are considering copying this implementation into another crate:
// please try not to. Visibilities in Rust are confusing and subtle, especially
// around visibility specifiers of items declared inside of a function body.
// This function is not a robust implementation of the transformation that it
// claims to implement. Always first try to restructure your code to avoid
// needing this logic.
pub fn vis_super(vis: &Visibility) -> Visibility {
    match vis {
        Visibility::Public(vis) => {
            // pub -> pub
            parse_quote!(#vis)
        }
        Visibility::Crate(vis) => {
            // crate -> crate
            parse_quote!(#vis)
        }
        Visibility::Restricted(vis) => {
            if vis.path.segments[0].ident == "self" {
                // pub(self) -> pub(in super)
                // pub(in self::super) -> pub(in super::super)
                let path = vis.path.segments.iter().skip(1);
                parse_quote!(pub(in super #(::#path)*))
            } else if vis.path.segments[0].ident == "super" {
                // pub(super) -> pub(in super::super)
                // pub(in super::super) -> pub(in super::super::super)
                let path = &vis.path;
                parse_quote!(pub(in super::#path))
            } else {
                // pub(crate) -> pub(crate)
                // pub(in crate::path) -> pub(in crate::path)
                parse_quote!(#vis)
            }
        }
        Visibility::Inherited => {
            // private -> pub(super)
            parse_quote!(pub(super))
        }
    }
}
