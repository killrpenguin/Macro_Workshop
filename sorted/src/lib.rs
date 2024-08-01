extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, visit_mut};

#[proc_macro_attribute]
pub fn sorted(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let input_stream: syn::Item = parse_macro_input!(item as syn::Item);
    if let Err(e) = sorted_impl::<(), syn::Error>(&input_stream) {
        out.extend(TokenStream::from(e.to_compile_error()));
    }
    out
}

fn sorted_impl<T, E>(input: &syn::Item) -> syn::Result<()> {
    match input {
        syn::Item::Enum(enum_variants) => {
            if !enum_variants.variants.is_empty() {
                let names: Vec<syn::Ident> = enum_variants
                    .variants
                    .iter()
                    .map(|variant| variant.ident.clone())
                    .collect();
                if let Err(e) = lexicographic_sort(names, enum_variants.variants.len()) {
                    return Err(e);
                } else {
                    return Ok(());
                }
            }
            return Ok(());
        }
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

#[proc_macro_attribute]
pub fn check(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut stream: syn::ItemFn = parse_macro_input!(item as syn::ItemFn);
    let mut lm = LexicographicMatch::default();
    lm.visit_item_fn_mut(&mut stream);
    let mut out = quote! { #stream };
    out.extend(lm.errors.into_iter().map(|err| err.to_compile_error()));
    out.into()
}

#[derive(Default, Debug)]
struct LexicographicMatch {
    errors: Vec<syn::Error>,
}

impl visit_mut::VisitMut for LexicographicMatch {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if node.attrs.iter().any(|expr| expr.path().is_ident("sorted")) {
            node.attrs.retain(|expr| !expr.path().is_ident("sorted"));
            let mut names = Vec::with_capacity(node.arms.len());
            for arm in node.arms.iter() {
                let path_segs =
                    get_arm_lh(&arm.pat).expect("Expected path in visit_expr_match_mut.");
                names.push(path_segs.clone());
            }
            let _ = lexicographic_sort(names, node.arms.len())
                .unwrap_or_else(|err| self.errors.push(err));
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
}

fn lexicographic_sort(idents: Vec<syn::Ident>, len: usize) -> syn::Result<()> {
    let mut sorted_list: Vec<_> = Vec::with_capacity(len);
    for ident in idents {
        let name = ident.to_string();
        if sorted_list
            .last()
            .map(|last_item| &name < &last_item)
            .unwrap_or(false)
        {
            let should_be = sorted_list
                .binary_search(&name)
                .expect_err("Expected binary search value in visit_expr_match_mut.");

            return Err(syn::Error::new(
                ident.span(),
                format!(
                    "{} should sort before {}",
                    name.to_string(),
                    sorted_list[should_be]
                ),
            ));
        }
        sorted_list.push(name);
    }
    Ok(())
}

fn get_arm_lh<'a>(arm: &'a syn::Pat) -> Option<&syn::Ident> {
    match arm {
        syn::Pat::Struct(pat_struct) => Some(&pat_struct.path.segments[0].ident),
        syn::Pat::TupleStruct(pat_tpl_struct) => Some(&pat_tpl_struct.path.segments[0].ident),
        syn::Pat::Path(pat_path) => Some(&pat_path.path.segments[0].ident),
        _ => None,
    }
}
