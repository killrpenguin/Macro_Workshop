extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse::{Parse, ParseStream, Result};
use syn::{braced, parse_macro_input, Token};

#[derive(Debug)]
struct SeqMacroInput {
    ident: syn::Ident,
    from: (syn::LitInt, u64),
    to: (syn::LitInt, u64),
    token_stream: proc_macro2::TokenStream,
}

fn lit_range(input: &ParseStream) -> Option<(syn::LitInt, u64)> {
    if let Ok(syn::Lit::Int(number)) = input.parse() {
        Some((
            number.clone(),
            number
                .base10_parse::<u64>()
                .expect("Failed to find int in acceptable range."),
        ))
    } else {
        None
    }
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?;
        let _in = input.parse::<Token![in]>()?;
        let from = lit_range(&input).unwrap();
        let _dots = input.parse::<Token![..]>()?;
        let to = lit_range(&input).unwrap();
        let content;
        let _braces = braced!(content in &input);
 
        let token_stream = TokenStream2::parse(&content)?;

        Ok(SeqMacroInput {
            ident,
            from,
            to,
            token_stream,
        })
    }
}

impl Into<proc_macro2::TokenStream> for SeqMacroInput {
    fn into(self) -> proc_macro2::TokenStream {
        (self.from.1..self.to.1)
            .map(|num| self.expand(self.token_stream.clone(), num))
            .collect::<TokenStream2>()
            .into()
    }
}

impl SeqMacroInput {
    fn expand(&self, stream: TokenStream2, num: u64) -> TokenStream2 {
        stream
            .into_iter()
            .map(|tt| {
                self.expand_inner(tt, num)
            })
            .collect()
    }

    fn expand_inner(&self, stream: proc_macro2::TokenTree, num: u64) -> proc_macro2::TokenTree {
        match stream {
            proc_macro2::TokenTree::Group(group) => {
                let mut expanded =
                    proc_macro2::Group::new(group.delimiter(), self.expand(group.stream(), num));
                expanded.set_span(group.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ref seq_ident) if seq_ident == &self.ident => {
                let mut lit = proc_macro2::Literal::u64_unsuffixed(num);
                lit.set_span(seq_ident.span());
                proc_macro2::TokenTree::Literal(lit)
            }
            stream => stream,
        }
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: TokenStream2 = input.into();
    output.into()
}
