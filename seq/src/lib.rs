extern crate proc_macro;
extern crate syn;

// @ 2:47:20

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse::{Parse, ParseStream, Result};
use syn::{braced, parse_macro_input, Token};

#[derive(Debug)]
struct SeqMacroInput {
    ident: syn::Ident,
    from: u64,
    to: u64,
    token_stream: proc_macro2::TokenStream,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: syn::Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let from = input
            .parse::<syn::LitInt>()
            .expect("failed to find number.")
            .base10_parse::<u64>()?;
        input.parse::<Token![..]>()?;
        let mut inclusive_range: Option<Token![=]> = None;
        if input.peek(Token![=]) {
            inclusive_range = Some(input.parse::<Token![=]>()?);
        }
        let mut to = input
            .parse::<syn::LitInt>()
            .expect("failed to find number.")
            .base10_parse::<u64>()?;
        let content;
        let _braces = braced!(content in &input);
        let token_stream = TokenStream2::parse(&content)?;
        if inclusive_range.is_some() {
            to += 1;
        }

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
        self.expand_outer(self.token_stream.clone())
    }
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    ReplaceIdent(u64),
    ReplaceSequence,
}

impl SeqMacroInput {
    fn expand_pass(&self, stream: TokenStream2, mode: Mode) -> (TokenStream2, bool) {
        let mut out = proc_macro2::TokenStream::new();
        let mut mutated = false;
        let mut tts = stream.into_iter();
        while let Some(some_tree) = tts.next() {
            out.extend(self.expand_repetitions(some_tree, &mut tts, &mut mutated, mode.clone()));
        }
        (out, mutated)
    }

    fn expand_outer(&self, stream: TokenStream2) -> TokenStream2 {
        // expand
        let (out, mutated) = self.expand_pass(stream.clone(), Mode::ReplaceSequence);
        if mutated {
            return out;
        }
        (self.from..self.to)
            .map(|num| self.expand_pass(self.token_stream.clone(), Mode::ReplaceIdent(num)))
            .map(|(ts, _)| ts)
            .collect()
    }

    fn expand_repetitions(
        //expanded pass2
        &self,
        tree: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mutated: &mut bool,
        mode: Mode,
    ) -> TokenStream2 {
        let ret_tree = match tree {
            proc_macro2::TokenTree::Group(group) => {
                let (expanded, g_mutated) = self.expand_pass(group.stream(), mode);

                let mut expanded = proc_macro2::Group::new(group.delimiter(), expanded);
                *mutated |= g_mutated;
                expanded.set_span(group.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ref seq_ident) if seq_ident == &self.ident => {
                if let Mode::ReplaceIdent(num) = mode {
                    let mut lit = proc_macro2::Literal::u64_unsuffixed(num);
                    lit.set_span(seq_ident.span());
                    *mutated = true;
                    proc_macro2::TokenTree::Literal(lit)
                } else {
                    proc_macro2::TokenTree::Ident(seq_ident.clone())
                }
            }
            proc_macro2::TokenTree::Ident(mut seq_ident) => {
                let mut peek = rest.clone();
                match (mode, peek.next(), peek.next()) {
                    (
                        Mode::ReplaceIdent(num),
                        Some(proc_macro2::TokenTree::Punct(punct)),
                        Some(proc_macro2::TokenTree::Ident(nested_ident)),
                    ) if punct.as_char() == '~' && nested_ident == self.ident => {
                        seq_ident = proc_macro2::Ident::new(
                            &format!("{}{}", seq_ident, num),
                            seq_ident.span(),
                        );
                        *rest = peek;
                        *mutated = true;
                    }
                    _ => {}
                }
                proc_macro2::TokenTree::Ident(seq_ident)
            }
            proc_macro2::TokenTree::Punct(punct) if punct.as_char() == '#' => {
                if let Mode::ReplaceSequence = mode {
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (
                            Some(proc_macro2::TokenTree::Group(rep)),
                            Some(proc_macro2::TokenTree::Punct(star)),
                        ) if rep.delimiter() == proc_macro2::Delimiter::Parenthesis
                            && star.as_char() == '*' =>
                        {
                            *mutated = true;
                            *rest = peek;
                            return (self.from..self.to)
                                .map(|num| self.expand_pass(rep.stream(), Mode::ReplaceIdent(num)))
                                .map(|(ts, _)| ts)
                                .collect();
                        }
                        _ => {}
                    }
                }
                proc_macro2::TokenTree::Punct(punct)
            }
            stream => stream,
        };
        std::iter::once(ret_tree).collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: TokenStream2 = input.into();
    output.into()
}
