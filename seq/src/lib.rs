use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use syn::{braced, parse::Parse, parse_macro_input, Ident, LitInt, Token};

struct Seq {
    placeholder: Ident,
    min: u64,
    max: u64,
    inclusive: bool,
    body: TokenStream2,
    with_repeat_patterns: bool,
}
impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let placeholder = input.parse()?;
        _ = input.parse::<Token![in]>()?;
        let min: LitInt = input.parse()?;
        let min = min.base10_parse()?;
        _ = input.parse::<Token![..]>()?;
        let inclusive = match input.parse::<Token![=]>() {
            Ok(_) => true,
            Err(_) => false,
        };
        let max: LitInt = input.parse()?;
        let max = max.base10_parse()?;

        let body_stream;
        let _ = braced!(body_stream in input);
        let body: TokenStream2 = body_stream.parse()?;

        let with_repeat_patterns = contains_repeat_patterns(body.clone());

        Ok(Self {
            placeholder,
            min,
            max,
            inclusive,
            body,
            with_repeat_patterns,
        })
    }
}

impl ToTokens for Seq {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        if self.with_repeat_patterns {
            self.expand_only_repeat_patterns(self.body.clone(), tokens);
        } else {
            for v in self.get_range() {
                let t_body = replace_place_holder(self.body.clone(), self.placeholder.clone(), v);
                tokens.extend(t_body);
            }
        }
    }
}
impl Seq {
    fn get_range(&self) -> std::ops::Range<u64> {
        if self.inclusive {
            self.min..(self.max + 1)
        } else {
            self.min..self.max
        }
    }
    fn expand_only_repeat_patterns(
        &self,
        src_tokens: TokenStream2,
        target_tokens: &mut TokenStream2,
    ) {
        let mut token_iter = src_tokens.into_iter();
        while let Some(tt) = token_iter.next() {
            match &tt {
                // Punct #, verify the next two
                TokenTree::Punct(punct) if punct.as_char() == '#' => {
                    match peek_next_two(&token_iter) {
                        Some((TokenTree::Group(group), TokenTree::Punct(end_punct)))
                            if group.delimiter() == Delimiter::Parenthesis
                                && end_punct.as_char() == '*' =>
                        {
                            // In that case we simply discard the next two tokens
                            _ = token_iter.next();
                            _ = token_iter.next().unwrap();

                            for v in self.get_range() {
                                let t_body = replace_place_holder(
                                    group.stream(),
                                    self.placeholder.clone(),
                                    v,
                                );
                                target_tokens.extend(t_body);
                            }
                            continue;
                        }
                        _ => (),
                    }
                }
                // Group => Recursively apply the logik
                TokenTree::Group(group) => {
                    let mut new_token_stream = TokenStream2::new();
                    self.expand_only_repeat_patterns(group.stream(), &mut new_token_stream);
                    let mut new_group = Group::new(group.delimiter(), new_token_stream);
                    new_group.set_span(group.span());
                    target_tokens.extend(Some(TokenTree::Group(new_group)).into_iter());
                    continue;
                }
                // Else nothing to do
                _ => (),
            };
            target_tokens.extend(Some(tt).into_iter());
        }
    }
}
fn replace_place_holder(tokens: TokenStream2, placeholder: Ident, value: u64) -> TokenStream2 {
    let mut token_iter = tokens.into_iter();
    let mut t_tokenstream = TokenStream2::new();

    while let Some(tt) = token_iter.next() {
        let new_tt = match tt {
            // Group => Recursively apply the logik
            TokenTree::Group(group) => {
                let mut new_group = Group::new(
                    group.delimiter(),
                    replace_place_holder(group.stream(), placeholder.clone(), value),
                );
                new_group.set_span(group.span());
                TokenTree::Group(new_group)
            }
            // N
            TokenTree::Ident(ident) if ident == placeholder => {
                let mut lit = Literal::u64_unsuffixed(value);
                lit.set_span(ident.span());
                TokenTree::Literal(lit)
            }
            // Can be :
            // - N
            // - Ident~N
            // - Something to forward as-is
            TokenTree::Ident(ident) => {
                // N
                if ident == placeholder {
                    let mut lit = Literal::u64_unsuffixed(value);
                    lit.set_span(ident.span());
                    TokenTree::Literal(lit)
                } else {
                    let is_ident_tilde_n =
                        peek_next_two(&token_iter).is_some_and(|(tt1, tt2)| match (tt1, tt2) {
                            (TokenTree::Punct(punct), TokenTree::Ident(ident))
                                if punct.as_char() == '~' && ident == placeholder =>
                            {
                                true
                            }
                            _ => false,
                        });
                    // Is it Ident~N ?
                    if is_ident_tilde_n {
                        // In that case we simply discard the next two tokens
                        _ = token_iter.next();
                        _ = token_iter.next().unwrap();

                        // And we construct a new Ident
                        let new_ident = Ident::new(&format!("{ident}{value}",), ident.span());
                        TokenTree::Ident(new_ident)
                    } else {
                        // Forward as-is
                        TokenTree::Ident(ident)
                    }
                }
            }
            // Else Forward as-is
            _ => tt,
        };
        t_tokenstream.extend(Some(new_tt).into_iter());
    }
    t_tokenstream
}

fn peek_next_two(tt_iter: &proc_macro2::token_stream::IntoIter) -> Option<(TokenTree, TokenTree)> {
    let mut peek = tt_iter.clone();
    match (peek.next(), peek.next()) {
        (Some(tt1), Some(tt2)) => Some((tt1, tt2)),
        _ => None,
    }
}

fn contains_repeat_patterns(stream: TokenStream2) -> bool {
    let mut token_iter = stream.into_iter();
    while let Some(tt) = token_iter.next() {
        match tt {
            // Punct #, verify the next two
            TokenTree::Punct(punct) if punct.as_char() == '#' => match peek_next_two(&token_iter) {
                Some((TokenTree::Group(group), TokenTree::Punct(end_punct)))
                    if group.delimiter() == Delimiter::Parenthesis
                        && end_punct.as_char() == '*' =>
                {
                    return true
                }
                _ => (),
            },
            // Group => Recursively apply the logik
            TokenTree::Group(group) => {
                if contains_repeat_patterns(group.stream()) {
                    return true;
                }
            }
            // Else nothing to do
            _ => (),
        }
    }
    false
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);
    quote! {#seq}.into()
}
