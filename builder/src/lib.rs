use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse, parse_macro_input, AngleBracketedGenericArguments, Attribute, DataStruct,
    DeriveInput, Field, GenericArgument, Ident, LitStr, Path, PathArguments, PathSegment, Token,
    Type, TypePath,
};

/// Take a ref on a [Type] and verify if it corresponds to the given `wrap_type_name`. If yes, it extracts the
/// inner [Type] and returns it in [Option::Some], else it returns [Option::None].
///
/// Works only for Types that accept ONE (and only one) generic args, such as [Option<T>], [Vec<T>], ...
///
/// # Returns
/// Returns [Some(Type)] with the inner Generic if `wrap_type_name` was matched, else return [None]
///
/// # Example
/// If `ty` is an Option<String>, extract_inner_type(ty, "Option") returns Some(String)
fn extract_inner_type(ty: &Type, wrap_type_name: &str) -> Option<Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
    }) = ty
    {
        for segment in segments.iter() {
            if let PathSegment {
                ident,
                arguments:
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
            } = segment
            {
                if ident.to_string() == wrap_type_name {
                    if let GenericArgument::Type(ty) = args[0].clone() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

enum BuilderArg {
    Each(Ident),
}
impl Parse for BuilderArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let param = input.parse::<Ident>()?;
        _ = input.parse::<Token![=]>()?;
        let each_name = input.parse::<LitStr>()?;
        let each_identifier = Ident::new(&each_name.value(), param.span());
        match param.to_string().as_str() {
            "each" => Ok(Self::Each(each_identifier)),
            _ => Err(syn::Error::new(
                param.span(),
                format!("Unknown builder param `{param}`",),
            )),
        }
    }
}

struct BuilderArgs(Vec<BuilderArg>);
impl Parse for BuilderArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut args = vec![];
        loop {
            args.push(input.parse::<BuilderArg>()?);
            if let Err(_) = input.parse::<Token![,]>() {
                break;
            }
        }
        Ok(Self(args))
    }
}
impl BuilderArgs {
    fn from_attribute(att: Attribute) -> syn::Result<Option<Self>> {
        match att.meta {
            syn::Meta::List(meta_list) if meta_list.path.is_ident("builder") => {
                meta_list.parse_args::<BuilderArgs>().map(|ba| Some(ba))
            }
            _ => Ok(None),
        }
    }
}

struct BasicField {
    ident: Ident,
    ty: Type,
}

enum BuildFieldType {
    Regular(BasicField),
    Optionnal(BasicField),
    Each {
        field: BasicField,
        builder_fct_ident: Ident,
    },
}

impl TryFrom<Field> for BuildFieldType {
    type Error = syn::Error;

    fn try_from(value: Field) -> Result<Self, Self::Error> {
        let Field {
            attrs, ident, ty, ..
        } = value;
        let ident = ident.expect("Tuple structs are not supported");
        for attr in attrs {
            if let Some(builder_args) = BuilderArgs::from_attribute(attr)? {
                for builder_arg in builder_args.0 {
                    match builder_arg {
                        BuilderArg::Each(builder_fct_ident) => {
                            if let Some(ty) = extract_inner_type(&ty, "Vec") {
                                return Ok(BuildFieldType::Each {
                                    field: BasicField { ident, ty },
                                    builder_fct_ident,
                                });
                            } else {
                                panic!("The `each` builder param can only be used on Vec<...>");
                            }
                        }
                    }
                }
            }
        }
        if let Some(ty) = extract_inner_type(&ty, "Option") {
            Ok(BuildFieldType::Optionnal(BasicField { ident, ty }))
        } else {
            Ok(BuildFieldType::Regular(BasicField { ident, ty }))
        }
    }
}

impl BuildFieldType {
    fn builder_field(&self) -> impl ToTokens {
        match self {
            BuildFieldType::Regular(BasicField { ident, ty })
            | BuildFieldType::Optionnal(BasicField { ident, ty }) => {
                quote! {#ident: ::core::option::Option<#ty>,}
            }
            BuildFieldType::Each {
                field: BasicField { ident, ty },
                ..
            } => quote! {#ident: ::core::option::Option<::std::vec::Vec<#ty>>,},
        }
    }
    fn builder_constructor_field(&self) -> impl ToTokens {
        match self {
            BuildFieldType::Regular(BasicField { ident, .. })
            | BuildFieldType::Optionnal(BasicField { ident, .. }) => {
                quote! {#ident: ::core::option::Option::None,}
            }
            BuildFieldType::Each {
                field: BasicField { ident, .. },
                ..
            } => quote! {#ident: ::core::option::Option::Some(vec![]),},
        }
    }
    fn builder_method(&self) -> impl ToTokens {
        match self {
            BuildFieldType::Regular(BasicField { ident, ty })
            | BuildFieldType::Optionnal(BasicField { ident, ty }) => quote! {
                pub fn #ident(&mut self, #ident: #ty ) -> &mut Self {
                    self.#ident = ::core::option::Option::Some(#ident);
                    self
                }
            },
            BuildFieldType::Each {
                field: BasicField { ident, ty },
                builder_fct_ident,
            } => quote! {
                pub fn #builder_fct_ident(&mut self, #builder_fct_ident: #ty ) -> &mut Self {
                    self.#ident.as_mut().unwrap().push(#builder_fct_ident);
                    self
                }
            },
        }
    }
    fn build_method_body(&self) -> impl ToTokens {
        match self {
            BuildFieldType::Regular(BasicField { ident, .. }) => {
                let error = LitStr::new(&format!("Field `{ident}` is not set"), ident.span());
                quote! {
                    #ident: self.#ident.take().ok_or_else(||#error)?,
                }
            }
            BuildFieldType::Optionnal(BasicField { ident, .. }) => quote! {
                #ident: self.#ident.take(),
            },
            BuildFieldType::Each {
                field: BasicField { ident, .. },
                ..
            } => quote! {
                #ident: self.#ident.take().unwrap(),
            },
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let DeriveInput {
        attrs: _,
        vis: _,
        ident,
        generics: _,
        data,
    } = parse_macro_input!(input as DeriveInput);

    let struct_name = ident;
    let struct_builder_name = format_ident!("{struct_name}Builder");

    let fields = match data {
        syn::Data::Struct(DataStruct { fields, .. }) => fields
            .into_iter()
            .map(BuildFieldType::try_from)
            .collect::<syn::Result<Vec<_>>>(),
        syn::Data::Enum(_) => unimplemented!("Only supports struct types"),
        syn::Data::Union(_) => unimplemented!("Only supports struct types"),
    };
    let fields = match fields {
        Ok(fields) => fields,
        Err(e) => return e.to_compile_error().into(),
    };
    let builder_fields_iter = fields.iter().map(|f| f.builder_field());

    let builder_constructor_fields_iter = fields.iter().map(|f| f.builder_constructor_field());

    let builder_methods_iter = fields.iter().map(|f| f.builder_method());

    let build_method_body_iter = fields.iter().map(|f| f.build_method_body());

    // Build the output
    let expanded = quote! {
        impl #struct_name {
            pub fn builder() -> #struct_builder_name {
                #struct_builder_name {
                    #( #builder_constructor_fields_iter)*
                }
            }
        }
        pub struct #struct_builder_name {
            #( #builder_fields_iter)*
        }
        impl #struct_builder_name {
            #( #builder_methods_iter)*

            pub fn build(&mut self) -> ::core::result::Result<#struct_name, ::std::boxed::Box<dyn ::core::error::Error>> {
                Ok(#struct_name {
                    #( #build_method_body_iter)*
                })
            }
        }
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}
