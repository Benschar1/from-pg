use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Data, DeriveInput, ExprPath, Field, Fields, Ident, Token, Type, parse::ParseStream,
    parse_macro_input, spanned::Spanned,
};

const MACRO_NAME: &'static str = "frompg";

/// For a type using the frompg macro, you can write `#[frompg(from = T, func = F)] above a field`.
/// This retrieves any valid attributes from a field, and can return an error.
#[proc_macro_derive(FromPg, attributes(frompg))]
pub fn frompg(_item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(_item as DeriveInput);

    let item_name = input.ident.clone();

    let new_struct = match input.data {
        Data::Struct(s) => from_pg_helper(&item_name, &s.fields),
        _ => Err(syn::Error::new(
            item_name.span(),
            "frompg only supports structs",
        )),
    }
    .unwrap_or_else(|e| e.into_compile_error());

    new_struct.into()
}

fn from_pg_helper(
    item_name: &Ident,
    item_fields: &Fields,
) -> syn::Result<proc_macro2::TokenStream> {
    let conf_name = format_ident!("{}Config", item_name.clone());
    let err_name = format_ident!("{}Error", item_name.clone());

    if item_fields.is_empty() {
        return Ok(quote! {
            impl FromPg for #item_name {
                type Config = ();
                type Error  = ::std::convert::Infallible;
                fn from_pg(
                    _: &::tokio_postgres::row::Row,
                    _: &()
                ) -> Result<Self, Self::Error> {
                    Ok(Self {})
                }
            }
        });
    }

    let fields = map_fields(item_fields, |s| s.to_string())?;
    let set_fields = map_fields(item_fields, |s| format!("set_{s}"))?;
    let prefix_fields = map_fields(item_fields, |s| format!("prefix_{s}"))?;

    let field_deserializations = item_fields
        .iter()
        .map(|field| {
            let fd = FieldDeserializer::from(field)?;
            let field_name = field
                .ident
                .as_ref()
                .ok_or(syn::Error::new(field.span(), "fields must be named"))?;

            Ok(match fd {
                Some(FieldDeserializer { ty, func }) => quote! {
                    row
                        .try_get::<_,#ty>(conf.#field_name.as_str())
                        .map_err(|e| Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                        .and_then(|val| #func(val).map_err(|e| e.into()))
                },
                _ => quote! {
                    row
                        .try_get(conf.#field_name.as_str())
                        .map_err(|e| Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                },
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote! {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct #conf_name {
            #(
                #fields: String
            ),*
        }

        impl ::std::default::Default for #conf_name {
            fn default() -> Self {
                Self {
                    #(
                        #fields: String::from(stringify!(#fields))
                    ),*
                }
            }
        }

        impl #conf_name {
            pub fn new() -> Self {
                Self::default()
            }

            #(
                pub fn #fields(&self) -> &str {
                    self.#fields.as_ref()
                }

                pub fn #set_fields(self, name: String) -> Self {
                    Self {
                        #fields: name,
                        ..self
                    }
                }

                pub fn #prefix_fields(self, prefix: String) -> Self {
                    let mut name = prefix.clone();
                    name.push_str(stringify!(#fields));
                    Self {
                        #fields: name,
                        ..self
                    }
                }
            )*
        }

        #[derive(Debug, Default)]
        pub struct #err_name {
            #(
                #fields: Option<Box<dyn ::std::error::Error + ::core::marker::Sync + ::core::marker::Send>>
            ),*
        }

        impl ::std::fmt::Display for #err_name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                let mut messages = Vec::new();
                #(
                    if let Some(ref err) = self.#fields {
                        messages.push(format!("{}: {}", stringify!(#fields), err));
                    }
                )*
                write!(f, "{}", messages.join("; "))
            }
        }

        impl ::std::error::Error for #err_name {}

        impl FromPg for #item_name {
            type Config = #conf_name;
            type Error = #err_name;

            fn from_pg(row: &::tokio_postgres::row::Row, conf: &Self::Config) -> Result<Self, Self::Error> {
                match ( #( #field_deserializations ),* ) {
                    ( #( Ok(#fields) ),* ) => Ok(Self {
                        #( #fields ),*
                    }),
                    ( #( #fields ),* ) => Err(Self::Error {
                        #(
                            #fields: #fields.err()
                        ),*
                    })
                }
            }
        }
    })
}

struct FieldDeserializer {
    pub ty: Type,
    pub func: ExprPath,
}

impl FieldDeserializer {
    fn from(field: &Field) -> syn::Result<Option<Self>> {
        field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident(MACRO_NAME))
            .map(|attr| {
                Ok(attr
                    .meta
                    .require_list()?
                    .parse_args::<FieldDeserializer>()?)
            })
            .transpose()
    }
}

/// For a type using the frompg macro, you can write `#[frompg(from = T, func = F)] above a field`.
/// This retrieves any valid attributes from a field, and can return an error.
impl syn::parse::Parse for FieldDeserializer {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let from_token: Ident = input.parse()?;
        if from_token != "from" {
            return Err(syn::Error::new(from_token.span(), "expected `from`"));
        }
        input.parse::<Token![=]>()?;
        let ty: Type = input.parse()?;
        input.parse::<Token![,]>()?;
        let func_token: Ident = input.parse()?;
        if func_token != "func" {
            return Err(syn::Error::new(func_token.span(), "expected `func`"));
        }
        input.parse::<Token![=]>()?;
        let func: ExprPath = input.parse()?;
        Ok(Self { ty, func })
    }
}

/// Maps the fields of a DataStruct using the given function.
/// Returns `None` if any fields are `None`.
fn map_fields(fields: &Fields, f: impl Fn(&str) -> String) -> syn::Result<Vec<Ident>> {
    fields
        .iter()
        .map(|field| {
            Ok(Ident::new(
                &f(&field
                    .ident
                    .as_ref()
                    .ok_or(syn::Error::new(fields.span(), "fields must be named"))?
                    .to_string()),
                field.ident.span(),
            ))
        })
        .collect()
}
