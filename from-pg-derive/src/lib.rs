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

    let field_infos: Vec<_> = item_fields
        .iter()
        .map(|field| {
            let fd = FieldDeserializer::from(field)?;
            let field_name = field
                .ident
                .as_ref()
                .ok_or(syn::Error::new(field.span(), "fields must be named"))?;

            Ok(match fd {
                Some(FieldDeserializer::Custom { ty, func }) => FieldInfo::Custom {
                    name: field_name.clone(),
                    ty,
                    func,
                },
                Some(FieldDeserializer::Derive) => {
                    let (derived_ty, is_option) = extract_option_inner_type(&field.ty);
                    FieldInfo::Derive {
                        name: field_name.clone(),
                        ty: derived_ty,
                        is_option,
                    }
                }
                _ => FieldInfo::Default {
                    name: field_name.clone(),
                },
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let field_deserializations = field_infos
        .iter()
        .map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, ty, func } => quote! {
                    row
                        .try_get::<_,#ty>(conf.#name.as_str())
                        .map_err(|e| Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                        .and_then(|val| #func(val).map_err(|e| e.into()))
                },
                FieldInfo::Derive { name, ty, is_option } => {
                    if *is_option {
                        quote! {
                            {
                                // Helper type to detect null values without casting to a specific type
                                // It always succeeds for non-null values, and always fails for null/missing
                                struct AnySql;
                                impl tokio_postgres::types::FromSql<'_> for AnySql {
                                    fn from_sql(
                                        _type_: &tokio_postgres::types::Type,
                                        _raw: &[u8]
                                    ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
                                        Ok(AnySql)
                                    }
                                    
                                    fn accepts(_type_: &tokio_postgres::types::Type) -> bool {
                                        true
                                    }
                                }
                                
                                let has_non_null_fields = conf.#name.fields().iter().any(|&field_name| {
                                    row.try_get::<_, AnySql>(field_name).is_ok()
                                });
                                
                                if has_non_null_fields {
                                    match <#ty as FromPg>::from_pg(row, &conf.#name) {
                                        Ok(val) => Ok(Some(val)),
                                        Err(e) => Err(Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                                    }
                                } else {
                                    Ok(None)
                                }
                            }
                        }
                    } else {
                        quote! {
                            <#ty as FromPg>::from_pg(row, &conf.#name)
                                .map_err(|e| Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                        }
                    }
                },
                FieldInfo::Default { name, .. } => quote! {
                    row
                        .try_get(conf.#name.as_str())
                        .map_err(|e| Box::new(e) as Box<(dyn std::error::Error + Send + Sync)>)
                },
            }
        })
        .collect::<Vec<_>>();

    let config_fields = field_infos
        .iter()
        .map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, .. } => quote! {
                    #name: String
                },
                FieldInfo::Derive { name, ty, .. } => quote! {
                    #name: <#ty as FromPg>::Config
                },
                FieldInfo::Default { name, .. } => quote! {
                    #name: String
                },
            }
        })
        .collect::<Vec<_>>();

    let default_fields = field_infos
        .iter()
        .map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, .. } => quote! {
                    #name: String::from(stringify!(#name))
                },
                FieldInfo::Derive { name, ty, .. } => quote! {
                    #name: <#ty as FromPg>::Config::default()
                },
                FieldInfo::Default { name, .. } => quote! {
                    #name: String::from(stringify!(#name))
                },
            }
        })
        .collect::<Vec<_>>();

    let getter_methods = field_infos
        .iter()
        .map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, .. } => quote! {
                    pub fn #name(&self) -> &str {
                        self.#name.as_ref()
                    }
                },
                FieldInfo::Derive { name, ty, .. } => quote! {
                    pub fn #name(&self) -> &<#ty as FromPg>::Config {
                        &self.#name
                    }
                },
                FieldInfo::Default { name, .. } => quote! {
                    pub fn #name(&self) -> &str {
                        self.#name.as_ref()
                    }
                },
            }
        })
        .collect::<Vec<_>>();

    // only include non-derived fields
    let fields_method = {
        let field_getters: Vec<_> = field_infos
            .iter()
            .filter_map(|field_info| match field_info {
                FieldInfo::Custom { name, .. } => Some(quote! { self.#name() }),
                FieldInfo::Default { name, .. } => Some(quote! { self.#name() }),
                FieldInfo::Derive { .. } => None, // Exclude derived fields
            })
            .collect();

        quote! {
            pub fn fields(&self) -> Vec<&str> {
                vec![#( #field_getters ),*]
            }
        }
    };

    let setter_methods = field_infos
        .iter()
        .map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, .. } => {
                    let set_method = format_ident!("set_{}", name);
                    quote! {
                        pub fn #set_method(self, name: String) -> Self {
                            Self {
                                #name: name,
                                ..self
                            }
                        }
                    }
                }
                FieldInfo::Derive { name, ty, .. } => {
                    let set_method = format_ident!("set_{}", name);
                    quote! {
                        pub fn #set_method(self, config: <#ty as FromPg>::Config) -> Self {
                            Self {
                                #name: config,
                                ..self
                            }
                        }
                    }
                }
                FieldInfo::Default { name, .. } => {
                    let set_method = format_ident!("set_{}", name);
                    quote! {
                        pub fn #set_method(self, name: String) -> Self {
                            Self {
                                #name: name,
                                ..self
                            }
                        }
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    let prefix_methods = field_infos
        .iter()
        .filter_map(|field_info| {
            match field_info {
                FieldInfo::Custom { name, .. } => {
                    let prefix_method = format_ident!("prefix_{}", name);
                    Some(quote! {
                        pub fn #prefix_method(self, prefix: String) -> Self {
                            let mut name = prefix.clone();
                            name.push_str(stringify!(#name));
                            Self {
                                #name: name,
                                ..self
                            }
                        }
                    })
                }
                FieldInfo::Derive { .. } => {
                    None
                }
                FieldInfo::Default { name, .. } => {
                    let prefix_method = format_ident!("prefix_{}", name);
                    Some(quote! {
                        pub fn #prefix_method(self, prefix: String) -> Self {
                            let mut name = prefix.clone();
                            name.push_str(stringify!(#name));
                            Self {
                                #name: name,
                                ..self
                            }
                        }
                    })
                }
            }
        }).collect::<Vec<_>>();

    Ok(quote! {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct #conf_name {
            #( #config_fields ),*
        }

        impl ::std::default::Default for #conf_name {
            fn default() -> Self {
                Self {
                    #( #default_fields ),*
                }
            }
        }

        impl #conf_name {
            pub fn new() -> Self {
                Self::default()
            }

            #( #getter_methods )*

            #( #setter_methods )*

            #( #prefix_methods )*

            #fields_method
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
                        #( #fields: #fields.err() ),*
                    })
                }
            }
        }
    })
}

enum FieldInfo {
    Custom {
        name: Ident,
        ty: Type,
        func: ExprPath,
    },
    Derive {
        name: Ident,
        ty: Type,
        is_option: bool,
    },
    Default {
        name: Ident,
    },
}

/// Extracts the inner type of an Option<T> and returns (Type, bool) where bool indicates if it was an Option
fn extract_option_inner_type(ty: &Type) -> (Type, bool) {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                        return (inner_ty.clone(), true);
                    }
                }
            }
        }
    }
    (ty.clone(), false)
}

enum FieldDeserializer {
    Custom { ty: Type, func: ExprPath },
    Derive,
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
/// Alternatively, you can write `#[frompg(derive = T)]` to use FromPg instead of FromSql.
impl syn::parse::Parse for FieldDeserializer {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first_token: Ident = input.parse()?;

        match first_token.to_string().as_str() {
            "from" => {
                input.parse::<Token![=]>()?;
                let ty: Type = input.parse()?;
                input.parse::<Token![,]>()?;
                let func_token: Ident = input.parse()?;
                if func_token != "func" {
                    return Err(syn::Error::new(func_token.span(), "expected `func`"));
                }
                input.parse::<Token![=]>()?;
                let func: ExprPath = input.parse()?;
                Ok(Self::Custom { ty, func })
            }
            "derive" => {
                input.parse::<Token![=]>()?;
                let _ty: Type = input.parse()?;
                Ok(Self::Derive)
            }
            _ => Err(syn::Error::new(
                first_token.span(),
                "expected `from` or `derive`",
            )),
        }
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
