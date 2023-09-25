use proc_macro::{Span, TokenStream};
use quote::quote;
use syn;

fn generate_ident(mut id: usize) -> syn::Ident {
    const ALPHABET: &[u8] = b"abcdefghijklmnopqrstuvwxyz";

    id += 1;
    let mut str = String::new();
    while id != 0 {
        let d = id % ALPHABET.len();
        id /= ALPHABET.len();

        str.push(ALPHABET[d].into());
    }
    syn::Ident::new(str.as_str(), Span::call_site().into())
}

fn name_of_field(field: &syn::Field) -> &syn::Ident {
    field
        .ident
        .as_ref()
        .expect("named fields in struct have names (duh)")
}

#[proc_macro_derive(Deserialize)]
pub fn deserialize_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    deserialize_derive_impl(&ast)
}

fn deserialize_derive_impl(ast: &syn::DeriveInput) -> TokenStream {
    if !ast.generics.params.is_empty() {
        panic!("Deserialize cannot be derived on generic types yet");
    }

    let name = &ast.ident;

    fn handle_fields(fields: &syn::Fields, types: &mut Vec<syn::Type>) -> proc_macro2::TokenStream {
        match fields {
            syn::Fields::Named(named) => {
                let fields: Vec<_> = named
                    .named
                    .pairs()
                    .map(|pair| {
                        let value = *pair.value();
                        types.push(value.ty.clone());
                        let name = name_of_field(value);
                        quote! {
                            #name: sender.auto()?
                        }
                    })
                    .collect();
                quote! {
                    { #(#fields,)* }
                }
            }
            syn::Fields::Unnamed(unnamed) => {
                for field in &unnamed.unnamed {
                    types.push(field.ty.clone());
                }
                let fields = vec![quote! { sender.auto()? }; unnamed.unnamed.len()];
                quote! {
                    ( #(#fields,)* )
                }
            }
            syn::Fields::Unit => quote! {},
        }
    }

    let mut types = Vec::new();

    let make = match &ast.data {
        syn::Data::Struct(s) => {
            let fields = handle_fields(&s.fields, &mut types);
            quote! {
                Ok(Self #fields)
            }
        }
        syn::Data::Enum(e) => {
            assert!(e.variants.len() < u8::MAX as usize);

            let arms: Vec<_> = e
                .variants
                .iter()
                .enumerate()
                .map(|(id, variant)| {
                    let id = id as u8;
                    let fields = handle_fields(&variant.fields, &mut types);
                    let variant_name = &variant.ident;
                    quote! {
                        #id => Ok(Self::#variant_name #fields)
                    }
                })
                .collect();

            types.push(syn::Type::Verbatim(quote! {u8}));
            let n_variants = e.variants.len();
            quote! {
                let variant: u8 = sender.auto()?;
                match variant {
                    #(#arms,)*
                    _ => Err(format!("cannot parse variant {} because variant id {variant} is out of range 0..{}", stringify!(#name), stringify!(#n_variants)).into()),
                }
            }
        }
        syn::Data::Union(_) => todo!("unions cannot be serialized"),
    };

    let types: Vec<_> = types
        .into_iter()
        .map(|ty| {
            quote! {
                #ty: Deserialize<Item>
            }
        })
        .collect();

    quote! {
        impl<Item> Deserialize<Item> for #name
        where
            #(#types,)*
        {
            fn deserialize<S: Sender<Item = Item> + ?Sized>(sender: &mut S) -> Result<Self, Error<S::Error>> {
                #make
            }
        }
    }.into()
}

#[proc_macro_derive(Serialize)]
pub fn serialize_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    serialize_derive_impl(&ast)
}

fn serialize_derive_impl(ast: &syn::DeriveInput) -> TokenStream {
    if !ast.generics.params.is_empty() {
        panic!("Deserialize cannot be derived on generic types yet");
    }

    let name = &ast.ident;

    fn handle_fields(
        fields: &syn::Fields,
        from_ident: impl Fn(&syn::Ident) -> proc_macro2::TokenStream,
        from_id: impl Fn(usize) -> proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        match fields {
            syn::Fields::Named(named) => named
                .named
                .pairs()
                .map(|pair| {
                    let internal = from_ident(name_of_field(pair.value()));
                    quote! {
                        receiver.auto(#internal)?
                    }
                })
                .collect(),
            syn::Fields::Unnamed(unnamed) => unnamed
                .unnamed
                .pairs()
                .enumerate()
                .map(|(id, _)| {
                    let internal = from_id(id);
                    quote! { receiver.auto(#internal)? }
                })
                .collect(),
            syn::Fields::Unit => Vec::new(),
        }
        .iter()
        .fold(quote! {}, |acc, item| quote! { #acc #item; })
    }

    fn make_captures(fields: &syn::Fields) -> proc_macro2::TokenStream {
        match fields {
            syn::Fields::Named(named) => {
                let internal = named
                    .named
                    .pairs()
                    .map(|pair| name_of_field(pair.value()))
                    .fold(quote! {}, |acc, item| quote! { #acc #item, });
                quote! {
                    { #internal }
                }
            }
            syn::Fields::Unnamed(unnamed) => {
                let internal = unnamed
                    .unnamed
                    .pairs()
                    .enumerate()
                    .map(|(id, _)| {
                        let ident = generate_ident(id);
                        quote! { #ident }
                    })
                    .fold(quote! {}, |acc, item| quote! { #acc #item, });
                quote! { ( #internal ) }
            }
            syn::Fields::Unit => quote! {},
        }
    }

    let mut types = Vec::new();

    let calls = match &ast.data {
        syn::Data::Struct(s) => {
            for field in &s.fields {
                types.push(field.ty.clone());
            }

            handle_fields(
                &s.fields,
                |ident| quote! { self.#ident },
                |id| {
                    let id = syn::Index::from(id);
                    quote! { self.#id }
                },
            )
        }
        syn::Data::Enum(e) => {
            assert!(e.variants.len() < u8::MAX as usize);
            let total: Vec<_> = e
                .variants
                .iter()
                .enumerate()
                .map(|(id, variant)| {
                    let id = id as u8;
                    let name = &variant.ident;
                    let captures = make_captures(&variant.fields);
                    let internal = handle_fields(
                        &variant.fields,
                        |ident| quote! { #ident },
                        |id| {
                            let pseudo_name = generate_ident(id);
                            quote! { #pseudo_name }
                        },
                    );

                    types.push(syn::Type::Verbatim(quote! {u8}));
                    for field in &variant.fields {
                        types.push(field.ty.clone());
                    }

                    quote! {
                        Self::#name #captures => { receiver.auto(#id)?; #internal }
                    }
                })
                .collect();

            quote! { match self { #(#total)* } }
        }
        syn::Data::Union(_) => todo!("unions cannot be serialized"),
    };

    let types: Vec<_> = types
        .into_iter()
        .map(|ty| {
            quote! {
                #ty: Serialize<Item>
            }
        })
        .collect();

    let on_ref = false; // TODO get from attributes or sth
    if on_ref {
        todo!();
    } else {
        quote! {
            impl<Item> Serialize<Item> for #name
            where
                #(#types,)*
            {
                fn serialize<R: Receiver<Item = Item> + ?Sized>(self, receiver: &mut R) -> Result<(), R::Error> {
                    #calls
                    Ok(())
                }
            }
        }
    }
    .into()
}
