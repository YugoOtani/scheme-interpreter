use proc_macro::TokenStream;
use proc_macro2;
use quote::quote;
use syn::{ext::IdentExt, parse_macro_input, DeriveInput};

#[proc_macro_derive(Print)]
pub fn print_derive(input: TokenStream) -> TokenStream {
    let input = &parse_macro_input!(input as DeriveInput);
    generate_print(input)
}

fn generate_print(derive_input: &DeriveInput) -> TokenStream {
    let name = &derive_input.ident;
    let print_impl = match &derive_input.data {
        syn::Data::Struct(s) => generate_print_struct(s),
        syn::Data::Enum(e) => generate_print_enum(e),
        syn::Data::Union(u) => generate_print_union(u),
    };
    let indent_impl = quote! {
        pub fn indent(n_indent:usize){
            for _ in 0..n_indent{
                print!("  ");
            }
        }

    };
    quote! {
        impl Print for #name {
            #print_impl
        }
        impl #name{
            #indent_impl
        }
    }
    .into()
}
fn generate_print_enum(e: &syn::DataEnum) -> proc_macro2::TokenStream {
    let type_name = stringify!(e);
    quote! {
        fn print(&self,n_indent:usize){
            Self::indent(n_indent);
            println!(#type_name);
        }
    }
}
fn generate_print_union(u: &syn::DataUnion) -> proc_macro2::TokenStream {
    let type_name = stringify!(u);
    quote! {
        fn print(&self,n_indent:usize){
            Self::indent(n_indent);
            println!(#type_name);
        }
    }
}
fn generate_print_struct(s: &syn::DataStruct) -> proc_macro2::TokenStream {
    /*let mut v = vec![];
    for field in &s.fields {
        v.push(quote! {
            self.#field.print(n_indent);
        })
    }*/
    let type_name = stringify!(s);
    quote! {
        fn print(&self,n_indent:usize){
            Self::indent(n_indent);
            println!(#type_name);
            //#(#v)*
        }
    }
}

/*pub trait Print {
    fn print(&self, n_indent: usize);
}

impl Print for usize {
    fn print(&self, n_indent: usize) {
        indent(n_indent);
        println!("{self}")
    }
}
pub fn indent(level: usize) {
    for _ in 0..level {
        print!("  ")
    }
}*/
