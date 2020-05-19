extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;

#[proc_macro_derive(ImplementHeight)]
pub fn derive_height(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    let name = &input.ident;

    /*
     * This is a pretty dumb macro that assumes the item
     * has a field called "height" whose value is the height in cm.
     */

    format!(
        "impl Height for {} {{
            fn height_in_metres(&self) -> f32 {{
                (self.height as f32) / 100.0
            }}
        }}",
        name
    ).parse().unwrap()
}
