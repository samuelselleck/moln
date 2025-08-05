use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_quote, Block, Expr, ItemFn};

#[proc_macro_attribute]
pub fn token_context(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item_fn: ItemFn = syn::parse_macro_input!(item);
    let attrs: Expr = syn::parse_macro_input!(attr);

    // TODO instead look for an argument with
    // type signature &mut TokenIterator<...>
    // let Some(FnArg::Typed(PatType { pat, .. })) = item_fn.sig.inputs.first() else {
    //     panic!("not ident");
    // };
    // let Pat::Ident(PatIdent { ident, .. }) = pat.as_ref() else {
    //     panic!("not ident");
    // };
    let block = &item_fn.block;
    let new_block: Block = parse_quote!({
        // TODO use std::any::type_name::<#function name>();
        self.push_context(#attrs);
        let res = #block;
        self.pop_context();
        // println!("res: {:#?}", res);
        res
    });
    item_fn.block = Box::new(new_block);
    proc_macro::TokenStream::from(item_fn.to_token_stream())
}
