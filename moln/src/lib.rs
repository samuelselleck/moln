mod error;
mod lexer;
mod parser;
mod semantic;
mod symbol_interning;
mod token_stream;
mod utils;

pub use parser::Parser;
pub use semantic::SemanticContext;

#[cfg(test)]
mod tests {
    use crate::{Parser, SemanticContext};

    /// Large test that runs all examples
    /// in the test_files directory
    #[test]
    fn test_examples() {
        let test_files = std::fs::read_dir("./test_files").unwrap();
        let mut results = vec![];
        for dir_entr in test_files {
            let path = dir_entr.unwrap().path();
            let contents = std::fs::read_to_string(&path).unwrap();
            let ast = Parser::new(&contents)
                .vadermoln()
                .and_then(|ast| SemanticContext::new().semantic_analysis(ast));
            if let Err(e) = &ast {
                eprintln!(
                    "[{:?}] couldn't parse: {:?}",
                    path,
                    e.print_with_file(&path.to_string_lossy().into_owned(), &contents)
                );
            }
            if let Err(err) = ast {
                results.push((path, format!("{:?}", err)));
            }
        }
        if results.is_empty() {
            return;
        }

        panic!("one or more vadermoln example files failed to parse, run test with -- --nocapture")
    }
}
