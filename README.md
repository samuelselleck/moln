## About
Another programming language!

Made for fun :) Maybe for use for this or next years advent of code?

In very very early development:
- Has functions, expressions, declarations, conditionals, basic type checking.
- Lexer, parser, and simple semantic analysis steps exist - no execution yet.
- At the moment following Rust syntax very closely while setting up a basic
  functioning language, will most likely diverge over time.

Small example program showcasing a nice error message:
```rust
fn main() {
	let val = "world!";
	println("hello ", val);
}

// no builtins yet 
fn println(s: str, val: i64) {}

```
```
error[E]: type missmatch
  ┌─ test_file.ln:3:20
  │
3 │     println("hello ", val);
  │                       ^^^ expected type i64, found str

```

## Run
Debug print an AST: `cd moln && cargo run <file.pax>` - test files are available in `moln/test_files`

## Tests
`cd moln && cargo test` - runs all test files and verifies no errors occurred. No unit tests/fuzz tests yet.

## Project Structure
The main project is organized into these modules:
- `lexer`: Handles tokenization of the input source
- `parser`: Contains the main parsing logic
- `ast`: Defines the structure of the Abstract Syntax Tree
- `utils`: Provides utility functions and structures (e.g., MultiPeek iterator)
