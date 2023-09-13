# lemu

mindustry Logic EMUlator.

## Usage

```rust
use lemu::Executor;
let mut lex = Executor::with_output(std::io::stdout()).program(r#"print "hello world""#).expect("program ok");
lex.run();
```