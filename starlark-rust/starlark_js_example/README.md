# Starlark JS

This directory contains an example project making use of Starlark
WebAssembly/WASM. To try it:

```
rustup target add wasm32-unknown-unknown
cargo build --target wasm32-unknown-unknown --release
cp ../target/wasm32-unknown-unknown/release/starlark_js.wasm .
python -m http.server
```

Then visit [http://localhost:8000](http://localhost:8000).
