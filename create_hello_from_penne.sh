#!/bin/sh
/opt/wasi-sdk/bin/clang -nostartfiles -nostdlib \
	-Wl,--allow-undefined -Wl,--import-memory \
	-Wl,--no-entry -Wl,--export=update \
	bin/src/samples/wasm4_hello_from_penne.pn.ll \
	--target=wasm32-unknown-wasi \
	-o bin/hello_from_penne.wasm
