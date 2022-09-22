#!/bin/sh
/opt/wasi-sdk/bin/clang -nostartfiles -nostdlib \
	-Wl,--no-entry -Wl,--allow-undefined -Wl,--export=update \
	bin/src/samples/wasm4_hello_from_penne.pn.ll \
	--target=wasm32-unknown-wasi \
	-o bin/hello_from_penne.wasm
