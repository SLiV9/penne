#!/bin/sh
/opt/wasi-sdk/bin/clang -nostartfiles -nostdlib \
	-Wl,--allow-undefined -Wl,--import-memory \
	-Wl,--initial-memory=65536 -Wl,--max-memory=65536 \
	-Wl,-z,stack-size=14752 -Wl,--stack-first \
	-Wl,--no-entry -Wl,--export=update \
	bin/src/samples/wasm4_hello_from_penne.pn.ll \
	--target=wasm32-unknown-unknown \
	-o bin/hello_from_penne.wasm
