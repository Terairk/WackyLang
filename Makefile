all:
	cargo build --release
	cp target/release/wacc_syntax compile

clean:
	cargo clean
	rm -f compile

.PHONY: all clean
