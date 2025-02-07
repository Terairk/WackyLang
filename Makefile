all:
	cargo build --release
	cp target/release/syntax compile

clean:
	cargo clean
	rm -f compile

.PHONY: all clean
