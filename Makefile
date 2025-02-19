all:
	cargo build --release
	cp target/release/compiler compile

clean:
	cargo clean
	rm -f compile

.PHONY: all clean
