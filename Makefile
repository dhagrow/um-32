all: machine-c machine-fs machine-nim machine-rust
clean: machine-c-clean machine-fs-clean machine-nim-clean machine-rust-clean

machine-c:
	$(MAKE) -C C
machine-c-clean:
	$(MAKE) -C C clean

machine-fs:
	$(MAKE) -C F#
machine-fs-clean:
	$(MAKE) -C F# clean

machine-nim:
	$(MAKE) -C Nim
machine-nim-clean:
	$(MAKE) -C Nim clean

machine-rust:
	$(MAKE) -C Rust
machine-rust-clean:
	$(MAKE) -C Rust clean
