all: machine-c machine-d machine-fs machine-go machine-nim machine-rust
clean: machine-c-clean machine-d-clean machine-fs-clean machine-go-clean machine-nim-clean machine-rust-clean

machine-c:
	$(MAKE) -C C
machine-c-clean:
	$(MAKE) -C C clean

machine-d:
	$(MAKE) -C D
machine-d-clean:
	$(MAKE) -C D clean

machine-fs:
	$(MAKE) -C F#
machine-fs-clean:
	$(MAKE) -C F# clean

machine-go:
	$(MAKE) -C Go
machine-go-clean:
	$(MAKE) -C Go clean

machine-nim:
	$(MAKE) -C Nim
machine-nim-clean:
	$(MAKE) -C Nim clean

machine-rust:
	$(MAKE) -C Rust
machine-rust-clean:
	$(MAKE) -C Rust clean
