all: machine-c machine-nim machine-rust #machine-fs

machine-c:
	$(MAKE) -C C

machine-fs:
	$(MAKE) -C F#

machine-nim:
	$(MAKE) -C Nim

machine-rust:
	$(MAKE) -C Rust
