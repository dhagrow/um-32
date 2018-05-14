all: machine-c machine-fs

machine-c: machine.c
	gcc -o machine-c machine.c

machine-fs: machine.fs
	fsharpc -o machine-fs machine.fs

machine-fs-docker:
	fsharp bash -c "fsharpc machine.fs && mono ./machine.exe"
