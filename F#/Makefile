all: machine.fs
	fsharpc -o machine-fs machine.fs

machine-fs-docker:
	fsharp bash -c "fsharpc machine.fs && mono ./machine.exe"
