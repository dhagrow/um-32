all: machine

machine: machine.fs
	fsharpc -o machine machine.fs

machine-fs-docker:
	fsharp bash -c "fsharpc machine.fs && mono ./machine.exe"

clean:
	rm -f machine FSharp.Core.dll
