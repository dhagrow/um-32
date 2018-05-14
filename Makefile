all: machine

fsharp:
	fsharp bash -c "fsharpc machine.fs && mono ./machine.exe"
