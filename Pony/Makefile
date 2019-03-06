all: machine

machine: machine.pony
	sudo docker run --rm -v ${CURDIR}:/src/main ponylang/ponyc ponyc -b machine

clean:
	rm -f machine
