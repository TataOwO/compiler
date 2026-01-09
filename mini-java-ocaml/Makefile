
all: minijava minijava.exe
	./minijava.exe --debug test.java
	gcc -no-pie -g test.s && ./a.out

minijava.exe:
	dune build minijava.exe

minijava: minijava.exe
	ln -sf minijava.exe minijava

clean:
	dune clean
	rm -f minijava

.PHONY: all clean minijava.exe
