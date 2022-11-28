
top: exe

run: _build/explore.exe
	./$<

exe: _build _build/explore.exe

_build/explore.exe: _build/explore.o
	@echo Linking
	g++ $^ -o $@

_build/%.o: src/%.C Makefile
	@echo Building $<
	gcc -Wall -Werror $< -c -o $@ -O1

_build: ; @mkdir -p $@
