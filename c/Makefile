
top: exe

run: _build/main.exe
	./$<

exe: _build/main.exe

_build/main.exe: _build/main.o
	@echo Linking
	g++ $^ -o $@

_build/%.o: src/%.C Makefile
	@echo Building $<
	gcc -Wall -Werror $< -c -o $@ -O1

_build: ; @mkdir -p $@
