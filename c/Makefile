
top: _build _build/again.exe

run: _build/again.exe
	./$< 500

_build/%.exe: _build/%.o
	@echo Linking
	g++ $^ -o $@

_build/%.o: src/%.C Makefile
	@echo Building $<
	gcc -Wall -Werror $< -c -o $@ -O1

_build: ; @mkdir -p $@
