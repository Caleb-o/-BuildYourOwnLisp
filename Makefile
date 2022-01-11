#!/bin/make
OUT		:=byol
OPS		:=-Wall
REL_OPS	:=-pedantic -Werror -O3
SRC		:=$(shell find src -type f -iregex ".*\.c")

.DEFAULT_GOAL = debug


debug:
	gcc -g $(OPS) $(SRC) -o bin/debug_$(OUT)
	@echo Done

release:
	gcc $(REL_OPS) $(OPS) $(SRC) -o bin/$(OUT)
	@echo Done