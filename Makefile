#
# Copyright (c) 2020, Yizhou Shan, UCSD.
#

# o Do not use make's built-in rules and variables
#   (this increases performance and avoids hard-to-debug behaviour);
# o Look for make include files relative to root of kernel src
MAKEFLAGS += -rR --include-dir=$(CURDIR)

# Do not print "Entering directory ...",
# but we want to display it when entering to the output directory
MAKEFLAGS += --no-print-directory

# Beautify output
ifeq ("$(origin V)", "command line")
  KBUILD_VERBOSE = $(V)
endif
ifndef KBUILD_VERBOSE
  KBUILD_VERBOSE = 0
endif

ifeq ($(KBUILD_VERBOSE), 1)
  Q =
else
  Q = @
endif
export Q KBUILD_VERBOSE

all:
	make -C src/verilog-lib/axis/rtl
	mkdir -p generated_pkts
	mkdir -p generated_rtl
	mkdir -p generated_rtl/apps
	mkdir -p generated_rtl/lib
	mkdir -p generated_rtl/supernic
	sbt "runMain supernic.CIGenerate"
	sbt "runMain supernic.CISimulation"

gen:
	sbt "runMain supernic.CIGenerate"

sim:
	sbt "runMain supernic.CISimulation"

clean:
	rm -rf target/
	rm -rf project/
	rm -rf *.log
	rm -rf generated_rtl/
	rm -rf generated_sim/
	rm -rf tmp/
	rm -rf .bsp/
