# Obx - Oberon+ Compiler
Obx is an implementation of the [Oberon+](https://oberon-lang.github.io/) compiler, currently targeting
RISC-V (rv64imafd) on Linux.

## Get Started
Obx is implemented in Go. You should have recent Go toolchain (Go >= 1.18) installed on your system. You can download
and install Go from the [website here](https://go.dev/doc/install).

### Build
```shell
# clone the project repository
$ git clone https://github.com/anthonyabeo/obx.git 

# change into the project directory root
$ cd obx

# run tests, build and install the executable 
$ go test ./... -v
$ go install cmd/obx.go
```
The executable should be installed in your `$GOBIN` directory. If you have not set the path for `$GOBIN`
do so before running the `install` command.


### Usage
```
$ obx -h

Usage:
  obx [command]

Available Commands:
  build       compile module or definition (along with its dependencies) into an object file
  completion  Generate the autocompletion script for the specified shell
  help        Help about any command

Flags:
  -h, --help   help for obx

Use "obx [command] --help" for more information about a command.
```

### Example
The program below is in the `examples/basics/Main.obx` file.
```
MODULE LoopTest;
    VAR x, y: INTEGER;

    BEGIN
        x := 0;
        y := 20;
        WHILE x < y DO
            x := x + 1
        END
        printf("Final x: %d\n", x)
END LoopTest.
```

```shell
$ obx build -S -p examples/loop 
```
Output:
```
	.section .bss
	.align 2
x: .skip 4

	.align 2
y: .skip 4

	.section .rodata
str_const_0: .string "Final x: %d\n"

	.section .text
	.align 2
	.globl main
	.type main, @function
main:
	addi sp, sp, -48
	sd fp, 40(sp)
	sd ra, 32(sp)
	sd s11, 24(sp)
	sd s10, 16(sp)
	sd s9, 8(sp)
	addi fp, sp, 48
	li s11, 0
	la t0, x
	sw s11, 0(t0)
	li s11, 20
	la t1, y
	sw s11, 0(t1)
	j while_loop_0_1

while_loop_0_1:
	la t2, x
	lw s11, 0(t2)
	la t3, y
	lw s10, 0(t3)
	slt s9, s11, s10
	seqz s10, s9
	bne s10, x0, while_loop_0_exit_2
	j if_end_3

if_end_3:
	li s10, 1
	la t4, x
	lw s9, 0(t4)
	add s11, s9, s10
	la t5, x
	sw s11, 0(t5)
	j while_loop_0_1

while_loop_0_exit_2:
	la s11, str_const_0
	mv a0, s11
	la t6, x
	lw s11, 0(t6)
	mv a1, s11
	jal ra, printf
	mv s11, a0
	j __init_LoopTest_exit

__init_LoopTest_exit:
	ld s9, 8(sp)
	ld s10, 16(sp)
	ld s11, 24(sp)
	ld ra, 32(sp)
	ld fp, 40(sp)
	addi sp, sp, 48
	ret 
	.size main, .-main
```