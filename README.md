# Obx - Oberon+ Compiler
Obx is an implementation of the [Oberon+](https://oberon-lang.github.io/) compiler.

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
module Main
    var a, b, total: integer

begin
    a := 0
    b := 10
    total := 0

    while a < b do
        total := total + 1
        a := a + 1
    end

    assert(total = 55)
end Main
```

```shell
$ obx build -e Main -p ./examples/basics --emit-ir
```
Output:
```
%Main:
    a := 0
    b := 10
    total := 0
    jmp label %loop

%loop:
    t0 := a < b
    br t0, label %if.then, label %if.else

%if.then:
    total := total + 1
    a := a + 1
    jmp label %loop

%if.else:
    jmp label %cont

%cont:
    t1 := total == 55
    call assert(t1)
```

Run the `ssa` pass to convert the program to static single assignment (SSA) form

```shell
$ obx build -e Main -p ./examples/basics --emit-ir --opt ssa
```
```
%Main:
    a0 := 0
    b0 := 10
    total0 := 0
    jmp label %loop

%loop:
    total1 := phi [ total2, %if.then ], [ total0, %Main ]
    a1 := phi [ a0, %Main ], [ a2, %if.then ]
    t0 := a1 < b0
    br t0, label %if.then, label %if.else

%if.then:
    total2 := total1 + 1
    a2 := a1 + 1
    jmp label %loop

%if.else:
    jmp label %cont

%cont:
    t1 := total1 == 55
    call assert(t1)
```