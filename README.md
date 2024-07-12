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
define i32 @main() {
%entry:
    jmp label %main

%main:
    a = i32 0
    b = i32 10
    total = i32 0
    jmp label %loop

%loop:
    t0 = icmp ule a, b
    br t0, label %if.then, label %if.else

%if.then:
    total = add total, i32 1
    a = add a, i32 1
    jmp label %loop

%if.else:
    jmp label %cont

%cont:
    t1 = icmp eq total, i32 55
    call assert(t1)
    ret i32 0
}

```

Run the `ssa` pass to convert the program to static single assignment (SSA) form

```shell
$ obx build -e Main -p ./examples/basics --emit-ir --opt ssa
```
```
define i32 @main() {
%entry:
    jmp label %main

%main:
    a0 = i32 0
    b0 = i32 10
    total0 = i32 0
    jmp label %loop

%loop:
    a1 = phi [ a0, %main ], [ a2, %if.then ]
    total1 = phi [ total0, %main ], [ total2, %if.then ]
    t0 = icmp ule a1, b0
    br t0, label %if.then, label %if.else

%if.then:
    total2 = add total1, i32 1
    a2 = add a1, i32 1
    jmp label %loop

%if.else:
    jmp label %cont

%cont:
    t1 = icmp eq total1, i32 55
    call assert(t1)
    ret i32 0
}
```