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
The program below is in the `Main.obx` file in `/examples/if-else`.
```
module Main
    var a, b, max: integer

begin
    a := 5
    b := 10

    if a > b then
        max := a
    else
        max := b
    end

    assert(max = 10)
end Main
```

```shell
$ obx build -e Main -p ./examples/if-else --emit-ir --opt mem2reg
```
Output:
```
define i32 @main() {
%entry:
    br label %main

%main:
    %2 = icmp ugt i32 5, 10
    br i1 %2, label %if.then, label %if.else

%if.then:
    br label %cont

%if.else:
    br label %cont
    
%cont:
    %max = phi i32 [ 5, %if.then ], [ 10, %if.else ]
    %6 = icmp eq i32 %max, 10
    %assert = call void assert(i1 %6)
    ret i32 0
}
```