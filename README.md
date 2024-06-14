# Obx - Oberon+ Compiler
Obx is an implementation of the [Oberon+](https://oberon-lang.github.io/) compiler.

## Get Started
Obx is implemented in Go. You should have recent Go toolchain (Go >= 1.18) installed on your system. 

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
```shell
$ obx help

NAME:
   obx - Oberon+ Compiler

USAGE:
   obx [global options] command [command options] 

COMMANDS:
   build    build [flags]
   help, h  Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --help, -h  show help

```