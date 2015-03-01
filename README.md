# mal - Make a Lisp

## Description

Mal is a Clojure inspired Lisp interpreter.

Mal is implemented in 27 different languages:

* Bash shell
* C
* C#
* Clojure
* CoffeeScript
* Erlang
* Forth
* Go
* Haskell
* Java
* Javascript ([Online Demo](http://kanaka.github.io/mal))
* Lua
* GNU Make
* mal itself
* MATLAB
* [miniMAL](https://github.com/kanaka/miniMAL)
* OCaml
* Perl
* PHP
* Postscript
* Python
* R
* Racket
* Ruby
* Rust
* Scala
* Visual Basic.NET


Mal is a [learning tool](process/guide.md). Each implementation of mal is separated into 11
incremental, self-contained (and testable) steps that demonstrate core
concepts of Lisp. The last step is capable of self-hosting (running
the mal implemenation of mal).

The mal (make a lisp) steps are:

* [step0_repl](process/guide.md#step0)
* [step1_read_print](process/guide.md#step1)
* [step2_eval](process/guide.md#step2)
* [step3_env](process/guide.md#step3)
* [step4_if_fn_do](process/guide.md#step4)
* [step5_tco](process/guide.md#step5)
* [step6_file](process/guide.md#step6)
* [step7_quote](process/guide.md#step7)
* [step8_macros](process/guide.md#step8)
* [step9_try](process/guide.md#step9)
* [stepA_mal](process/guide.md#stepA)


Mal was presented publicly for the first time in a lightning talk at
Clojure West 2014 (unfortunately there is no video). See
mal/clojurewest2014.mal for the presentation that was given at the
conference (yes the presentation is a mal program).

## Building/running implementations

### Bash 4

```
cd bash
bash stepX_YYY.sh
```

### C

The C implementation of mal requires the following libraries (lib and
header packages): glib, libffi6 and either the libedit or GNU readline library.

```
cd c
make
./stepX_YYY
```

### C# ###

The C# implementation of mal has been tested on Linux using the Mono
C# compiler (mcs) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the C# implementation.

```
cd cs
make
mono ./stepX_YYY.exe
```


### Clojure

```
cd clojure
lein with-profile +stepX trampoline run
```

### CoffeeScript

```
sudo npm install -g coffee-script
cd coffee
coffee ./stepX_YYY
```

### Erlang

```
cd eralng
escript stepX_YYY.erl
```

### Forth

```
cd forth
gforth stepX_YYY.fs
```

### Go

You Go implementation of mal requires that go is installed on on the
path. The implementation has been tested with Go 1.3.1.

```
cd go
make
./stepX_YYY
```


### Haskell

Install the Haskell compiler (ghc/ghci), the Haskell platform and
either the editline package (BSD) or the readline package (GPL). On
Ubuntu these packages are: ghc, haskell-platform,
libghc-readline-dev/libghc-editline-dev

```
cd haskell
make
./stepX_YYY
```


### Java 1.7

The Java implementation of mal requires maven2 to build.

```
cd java
mvn compile
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY
    # OR
mvn -quiet exec:java -Dexec.mainClass=mal.stepX_YYY -Dexec.args="CMDLINE_ARGS"
```

### Javascript/Node

```
cd js
npm update
node stepX_YYY.js
```

### Lua

Running the Lua implementation of mal requires lua 5.1 or later,
luarocks and the lua-rex-pcre library installed.

```
cd lua
make  # to build and link linenoise.so
./stepX_YYY.lua
```

### Mal

Running the mal implementation of mal involves running stepA of one of
the other implementations and passing the mal step to run as a command
line argument.

```
cd IMPL
IMPL_STEPA_CMD ../mal/stepX_YYY.mal

```

### GNU Make 3.81

```
cd make
make -f stepX_YYY.mk
```

### OCaml 4.01.0

```
cd ocaml
make
./stepX_YYY
```

### MATLAB

The MATLAB implementation of mal has been tested with MATLAB version
R2014a on Linux. Note that MATLAB is a commercial product. It should
be fairly simple to support GNU Octave once it support classdef object
syntax.

```
cd matlab
./stepX_YYY
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY();quit;"
    # OR with command line arguments
matlab -nodisplay -nosplash -nodesktop -nojvm -r "stepX_YYY('arg1','arg2');quit;"
```

### miniMAL

[miniMAL](https://github.com/kanaka/miniMAL) is small Lisp interpreter
implemented in less than 1024 bytes of JavaScript. To run the miniMAL
implementation of mal you need to download/install the miniMAL
interpreter (which requires Node.js).
```
# Download miniMAL itself
git clone https://github.com/kanaka/miniMAL ../miniMAL.git
export PATH=`pwd`/miniMAL.git:$PATH
# Now run mal implementated in miniMAL
cd miniMAL
miniMAL ./stepX_YYY
```

### Perl 5.8

For readline line editing support, install Term::ReadLine::Perl or
Term::ReadLine::Gnu from CPAN.

```
cd perl
perl stepX_YYY.pl
```


### PHP 5.3

The PHP implementation of mal requires the php command line interface
to run.

```
cd php
php stepX_YYY.php
```

### Postscript Level 2/3

The Postscript implementation of mal requires ghostscript to run. It
has been tested with ghostscript 9.10.

```
cd ps
gs -q -dNODISPLAY -I./ stepX_YYY.ps
```

### Python (2 or 3)

```
cd python
python stepX_YYY.py
```

### R

The R implementation of mal requires R (r-base-core) to run.

```
cd r
make libs  # to download and build rdyncall
Rscript stepX_YYY.r
```

### Racket (5.3)

The Racket implementation of mal requires the Racket
compiler/interpreter to run.

```
cd racket
./stepX_YYY.rb
```

### Ruby (1.9+)

```
cd ruby
ruby stepX_YYY.rb
```

### Rust (0.13)

The rust implementation of mal requires the rust compiler and build
tool (cargo) to build.

```
cd rust
# Need patched pcre lib (should be temporary)
git clone https://github.com/kanaka/rust-pcre cadencemarseille-pcre
cargo build
./target/stepX_YYY
```

### Scala ###

Install scala and sbt (http://www.scala-sbt.org/0.13/tutorial/Installing-sbt-on-Linux.html):

```
cd scala
sbt 'run-main stepX_YYY'
    # OR
sbt compile
scala -classpath target/scala*/classes stepX_YYY
```

### Visual Basic.NET ###

The VB.NET implementation of mal has been tested on Linux using the Mono
VB compiler (vbnc) and the Mono runtime (version 2.10.8.1). Both are
required to build and run the VB.NET implementation.

```
cd vb
make
mono ./stepX_YYY.exe
```



## Running tests

The are nearly 500 generic Mal tests (for all implementations) in the
`tests/` directory. Each step has a corresponding test file containing
tests specific to that step. The `runtest.py` test harness uses
pexpect to launch a Mal step implementation and then feeds the tests
one at a time to the implementation and compares the output/return
value to the expected output/return value.

To simplify the process of running tests, a top level Makefile is
provided with convenient test targets.

* To run all the tests across all implementations (be prepared to wait):

```
make test
```

* To run all tests against a single implementation:

```
make test^IMPL

# e.g.
make test^clojure
make test^js
```

* To run tests for a single step against all implementations:

```
make test^stepX

# e.g.
make test^step2
make test^step7
```

* To run a specifc step against a single implementation:

```
make test^IMPL^stepX

# e.g
make test^ruby^step3
make test^ps^step4
```

## License

Mal (make-a-lisp) is licensed under the MPL 2.0 (Mozilla Public
License 2.0). See LICENSE.txt for more details.

