# dextool-mcdc
**Dextool-Mcdc** is a a dextool plugin for generating test cases with MC/DC coverage from C++ code. Test cases are found using incremental SAT solving with Z3.

# Dependencies
* Dextool (https://github.com/joakim-brannstrom/dextool)
* Z3 (https://github.com/Z3Prover/z3)
* python-pip & pip compdb
* Ubuntu 18.1+ (Recommended)

# Installation
1. Clone this repository
2. Install Dextool _dependencies_:
```sh
sudo apt install build-essential cmake llvm-4.0 llvm-4.0-dev clang-4.0 libclang-4.0-dev libsqlite3-dev ldc dub
```
3. Install Z3:
```sh
git clone https://github.com/Z3Prover/z3
cd z3
python scripts/mk_make.py --python
cd build
make
sudo make install
```
4. Stand in root directory and compile dextool:
```sh
make
```

* Newer versions of llvm and clang can be used.

5. Install compdb:
```sh
sudo apt install python-pip && pip install compdb
```

# Usage
For projects with CMake, generate a compilation database with CMake and use compdb to generate a new compilation database with headers:

```sh
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
compdb -p src/build/ list > compile_commands.json
```

You can now run the plugin on your project like:
```sh
dextool-mcdc --compile-db compile_commands.json
```
