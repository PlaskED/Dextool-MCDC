SOLVER=~/thesis/z3-python/solverz3.py
CMAKE_NORM="cmake .."
MAKE_FLAGS="sudo make dextool-cppmcdc -j8"
RUN_NORM="build/dextool-cppmcdc"

#CMAKE_DEBUG="cmake -Wdev -DCMAKE_BUILD_TYPE=Debug -DBUILD_TEST=ON .."
CMAKE_DEBUG="cmake -Wdev -DCMAKE_BUILD_TYPE=Debug .."
MAKE_FLAGS_DEBUG="sudo make dextool_debug-cppmcdc -j8"
RUN_DEBUG="build/dextool_debug-cppmcdc"

#build/dextool_debug-cppmcdc --ns solutio --compile-db compile_commands.json --out out --solver $SOLVER
#build/dextool_debug-cppmcdc --ns solutio --compile-db compile_commands.json --out SolutioCpp/Examples/McdcTest --solver $SOLVE
cd build && $MAKE_FLAGS_DEBUG && cd .. && $RUN_DEBUG --solver $SOLVER --ns Foo --in foo.cpp --in foo.hpp --out out && cat out/mcdc.cpp

#To generate a compilation database use 'cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..' for the source project
