# clang14-toolchain.cmake
set(CMAKE_C_COMPILER /opt/homebrew/opt/llvm@14/bin/clang)
set(CMAKE_CXX_COMPILER /opt/homebrew/opt/llvm@14/bin/clang++)

set(CMAKE_C_FLAGS "-fcolor-diagnostics" CACHE STRING "" FORCE)
set(CMAKE_CXX_FLAGS "-fcolor-diagnostics -stdlib=libc++" CACHE STRING "" FORCE)
