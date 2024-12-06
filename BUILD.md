# Build commands

(WIP)

### Force remove jump table list at the beginning of the function (and also reduce the size)
```
clang++ -std=c++2b fubuki.cpp -o fubuki.exe -g -gcodeview -Xlinker /INCREMENTAL:NO
```

### Link with DLL (no static)

```
clang-cl -std:c++latest -MD fubuki.cpp /O1 -o fubuki.exe /link /INCREMENTAL:NO    
```

### No debug (disable assert)

```
clang++ -std=c++2b fubuki.cpp -o fubuki.exe -O3 -Oz -flto -fuse-ld=lld -fwhole-program-vtables -fvirtual-function-elimination -march=native -mtune=native -DNDEBUG
```

```
clang-cl -std:c++latest /arch:AVX2 -MD fubuki.cpp /O1 /Fefubuki.exe /DNDEBUG /link /INCREMENTAL:NO
```

### Coverage

```
clang++ -std=c++2b fubuki.cpp -o fubuki.exe -O3 -Oz -flto -fuse-ld=lld -fwhole-program-vtables -fvirtual-function-elimination -march=native -mtune=native -fprofile-arcs -ftest-coverage -fprofile-instr-generate -fcoverage-mapping
```

### Profiling

```
rm *.gcda
rm *.gcno
rm *.profdata
rm *.profraw
$env:LLVM_PROFILE_FILE="fubuki.profraw"
./fubuki
llvm-profdata merge -sparse .\fubuki.profraw -o fubuki.profdata
llvm-cov show ./fubuki.exe -instr-profile="fubuki.profdata" -format=html -Xdemangle=msvcfilt | Out-File fubuki.report.html
```

### Optimization view

```
clang++ -std=c++2b fubuki.cpp -o fubuki.exe -O3 -flto -fuse-ld=lld -march=native -mtune=native -DNDEBUG "-Rpass-missed=.*" "-Rpass=.*" "-Rpass-analysis=.*" -fsave-optimization-record
python "D:\tools\optview2\opt-viewer.py" .\fubuki.opt.yaml
```

### Release

```
clang++ -std=c++2b fubuki.cpp -o fubuki.exe -O3 -flto -fuse-ld=lld -march=native -mtune=native -DNDEBUG
```
