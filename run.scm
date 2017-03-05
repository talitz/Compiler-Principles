(load "./compiler.scm")
(compile-scheme-file "arch/test.scm" "test.c")
(system "gcc -o test test.c")
(exit)