(load "./compiler.scm")
(compile-scheme-file "arch/test.scm" "arch/test.c")
(system "gcc -o arch/test arch/test.c")
(exit)