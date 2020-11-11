(package
 (name "foo")
 (cond-expand (chicken (cflags "-D" "FOO")
                       (ldflags "-lm"))))
