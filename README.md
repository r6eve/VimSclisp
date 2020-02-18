# VimSclisp

Lisp implementation in VimScript.

## Usage

```
$ vim vim_sclisp.vim

:so %

:call REPL()

> (car '(a b c))
a
> (cdr '(a b c))
(b c)
> (cons 1 (cons 2 (cons 3 ())))
(1 2 3)
> (defun fact (n) (if (eq n 0) 1 (* n (fact (- n 1)))))
fact
> (fact 10)
3628800
> (defun fib (n) (if (eq n 1) 1 (if (eq n 0) 1 (+ (fib (- n 1)) (fib (- n 2))))))
fib
> (fib 12)
233
> (defun gen (n) (lambda (m) (setq n (+ n m))))
gen
> (setq x (gen 100))
<expr>
> (x 10)
110
> (x 90)
200
> (x 300)
500
```

## Reference

[LISP Implementation Advent Calendar](https://atnd.org/events/58967)

## License

Copyright © 2020 r6eve

Licensed under the [Boost Software License - Version 1.0](LICENSE_1_0.txt)
