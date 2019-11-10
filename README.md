Choice
===========

Choice monad. See
[the Haskell library](http://hackage.haskell.org/packages/archive/logict/0.2.3/doc/html/Control-Monad-Logic.html)
or [this paper](http://homes.sice.indiana.edu/ccshan/logicprog/LogicT-icfp2005.pdf)

See [online documentation](http://cedeela.fr/~simon/software/choice/Choice.html).

## Example

```ocaml
> #require "choice";;
> open Choice.Infix;;
> let c = Choice.lift2 (fun x y -> x,y)
  (Choice.of_list [1;2;3;4]) (Choice.of_list ["1"; "3"]);;
val c : (int * string) Choice.t = <abstr>
> let c2 = Choice.filter c (fun (x,y) -> x = int_of_string y);;
val c2 : (int * string) Choice.t = <abstr>
> let c3 = Choice.take 2 c2;;
val c3 : (int * string) Choice.t = <abstr>
> Choice.run_n 4 c3;;
- : (int * string) list = [(3, "3"); (1, "1")]
```

## Build

You need OCaml >= 4.00.1

    $ make
    $ make install

## License

The code is free, released under the BSD-2 license.
