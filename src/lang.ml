(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)
type exp =
| EInt of int
| EAdd of exp * exp

let rec interpret (e:exp) : int =
  match e with
  | EInt n        -> n
  | EAdd (e1, e2) -> interpret e1 + interpret e2
