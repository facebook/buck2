type t = {
  foo: int;
  bar: string;
}
[@@deriving record_selectors]

let (r: t) = { foo = 4; bar = "quux" }
let (): unit = Printf.printf "%d %s\n" (foo r) (bar r)
