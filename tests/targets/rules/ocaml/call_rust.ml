external add : int -> int -> int = "add"

let () = Printf.printf "%d + %d = %d\n" 42 8 (add 42 8)
