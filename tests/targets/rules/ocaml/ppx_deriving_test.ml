module M = struct
  type t = T of int
  [@@deriving show, eq, ord]
end

let () = print_string (M.show (M.T 3))
