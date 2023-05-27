let print_hello () = A.print_hello ()

module B: module type of B = struct
  include B
 end
