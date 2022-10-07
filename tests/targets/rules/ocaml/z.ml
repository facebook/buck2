let ship_it =
{|              |    |    |
             )_)  )_)  )_)
            )___))___))___)\
           )____)____)_____)\\
         _____|____|____|____\\\__
---------\      SHIP IT      /---------
  ^^^^^ ^^^^^^^^^^^^^^^^^^^^^
    ^^^^      ^^^^     ^^^    ^^
         ^^^^      ^^^
 |} in

    (* Uncomment the following line to observe a compile error: 'W' is unbound
       because count_impl is not a direct dependency of ship_it.
    *)
    (*
    let _ = W.count [1, 2, 3] in
    *)
    Printf.printf "\n%34s\n%s\n" (Y.msg (X.count [1; 2; 3])) ship_it
