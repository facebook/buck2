let try_finally ~(f : unit -> 'a Lwt.t) ~(finally : unit -> unit Lwt.t) :
    'a Lwt.t =
  let%lwt res =
    try%lwt
      let%lwt result = f () in
      Lwt.return result
    with
    | e ->
      let%lwt () = finally () in
      raise e
  in
  let%lwt () = finally () in
  Lwt.return res
