let with_tempdir f =
  let%lwt result =
    Lwt_utils.try_finally ~f ~finally:(fun () -> Lwt.return_unit)
  in
  Lwt.return result
