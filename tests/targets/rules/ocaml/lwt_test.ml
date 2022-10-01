let () =
  Lwt_main.run begin
      let%lwt () = Lwt_io.printl "Enter any phrase (then RET):" in
      let%lwt data = Lwt_io.(read_line stdin) in
      let%lwt () = Lwt_io.printl data in
      Lwt.return ()
    end
