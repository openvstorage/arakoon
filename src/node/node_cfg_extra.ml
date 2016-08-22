(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open Node_cfg
open Node_cfg

let validate_dirs t =
  let open Lwt in
  Logger.debug_ "Node_cfg.validate_dirs" >>= fun () ->
  if t.is_test then Lwt.return ()
  else
    begin
      let is_ok name =
        try
          let s = Unix.stat name in s.Unix.st_kind = Unix.S_DIR
        with _ -> false
      in
      let verify_exists dir msg exn =
        if not (is_ok dir)
        then
          begin
            Logger.fatal_f_ "%s '%s' doesn't exist, or insufficient permissions" msg dir >>= fun () ->
            Lwt.fail exn
          end
        else
          Lwt.return ()
      in

      let verify_writable dir msg exn =
        let handle_exn =
          (* Work-around: Logger macro doesn't accept an 'exn' argument not
           * called 'exn' *)
          let log exn =
            Logger.fatal_f_ ~exn "Failure while verifying %s '%s'" msg dir
          in
          function
          (* Set of exceptions we map to some specific exit code *)
          | Unix.Unix_error(Unix.EPERM, _, _)
          | Unix.Unix_error(Unix.EACCES, _, _)
          | Unix.Unix_error(Unix.EROFS, _, _)
          | Unix.Unix_error(Unix.ENOSPC, _, _) as e ->
             log e >>= fun () ->
             Lwt.fail exn
          | e ->
             log e >>= fun () ->
             Lwt.fail e
        in


        let safe_unlink fn =
          Lwt.catch
            (fun () -> File_system.unlink fn)
            handle_exn in

        let fn = Printf.sprintf "%s/touch-%d" dir (Unix.getpid ()) in

        let check () =
          safe_unlink fn >>= fun () ->
          let go () =
            Logger.debug_f_ "Touching %S" fn >>= fun () ->
            Lwt_unix.openfile fn
                              [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT; Lwt_unix.O_EXCL]
                              0o600 >>= fun fd ->

            Lwt.finalize
              (fun () ->
               Logger.debug_f_ "Write byte to %S" fn >>= fun () ->
               Lwt_unix.write fd "0" 0 1 >>= fun _ ->
               if t._fsync_tlog_dir
               then
                 begin
                   Logger.debug_f_ "Fsync %S" fn >>= fun () ->
                   Lwt_unix.fsync fd
                 end
               else
                 Lwt.return ())
              (fun () ->
               Lwt_unix.close fd)
          in
          Lwt.catch go handle_exn
        in

        Lwt.finalize check (fun () -> safe_unlink fn)
      in

      let verify dir msg exn =
        verify_exists dir msg exn >>= fun () ->
        verify_writable dir msg exn
      in

      verify t.home "Home dir" (InvalidHomeDir t.home) >>= fun () ->
      verify t.tlog_dir "Tlog dir" (InvalidTlogDir t.tlog_dir) >>= fun () ->
      verify t.tlx_dir "Tlx dir" (InvalidTlxDir t.tlx_dir) >>= fun () ->
      verify t.head_dir "Head dir" (InvalidHeadDir t.head_dir)

    end
