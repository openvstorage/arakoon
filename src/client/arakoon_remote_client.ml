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

open Lwt
open Protocol_common
open Statistics
open Arakoon_client

class remote_client ((ic,oc) as conn) =

  (object
    method exists ?(consistency=Consistent) key =
      request  oc (fun buf -> exists_to ~consistency buf key) >>= fun () ->
      response ic Llio.input_bool

    method get ?(consistency=Consistent) key = Protocol_common.get conn ~consistency key

    method set key value = Protocol_common.set conn key value

    method confirm key value =
      request  oc (fun buf -> confirm_to buf key value) >>= fun () ->
      response ic nothing

    method aSSert ?(consistency=Consistent) key vo =
      request oc (fun buf -> assert_to ~consistency buf key vo) >>= fun () ->
      response ic nothing

    method aSSert_exists ?(consistency=Consistent) key =
      request oc (fun buf -> assert_exists_to ~consistency buf key) >>= fun () ->
      response ic nothing

    method delete key =
      request  oc (fun buf -> delete_to buf key) >>= fun () ->
      response ic nothing

    method delete_prefix prefix = Protocol_common.delete_prefix conn prefix

    method range ?(consistency=Consistent) first finc last linc max =
      request oc (fun buf -> range_to buf ~consistency first finc last linc max)
      >>= fun () ->
      response ic Llio.input_string_list

    method range_entries ?(consistency=Consistent) ~first ~finc ~last ~linc ~max =
      request oc (fun buf -> range_entries_to buf ~consistency first finc last linc max)
      >>= fun () ->
      response ic Llio.input_kv_list

    method rev_range_entries ?(consistency=Consistent) ~first ~finc ~last ~linc ~max =
      request oc (fun buf -> rev_range_entries_to buf ~consistency first finc last linc max)
      >>= fun () ->
      response ic Llio.input_kv_list

    method prefix_keys ?(consistency=Consistent) pref max =
      request  oc (fun buf -> prefix_keys_to buf ~consistency pref max) >>= fun () ->
      response ic Llio.input_string_list

    method test_and_set key expected wanted =
      request  oc (fun buf -> test_and_set_to buf key expected wanted) >>= fun () ->
      response ic Llio.input_string_option

    method replace key wanted =
      request oc (fun buf -> replace_to buf key wanted) >>= fun () ->
      response ic Llio.input_string_option

    method user_function name po =
      request  oc (fun buf -> user_function_to buf name po) >>= fun () ->
      response ic Llio.input_string_option

    method user_hook ?(consistency=Consistent) name =
      request  oc (fun buf -> user_hook_to buf ~consistency name) >>= fun () ->
      Lwt.return (ic, oc)

    method multi_get ?(consistency=Consistent) keys =
      request  oc (fun buf -> multiget_to buf ~consistency keys) >>= fun () ->
      response ic
        (fun ic -> Llio.input_string_list ic >>= fun x ->
          Lwt.return (List.rev x))

    method multi_get_option ?(consistency=Consistent) keys =
      request oc (fun buf -> multiget_option_to buf ~consistency keys) >>= fun () ->
      response ic (Llio.input_list Llio.input_string_option)

    method sequence changes = Protocol_common.sequence conn changes

    method synced_sequence changes = Protocol_common.synced_sequence conn changes

    method who_master () = Protocol_common.who_master conn

    method expect_progress_possible () =
      request  oc (fun buf -> expect_progress_possible_to buf) >>= fun () ->
      response ic Llio.input_bool

    method statistics () =
      request oc (fun buf -> command_to buf STATISTICS) >>= fun () ->
      response ic
        (fun ic -> Llio.input_string ic >>= fun ss ->
          let s = Statistics.from_buffer (Llio.make_buffer ss 0) in
          Lwt.return s
        )

    method ping client_id cluster_id =
      request  oc (fun buf -> ping_to buf client_id cluster_id) >>= fun () ->
      response ic Llio.input_string

    method get_key_count () =
      request  oc (fun buf -> get_key_count_to buf ) >>= fun () ->
      response ic Llio.input_int64

    method get_cluster_cfgs () =
      Protocol_common.get_nursery_cfg conn

    method version () =
      Protocol_common.version conn

    method current_state () = Protocol_common.current_state conn
    method nop () = Protocol_common.nop conn
    method get_txid () = Protocol_common.get_txid conn

    method script (s:Joy.Language.program) =
      let is_update = Joy.Language.is_update s in
      Protocol_common.script conn is_update s

end: Arakoon_client.client )



let make_remote_client cluster connection =
  Protocol_common.prologue cluster connection >>= fun () ->
  let client = new remote_client connection in
  let ac = (client :> Arakoon_client.client) in
  Lwt.return ac
