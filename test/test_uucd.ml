(*---------------------------------------------------------------------------
   Copyright (c) 2024 The uucd programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let test_decode () =
  Test.test "Uucd.decode" @@ fun () ->
  let cwd = Os.Dir.cwd () |> Test.get_ok ~__POS__ in
  let inf = Fpath.(cwd // v "test/ucd.xml") in
  let inf = Fpath.to_string inf in
  try
    In_channel.with_open_bin inf @@ fun ic ->
    let d = Uucd.decoder (`Channel ic) in
    match Uucd.decode d with
    | `Ok db ->
        let props = Uucd.Cpmap.find 0x0020 db.repertoire in
        Test.option ~__POS__ (Uucd.find props Uucd.general_category) (Some `Zs)
    | `Error e ->
        let (l0, c0), (l1, c1) = Uucd.decoded_range d in
        Test.failstop ~__POS__ "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e
  with
  | Sys_error e -> Test.failstop "%s" e ~__POS__

let main () =
  Test.main @@ fun () ->
  test_decode ();
  ()

let () = if !Sys.interactive then () else exit (main ())
