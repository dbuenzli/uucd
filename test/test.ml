(*---------------------------------------------------------------------------
   Copyright (c) 2012 The uucd programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let ucd_or_die inf =
  try
    let ic = if inf = "-" then stdin else open_in inf in
    let d = Uucd.decoder (`Channel ic) in
    match Uucd.decode d with
    | `Ok db -> db
    | `Error e ->
        let (l0, c0), (l1, c1) = Uucd.decoded_range d in
        Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
        exit 1
  with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let ucd_from_marshaled : string -> Uucd.t =
  fun inf -> Marshal.from_channel (open_in inf)

let main () =
  let usage = "test [ucd.xml]" in
  let inf = ref None in
  let anon_fun file = match !inf with
  | Some _ -> raise (Arg.Bad ("Don't now what to do with " ^ file))
  | None -> inf := Some file
  in
  Arg.parse [] anon_fun usage;
  let inf = Option.value ~default:"-" !inf in
  Marshal.to_channel stdout (ucd_or_die inf) []

let () = if !Sys.interactive then () else main ()
