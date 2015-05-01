(* This code is in the public domain. *)

let ucd_or_die inf = try
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
  let argc = Array.length Sys.argv in
  let inf = if argc = 1 then "-" else Sys.argv.(1) in
  Marshal.to_channel stdout (ucd_or_die inf) []

let () = if (not !Sys.interactive) then  main ()
