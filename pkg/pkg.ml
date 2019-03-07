#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let distrib =
  (* FIXME OPAMv2, move this to an x-unicode-version field in the opam file. *)
  let watermarks = ("UNICODE_VERSION", `String "12.0.0") :: Pkg.watermarks in
  Pkg.distrib ~watermarks ()

let () =
  Pkg.describe "uucd" ~distrib @@ fun c ->
  Ok [ Pkg.mllib ~api:["Uucd"] "src/uucd.mllib";
       Pkg.test ~run:false "test/test"; ]
