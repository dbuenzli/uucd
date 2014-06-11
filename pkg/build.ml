#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let () = 
  Pkg.describe "uucd" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/uucd";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]



