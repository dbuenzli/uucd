open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let xmlm = B0_ocaml.libname "xmlm"
let uucd = B0_ocaml.libname "uucd"

(* Libraries *)

let uucd_lib =
  let srcs = [ `Dir ~/"src" ] and requires = [ xmlm ] in
  B0_ocaml.lib uucd ~doc:"Uucd library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [ `File ~/"test/test.ml" ] in
  let requires = [ uucd ] in
  B0_ocaml.exe "test" ~doc:"Test decoder" ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The uucd programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/uucd"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/uucd/doc/Uucd"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/uucd.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/uucd/issues"
    |> B0_meta.(add description_tags)
      ["unicode"; "database"; "decoder"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "xmlm", {||} ]
  in
  B0_pack.make "default" ~doc:"uucd package" ~meta ~locked:true @@
  B0_unit.list ()
