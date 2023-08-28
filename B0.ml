open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let uucd = B0_ocaml.libname "uucd"
let xmlm = B0_ocaml.libname "xmlm"

(* Libraries *)

let uucd_lib =
  let srcs = [ `Dir (Fpath.v "src") ] in
  let requires = [xmlm] in
  B0_ocaml.lib uucd ~doc:"Uucd library" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [`File (Fpath.v "test/test.ml")] in
  let requires = [ uucd ] in
  B0_ocaml.exe "test" ~doc:"Test decoder" ~srcs ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The uucd programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/uucd"
    |> add online_doc "https://erratique.ch/software/uucd/doc/Uucd"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/uucd.git"
    |> add issues "https://github.com/dbuenzli/uucd/issues"
    |> add description_tags ["unicode"; "database"; "decoder"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "xmlm", {||};
      ]

  in
  B0_pack.v "default" ~doc:"uucd package" ~meta ~locked:true @@
  B0_unit.list ()
