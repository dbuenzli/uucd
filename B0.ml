open B0_kit.V000
open Result.Syntax

let unicode_version = 16, 0, 0, None (* Adjust on new releases *)

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let xmlm = B0_ocaml.libname "xmlm"
let uucd = B0_ocaml.libname "uucd"

(* Libraries *)

let uucd_lib =
  let srcs = [ `Dir ~/"src" ] and requires = [ xmlm ] in
  B0_ocaml.lib uucd ~doc:"Uucd library" ~srcs ~requires

(* Actions *)

let uc_base = "http://www.unicode.org/Public"

let download_ucdxml =
  let doc = "Download the Unicode character database to test/ucd.xml" in
  B0_unit.of_action "download-ucdxml" ~doc @@ fun env _ ~args:_ ->
  let* unzip = B0_env.get_cmd env (Cmd.arg "unzip") in
  let version = String.of_version unicode_version in
  let ucd_url = Fmt.str "%s/%s/ucdxml/ucd.all.grouped.zip" uc_base version in
  let ucd_file = B0_env.in_scope_dir env ~/"test/ucd.xml" in
  Result.join @@ Os.File.with_tmp_fd @@ fun tmpfile tmpfd ->
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" ucd_url Fpath.pp ucd_file);
  let* () = B0_action_kit.fetch_url env ucd_url tmpfile in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true ucd_file in
  Os.Cmd.run Cmd.(unzip % "-p" %% path tmpfile) ~stdout

let show_version =
  B0_unit.of_action "unicode-version" ~doc:"Show supported unicode version" @@
  fun _ _ ~args:_ ->
  Ok (Log.app (fun m -> m "%s" (String.of_version unicode_version)))

(* Tests *)

let test_uucd =
  let srcs = [ `File ~/"test/test_uucd.ml" ] in
  let meta =
    B0_meta.(empty |> tag test |> tag run |> ~~ B0_unit.Action.cwd `Scope_dir)
  in
  let requires = [uucd; b0_std] in
  B0_ocaml.exe "test_uucd" ~doc:"Test decoder" ~srcs ~requires ~meta

let example =
  let srcs = [ `File ~/"test/example.ml" ] in
  let meta = B0_meta.(empty |> tag test) in
  B0_ocaml.exe "example" ~doc:"Sample code" ~srcs ~meta ~requires:[uucd]

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The uucd programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/uucd"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/uucd/doc/Uucd"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/uucd.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/uucd/issues"
    |> ~~ B0_meta.description_tags
      ["unicode"; "database"; "decoder"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "xmlm", {||} ]
  in
  B0_pack.make "default" ~doc:"uucd package" ~meta ~locked:true @@
  B0_unit.list ()
