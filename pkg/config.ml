#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  let vars =
    [ "NAME", "uucd";
      "VERSION", Git.describe ~chop_v:true "master";
      "UNICODE_VERSION", "7.0.0";
      "MAINTAINER", "Daniel Bünzli <daniel.buenzl i\\@erratique.ch>" ]
end
