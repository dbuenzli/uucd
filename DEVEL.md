# New Unicode release 

Bump the Unicode release number at the top of the `B0.ml` file and in
`pkg/pkg.ml`. Verify that everything is as expected with:

    b0 -- unicode-version

Download the latest xml unicode database to the `test/ucd.xml` file
which is ignored by git. If you have `curl` and `unzip` in your `PATH`
you can simply issue:

    b0 -- download-ucdxml 

Then you should run

    b0 test
   
this will likely fail with a parse error. Adjust the parser and 
datatypes with the help of: 

  <https://www.unicode.org/reports/tr42/proposed.html>
  
  
