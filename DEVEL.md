# On a new unicode version 

Download the latest xml unicode database from: 

  <https://www.unicode.org/Public/%%VERSION%%/ucd.all.grouped.zip>
   
uncompress it to, for example `/tmp/ucd.xml` and run

    b0 -- test /tmp/ucd.xml
   
this will likely fail with a parse error. Adjust the parser and 
datatypes with the help of: 

  <https://www.unicode.org/reports/tr42/proposed.html>
  
  
