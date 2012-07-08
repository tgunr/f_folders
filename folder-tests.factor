! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: combinators definitions io io.directories
io.directories.hierarchy io.encodings.utf8 io.files io.pathnames
kernel namespaces sequences tools.test ;
IN: folder.tests

CONSTANT: TESTf "resource:work/folder/test"
SYMBOL: testfolder

: test-folder-delete ( -- )
    testfolder get delete-tree ;

: set-test-folder-path ( -- )
    \  testfolder where first
    parent-directory "test/" append
    testfolder set
    ;

[ t ] [ "   test   " trim-whitespace "test" = ] unit-test
[ t ] [ [ "~" "factor" ] components-to-path "~/factor" = ] unit-test
[ t ] [ "directory" as-directory "directory/" = ] unit-test
[ t ] [ "directory/" as-directory "directory/" = ] unit-test
[ t ] [ "directory" as-file "directory" = ] unit-test
[ t ] [ "directory/" as-file "directory" = ] unit-test
[ f ] [ "directory" special-path? swap "directory" = and ] unit-test
[ t ] [ "resource:directory" special-path? swap "directory" = and ] unit-test
[ t ] [ "vocab:directory" special-path? swap "directory" = and ] unit-test
[ t ] [ "~directory" special-path? swap "directory" = and ] unit-test 
[ t ] [ "~/directory" special-path? swap "/directory" = and ] unit-test 
[ t ] [ "" root? "/" = ] unit-test
[ f ] [ "/folder" root? "/" = ] unit-test


! [ f ] [ TESTf dup delete-tree exists? ] unit-test
