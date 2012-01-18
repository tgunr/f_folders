! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors definitions folder io.directories io.pathnames kernel
prettyprint sequences sets tools.continuations tools.test ;
IN: folder.tests

FROM: io.pathnames => to-folder ;
: (create-test-folders) ( root-path -- )
    [ make-directories ] keep
    { "test1" "test2" } [ over swap append ] map nip
    [ first { "/a" "/b" "/c" "/d" "/aa" "/bb" } [ over swap append ] map nip ] keep
    [ second { "/b" "/d" "/e" "/f" "/bb" } [ over swap append ] map nip ] keep
    drop append
    [ make-directories ] each
    ;

: test-folder-path ( -- path )
    \ folder where first
    parent-directory "test/" append
    ;

: create-test-folders ( -- )
    test-folder-path 
    (create-test-folders)
    ;

: test-compare ( -- seq )
    create-test-folders
    "resource:work/folder/test/test1" to-folder
    "resource:work/folder/test/test2" to-folder
    folder-compare [ name>> ] map
    ;

[ { "kernel" } ] [
    "core" resource-path to-folder [
        "." to-folder directory-files [ "kernel" = ] filter
    ] with-directory
] unit-test

[ { "a" "aa" "c" } ] [ test-compare ] unit-test
[ { } ] [ "resource:work/folder/test" to-folder dup delete-folder folder-entries ] 
