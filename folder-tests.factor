! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: definitions io.directories io.directories.hierarchy io.files
io.pathnames kernel sequences tools.test ;
IN: folder.tests

: (create-test-folders) ( root-path -- )
    [ make-directories ] keep
    { "test1" "test2" } [ over swap append ] map nip
    [ first { "/a" "/b" "/c" "/d" "/aa" "/bb" } [ over swap append ] map nip ] keep
    [ second { "/b" "/d" "/e" "/f" "/bb" } [ over swap append ] map nip ] keep
    drop append
    [ make-directories ] each
    ;

: test-folder-path ( -- path )
    \  (create-test-folders) where first
    parent-directory "test/" append
    ;

: create-test-folders ( -- )
    test-folder-path 
    (create-test-folders)
    ;

: test-compare ( -- seq )
    create-test-folders
    "resource:work/folder/test/test1" 
    "resource:work/folder/test/test2" 
    directory-diff
    ;

[ { "a" "aa" "c" } ] [ test-compare ] unit-test
[ f ] [ create-test-folders "resource:work/folder/test" dup delete-tree exists? ] unit-test
