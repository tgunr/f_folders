! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: combinators definitions io io.directories
io.directories.hierarchy io.encodings.utf8 io.files io.pathnames
kernel namespaces sequences tools.test ;
IN: folder.tests

CONSTANT: TESTf "resource:work/folder/test"
SYMBOL: testfolder

: create-test-files ( path -- )
    dup make-directories
    dup file-name
    {
        { "a" [ "/a-file" append utf8 [ "aaaa" print ] with-file-writer ] }
        [ 2drop ]
    } case
    ;

: (create-test-folders) ( root-path -- )
    [ make-directories ] keep
    { "test1" "test2" } [ over swap append ] map nip
    [ first { "/a" "/b" "/c" "/d" "/aa" "/bb" } [ over swap append ] map nip ] keep
    [ second { "/b" "/d" "/e" "/f" "/bb" } [ over swap append ] map nip ] keep
    drop append
    [ create-test-files ] each
    ;

: test-folder-path ( -- path )
    \  (create-test-folders) where first
    parent-directory "test/" append
    dup testfolder set
    ;

: create-test-folders ( -- )
    test-folder-path 
    (create-test-folders)
    ;

: test-compare ( -- seq x )
    create-test-folders
    "resource:work/folder/test/test1" 
    "resource:work/folder/test/test2" 
    directory-diff
    ;

[ t ] [ create-test-folders t ] unit-test
! [ { "a" "aa" "c" } ] [ test-compare ] unit-test
! [ f ] [ TESTf dup delete-tree exists? ] unit-test
