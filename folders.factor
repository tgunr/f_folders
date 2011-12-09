! Copyright (C) 2011 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors classes.tuple combinators io io.backend io.files
io.files.links io.files.private io.pathnames kernel locals math
math.parser namespaces sequences splitting strings tools.continuations
unix.users ;

IN: io.pathnames
! IN: folders

! GENERIC: trim-whitespace ( str -- 'str )
! GENERIC: space? ( n -- ? )
! GENERIC: tab? ( n -- ? )

! M: fixnum space? ( ch -- ? ) 32 = ;
: space? ( ch -- ? ) 32 = ;
! M: fixnum tab? ( ch -- ? ) 9 = ;
: tab? ( ch -- ? ) 9 = ;

! M: string trim-whitespace ( str -- str' )
: trim-whitespace ( str -- str' )
    [ [ space? ] [ tab? ] bi or ] trim-head
    [ [ space? ] [ tab? ] bi or ] trim-tail
    ;

: components-to-path ( seq -- path )
    "/" join
    ;

: special-path? ( path -- rest ? )
    {
        { [ "resource:" ?head ] [ t ] }
        { [ "vocab:" ?head ] [ t ] }
        { [ "~" ?head ] [ t ] }
        [ f ]
    } cond ;

: home-path-?isEmpty ( path -- path' )
    [ "" = ] keep swap
    [ drop "/" ] [ ] if
    ;
    
: home-path ( path -- newpath )
    home-path-?isEmpty
    [ "/" head? ] keep
    path-components swap
    [ home prefix ]                        
    [                                      
      [ first user-passwd ] keep
      over 
      [ 0 swap remove-nth swap dir>> prefix ]
      [ nip ] if
    ] if
    components-to-path
    ;

M: string absolute-path
    trim-whitespace
    "resource:" ?head [
        trim-head-separators resource-path
        absolute-path
    ] [
        "vocab:" ?head [
            trim-head-separators vocab-path
            absolute-path
        ] [ "~" ?head
            [ home-path ]
            [ current-directory get prepend-path
            ] if
        ] if
    ] if
    ;

IN: io.directories

: bump-name-dropTail ( path -- seq )
    path-components dup
    length 1 - swap remove-nth 
    ;

:: bump-name-adjustExtension ( path -- path )
    path file-extension dup
    [ string>number dup
      [ 1 + number>string "." prepend
        path file-stem swap append ]
      [ drop path ".1" append ] if
    ]
    [ drop path ".1" append ] if
    ;

:: bump-name ( path -- 'path )
    path bump-name-dropTail
    path file-name bump-name-adjustExtension
    suffix components-to-path
    dup exists? [ bump-name ] [ ] if
    ;

:: move-aside ( path -- )
    path
    path bump-name
    move-file
    ;

IN: folders

TUPLE: folder < directory-entry path ;

! : new-folder ( class -- result )
!     new 0 >>name ; inline

! C: <folder> folder
: <folder> ( name type path -- folder ) folder boa ;

! ( -- derived1 )
! folder new-folder ;


: hasTilde ( path -- ? )
    "~" head?
;

: replaceTilde ( path -- path )
    path-components 0 swap remove-nth
    home prefix
    components-to-path
    ;

: expandingTildeInPath ( path -- path )
    dup hasTilde
    [ replaceTilde ] [ ] if  
    ;

:: to-folder ( directory-entry path -- folder )
    directory-entry tuple-slots
    path suffix
    folder prefix >tuple
    ;

:: full-path ( folder -- path )
    folder path>> "/" append
    folder name>> append
;

:: folder-entries ( path -- seq )
    path expandingTildeInPath
    normalize-path
    (directory-entries)
    [ name>> { "." ".." ".DS_Store" } member? not ] filter
    [ path to-folder ] map!
    ;

:: folder-content-names ( folder -- seq )
    folder full-path folder-entries
    [ name>> ] map ;

:: folder-link-cwd ( folder -- )
    cwd "/" append folder name>> append
    "linking in " over append print
    folder full-path
    swap make-link
    ;

: folder-is-name? ( str folder -- ? )
    swap name>> =  ;

: folder-in-seq? ( folder seq -- ? )
    swap [ folder-is-name? ] with find drop ;


