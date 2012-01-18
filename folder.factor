! Copyright (C) 2011 PolyMicro Systems.
! See http://polymicrosystems.com/license for license details.
!
! This vocabulary implements folder entries in a manner similar to directory entries.
! The primary difference is a folder entry contains the complete path to the entry
! making it easier to work with and manipulate items.

USING: accessors ascii classes.tuple combinators continuations fry io
io.backend io.files.info io.files.links io.files.types kernel math
math.parser namespaces sequences sets splitting strings unix.users ;

IN: io.pathnames
! IN: folder

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

: root-if-empty ( path -- path' )
    [ "" = ] keep "/" swap ?
    ;
    
: home-path ( path -- newpath )
    root-if-empty
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

: path-without-extension ( path -- path )
    [ [ CHAR: . = ] find-last drop ] keep swap head ;

: bump-name ( path -- path )
    dup file-extension string>number [
        [ path-without-extension "." ] dip 1 +
        number>string 3append
    ] [ ".1" append ] if* ;

: (move-aside-error) ( error -- )
    "move-aside: Error: " over errno>>
      number>string append
      " " append over args>> first append
      " " append swap args>> second append print
;

: move-aside ( path -- )
    dup bump-name
    [ move-file ]
    [ (move-aside-error) 2drop ]
    recover
    ;

IN: folder

TUPLE: folder < directory-entry path ;

! : new-folder ( class -- result )
!     new 0 >>name ; inline

! C: <folder> folder
: <folder> ( name type path -- folder ) folder boa ;
! : folder? ( obj -- ? )   folder instance? ;

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

IN: io.pathnames
: split-path ( path -- path stem )
    path-components
    [  "/" "" ]
    [
    [ last ] keep
    [ length ] keep
    [ 1 - ] dip
    remove-nth
    components-to-path
    "/" swap append
    ]
    if-empty
;

: new-folder ( path name ? -- folder )
    [ +directory+ swap ]
    [ +regular-file+ swap ]
    if
    folder boa
;

IN: string
GENERIC: to-folder ( path -- folder )
M: string to-folder ( path -- folder )
! : to-folder ( path -- folder )
    normalize-path
    [ file-info directory? ] keep
    split-path
    rot
    new-folder
;

IN: io.directories
GENERIC: to-folder ( path directory-entry -- folder )
M: directory-entry to-folder 
    tuple-slots
    swap suffix
    folder prefix
    >tuple
;

IN: folder

GENERIC: pathname ( folder -- path )
M: folder pathname 
    [ path>> "/" append ] keep
    name>> append
;

M: folder equal?
    ! 2dup
    [ name>> ] bi@
    ! over 5 head "Larry" =
    ! [ break "HERE" drop 2swap 2drop ]
    ! [ 2swap 2drop ] if
    = ;

M: folder hashcode*
    ! dup pathname parent-directory write
    name>> >lower
    ! dup write
    hashcode*
    ! [ name>> ] keep
    ! swap string-hashcode
    ! break
    ! [ ] [ name>> dup rehash-string string-hashcode ] ?if
    ! ": " write dup . 
    ;

GENERIC: (folder-entries) ( folder-entry -- seq )
FROM: io.directories => (directory-entries) ;
FROM: string => to-folder ;
M: folder (folder-entries)
    [ pathname ] keep
    [ (directory-entries) ] dip swap
    [ over pathname "/" append
      [ name>> ] dip swap append to-folder
    ] map
      nip
    ;

! GENERIC: folder-entries ( folder-entry -- seq )
! FROM: io.directories => to-folder ;
! M: folder folder-entries
!     pathname
!     [ normalize-path (directory-entries) ] keep
!     [ [ name>> { "." ".." ".DS_Store" } member? not ] filter ] dip
!     swap [ to-folder ] with map!
! ;

FROM: string => to-folder ;
: cwd ( -- folder )
    cwd to-folder ;

: cd ( folder -- )
    path>> cd ;

: set-current-directory ( folder -- )
    pathname set-current-directory ;

: with-folder ( folder quot -- )
    [ current-folder ] dip  with-variable ; inline

: folder-files ( folder -- seq )
    folder-entries
    [ name>> ] map ;

: with-folder-entries ( folder quot -- )
    '[ "" folder-entries @ ] with-folder ; inline

: with-folder-files ( folder quot -- )
    '[ "" folder-entries @ ] with-folder ; inline

: path-to-cwd ( folder -- path )
    cwd "/" append
    swap name>> append
;
    
: link-to-cwd ( folder-entry -- )
    [ path-to-cwd ] keep
    "linking in " over name>> append print
    pathname
    swap
    [ make-link ]
    [ (move-aside-error) 2drop ]
    recover
;

: name=? ( str folder-entry -- ? )
    swap name>> =  ;

: name-in-seq? ( folder-entry string-seq -- ? )
    swap [ name=? ] with find drop ;

: make-folder ( folder -- )
    pathname make-directories
;

DEFER: delete-folder

: delete-folder-entry ( file|folder-entry -- )
    [ type>> ] keep
    swap +directory+ = 
    [ delete-folder ]
    [ delete-file ]
    if
    ;

: delete-folder ( folder-entry -- )
    [ folder-entries ] keep
    swap [ delete-folder-entry ] each
    pathname delete-directory
    ;

: src-dst-entries ( src-folder dst-folder -- src-entries dst-entries )
    [ folder-entries ] dip
    folder-entries
    ;

: folder-diff ( src-folder dst-folder -- seq )
    src-dst-entries diff
    ;

: folder-intersect ( src-folder dst-folder -- seq )
    src-dst-entries intersect
    ;

: folder-union ( src-folder dst-folder -- seq )
    src-dst-entries union
    ;

! GENERIC: move-to-folder ( dst-folder obj -- )
: move-to-folder ( src dst -- )
    pathname [ [ pathname ] map ] dip
    move-files-into
    ;

