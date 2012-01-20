! Copyright (C) 2011 PolyMicro Systems.
! See http://polymicrosystems.com/license for license details.
!
! This vocabulary implements folder entries in a manner similar to directory entries.
! The primary difference is a folder entry contains the complete path to the entry
! making it easier to work with and manipulate items.

USING: accessors ascii checksums checksums.md5 classes.tuple
combinators continuations fry io io.backend io.directories.hierarchy
io.files io.files.info io.files.links kernel locals math math.parser
namespaces prettyprint sequences sets splitting strings
tools.continuations unix.users ;

IN: io.pathnames

: space? ( ch -- ? ) 32 = ;
: tab? ( ch -- ? ) 9 = ;

: trim-whitespace ( str -- str' )
    [ [ space? ] [ tab? ] bi or ] trim-head
    [ [ space? ] [ tab? ] bi or ] trim-tail
    ;

: components-to-path ( seq -- path )
    "/" join
    ;

: as-directory ( path -- path' )
    [ last CHAR: / = ] keep
    swap
    [ ]
    [ "/" append ] if
    ;

: as-file ( path -- path' )
    [ last CHAR: / = ] keep swap
    [ [ length ] keep remove-nth! ]
    [ ] if
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
    break
    dup file-extension string>number [
        [ path-without-extension "." ] dip 1 +
        number>string 3append
    ] [ ".1" append ] if*
    [ exists? ] keep swap
    [ bump-name ]
    [ ]
    if
    ;

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

IN: folder
TUPLE: folder-entry < directory-entry path ;

: <folder> ( name type path -- folder ) folder-entry boa ;

IN: io.files.types
SYMBOL: +nonexistent-file+

IN: string
GENERIC: to-folder ( path -- folder )
M: string to-folder ( path -- folder )
    normalize-path
    dup 
    [ file-info type>> ]
    [ break nip drop +nonexistent-file+ ]
    recover
    [ split-path ] dip
    swap 
    <folder>
;

IN: io.directories
GENERIC: to-folder ( path directory-entry -- folder )
M: directory-entry to-folder 
    tuple-slots
    swap suffix
    folder-entry prefix
    >tuple
;

IN: folder

GENERIC: pathname ( folder -- path )
M: folder-entry pathname 
    [ path>> "/" append ] keep
    name>> append
;

M: folder-entry equal?
    ! 2dup
    [ name>> ] bi@
    ! over 5 head "Larry" =
    ! [ break "HERE" drop 2swap 2drop ]
    ! [ 2swap 2drop ] if
    = ;

M: folder-entry hashcode*
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
: (directory-entries) ( path -- seq )
    [ pathname ] keep
    [ (directory-entries) ] dip swap
    [ over pathname "/" append
      [ name>> ] dip swap append to-folder
    ] map
      nip
    ;

GENERIC: folder-entries ( folder-entry -- seq )
FROM: io.directories => to-folder ;
! M: folder-entry directory-entries
: directory-entries ( path -- seq )
    pathname
    [ normalize-path (directory-entries) ] keep
    [ [ name>> { "." ".." } member? not ] filter ] dip
    swap [ to-folder ] with map!
;

: directory-files ( path -- seq )
    directory-entries [ name>> ] map ;

: with-directory-entries ( path quot -- )
    '[ "" directory-entries @ ] with-directory ; inline

: with-directory-files ( path quot -- )
    '[ "" directory-files @ ] with-directory ; inline

IN: io.directories

: link-to-current-directory ( path -- )
    [ file-name ] keep 
    "link: " over append print
    current-directory get as-directory
    rot append
    "  to: " over append print
    swap
    [ make-link ]
    [ (move-aside-error) 2drop ]
    recover
;

: is-directory? ( directory-entry -- ? )
    type>> +directory+ = ;

: is-file? ( directory-entry -- ? )
    type>> +regular-file+ = ;

: is-symbolic? ( directory-entry -- ? )
    type>> +symbolic-link+ = ;

: hidden? ( name -- ? )
    first CHAR: . = ;

: name=? ( str directory-entry -- ? )
    swap name>> =  ;

: name-in-seq? ( directory-entry string-seq -- ? )
    swap [ name=? ] with find drop ;

: src-dst-entries ( src dst -- src-entries dst-entries )
    [ directory-entries [ name>> ] map ] dip
    directory-entries [ name>> ] map
    ;

FROM: io.directories.search => each-directory-entry ;

: directory-diff ( src dst -- seq )
    src-dst-entries diff
    ;

: directory-intersect ( src dst -- seq )
    src-dst-entries intersect
    ;

: directory-union ( src dst -- seq )
    src-dst-entries union
    ;

: directory-mdmove ( src dst -- )
    2drop
    ;

: directory-mdmove-intersect ( src dst -- )
    2drop
    ;

: wwcd ( -- path )
    "." normalize-path ;

: checksum= ( file1 file2 -- ? )
    [ pathname md5 checksum-file ] dip
    pathname md5 checksum-file =
    ;

FROM: string => to-folder ;
:: gather-files ( seq path1 path2 -- seq1 seq2 )
    path1 as-directory :> path1
    path2 as-directory :> path2
    { } { } 
    seq [ 
        [ path1 prepend to-folder 
          dup is-file?
          [ suffix ] [ drop ] if
        ] keep 
        path2 prepend to-folder
        dup is-file? [ rot swap suffix ] [ drop swap ] if
        swap
    ] each
    ;

:: directory-mddiff ( src dst -- seq )
    src directory-tree-files
    dst directory-tree-files
    intersect
    [ 1 head "." = not ] filter  
    src dst gather-files
    [ [ checksum= ] 2keep rot
      [ 2drop f ]
      [ { } swap prefix  swap suffix  ] if
    ] 2map
    [ ] filter
    [ first pathname ] map
    ;

CONSTANT: f1 "/Sources/OpenPrograph/XMacVPL/Source Marten"
CONSTANT: f2 "/Sources/OpenPrograph/xmacvpl-pmn/Source Marten"

: mdtest ( -- )
    f1 f2 directory-mddiff
    [ name>> ] map
    [ print ] each
    ;