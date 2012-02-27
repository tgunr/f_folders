! Copyright (C) 2011 PolyMicro Systems.
! See http://polymicrosystems.com/license for license details.
!
! This vocabulary implements folder entries in a manner similar to directory entries.
! The primary difference is a folder entry contains the complete path to the entry
! making it easier to work with and manipulate items.

USING: accessors ascii checksums checksums.md5 classes.tuple
combinators continuations fry io io.backend io.directories.hierarchy
io.files io.files.info io.files.links kernel locals math math.parser
namespaces sequences sets splitting strings tools.continuations
unix.users regexp tools.profiler.sampling libc macros ;

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

: remove-acl ( path -- )
    "chmod -N " prepend
    system drop
    ;

: move-aside ( path -- )
    dup bump-name
    [ over remove-acl move-file ]
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

IN: folder.compare
TUPLE: folder-compare src-path dst-path dir-word filter src-entries dst-entries ;
: <folder-compare> ( src dst -- folder.compare )
    folder-compare new
    swap >>dst-path
    swap  >>src-path
;
    

IN: io.files.types
SYMBOL: +nonexistent-file+

IN: string
GENERIC: to-folder ( path -- folder )
M: string to-folder ( path -- folder )
    normalize-path
    dup 
    [ file-info type>> ]
    [ nip drop +nonexistent-file+ ]
    recover
    [ split-path ] dip
    swap 
    <folder>
;

IN: io.directories
GENERIC: to-folder ( path directory-entry -- folder )
M: directory-entry to-folder 
    tuple-slots
    swap normalize-path suffix
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

FROM: io.directories => to-folder ;
FROM: io.pathnames => pathname ;
! M: folder-entry directory-entries
: directory-entries ( path -- seq )
    [ normalize-path (directory-entries) ] keep
    [ [ name>> { "." ".." } member? not ] filter ] dip
    swap [ to-folder ] with map!
;

: directory-files ( path -- seq )
    directory-entries [ name>> ] map ;

: with-directory-entries ( path quot -- )
    [ [ "" directory-entries ] ] dip compose with-directory ; inline

: with-directory-files ( path quot -- )
    [ [ "" directory-files ] ] dip compose with-directory ; inline

IN: io.directories

: print-linking ( path1 path2 -- )
    swap "link: " prepend print
    "  to: " prepend print
;

: link-to-current-directory ( path -- )
    normalize-path
    dup file-name
    current-directory get as-directory prepend
    2dup print-linking
    dup exists?
    [ dup move-aside ] [ ] if
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

: is-hidden? ( directory-entry -- ? )
    name>> first CHAR: . = ;

: name=? ( str directory-entry -- ? )
    swap name>> =  ;

: name-in-seq? ( directory-entry string-seq -- ? )
    swap [ name=? ] with find drop ;

SYMBOL: folder-pair

: set-folder-pair-src ( src dir-word -- seq )
    [ directory-tree-files ]
    [ directory-files ]
    if
    folder-pair get  swap >>src-entries
    src-entries>>
    ;

: set-folder-pair-dst ( dst dir-word -- seq )
    [ directory-tree-files ]
    [ directory-files ]
    if
    folder-pair get swap >>dst-entries
    dst-entries>>
    ;

FROM: namespaces => set ;
:: set-folder-pair ( src dst dir-word -- src-seq dst-seq )
    src dst <folder-compare>
    folder-pair set
    src dir-word set-folder-pair-src
    dst dir-word set-folder-pair-dst
    ;

FROM: namespaces => set ;
:: src-dst-entries ( src dst dir-word -- src-entries dst-entries )
    src dst dir-word set-folder-pair
    ;

: src-folder-entries ( folder.compare -- folder.compare' )
    [ dup filter>>
      over src-path>>
      rot dir-word>> call( path -- seq )
      [ swap matches? not ] with filter ] keep
    swap >>src-entries
;

: dst-folder-entries ( folder.compare -- folder.compare' )
    [ dup filter>>
      over dst-path>>
      rot dir-word>> call( path -- seq )
      [ swap matches? not ] with filter ] keep
    swap >>dst-entries
;

: regexp-nothing ( -- regexp )  "(?!.*)" ;
: regexp-hidden ( -- regexp ) "[.].*" ;

: set-filter ( filter folder.compare -- folder.compare' )
    over
    [ swap <regexp> ]
    ! a regexp that doesn't match anything
    [ nip regexp-nothing <regexp> ]
    if
    >>filter
    ;

: set-deep ( dir-word folder.compare -- folder.compare' )
    swap
    [ [ directory-tree-files ] >>dir-word ]
    [ [ directory-files ] >>dir-word ]
    if
    ;

: new-folder-compare ( src dst filter deep? -- folder.compare )
    [ <folder-compare> ] 2dip
    rot set-deep set-filter
    dup src-folder-entries
    swap dst-folder-entries
    drop
;

: folder-compare-entries ( folder.compare -- src-entries dst-entries )
    dup src-entries>>
    [ dst-entries>> ] dip
;

: directory-diff ( folder.compare -- seq )
   folder-compare-entries diff
   ;

: directory-intersect ( folder.compare -- seq )
    folder-compare-entries intersect
    ;

: directory-union ( folder.compare -- seq )
    folder-compare-entries union
    ;

: directory-move ( folder.compare -- seq )
    ! List of files in SRC not in DST
    folder-compare-entries diff
    ! List of files in SRC and in DST
    ! List of files in DST not in SRC (prune)
    ;

: wwcd ( -- path )
    "." normalize-path ;

FROM: folder => pathname ;
: checksum= ( folder-entry1 folder-entry2 -- ? )
    "progress" get-global
    [ over pathname  "comparing: " prepend print
      dup pathname  "      with: " prepend print
    ]
    [ ] if
    [ pathname md5 checksum-file ] dip
    pathname md5 checksum-file =
    ;

FROM: string => to-folder ;
:: gather-files ( seq folder.compare -- seq1 seq2 )
    folder.compare src-path>> as-directory :> path1
    folder.compare dst-path>> as-directory :> path2
    { } { } 
    seq [ 
        [ path1 prepend to-folder
          dup is-file?  over is-hidden?  not and
          [ suffix ] [ drop ] if
        ] keep 
        path2 prepend to-folder
        dup is-file? over is-hidden?  not and
        [ rot swap suffix ] [ drop swap ] if
        swap
    ] each
    ;

:: directory-md5diff ( folder.compare -- seq )
    [
        folder.compare folder-compare-entries intersect
        folder.compare gather-files    
    [ [ checksum= ] 2keep rot
      [ 2drop f ]
      [ { } swap prefix  swap suffix  ] if
    ] 2map
    [ ] filter
    [ first pathname ] map
    ] profile
    ;

: directory-rm-intersect ( src dst -- )
    break
    regexp-hidden f  new-folder-compare
    [ directory-intersect ] keep
    dst-path>> as-directory swap 
    [ over prepend ] map
    [ delete-file ] each
    drop
    ;