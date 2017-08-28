! File: folder.factor
! Version: 0.1
! DRI: Dave Carlton <davec@polymicro.net>
! Description: Folder Summary
! This vocabulary implements folder entries in a manner similar to directory entries.
! The primary difference is a folder entry contains the complete path to the entry
! making it easier to work with and manipulate items.
!
! Copyright (C) 2011 PolyMicro Systems.
! See http://polymicrosystems.com/license for license details.


USING: accessors ascii checksums checksums.adler-32 checksums.fnv1
checksums.md5 checksums.sha classes.tuple combinators continuations
file.xattr io io.backend io.files io.files.info io.files.links
io.streams.c kernel libc math math.order math.parser namespaces
prettyprint.custom regexp sequences sorting splitting strings
prettyprint unix.users ;

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
    [ CHAR: / = ] trim-tail
    ;

: special-path? ( path -- rest ? )
    {
        { [ "resource:" ?head ] [ t ] }
        { [ "vocab:" ?head ] [ t ] }
        { [ "~" ?head ] [ t ] }
        [ f ]
    } cond ;

: root? ( path -- path' )
    [ "" = ] keep "/" swap ?
    ;

: home-path ( path -- newpath )
    root?
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

: remove-extension ( path -- path )
    [ [ CHAR: . = ] find-last drop ] keep swap head ;

: bump-name ( path -- path )
    dup file-extension string>number [
        [ remove-extension "." ] dip 1 +
        number>string 3append
    ] [ ".1" append ] if*
    dup exists?
    [ bump-name ] [ ] if
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

IN: io.pathnames
: last-path-component ( path -- path stem )
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

IN: io.files.types

SYMBOL: +nonexistent-file+

IN: folder

CONSTANT: TEST "~/test"
CONSTANT: DOMAIN-ID "net.polymicro."
    
TUPLE: folder-entry < directory-entry path info attributes entries fingerprint ;

: (folder-entry) ( name info path -- name type path info attributes entries fingerprint )
        swap normalize-path
    over ! check for existence
    [
        [ dup type>> swap ] dip  swap ! adjust stack: name type path info
    ] [
        [ drop +nonexistent-file+ ] dip f
    ] if
    f f f ! attributes, entries and fingerprint are f until needed
    ;

 : <folder-entry> ( name info path -- folder )
     (folder-entry) folder-entry boa ;

: does-exist? ( entry -- ? )
    type>> +nonexistent-file+ ≠ ;

: is-directory? ( entry -- ? )
    type>> +directory+ = ;

: is-file? ( entry -- ? )
    type>> +regular-file+ = ;

: is-symbolic? ( entry -- ? )
    type>> +symbolic-link+ = ;

: is-hidden? ( entry -- ? )
    name>> first CHAR: . = ;

: name=? ( str entry -- ? )
    swap name>> =  ;

: name=~? ( regexp entry -- ? )
    name>> swap re-contains? ; 

: name-in-seq? ( string-seq entry -- ? )
    [ name=? ] with find drop ;

: has-entries? ( entry -- ? )
    entries>> length f = not ;

: wwcd ( -- path )
    "." normalize-path ;

! Default is shallow, will not look into subdirectories
SYMBOL: deep-folder

: deep ( -- )   t deep-folder set ;
: ¬deep ( -- )  f deep-folder set ;

: is-dot? ( entry -- ? )
    name>> { "." ".." } member? ;

: is-dot-dir? ( entry -- entry ? )
    [ is-directory? ] keep 
    [ is-dot? ] keep  
    [ and ] dip 
    swap
    ;

    ! Accessors (note the « character is option/alt-\
: «name ( folder.entry -- name folder.entry )
    [ name>> ] keep ;
: «extension ( folder.entry -- name folder-entry )
    [ name>> file-extension ] keep ;
: «type ( folder.entry -- type folder.entry )
    [ type>> ] keep ;
: «path ( folder.entry -- path folder.entry )
    [ path>> ] keep ;
: «pathname ( folder.entry -- pathname folder.entry )
    «path «name [ "/" append ]  2dip  [ append ] dip ; 
: «info ( folder.entry -- file-info folder.entry )
    [ info>> ] keep ;
: attributes» ( folder.entry -- attributes )
    dup does-exist? [
        [ attributes>> ] keep
        swap
        [ attributes>> ]
        [ «pathname  swap get-xattrs
           [ >>attributes ] keep
           nip
        ] if
    ]
    [ drop f ]
    if
    ;
: «attributes ( folder.entry -- file-info folder.entry )
    [ attributes» ] keep ;

: «entries ( folder.entry -- entries folder.entry )
    [ entries>> ] keep ;

IN: string

GENERIC: to-folder ( path -- folder )

M: string to-folder ( path -- folder )
    dup [ file-info ]
    [ nip drop f ]
    recover
    [ last-path-component ] dip
    <folder-entry>
;

IN: folder

: string-to-folder ( path -- folder )
    to-folder ;

IN: io.directories

GENERIC: to-folder ( path entry -- folder )

M: directory-entry to-folder 
B    tuple-slots
    swap normalize-path suffix
    folder-entry prefix
    >tuple
;

IN: folder

GENERIC: pathname ( folder-entry -- path )

M: folder-entry pathname 
    [ path>> "/" append ] keep
    name>> append
    ;

: !fingerprint ( entry -- entry )
    [ pathname fnv1-32 checksum-file ] keep
    swap >>fingerprint ;

M: folder-entry equal?
    ! 2dup
    [ name>> ] bi@
    ! over 5 head "Larry" =
    ! [ break "HERE" drop 2swap 2drop ]
    ! [ 2swap 2drop ] if
    = ;

M: folder-entry hashcode*
    ! dup pathname parent-directory write
    dup attributes>>
    [ attributes>> ]
    [ name>> >lower ]
    if
    ! dup write
    hashcode*
    ! [ name>> ] keep
    ! swap string-hashcode
    ! break
    ! [ ] [ name>> dup rehash-string string-hashcode ] ?if
    ! ": " write dup . 
    ;

FROM: folder => pathname ;
: fingerprint= ( entry1 entry2 -- ? )
    "progress" get-global
    [ over pathname  "comparing: " prepend print
      dup pathname  "      with: " prepend print
    ]
    [ ] if
    [ pathname sha1 checksum-file ] dip
    pathname md5 checksum-file =
    ;

: set-fingerprint-xattr ( fingerprint entry -- )
    "net.polymicro." "checksum.fnv1-32 " append
    rot number>string append
    " " append
    [ pathname ] dip prepend
    "xattr -w " prepend
    dup print 
    system .
;

: store-fingerprint ( entry -- entry )
    dup fingerprint>>
    [ fingerprint>> ]
    [ «pathname
       "net.polymicro." "checksum.fnv1-32 " append
       [ fingerprint>> ] dip
       append
       "xattr -w " prepend system drop
    ]
    if
;

: fingerprint-entries ( entries -- entries )
    [ !fingerprint ] map ;

: «fingerprint ( entry -- hash )
    fingerprint>> 0 swap [ + ] each ;

: fingerprint-sort ( entries -- entries )
    [ «fingerprint [ «fingerprint ] dip  <=> ] sort ;

: spotcheck-entry ( entry -- entry )
    dup
    pathname "r" fopen >>fingerprint
    B{ }
    [ over fingerprint>> fgetc
      f over ≠ swap
      over [ rot swap suffix
             [ dup fingerprint>> 100000 1 rot fseek ] 2dip swap
           ]
      [ drop ] if
    ] loop
    adler-32 checksum-bytes
    over fingerprint>> fclose
    >>fingerprint ;

FROM: io.pathnames => absolute-path ;
M: folder-entry absolute-path pathname absolute-path ;

FROM: io.directories => (directory-entries) ;
FROM: string => to-folder ;

: (folder-entries) ( folder -- entries )
    «type [ +nonexistent-file+ = ] dip  swap
    [ drop { } ]
    [
        [ pathname ] keep
        [ (directory-entries) ] dip
        [ [ is-dot-dir? not nip ] filter ] dip
        swap
        [ over pathname "/" append
          [ name>> ] dip swap append to-folder
        ] map
        >>entries entries>>
    ] if
    ;

! Recurse into folder and get every file stored into entries
: (folder-entries-deep) ( folder -- entries )
   (folder-entries)
    [
        dup is-directory?
        [
            is-dot-dir? not 
            [ 
                [ pathname to-folder (folder-entries-deep) ] keep 
                swap >>entries
            ] when
        ] when
    ] map
    ;

: (deep-entries) ( entries -- all-entries )
    { } swap
    [
        dup is-directory? [
            [ is-dot-dir? not ] keep
            swap
            [ entries>> (deep-entries)
              nip append
            ] 
            [ dup is-dot? 
              [ 2drop ]
              [ drop suffix ]
              if
            ] if
        ]
        [ suffix ] if
    ] each
    ;
        
FROM: io.directories => to-folder ;
FROM: io.pathnames => pathname ;

: folder-entries ( folder -- entries )
    deep-folder get
    [ (folder-entries-deep)
      (deep-entries)
    ]
    [ (folder-entries) ]
    if
    [ is-dot? not ] filter 
;

: folder-filenames ( folder -- seq )
    folder-entries [ name>> ] map! ;

: with-folder-directory ( folder quot -- )
     [ pathname ] dip  with-directory ; inline

: with-folder ( folder quot -- )
     [ folder-entries ] dip each ; inline

: with-folder-filenames ( folder quot -- )
     [ folder-filenames ] dip each ; inline

M: folder-entry pprint* pprint-object ;

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

