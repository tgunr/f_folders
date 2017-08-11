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


USING: accessors ascii checksums checksums.md5 classes.tuple
combinators continuations io io.backend io.files io.files.info
io.files.links kernel libc math math.parser namespaces
regexp prettyprint.custom sequences splitting strings unix.ffi unix.users ;

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

IN: folder

TUPLE: folder-entry < directory-entry path owner group permissions acl data entries ;

: <folder-entry> ( name type path -- folder )
    normalize-path
    f 
    f 
    f
    f
    f
    f
    folder-entry boa ;

! Accessors (note the « character is option/alt-\
: «name ( folder.entry -- name folder.entry )
    [ name>> ] keep ;
: «extension ( folder.entry -- name folder-entry )
    [ name>> file-extension ] keep ;
: «type ( folder.entry -- type folder.entry )
    [ type>> ] keep ;
: «path ( folder.entry -- path folder.entry )
    [ path>> ] keep ;
: «entries ( folder.entry -- entries folder.entry )
    [ entries>> ] keep ;

IN: io.files.types
SYMBOL: +nonexistent-file+

IN: string
GENERIC: to-folder ( path -- folder )
M: string to-folder ( path -- folder )
    dup 
    [ file-info type>> ]
    [ nip drop +nonexistent-file+ ]
    recover
    [ last-path-component ] dip
    swap 
    <folder-entry>
;

IN: folder
: string-to-folder ( path -- folder )
    to-folder ;

IN: io.directories
GENERIC: to-folder ( path entry -- folder )
M: directory-entry to-folder 
    tuple-slots
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
: path+name ( folder-entry -- path )
    pathname ;

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

FROM: io.pathnames => absolute-path ;
M: folder-entry absolute-path pathname absolute-path ;

FROM: io.directories => (directory-entries) ;
FROM: string => to-folder ;

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
    [ name>> ] dip re-contains? ; 

: name-in-seq? ( string-seq entry -- ? )
    [ name=? ] with find drop ;

: has-entries? ( entry -- ? )
    entries>> length f = not ;

: wwcd ( -- path )
    "." normalize-path ;

: (folder-entries) ( folder -- folders )
    [ pathname ] keep
    [ (directory-entries) ] dip swap
    [ over pathname "/" append
      [ name>> ] dip swap append to-folder
    ] map
    nip
    ;

: (folder-entries-deep) ( folder -- folders )
   (folder-entries)
    [ ! entry
            [ is-directory? ] keep ! ? entry
            [ name>> { "." ".." } member? not ] keep ! ? ? entry 
            [ and ] dip ! ? entry
            swap ! entry ?
            [ ! entry
                [ pathname to-folder (folder-entries-deep) ] keep ! entries entry
                swap >>entries
            ] when ! entry
    ] map
   ;
     
FROM: io.directories => to-folder ;
FROM: io.pathnames => pathname ;
: folder-entries ( folder -- seq )
    (folder-entries) 
    [ name>> { "." ".." } member? not ] filter 
;

: folder-entries-deep ( folder -- entries )
    (folder-entries-deep)
    [ name>> { "." ".." } member? not ] filter 
    ;

: folder-filenames ( folder -- seq )
    folder-entries [ name>> ] map! ;

: with-folder-directory ( folder quot -- )
     [ pathname ] dip  with-directory ; inline

: with-folder-entries ( folder quot -- )
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

FROM: folder => pathname ;
: checksum= ( entry1 entry2 -- ? )
    "progress" get-global
    [ over pathname  "comparing: " prepend print
      dup pathname  "      with: " prepend print
    ]
    [ ] if
    [ pathname md5 checksum-file ] dip
    pathname md5 checksum-file =
    ;



