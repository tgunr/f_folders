! File: collection.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Operations on collections of folders/files
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.sha folder io
continuations io.directories io.files.links io.pathnames kernel layouts locals
math namespaces prettyprint regexp sequences sequences.extras
serialize splitting threads words unix.ffi ;

IN: folder.collection
FROM: string => to-folder ;

! CONSTANT: COLLECTION-FOLDER "/gamma/pictures"
! CONSTANT: COLLECTION-FOLDER "/Users/davec/Music/Plex"
VARIABLE: COLLECTION-FOLDER "~/test"

: name. ( entry -- entry )
    [ name>> print ] keep ;

SYMBOL: collection
: get-collection ( -- seq )
    collection get [
        COLLECTION-FOLDER qualified-directory-files
        "Collecting files:" print
        [ dup file-name print
          sha1 checksum-file
          yield ] map-zip 
        collection set
    ] unless
    collection get
    [ second ] collect-by
    ;

CONSTANT: COLLECTION-SAVE "/tmp/collection.fstore"

: save-collection ( -- )
    collection get COLLECTION-SAVE save-fstore ;
: (restore-collection) ( path -- )
    load-fstore  collection set ;
: restore-collection ( -- )
    COLLECTION-SAVE (restore-collection) ; 

: filter-entries ( entries -- entries )
    HERE.S
    filter-dot-names
    ;

: (collect-files) ( path -- )
    to-folder
    "Collecting files:" print
    deep folder-entries
    filter-entries 
    HERE" Fingerprinting"
    100 count-by
    [
     !   dup name>> HERE.
        fingerprintGet
        yield
    ] map>alist
    HERE" Collect by"
    [ first ] collect-by
    collection set
    HERE" Save collection"
    save-collection
    ;

: (collect-files-callback) ( thread -- thread )
    [ "Collect files completed" print ] >>exit-handler ;

: collect-files ( path -- )
    restore-collection
    collection get 
    [ drop ]
    [
        [ (collect-files) ] curry "Collect Files" <thread>
        (collect-files-callback) (spawn)
    ]
    if
    ;

: get-collection-entries ( -- )
    COLLECTION-FOLDER to-folder
    folder-entries
    fingerprint-entries
    collection set
    ;

: collection-by-folders ( -- seq )
    .HERE
    { }  collection get values
    [ [ second suffix ] each ] each
    [ pathname>> ] collect-by 
;

: collection-files ( -- )
    .HERE
    100 count-by
    COLLECTION-FOLDER collect-files 
    ;

