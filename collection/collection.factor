! File: collection.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Operations on collections of folders/files
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.sha continuations folder
folder.collection.music io io.directories io.files.links io.pathnames
kernel layouts locals math namespaces prettyprint regexp sequences
sequences.extras serialize splitting threads unix.ffi words ;

IN: folder.collection
FROM: string => to-folder ;

SYMBOL: COLLECTION-FOLDER 
SYMBOL: COLLECTION
SYMBOL: collection

: name. ( entry -- entry )
    [ name>> print ] keep ;

! : get-collection ( -- seq )
!     collection get [
!         COLLECTION-FOLDER get
!         qualified-directory-files
!         "Collecting files:" print
!         [ dup file-name print
!           sha1 checksum-file
!           yield ] map-zip 
!         collection set
!     ] unless
!     collection get
!     [ second ] collect-by
!     ;

: save-collection ( -- )
    collection get COLLECTION get save-fstore ;
: (restore-collection) ( path -- )
    load-fstore  collection set ;
: restore-collection ( -- )
    COLLECTION get (restore-collection) ; 

: filter-entries ( entries -- entries )
    [ is-dot? not ] filter ;

: filter-single-entry ( entries -- entries )
    [ entries>> length 1  = ] filter ;

: (collect-files) ( path -- )
    to-folder
    "Collecting files:" print
    deep folder-entries
    HERE" Fingerprinting"
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
    collection get 
    [ drop ]
    [ 100 count-by
      [ (collect-files) ] curry "Collect Files" <thread>
      (collect-files-callback) (spawn)
    ]
    if
    ;

: get-collection-entries ( -- )
    COLLECTION-FOLDER get to-folder
    folder-entries
    fingerprint-entries
    collection set
    ;

: collection-by-folders ( -- seq )
    .HERE
    { }  collection get values
    [ [ second suffix ] each ] each
    [ pathnameRead ] collect-by 
;



        
    
