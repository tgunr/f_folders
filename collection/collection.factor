! File: collection.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Operations on collections of folders/files
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.sha continuations folder
io io.directories io.files.links io.pathnames
kernel layouts locals math namespaces prettyprint regexp sequences
sequences.extras serialize splitting threads unix.ffi words ;

IN: folder.collection
FROM: string => to-folder ;

SYMBOL: COLLECTION-FOLDER 
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

: collection-file ( -- path )
    COLLECTION-FOLDER get
    "/collection.save" append ;

: save-collection ( -- )
    collection get collection-file save-fstore ;
 
: (restore-collection) ( path -- )
    load-fstore  collection set ;

: restore-collection ( -- )
   collection-file (restore-collection) ; 

: filter-entries ( entries -- entries )
    [ is-dot? not ] filter ;

: filter-single-entry ( entries -- entries )
    [ entries>> length 1  = ] filter ;

: (collect-files) ( path -- )
    to-folder
    HERE"Collecting files:"
    deep folder-entries
    HERE" Fingerprinting, showing progress every 100 files"
    [
        ENTRY-COUNT inc
        ENTRY-COUNT get
        COUNTBY get mod 0 = [  
            dup name>> HERE.
        ] when
        fingerprintGet
        yield .countby
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
    COLLECTION-FOLDER set
    collection get 
    [ "collection already exists" print ] 
    [ COLLECTION-FOLDER get  dup 
        [ 100 count-by
          [ (collect-files) ] curry "Collect Files" <thread>
          (collect-files-callback)
          (spawn)
        ]
        [ drop  "COLLECTION-FOLDER IS NOT SET!" print ]
        if
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



        
    
