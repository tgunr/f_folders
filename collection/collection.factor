! File: collection.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Operations on collections of folders/files
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs checksums checksums.sha continuations
folder io io.backend io.directories io.files.links io.pathnames
kernel layouts locals math namespaces prettyprint regexp
sequences sequences.extras serialize splitting threads unix.ffi
words ;

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

: collection-path ( -- path )
    COLLECTION-FOLDER get
    "/collection.save" append  normalize-path ;

: save-collection ( -- )
    collection get collection-path save-fstore ;
 
: (restore-collection) ( path -- )
    load-fstore  collection set ;

: restore-collection ( -- )
   collection-path (restore-collection) ; 

: not-hidden ( entries -- entries )
    [ is-dot? not ] filter ;

: filter-single-entry ( entries -- entries )
    [ entries>> length 1  = ] filter ;

: (fingerprint) ( path -- )
    to-folder
    HERE"Fingerprinting files:"
    deep folder-entries
    HERE" Fingerprinting, showing progress every 100 files"
    [
        ENTRY-COUNT inc
        ENTRY-COUNT get
        COUNTBY get mod 0 = [  
            dup name>> HERE.
        ] when
        fingerprintGet
        yield
    ] map>alist
    HERE" Collect by"
    [ first ] collect-by
    collection set
    HERE" Save collection"
    save-collection
    ;

: (fingerprint-callback) ( thread -- thread )
    [ "Fingerprint files completed" print ] >>exit-handler ;

: path-fingerprint ( path -- )
    COLLECTION-FOLDER set
    collection get 
    [ "collection already exists" print ] 
    [ COLLECTION-FOLDER get  dup 
        [ 100 count-by
          [ (fingerprint) ] curry "Collect Files" <thread>
          (fingerprint-callback)
          (spawn)
        ]
        [ drop
          "COLLECTION IS NOT SET!" print ]
        if
    ]
    if
    ;

: (collect) ( path -- )
    to-folder
    HERE" Collecting files:"
    deep folder-entries
    collection set
    HERE" Save collection"
    save-collection
    ;

: path-collect ( path -- )
    COLLECTION-FOLDER set
    collection get 
    [ "collection already exists" print ] 
    [ COLLECTION-FOLDER get  dup 
      [ (collect) ] 
      [ drop  "COLLECTION-FOLDER IS NOT SET!" print ]
      if
    ]
    if
    ;

: path-collect! ( path -- )
    f collection set  path-collect ;

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



        
    
