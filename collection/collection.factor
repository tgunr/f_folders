! File: collection.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Operations on collections of folders/files
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors assocs checksums checksums.adler-32
checksums.fnv1 checksums.md5 checksums.sha continuations
file.xattr folder folder.fingerprint io io.backend io.files.info
io.files.types io.pathnames io.streams.c kernel math math.order
random namespaces sequences serialize sorting strings string threads words
math.parser ;
IN: folder.collection

SYMBOL: COLLECTION-FOLDER 
SYMBOL: collection

: name. ( entry -- entry )
    [ name>> print ] keep ;

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

: (collect) ( path -- )
    to-folder-tree
    "Collecting files" print
    folder-entries
    collection set
    "Saving collection" print
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

: by-folders ( collection -- collection )
    { }  swap values
    [ [ second suffix ] each ] each
    [ pathnameRead ] collect-by 
;

: collection-by-folders ( -- collection )
    collection get
    dup  random first >number string?
    [ ] [ by-folders ] if 
;


        
    
