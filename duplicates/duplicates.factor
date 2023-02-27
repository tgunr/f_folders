! File: duplicates.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Another fine Factor file!
! Copyright (C) 2017 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.sha folder folder.collection io
continuations io.directories io.files.links io.pathnames kernel layouts locals
math namespaces prettyprint prettyprint.config regexp sequences sequences.extras
serialize splitting threads words unix.ffi
folder.collection ;

FROM: string => to-folder ;
IN: folder.duplicates

SYMBOL: duplicates  
SYMBOL: errors  errors [ { } ] initialize

: which-to-delete? ( entry entry -- entry )
    { } 2sequence dup
    [ R/ .*_1\..*/ find-in-name ] filter 
    dup length
    [ drop first ]
    [ B drop first ] if
    ;
    
: shasum-dups ( dups -- shasumdups )
    "Checking full sha:" print
    [ ! V{ array array }
        [ ! { entry fingerprint }
            first ! entry
            dup pathnameRead
            dup print yield
            sha1 checksum-file ! entry fingerprint
            >>fingerprint ! entry
            [ fingerprint>> ] keep ! fingerprint entry
            [ { } ] 2dip ! { } fingerprint entry
            swap ! entry fingerprint
            [ suffix ] dip ! { entry } fingerprint }
            suffix ! { entry fingerprint }
        ] map
    ] map
    ;

: duplicates-from-collection ( -- {dups} )
    ! any element with 2 or more items is possible dup
    collection get
    [ nip
      length 2 >=
    ] assoc-filter
    values
    ;

: remove-entry ( entry -- )
    HERE.S
    pathnameRead duplicates get delete-at ;

: delete-entry ( entry -- )
    [ [ pathnameRead delete-file ] keep
      remove-entry ]
    [ error>string print  drop ]
    recover
;

: delete-dups ( dups -- )
    HERE.S
    [ name. delete-entry ] each ;

: dups-in-same-folder ( array -- dups )
    [ length 2 >= ] filter
    [
        [ second  [ fingerprintRead ] keep ]
        map>alist
        [ first ] collect-by values
        [ length 2 >= ] filter 
    ] map B
    ;

: collect-same-folder ( dups -- seq )
    [ second [ path>> ] keep ] map>alist
    [ first ] collect-by  values
    dups-in-same-folder
    ;

: shortest-name ( seq -- index )
    [let most-positive-fixnum :> len!  f :> result!  0 :> index! 
    [ index!
          nameRead length :> thisLen
          thisLen len <
          [ thisLen len!  index result! ] when
    ] each-index
    result ] ;

: .dupFolder ( folder -- )
    "Deleting duplicates in folder: " pprint
    pathRead print ;

: duplicates-by-folder ( -- )
    .HERE
    collection-by-folders
    duplicates set ;

: duplicates-by-fingerprint ( -- seq )
    { }  duplicates get values
    [ [ suffix ] each ] each
    [ fingerprintRead ] collect-by 
    ;

FROM: folder => pathname ;

: delete&relink ( entry1 entry2 -- )
    dup does-exist?
    [ dup delete-entry ] when
    ".... " over pathname append print
    hardlink-entries
    ;

: symlink-dups ( -- )
    duplicates-by-fingerprint
    [ nip length 2 >= ] assoc-filter values 
    [
        unclip
        "Symlink to: " over pathnameRead append print
        swap
        [ over swap
          [ pathnameRead ] bi@
          dup pprint ": " pprint
          dup delete-file
          link .
        ] each
        drop
    ] each
    ;

:: (delete-dups-in-folder) ( vector -- )
    vector 
    values :> dups!
    dups [ shortest-name ] keep nth name>> :> save
    dups [ name>> save = not ] filter dups! 
    dups empty? [
        "No duplicates" print ]
    [ dups first .dupFolder
      dups delete-dups ]
    if
    ;

: delete-dups-in-folder ( vector -- )
      [ B (delete-dups-in-folder) ] each ;

SYMBOL: dup-size
0 dup-size set
:: namex ( v -- )
    v values :> dups
    dups empty? [ B
        dups
        [ second :> e
          e name>> .
          dup-size get
          e info>> size-on-disk>> +
          dup-size set  ] each
    ] unless ;

SYMBOL: dup-process

! an array of vectors, each elementis an array of duplicate fingerprints
: process-each-duplicate ( dups -- )
    HERE.S
    [ 
      [ second pathRead ] collect-by
      values  [ length 2 >= ] filter
      dup-process get execute( x -- )
    ] each 
    ;

: process ( -- )
    .HERE
    duplicates-by-folder
    duplicates-from-collection
    process-each-duplicate
    ;

: do-process ( process -- )
    dup-process set  process ;

: delete-in-folder ( -- )
    \ delete-dups-in-folder do-process ;

: show-size ( -- )
    \ namex  do-process ;


: print-duplicates ( -- )
    duplicates-from-collection
    [ [ [ second [ path>> ] keep
          name>> append  pprint nl
        ] without-limits
      ] each nl
    ] each  
    ;        
