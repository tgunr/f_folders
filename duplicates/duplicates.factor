! File: duplicates.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Another fine Factor file!
! Copyright (C) 2017 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.adler-32 checksums.fnv1
checksums.md5 checksums.sha folder folder.ext io io.directories io.pathnames
io.streams.c kernel math math.order namespaces regexp sequences
words sequences.extras sorting splitting threads ;

FROM: string => to-folder ;
FROM: folder => pathname ;
IN: folder.duplicates

CONSTANT: CHECK-FOLDER "/Volumes/alpha_music"

: print-names ( entries -- )
    [ name>> print ] each ;

: dot-in-name ( entries -- entries )
    dup
    [ name>> "." split  first "" = ] filter
    "Multiple . in name" print
    print-names
    ;

: more-than-2 ( entries -- entries )
    dup
    "Multiple parts" print
    [ name>> "." split length 2 = not ] filter
    print-names
    ;

: underscore-in-name ( entries -- entries )
    dup 
    "Multiple underscores" print
    [ name>> "_" split length 3 = ] filter
    print-names
    ;

: find-in-name ( entry regexp -- t|f )
    [ name>> ] dip re-contains? ; 

: filter-_1 ( seq:{array} -- seq:{array} )
    [ R/ .*_1\..*/ swap first find-in-name ] filter ;

: filter-dot-names ( seq:{array} -- seq:{array} )
    [ R/ ^\..*/ swap name=~? not ] filter ;

SYMBOL: hold

: which-to-delete? ( entry entry -- entry )
    { } 2sequence dup
    [ R/ .*_1\..*/ find-in-name ] filter 
    dup length
    [ drop first ]
    [ B drop first ] if
    ;
    
: get-entries ( -- )
    CHECK-FOLDER to-folder
    folder-entries
    fingerprint-entries
    hold set
    ;

: get-collection ( -- seq )
    hold get [
        CHECK-FOLDER qualified-directory-files
        "Collecting files:" print
        [ dup file-name print
          sha1 checksum-file
          yield ] map-zip 
        hold set
    ] unless
    hold get
    [ second ] collect-by
    ;

: filter-entries ( entries -- entries )
    filter-dot-names
    ;

: get-files ( path -- )
    to-folder
    "Collecting files:" print
    deep folder-entries
    filter-entries 
    [ dup name>> HERE.
      fingerprintGet
      yield
    ] map>alist
    [ first ] collect-by
    hold set
    ;

: flatten-dups ( dups -- dupseq )
    { } swap
    [ [ suffix ] each ] each ;

: shasum-dups ( dups -- shasumdups )
    "Checking full sha:" print
    [ ! V{ array array }
        [ ! { entry fingerprint }
            first ! entry
            dup pathname
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

: get-dups ( entries -- dups )
    ! any element with 2 or more items is possible dup
    [ nip
      length 2 >=
    ] assoc-filter
    values
    ;

: delete-dups ( dups x -- x )
    "Deleting duplicates" print
    [ 1 tail
      [ first nameGet [ print ] dip
        pathname [ delete-file ] dip
        fingerprintGet hold get delete-at
      ] each
    ] each ;

: x ( path x -- x )
    hold get
    [ get-files ] [ drop ] if 
    hold get
    get-dups
!    shasum-dups
!    flatten-dups
!   [ second ] collect-by values
    delete-dups
    ;

