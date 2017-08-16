! File: duplicates.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Another fine Factor file!
! Copyright (C) 2017 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs binary-search checksums checksums.fnv1
checksums.sha debugger folder io io.directories io.pathnames
kernel math math.order namespaces prettyprint regexp sequences
sequences.extras splitting string threads ;
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
    [ first R/ \._.*/ swap name=~? not ] filter ;

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
    checksum-entries
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

: get-files ( path -- )
    to-folder
    "Collecting files:" print
    deep folder-entries
    [ !checksum
      dup name>> print
      dup stamp>>
      yield
    ] map>alist
    [ second ] collect-by
    hold set
    ;

: flatten-dups ( dups -- dupseq )
    { } swap
    [ [ suffix ] each ] each ;

: shasum-dups ( dups -- shasumdups )
    "Checking full sha:" print
    [ ! V{ array array }
        [ ! { entry stamp }
            first ! entry
            dup pathname
            dup print yield
            sha1 checksum-file ! entry stamp
            >>stamp ! entry
            [ stamp>> ] keep ! stamp entry
            [ { } ] 2dip ! { } stamp entry
            swap ! entry stamp
            [ suffix ] dip ! { entry } stamp }
            suffix ! { entry stamp }
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

: delete-dups ( dups -- )
    "Deleting duplicates" print
    [ 1 tail
      [ first «name [ print ] dip
«pathname [ delete-file ] dip
        stamp>> hold get delete-at
      ] each
    ] each ;

: x ( path -- )
    hold get
    [ get-files ] [ drop ] if 
    hold get
    get-dups
!    shasum-dups
!    flatten-dups
!   [ second ] collect-by values
    delete-dups
    ;

