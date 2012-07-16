! Copyright (C) 2012 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors folder io.directories io.directories.hierarchy
io.pathnames kernel locals namespaces regexp sequences sets ;

IN: folder.compare

TUPLE: folder-compare src-path dst-path dir-word filter src-entries dst-entries ;
: <folder-compare> ( src dst -- folder.compare )
    folder-compare new
    swap >>dst-path
    swap  >>src-path
;
    
SYMBOL: folder-pair

: set-folder-pair-src ( src dir-word -- seq )
    [ directory-tree-files ]
    [ directory-files ]
    if
    folder-pair get  swap >>src-entries
    src-entries>>
    ;

: set-folder-pair-dst ( dst dir-word -- seq )
    [ directory-tree-files ]
    [ directory-files ]
    if
    folder-pair get swap >>dst-entries
    dst-entries>>
    ;

FROM: namespaces => set ;
:: set-folder-pair ( src dst dir-word -- src-seq dst-seq )
    src dst <folder-compare>
    folder-pair set
    src dir-word set-folder-pair-src
    dst dir-word set-folder-pair-dst
    ;

FROM: namespaces => set ;
:: src-dst-entries ( src dst dir-word -- src-entries dst-entries )
    src dst dir-word set-folder-pair
    ;

: set-src-entries ( folder.compare -- folder.compare' )
    [ dup filter>>
      over src-path>>
      rot dir-word>> call( path -- seq )
      [ swap matches? not ] with filter ] keep
    swap >>src-entries
;

: set-dst-entries ( folder.compare -- folder.compare' )
    [ dup filter>>
      over dst-path>>
      rot dir-word>> call( path -- seq )
      [ swap matches? not ] with filter ] keep
    swap >>dst-entries
;

: regexp-nothing ( -- regexp )  "(?!.*)" ;
: regexp-hidden ( -- regexp ) "[.].*" ;

: set-filter ( filter folder.compare -- folder.compare' )
    over
    [ swap <regexp> ]
    ! a regexp that doesn't match anything
    [ nip regexp-nothing <regexp> ]
    if
    >>filter
    ;

: set-deep ( dir-word folder.compare -- folder.compare' )
    swap
    [ [ directory-tree-files ] >>dir-word ]
    [ [ directory-files ] >>dir-word ]
    if
    ;

: folder.compare-new ( src dst filter deep? -- folder.compare )
    [ <folder-compare> ] 2dip
    rot set-deep set-filter
    dup set-src-entries
    swap set-dst-entries
    drop
;

: folder-entries ( folder.compare -- src-entries dst-entries )
    dup src-entries>>
    swap dst-entries>>
;

: folder-diff ( folder.compare -- seq )
   folder-entries diff
   ;

: folder-intersect ( folder.compare -- seq )
    folder-entries intersect
    ;

: folder-union ( folder.compare -- seq )
    folder-entries union
    ;

: folder-move ( folder.compare -- seq )
    ! List of files in SRC not in DST
    folder-entries diff
    ! List of files in SRC and in DST
    ! List of files in DST not in SRC (prune)
    ;

FROM: string => to-folder ;
:: gather-files ( seq folder.compare -- seq1 seq2 )
    folder.compare src-path>> as-directory :> path1
    folder.compare dst-path>> as-directory :> path2
    { } { } 
    seq [ 
        [ path1 prepend to-folder
          dup is-file?  over is-hidden?  not and
          [ suffix ] [ drop ] if
        ] keep 
        path2 prepend to-folder
        dup is-file? over is-hidden?  not and
        [ rot swap suffix ] [ drop swap ] if
        swap
    ] each
    ;

FROM: folder => pathname ;
:: md5diff ( folder.compare -- x )
    folder.compare folder-entries intersect
    folder.compare gather-files 
    [ [ checksum= ] 2keep
      V{ } swap prefix
      swap prefix
      swap prefix
    ] 2map
     [ first ] filter
     [ 1 tail ] map
    ;

: rm ( folder.compare mode -- x )
    [ ] curry keep
    dst-path>> as-directory swap 
    [ over prepend ] map
    [ delete-file ] each
    drop
    ;


