! Copyright (C) 2012 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors classes combinators folder io io.directories
io.directories.hierarchy io.pathnames kernel locals namespaces regexp
sequences sets strings ;
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

: set-filter ( folder.compare filter -- folder.compare' )
    dup class-of
    {
        { string [ <regexp> ] }
        { regexp [ ] }
        [ 2drop regexp-nothing <regexp> ]
    } case
    >>filter 
    ;

: set-deep ( folder.compare t|f -- folder.compare' )
    [ [ directory-tree-files ] >>dir-word ]
    [ [ directory-files ] >>dir-word ]
    if
    ;

: folder.compare-new ( src dst filter deep? -- folder.compare )
    [ <folder-compare> ] 2dip
    [ set-filter ] dip set-deep
    set-src-entries
    set-dst-entries
;

GENERIC: to-folder-compare ( src dst -- folder.compare )
M: string to-folder-compare
    <folder-compare> ;

FROM: folder => pathname ;
M: folder-entry to-folder-compare
    swap pathname
    swap pathname
    <folder-compare> ;

: src-entries ( folder.compare -- seq )
    dup
    src-entries>> dup
    [ nip ]
    [ drop set-src-entries src-entries>> ] if 
    ;

: dst-entries ( folder.compare -- seq )
    dup
    dst-entries>> dup
    [ nip ] 
    [ drop set-dst-entries dst-entries>> ] if
    ;

: folder-entries ( folder.compare -- src-entries dst-entries )
    dup src-entries    
    swap dst-entries
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
     [ 1 tail first ] map
    ;

: rm ( folder.compare mode -- x )
    [ ] curry keep
    dst-path>> as-directory swap 
    [ over prepend ] map
    [ delete-file ] each
    drop
    ;

CONSTANT: f3 "/Volumes/data/Users/davec/Dropbox/FactorWork"
CONSTANT: f4 "/Volumes/data/Users/davec/Dropbox/FactorWork.1"
FROM: folder => pathname ;
: compare-this ( -- x )
    f3 f4 R/ .*[.]git.*/ t folder.compare-new
    ;

: test-diff ( -- )
    compare-this folder-diff [ print ] each
    ;
 
: test-intersect ( -- )
    compare-this folder-intersect [ print ] each
    ;

: test-union ( -- )
    compare-this folder-union [ print ] each
    ;

: test-md5 ( -- )
    compare-this md5diff [ pathname print ] each
    ;
