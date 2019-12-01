! File: music.factor
! Version: 0.1
! DRI: Dave Carlton
! Description: Another fine Factor file!
! Copyright (C) 2019 Dave Carlton.
! See http://factorcode.org/license.txt for BSD license.

USING: accessors assocs checksums checksums.sha continuations folder
folder.collection.music io io.directories io.files.links io.pathnames
kernel layouts locals math namespaces prettyprint regexp sequences
sequences.extras serialize splitting threads unix.ffi words folder.collection ;

IN: folder.collection.music

FROM: string => to-folder ;

CONSTANT: MUSIC-FOLDER "/Users/davec/Music/Plex"
CONSTANT: MUSIC-COLLECTION "/tmp/music.fstore"

SYMBOL: SINGLES

: restore-collection ( -- )
    MUSIC-COLLECTION (restore-collection) ; 


: (singles) ( -- folder )
    SINGLES get to-folder ;

: init-singles ( -- seq singles-folder )
    MUSIC-FOLDER COLLECTION-FOLDER set
    MUSIC-COLLECTION COLLECTION set
    "~/Music/Plex/Singles" SINGLES set
    get-collection
    filter-single-entry
    (singles)
    ;

: delete-single-entries ( seq -- )
    [ delete-entry-tree ] each ;

: do-singles ( -- )
    init-singles 
    over [ over move-entries ] each drop
    delete-single-entries ;

