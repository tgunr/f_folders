! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: definitions folder io.directories io.pathnames kernel
prettyprint sequences sets tools.continuations tools.test ;
IN: folder.tests


: folder-test ( x x -- seq )
    "~/factor/work/folders/test1" to-folder
    "~/factor/work/folders/test2" to-folder
    folder-compare
    ;

[ t ] [ create-test-folders t ] unit-test

