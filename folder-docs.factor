! Copyright (C) 2012 PolyMicro Systems.
! See http://factorcode.org/license.txt for BSD license.
USING: help.markup help.syntax io.directories kernel quotations
sequences strings prettyprint ;

IN: io.pathnames
HELP: bump-name
{ $values
  { "path" string }
  { "path'" string }
}
{ $description "Detects if the directory entry at path exists, and if so, adds a numbered"
  " extension to its name. If the the entry already has a numbered extension, the value"
  " is incremented and the check repeated until a unique file name is discovered."
  " Used by " { $link move-aside } " to save a file in place before a file operation occurs."
}
{ $notes
  {
      "If the entry " { $snippet "file" } " exists the name returned will be " { $snippet "file.1" } 
      " If the entry " { $snippet "file.1" } " exists the name returned will be " { $snippet "file.2" }
      " Note that if an entry contains a name representing a version, as in AnApp-1.2.3, the trailing"
      " value will be incremented."
  }
}
;

IN: folder

HELP: (directory-entries)
{ $values
    { "path" string }
    { "seq" sequence }
}
{ $description "Returns all of the items of a directory as folder entries given a"
  " a pathname as a string. Typically, you would normally use " { $link directory-entries }
  " to filter out the current and parent directories."
}
;

HELP: <folder>
{ $values
  { "name" string } { "type" { $link "file-types"  } } { "path" string }
    { "folder" folder-entry }
}
  { $description "Creates a folder-entry with the name, type, and path." }
{ $examples
    "Create a folder-entry for your home Desktop:"
    { $code
        """USING: folder ;
      "Desktop" +directory+ home <folder>"""
    }
  }
  
  ;

HELP: directory-entries
{ $values
    { "path" string }
    { "seq" sequence }
}
{ $description "Returns all of the items of a directory as folder entries given a"
  " a pathname as a string. Use " { $link (directory-entries) }
  " to include the current and parent directories."  
} ;

HELP: directory-files
{ $values
    { "path" "a pathname string" }
    { "seq" sequence }
}
{ $description "" } ;

HELP: folder-entry
{ $var-description "" } ;

HELP: pathname
{ $values
    { "folder" null }
    { "path" "a pathname string" }
}
{ $description "" } ;

HELP: with-directory-entries
{ $values
    { "path" "a pathname string" } { "quot" quotation }    
}
{ $description "" } ;

HELP: with-directory-files
{ $values
    { "path" "a pathname string" } { "quot" quotation }    
}
{ $description "" } ;

ARTICLE: "folder" "Folders"
"The " { $vocab-link "folder-entry" }
" vocabulary is a subclass of " { $snippet "directory-entry" } " in "
{ $vocab-link "io.directories" }  ". A folder-entry contains the an additional slot "
{ $slot "path" }
" as a full path to an entry. It is intended for use when the pathname is needed for more"
" complex operations on folders and files. The vocabulary contains words matching the usage"
" found in io.directories but works with folder entries instead of directory entries."
$nl
{ $subsections
    "cookbook"
    "help-impl"
} ;


ABOUT: "folder"
