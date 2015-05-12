.onAttach <-
function(libname,pkgname) {
  packageStartupMessage(' done.
  Warning: This is an alpha-build, so there may be (a lot of) bugs.\n
  ===============\n')
  }
.onLoad <-
function(libname, pkgname) {
  packageStartupMessage('
  ===============\n
  Loading STUART...',appendLF=FALSE)
  }
.rgedit.lastline2clipboard <-
function( clipboard.command = "xclip" ) 
{
  # The possible clipboard commands:
  clipboard.commands <- c( "xclip", "xsel", "pbcopy" );
  # The correct command line depending on the option used:
  pipe.commands <- c( "xclip -i -selection clipboard", "xsel --clipboard", "pbcopy" );
  names(pipe.commands) <- clipboard.commands;

  # Try to detect which clipboard commands are installed in the system:
  available.clipboard.commands <- c( FALSE, FALSE, FALSE ); names(available.clipboard.commands) <- clipboard.commands;
  for( i in 1:length(available.clipboard.commands) )
  {
    available.clipboard.commands[i] <- length(system( paste( "command -v ", names(available.clipboard.commands)[i], sep=""), intern=TRUE )) > 0;
  } 
  if( sum(available.clipboard.commands) == 0 )
  {
    stop( "No cpliboard manipulation programs installed on your machine! Please install xclip or xsel (on *nix) and pbcopy (on MacOSX)!\n" );
  }

  # Try to satisfly the user's preference:
  if( sum(clipboard.command == names(pipe.commands)) == 0 )
  {
    # Unknown clipboard command: print a warning and try your best:
    cat( "Warning: unknown option ", clipboard.command );
    clipboard.command = (names(available.clipboard.commands)[available.clipboard.commands])[1];
    cat( " -- using ", clipboard.command, " instead...\n" );
  }
  else if( !available.clipboard.commands[clipboard.command] )
  {
    # Preferred command not installed: print a warning and try your best:
    cat( "Warning: requested command ", clipboard.command );
    clipboard.command = (names(available.clipboard.commands)[available.clipboard.commands])[1];
    cat( " -- using ", clipboard.command, " instead...\n" );
  }

  # Save whole shitory to temporaty file:
  hist.file <- tempfile( "Rhistory" );
  savehistory( hist.file );
  full.hist <- readLines( hist.file );
  unlink( hist.file );

  # Read the last line before this one:
  last.line <- full.hist[ length(full.hist)-1 ];
 
  # And copy it to the clipboard: 
  clipboard.pipe <- pipe( pipe.commands[clipboard.command], "w" );
  cat( paste(last.line,"\n",sep=""), file=clipboard.pipe );
  close( clipboard.pipe );
  
  # Restore the history without the last command:
  hist.file <- tempfile( "Rhistory" );
  writeLines( full.hist[ -length(full.hist) ], hist.file );
  loadhistory( hist.file );
  unlink( hist.file );
}
