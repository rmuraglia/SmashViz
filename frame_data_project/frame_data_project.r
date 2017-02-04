# frame_data_project.r

# objective: automate generation of plots for frame data visualization project by Floor
# url: https://smashboards.com/threads/frame-data-visualization-project.445205/

# each character has own file
# maybe make the actual running call with bash script instead

## finds all .R files within a folder and soruces them
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)

  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]

  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)

  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}


for SCRIPT in /path/to/scripts/dir/*
    do
        if [ -f $SCRIPT -a -x $SCRIPT ]
        then
            $SCRIPT
        fi
    done



early, late are notifiers we want to consider
landing is a modifier we want to ignore

intangible, reflect are key words for attributes


KEY:
TYPE 1: START UP
TYPE 2: MAIN
TYPE 3: RECOVERY
TYPE 4: EARLY/LATE


ATTRIBS:
TYPE 1: ARMOR 
TYPE 2: AUTOCANCEL




    # shield or armor icon, shine icon http://i.imgur.com/OorqPe9.png, what for autocncel