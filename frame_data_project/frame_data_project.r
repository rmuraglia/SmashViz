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
TYPE 1: STARTUP or RECOVERY [yellow] (no melee hitbox (only projectile))
TYPE 2: MAIN [red]
TYPE 3: EARLY/LATE [off red]
TYPE 4: ITEM SPAWN


ATTRIBS:
TYPE 1: ARMOR 
TYPE 2: AUTOCANCEL
TYPE 3: PROJECTILE 
TYPE 4: REFLECT
TYPE 5: WINDBOX

start up means frames before 1) a hitbox, 2) an item spawn, 3) first active projectile frame 
recovery means 1) after active hitbox, 2) after item spawn, 3) after first active projectile, 4) until FAF for moves without hitboxes




    # shield or armor icon, shine icon http://i.imgur.com/OorqPe9.png, what for autocncel