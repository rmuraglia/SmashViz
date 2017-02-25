# sheik.r


library(jsonlite)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)
library(gridExtra)

source('../fdp_common.r')

moves_df <- import_from_kh('http://api.kuroganehammer.com/api/Characters/name/Sheik/detailedmoves')

# set up filter for moves to be excluded from the table
moves_df <- filter(moves_df, !grepl('throw|Landing', moveName))

# set up filter for moves that need to be handled manually 
skip_moves <- c('Rapid Jab$', 'Needle', 'Burst', 'Vanish', 'Bouncing')
skip_string <- paste(skip_moves, collapse='|')

# get general 'all move' information 
frame_max <- max(as.numeric(moves_df$FAF), na.rm=TRUE)
unique_moves <- sapply(moves_df$moveName, stem_move_names) %>% unique(.)

# add additional move slots
unique_moves[20] <- 'Needle Storm (Charging)'
unique_moves <- append(unique_moves, c('6 Needle Throw', '1-5 Needle Throw', 'Needle Hits'), 20)
unique_moves[26] <- 'Bouncing Fish (Earliest)'
unique_moves <- append(unique_moves, 'Bouncing Fish (Latest)')
