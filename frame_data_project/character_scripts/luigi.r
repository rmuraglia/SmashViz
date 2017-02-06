# luigi.r

library(jsonlite)
library(ggplot2)
library(tidyverse)

source('../fdp_common.r')

# load in move information and determine blacklist for moves that require manual processing
moves_df <- import_from_kh('http://api.kuroganehammer.com/api/Characters/name/Luigi/detailedmoves')

skip_moves <- c('Fireball')
skip_string <- paste(skip_moves, collapse='|')

# get general 'all move' information 
frame_max <- max(as.numeric(moves_df$FAF), na.rm=TRUE)
unique_moves <- sapply(moves_df$moveName, parse_move) %>% unique(.)
num_unique_moves <- length(unique_moves)

# initialize information stores
all_frames <- as.data.frame(matrix(NA, nrow=num_unique_moves, ncol=frame_max))
colnames(all_frames) <- c(1:frame_max)
rownames(all_frames) <- unique_moves
all_attribs <- all_frames


# run character specific stuff to fill in dfs for skip moves

# commong plotting functions with character name as passed param