# rosalina.r


library(jsonlite)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)
library(gridExtra)

source('../fdp_common.r')

moves_df <- import_from_kh('http://api.kuroganehammer.com/api/Characters/name/Rosalinaluma/detailedmoves')

# set up filters for uninteresting or problematic moves
moves_df <- filter(moves_df, !grepl('throw|Landing|Decend|Aerial|^Blade.*Late)$|Limit Break', moveName))
skip_moves <- c('Windbox', 'Blade Beam', '^Cross Slash.*)$')
skip_string <- paste(skip_moves, collapse='|')