# 4brv3-anthers.r

# purpose: compare 'theoretical' measure of character quality (4BR tier list score) with:
#   1. practical measure of character quality (win rate on anthers) -> subset to close matches, or matches for high ranked players
#   2. measure of character popularity (anthers use rate)

library(tidyverse)
library(grid)
library(scales)
library(png)

#####
# data prep
#####

# load 4BR data
br_raw <- read_delim('4BR-v3.txt', delim='\t', col_names=TRUE)
br_dat <- select(br_raw, char = Character, score = `Ordered Score`) %>% mutate(rank3 = nrow(br_raw) - score)

# fix character names to match anther's names
fix_names <- c('Rosalina And Luma', 'Megaman', 'Pacman', 'Mr. Game And Watch', 'Metaknight', 'Pit', 'Duck Hunt Duo')
fix_inds <- c(8, 16, 45, 39, 14, 30, 36)
br_dat[fix_inds,1] <- fix_names

# load anther's data
an_raw <- read_delim('/Users/rmuraglia/GitHub/SmashViz/stage_advantage/data/anthers_12_21_2016_wiiuranked_patch1.1.6_matches.tsv', delim='\t', na=c('NULL'))
an_dat <- an_raw %>% filter(!is.na(stage_pick)) %>% filter(game_winner != 3 | is.na(game_winner)) %>% mutate(game_winner = coalesce(game_winner, set_winner))

# load character map and update anther's data with character names
character_map <- read_delim('/Users/rmuraglia/GitHub/SmashViz/stage_advantage/data/characters.tsv', delim='\t', col_names=TRUE)
an_dat <- an_dat %>% mutate(p1_charid = factor(p1_charid, levels=character_map$id, labels=character_map$name), p2_charid = factor(p2_charid, levels=character_map$id, labels=character_map$name))

######
# make function for mapping names to rasters
######

imgpre <- '/Users/rmuraglia/GitHub/SmashViz/StockIcons/stock_90_'
imgpost <- '_01.png'

make_rasters <- function(df, alpha = 1) {
    raster_list <- list()
    for (i in 1:nrow(df)) {
        char_string <- df[i,1]

        # go through exceptions for character naming
        if (char_string=='Diddy Kong') { char_string <- 'diddy' 
        } else if (char_string=='Rosalina And Luma') { char_string <- 'rosetta' 
        } else if (char_string=='Zero Suit Samus') { char_string <- 'szerosuit' 
        } else if (char_string=='Corrin') { char_string <- 'kamui' 
        } else if (char_string=='Megaman') { char_string <- 'rockman' 
        } else if (char_string=='Villager') { char_string <- 'murabito' 
        } else if (char_string=='Toon Link') { char_string <- 'toonlink' 
        } else if (char_string=='Greninja') { char_string <- 'gekkouga' 
        } else if (char_string=='Captain Falcon') { char_string <- 'captain' 
        } else if (char_string=='Bowser') { char_string <- 'koopa' 
        } else if (char_string=='Olimar') { char_string <- 'pikmin' 
        } else if (char_string=='Donkey Kong') { char_string <- 'donkey' 
        } else if (char_string=='R.O.B.') { char_string <- 'robot' 
        } else if (char_string=='Robin') { char_string <- 'reflet' 
        } else if (char_string=='Duck Hunt Duo') { char_string <- 'duckhunt' 
        } else if (char_string=='Mr. Game And Watch') { char_string <- 'gamewatch' 
        } else if (char_string=='Little Mac') { char_string <- 'littlemac' 
        } else if (char_string=='Charizard') { char_string <- 'lizardon' 
        } else if (char_string=='Bowser Jr.') { char_string <- 'koopajr' 
        } else if (char_string=='Wii Fit Trainer') { char_string <- 'wiifit' 
        } else if (char_string=='Dr. Mario') { char_string <- 'drmario' 
        } else if (char_string=='King Dedede') { char_string <- 'dedede' 
        } else if (char_string=='Ganondorf') { char_string <- 'ganon' 
        } else if (char_string=='Jigglypuff') { char_string <- 'purin' 
        } else if (char_string=='Mii Gunner') { char_string <- 'miigunner' 
        } else if (char_string=='Mii Brawler') { char_string <- 'miienemyf' 
        } else if (char_string=='Mii Swordsman') { char_string <- 'miiswordsman' 
        }

        # force classic Wario skin
        if (char_string=='Wario') { img <- readPNG(paste(imgpre, tolower(char_string), '_05.png', sep='')) 
        } else { img <- readPNG(paste(imgpre, tolower(char_string), imgpost, sep='')) }

        # alpha controls transparency (0 = transparent, 1 = opaque)
        pre_raster <- matrix(rgb(img[,,1], img[,,2], img[,,3], img[,,4]*alpha), nrow=dim(img)[1])

        # turn into raster object
        raster_list[[i]] <- rasterGrob(pre_raster, interpolate=TRUE)
    }
    return(raster_list)
}

make_annotation<-function(raster, x, y) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

######
# 4BR vs character usage
######

all_an_usage <- bind_rows(select(an_dat, char = p1_charid), select(an_dat, char = p2_charid)) %>% group_by(char) %>% summarize(count = n()) %>% arrange(desc(count))

df1 <- full_join(br_dat, all_an_usage, by='char') %>% na.omit

rasters <- make_rasters(df1)

img_offset <- 1.5
p1 <- ggplot(data=df1, aes(x=rank3, y=count)) + geom_blank() + stat_smooth(se=FALSE, formula=y~poly(x,4), method='lm', linetype='dashed') + mapply(make_annotation, rasters, df1$rank3, log10(df1$count)) +  scale_y_log10(breaks=pretty_breaks(10), limits=c(4000, 60000)) + labs(x='4BR v3 score', y='Game count', title='Comparison of 4BR tier list placing with Anther\'s ladder usage rates by @Quappo_') 
ggsave(file='usage1.png', plot=p1, width=8, height=8, dpi=100)

img_offset <- 1500
p2 <- ggplot(data=df1, aes(x=rank3, y=count)) + geom_blank() + mapply(make_annotation, rasters, df1$rank3, df1$count) + scale_y_continuous(breaks=pretty_breaks(5)) + labs(x='4BR v3 score', y='Game count', title='Comparison of 4BR tier list placing with Anther\'s ladder usage rates by @Quappo_') 
ggsave(file='usage2.png', plot=p2, width=8, height=8, dpi=100)

#####
# 4BR vs win rate (all players)
#####
all_win_p1 <- an_dat %>% filter(game_winner == 1) %>% select(char = p1_charid)
all_win_p2 <- an_dat %>% filter(game_winner == 2) %>% select(char = p2_charid)
all_win_count <- bind_rows(all_win_p1, all_win_p2) %>% group_by(char) %>% summarize(win_count = n())
all_win_rate <- inner_join(all_an_usage, all_win_count, by='char') %>% mutate(win_rate = win_count/count) %>% arrange(desc(win_rate))

df2 <- inner_join(br_dat, all_win_rate, by='char')

img_offset <- 1.5
p3 <- ggplot(data=df2, aes(x=rank3, y=win_rate)) + geom_blank() + geom_abline(slope=0, intercept=0.5, colour='red', linetype='dashed') + mapply(make_annotation, rasters, df2$rank3, df2$win_rate) + labs(x='4BR v3 score', y='Win rate (all matches)', title='Comparison of 4BR tier list placing with Anther\'s ladder win rates by @Quappo_') 
ggsave(file='winrate1.png', plot=p3, width=8, height=8, dpi=100)

#####
# 4BR vs win rate (filter to close matches)
#####

gap_cutoff <- 100
an_filter1 <- filter(an_dat, abs(p1_rating - p2_rating) < gap_cutoff)

filter1_usage <- bind_rows(select(an_filter1, char = p1_charid), select(an_filter1, char=p2_charid)) %>% group_by(char) %>% summarize(count = n())
filter1_p1 <- an_filter1 %>% filter(game_winner==1) %>% select(char = p1_charid)
filter1_p2 <- an_filter1 %>% filter(game_winner==2) %>% select(char = p2_charid)
filter1_wins <- bind_rows(filter1_p1, filter1_p2) %>% group_by(char) %>% summarize(win_count = n())
filter1_win_rate <- inner_join(filter1_usage, filter1_wins, by='char') %>% mutate(win_rate = win_count/count) %>% arrange(desc(win_rate))
df3 <- inner_join(br_dat, filter1_win_rate, by='char')

img_offset <- 1.5
p4 <- ggplot(data=df3, aes(x=rank3, y=win_rate)) + geom_blank() + geom_abline(slope=0, intercept=0.5, colour='red', linetype='dashed') + mapply(make_annotation, rasters, df3$rank3, df3$win_rate) + labs(x='4BR v3 score', y='Win rate (all player skill levels)', title='Comparison of 4BR tier list placing with Anther\'s ladder win rates by @Quappo_') 
ggsave(file='winrate2.png', plot=p4, width=8, height=8, dpi=100)

#####
# 4BR vs win rate (close matches, gold and above)
#####

gold_thresh <- 1350
an_filter2 <- filter(an_filter1, p1_rating > gold_thresh & p2_rating > gold_thresh)
filter2_usage <- bind_rows(select(an_filter2, char = p1_charid), select(an_filter2, char=p2_charid)) %>% group_by(char) %>% summarize(count = n())
filter2_p1 <- an_filter2 %>% filter(game_winner==1) %>% select(char = p1_charid)
filter2_p2 <- an_filter2 %>% filter(game_winner==2) %>% select(char = p2_charid)
filter2_wins <- bind_rows(filter2_p1, filter2_p2) %>% group_by(char) %>% summarize(win_count = n())
filter2_win_rate <- inner_join(filter2_usage, filter2_wins, by='char') %>% mutate(win_rate = win_count/count) %>% arrange(desc(win_rate))
df4 <- inner_join(br_dat, filter2_win_rate, by='char') %>% filter(count>500)

rasters_sub <- make_rasters(df4)
img_offset <- 1.5
p5 <- ggplot(data=df4, aes(x=rank3, y=win_rate)) + geom_blank() + geom_abline(slope=0, intercept=0.5, colour='red', linetype='dashed') + mapply(make_annotation, rasters_sub, df4$rank3, df4$win_rate) + labs(x='4BR v3 score', y='Win rate (gold rank and above)', title='Comparison of 4BR tier list placing with Anther\'s ladder win rates by @Quappo_') 
ggsave(file='winrate3.png', plot=p5, width=8, height=8, dpi=100)


