# explore.r

library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(ggthemes)

###########
# load and clean data
###########

dat_raw <- read_delim('data/anthers_12_21_2016_wiiuranked_patch1.1.6.tsv', delim='\t', na=c('NULL'))

# generate these maps like:
# mysql -e 'select id, name from anthers_12_21_2016.characters where game_id = 4;' > characters.tsv
character_map <- read_delim('data/characters.tsv', delim='\t', col_names=TRUE)
stage_map <- read_delim('data/stages.tsv', delim='\t', col_names=TRUE)

# which fields have NAs?
na_fields <- apply(dat_raw, 2, function(x){any(is.na(x)) })
print(names(na_fields)[na_fields])
# stage_pick, game_winner, p1_bans, p2_bans

# we expect the bans to have NAs, but stage_pick and game_winner are weird.
stage_nas <- which(is.na(dat_raw$stage_pick)) # very few - will just filter
game_winner_nas <- which(is.na(dat_raw$game_winner)) # quite a few -  are most missing game winners from set deciding games where someone decided to bail?
filter(dat_raw, is.na(game_winner)) %>% group_by(match_count, game_number) %>% summarize(n()) # not really, hard to tell. if this is the only info missing, just infer game_winner from set count and winner.

# remove games with no stage selection, and remove game 3616839 - has game_winner marked at 3, set winner is 1 but p2 won g1, so unclear what happened here
dat_trim <- dat_raw %>% filter(!is.na(stage_pick)) %>% filter(game_winner != 3 | is.na(game_winner))

# infer NA game winners based on set winner
dat_clean <- mutate(dat_trim, game_winner = coalesce(game_winner, set_winner))

# map character and stage ids to names
dat_clean <- dat_clean %>% mutate(p1_charid = factor(p1_charid, levels=character_map$id, labels=character_map$name), p2_charid = factor(p2_charid, levels=character_map$id, labels=character_map$name), stage_pick = factor(stage_pick, levels=stage_map$id, labels=stage_map$name))

# get max number of bans
p1_ban_counts <- sapply(dat_clean$p1_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
p2_ban_counts <- sapply(dat_clean$p2_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
p1_ban_max <- max(p1_ban_counts)
p2_ban_max <- max(p2_ban_counts)

# split bans into separate fields
dat_final <- dat_clean %>% separate(p1_bans, into=paste('p1_ban', c(1:p1_ban_max), sep='_'), sep='-', remove=TRUE) %>% separate(p2_bans, into=paste('p2_ban', c(1:p2_ban_max), sep='_'), sep='-', remove=TRUE)



############
# quick visualizations
############

# check joint distribution of player ratings to see if most matches are between closely matched players -- when imbalanced, does it lean in the direction we expect?
rating_labels <- c('1' = 'P1 Win', '2' = 'P2 Win')
p01 <- ggplot(dat_clean, aes(x=p1_rating, y=p2_rating)) + geom_hex(binwidth=25) + facet_grid(.~game_winner, labeller = labeller(game_winner = rating_labels)) + geom_abline(slope=1, intercept=1) +  scale_fill_gradient(limits=c(250, 1600)) + ggtitle('Joint distribution of match participant ladder ratings')
ggsave(file='figures/ratings-joint-hex.png', width=12, height=6, dpi=150, plot=p01)

# check distribution of character use rates and rating
p1_info <- dat_clean %>% select(rating = p1_rating, charid = p1_charid)
p2_info <- dat_clean %>% select(rating = p2_rating, charid = p2_charid)
all_player_info <- union_all(p1_info, p2_info)

p02 <- ggplot(all_player_info, aes(x=rating)) + geom_histogram(binwidth=20, fill='lightsteelblue1', colour='dodgerblue3') + ggtitle('Distribution of player ladder ratings')
ggsave(file='figures/ratings-hist.png', width=6, height=4, dpi=150, plot=p02)

# to reorder bars: http://stackoverflow.com/a/27448463
p03 <- ggplot(all_player_info, aes(x=fct_infreq(charid))) + geom_bar(fill='coral', colour='coral4') + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), axis.title.x=element_blank()) + labs(title='Match participation (game counts) in ranked Wii U matches on Anther\'s Ladder from 05/20/2016 - 12/21/2016') 
ggsave(file='figures/character-matchcounts.png', width=12, height=6, dpi=150, plot=p03)

# check distribution of player rating differences
rating_diff <- dat_clean %>% transmute(rating_gap = p1_rating - p2_rating, abs_rating_gap = abs(rating_gap))

p04 <- ggplot(rating_diff, aes(x=rating_gap)) + geom_density(fill='lightsteelblue1', colour='dodgerblue3')
p05 <- ggplot(rating_diff, aes(x=abs_rating_gap)) + geom_density(fill='lightsteelblue1', colour='dodgerblue3')
p06 <- arrangeGrob(p04, p05, nrow=2)
grid.draw(p06)


# stage use counts
stage_stats <- dat_clean %>% select(game_number, stage_pick) %>% group_by(game_number, stage_pick) %>% summarize(count = n()) %>% group_by(game_number) %>% mutate(frequency = count/sum(count))
stage_order <- c('Battlefield', 'Final Destination', 'Smashville', 'Town and City', 'Lylat Cruise', 'Dreamland', 'Duck Hunt')
stage_abbrv <- c('BF', 'FD', 'SV', 'TC', 'LC', 'DL', 'DH')

# overall counts
p07 <- ggplot(stage_stats, aes(x=stage_pick, y=count)) + geom_bar(stat='identity') + scale_x_discrete(limits=stage_order) + scale_y_continuous(breaks=pretty_breaks(5)) + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
ggsave(file='figures/stage-counts.png', width=5, height=6, dpi=150, plot=p07)

# use frequency per game
p08 <- ggplot(stage_stats, aes(x=stage_pick, y=frequency)) + geom_bar(stat='identity') + scale_x_discrete(limits=stage_order, labels=stage_abbrv) + facet_grid(game_number~., labeller = labeller(game_number = setNames(paste('Game ', c(1:5), sep=''), c(1:5))))
ggsave(file='figures/stage-freqs-bygame.png', width=4, height=10, dpi=150, plot=p08)


######
# case studies
######

pick_char_games <- function(dat, name) {
    # pick out games belonging to a character (remove dittos)
    p1_games <- filter(dat, p1_charid == name) %>% mutate(char_win = ifelse(game_winner == 1, 1, 0))
    p2_games <- filter(dat, p2_charid == name) %>% mutate(char_win = ifelse(game_winner == 2, 1, 0))
    char_games <- union_all(p1_games, p2_games) %>% filter(p1_charid != p2_charid)
    return(char_games)
}

base_win_rate <- function(dat) {
    # get baseline win rate for that character, not factoring in stage or mu
    dat %>% group_by(char_win) %>% summarize(count = n()) %>% mutate(bwr = count/sum(count)) %>% select(-count)
}

stage_win_rate <- function(dat) {
    # get winrate per stage
    dat %>% group_by(char_win, stage_pick) %>% summarize(count = n()) %>% group_by(stage_pick) %>% mutate(swr = count/sum(count))
}

stage_differential <- function(bwr, swr, name) {
    swr_wins <- filter(swr, char_win == 1)
    inner_join(bwr, swr, by = 'char_win') %>% filter(char_win == 1) %>% mutate(win_diff = swr - bwr) %>% mutate(character = name) %>% select(character, base_win_rate = bwr, stage_pick, num_wins = count, stage_win_rate = swr, win_diff) %>% arrange(desc(win_diff))
}

case_study <- function(dat, name) {
    case_dat <- pick_char_games(dat, name)
    case_bwr <- base_win_rate(case_dat)
    case_swr <- stage_win_rate(case_dat)
    case_out <- stage_differential(case_bwr, case_swr, name)
    return(case_out)
}

bronze_max <- 1245

# choose gap cutoff:
# quantile(rating_diff$abs_rating_gap, probs = seq(0, 1, length.out=21))
gap_cutoff <- 100

### zss: expect + on bf/dl, - on lc

zss1 <- case_study(dat_clean, 'Zero Suit Samus')

# filter to only matches between players within 100 pts (roughly within one rating class eg bronze, silver)
zss2 <- filter(dat_clean, abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Zero Suit Samus')

# remove bronze players
zss3 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% case_study('Zero Suit Samus')

# remove bronze and filter to players within 100 pts
zss4 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Zero Suit Samus')

### little mac: expect + on fd - on dh
lm1 <- case_study(dat_clean, 'Little Mac')
lm2 <- filter(dat_clean, abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Little Mac')
lm3 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% case_study('Little Mac')
lm4 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Little Mac')

### game and watch -- how low will game counts go?
gnw1 <- case_study(dat_clean, 'Mr. Game And Watch')
gnw2 <- filter(dat_clean, abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Mr. Game And Watch')
gnw3 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% case_study('Mr. Game And Watch')
gnw4 <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Mr. Game And Watch')


# compare some top tiers
diddy <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Diddy Kong')
cloud <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Cloud')
sheik <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Sheik')
mario <-  filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Mario')
sonic <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Sonic')
zss <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Zero Suit Samus')
bayo <- filter(dat_clean, p1_rating > bronze_max & p2_rating > bronze_max) %>% filter(abs(p1_rating - p2_rating) < gap_cutoff) %>% case_study('Bayonetta')

top_tiers <- rbind(diddy, cloud, sheik, mario, sonic, zss, bayo) %>% arrange(desc(win_diff)) 

select(top_tiers, character, stage_pick, win_diff) %>% 
filter(abs(win_diff)>0.005) %>% as.data.frame()


