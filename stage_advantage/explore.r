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

dat_raw <- read_delim('data/anthers_12_21_2016_wiiuranked_patch1.1.6_matches.tsv', delim='\t', na=c('NULL'))

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
# maybe also as.Date(created_at) for easy comparisons to league breaks by season

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

# plat 1455+




## which mus are explored

baseline_mu_ratio <- function(dat, char1, char2, min_players, min_matches, min_rating, max_spread) {
    # get MU win ratio from data between <char1> and <char2>
    # require games to occur with players with a rating of at least <min_rating>
    # require players to be within <max_spread> rating points of each other
    # require <min_players> unique players of each character

    rating_filtered <- filter(dat, p1_rating > min_rating & p2_rating > min_rating) %>% filter(abs(p1_rating - p2_rating) < max_spread)
    games_A <- filter(rating_filtered, p1_charid == char1 & p2_charid == char2)
    games_B <- filter(rating_filtered, p1_charid == char2 & p2_charid == char1)
    players_1 <- union(select(games_A, pid = p1_id), select(games_B, pid = p2_id))
    players_2 <- union(select(games_A, pid = p2_id), select(games_B, pid = p1_id))
    games_all <- union_all(games_A, games_B)

    # throw error if unable to obtain a baseline win rate
    if (nrow(games_all) < min_matches) {
        # print('Not enough games to get a baseline')
        return(NA)
    }
    if (nrow(players_1) < min_players) {
        # print(paste('Not enough unique players for ', char1, ' to get a baseline', sep=''))
        return(NA)
    }
    if (nrow(players_2) < min_players) {
        # print(paste('Not enough unique players for ', char2, ' to get a baseline', sep=''))
        return(NA)
    }

    # compute baseline win rate from char1 perspective
    char1_wins <- bind_rows(transmute(games_A, c1_win = ifelse(game_winner == 1, 1, 0)), transmute(games_B, c1_win = ifelse(game_winner == 2, 1, 0)))
    char1_wr <- sum(char1_wins)/nrow(char1_wins)
    return(char1_wr)
}

min_rating <- 1350
max_spread <- 100
min_matches <- 50
min_players <- 5

baseline_wrs <- data_frame(p1 = character_map$name, p2 = character_map$name) %>% expand(p1, p2) %>% rowwise() %>% mutate(p1_wr = baseline_mu_ratio(dat_final, p1, p2, min_players, min_matches, min_rating, max_spread)) 

baseline2 <- baseline_wrs %>% mutate(p1 = factor(p1), p2 = factor(p2)) 

ggplot(baseline2, aes(x=p1, y=p2)) + geom_tile(aes(fill=!is.na(p1_wr)), colour='grey50') + scale_fill_manual(values=c('white', 'dodgerblue')) + theme(axis.text.x=element_text(size=8, angle=45, hjust=1, vjust=1), axis.text.y=element_text(size=8))


rm_list <- c('Dr. Mario', 'Duck Hunt Duo', 'Jigglypuff', 'Mii Brawler', 'Mii Gunner', 'Mii Swordsman', 'Olimar', 'Palutena', 'Random', 'Charizard', 'Wii Fit Trainer', 'Wario', 'Peach', 'Dark Pit', 'Pit', 'Mr. Game And Watch', 'Falco', 'Zelda', 'Metaknight', 'Kirby', 'Robin', 'Roy', 'Lucario', 'Bowser Jr.', 'Greninja', 'Lucina')
baseline3 <- baseline_wrs %>% filter(!(p1 %in% rm_list | p2 %in% rm_list)) %>% mutate(p1 = factor(p1), p2 = factor(p2)) 
ggplot(baseline3, aes(x=p1, y=p2)) + geom_tile(aes(fill=!is.na(p1_wr)), colour='grey50') + scale_fill_manual(values=c('white', 'dodgerblue')) + theme(axis.text.x=element_text(size=8, angle=45, hjust=1, vjust=1), axis.text.y=element_text(size=8))


choose_list <- c('Bayonetta', 'Diddy Kong', 'Cloud', 'Sheik', 'Mario', 'Sonic', 'Fox', 'Rosalina And Luma', 'Mewtwo', 'Marth', 'Zero Suit Samus', 'Ryu', 'Corrin', 'Pikachu', 'Megaman', 'Villager', 'Toon Link', 'Captain Falcon')
baseline4 <- baseline_wrs %>% filter(p1 %in% choose_list & p2 %in% choose_list) %>% mutate(p1 = factor(p1), p2 = factor(p2)) 
ggplot(baseline4, aes(x=p1, y=p2)) + geom_tile(aes(fill=!is.na(p1_wr)), colour='grey50') + scale_fill_manual(values=c('white', 'dodgerblue')) + theme(axis.text.x=element_text(size=8, angle=45, hjust=1, vjust=1), axis.text.y=element_text(size=8))


dat_filtered <- dat_final %>% filter(p1_rating > min_rating & p2_rating > min_rating) %>% filter(abs(p1_rating - p2_rating) < max_spread) %>% filter(p1_charid %in% choose_list & p2_charid %in% choose_list) %>% select(stage_pick, p1_charid, p2_charid) %>% group_by(p1_charid, p2_charid, stage_pick) %>% summarize(count=n()) %>% ungroup()

mu_stage_count <- function(dat, c1, c2, stage) {
    d1 <- filter(dat, p1_charid==c1 & p2_charid==c2 & stage_pick==stage)
    d2 <- filter(dat, p1_charid==c2 & p2_charid==c1 & stage_pick==stage)
    print(paste(c1, c2, stage, d1$count + d2$count, sep=', '))
}

for (i in 1:length(choose_list)) {
    for (j in i:length(choose_list)) {
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Battlefield')
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Final Destination')
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Duck Hunt')
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Town and City')
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Smashville')
        mu_stage_count(dat_filtered, choose_list[i], choose_list[j], 'Dreamland')
    }
}

################

mu_stage_count <- function(dat, c1, c2, stage) {
    d1 <- filter(dat, p1_charid==c1 & p2_charid==c2 & stage_pick==stage)
    d2 <- filter(dat, p1_charid==c2 & p2_charid==c1 & stage_pick==stage)
    if (nrow(d1) == 0 & nrow(d2) == 0) { 
        out_count <- 0
    } else if (nrow(d1) == 0 ) {
        out_count <- d2$count
    } else if (nrow(d2) == 0 ) {
        out_count <- d1$count
    } else { 
        out_count <- d1$count + d2$count 
    }
    # print(paste(c1, c2, stage, d1$count + d2$count, sep=', '))
    return(c(c1, c2, stage, out_count))
}

mu_stage_counts <- array(NA, dim=c(length(choose_list) * (length(choose_list)) * 7, 4))

stage_list <- c('Battlefield', 'Final Destination', 'Duck Hunt', 'Town and City', 'Smashville', 'Dreamland')

l <- 1
for (i in 1:length(choose_list)) {
    for (j in i:length(choose_list)) {
        for (k in stage_list) {
            mu_stage_counts[l,] <- mu_stage_count(dat_filtered, choose_list[i], choose_list[j], k)
            l <- l + 1
        }
    }
}

mu_stage_counts <- data_frame(p1=mu_stage_counts[,1], p2=mu_stage_counts[,2], stage=mu_stage_counts[,3], count=mu_stage_counts[,4]) 
mu_stage_counts <- mu_stage_counts %>% mutate(count = as.numeric(count))
table(mu_stage_counts$count)->jon
jon*as.numeric(names(jon))->bill
sum(bill[1:20])
