# stage_adv.r

library(tidyverse)
library(grid)
library(gridExtra)


###########
# load and clean data
###########

dat_raw <- read_delim('data/anthers_12_21_2016_wiiuranked_patch1.1.6_matches.tsv', delim='\t', na=c('NULL'))

# generate these maps like:
# mysql -e 'select id, name from anthers_12_21_2016.characters where game_id = 4;' > characters.tsv
character_map <- read_delim('data/characters.tsv', delim='\t', col_names=TRUE)
stage_map <- read_delim('data/stages.tsv', delim='\t', col_names=TRUE)

# remove games with no stage selection, and remove game 3616839 - has game_winner marked at 3, set winner is 1 but p2 won g1, so unclear what happened here
dat_trim <- dat_raw %>% filter(!is.na(stage_pick)) %>% filter(game_winner != 3 | is.na(game_winner))

# infer NA game winners based on set winner
dat_clean <- mutate(dat_trim, game_winner = coalesce(game_winner, set_winner))

# map character and stage ids to names
dat_clean <- dat_clean %>% mutate(p1_charid = factor(p1_charid, levels=character_map$id, labels=character_map$name), p2_charid = factor(p2_charid, levels=character_map$id, labels=character_map$name), stage_pick = factor(stage_pick, levels=stage_map$id, labels=stage_map$name))

# get max number of bans
# p1_ban_counts <- sapply(dat_clean$p1_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
# p2_ban_counts <- sapply(dat_clean$p2_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
# p1_ban_max <- max(p1_ban_counts)
# p2_ban_max <- max(p2_ban_counts)

# split bans into separate fields
# dat_final <- dat_clean %>% separate(p1_bans, into=paste('p1_ban', c(1:p1_ban_max), sep='_'), sep='-', remove=TRUE) %>% separate(p2_bans, into=paste('p2_ban', c(1:p2_ban_max), sep='_'), sep='-', remove=TRUE)

min_rating <- 1350
max_spread <- 100
min_matches <- 50
min_players <- 5

choose_list <- c('Bayonetta', 'Diddy Kong', 'Cloud', 'Sheik', 'Mario', 'Sonic', 'Fox', 'Rosalina And Luma', 'Mewtwo', 'Marth', 'Zero Suit Samus', 'Ryu', 'Corrin', 'Pikachu', 'Megaman', 'Villager', 'Toon Link', 'Captain Falcon')

dat_final <- dat_clean %>% filter(p1_rating > min_rating & p2_rating > min_rating) %>% filter(abs(p1_rating - p2_rating) < max_spread) %>% filter(p1_charid %in% choose_list & p2_charid %in% choose_list) %>% select(p1 = p1_charid, p2 = p2_charid, stage_pick, game_winner)

##########
# compute stage advantage and character matchup metrics
##########

# stage_list <- c('Battlefield', 'Final Destination', 'Duck Hunt', 'Town and City', 'Smashville', 'Dreamland', 'Lylat Cruise')
choose_ids <- c(272, 127, 268, 164, 144, 166, 132, 162, 186, 145, 173, 229, 273, 157, 146, 168, 167, 124)
stage_ids <- c(108, 63, 61, 85, 68, 119, 102)

stage_win_dat <- expand.grid(use_char = choose_ids, opp_char = choose_ids, stage = stage_ids) %>% as_data_frame %>% mutate(use_char = factor(use_char, levels=character_map$id, labels=character_map$name), opp_char = factor(opp_char, levels=character_map$id, labels=character_map$name), stage = factor(stage, levels=stage_map$id, labels=stage_map$name))

get_win_count <- function(dat, use_char, opp_char, stage) {
    g1 <- filter(dat, p1 == use_char & p2 == opp_char & stage_pick == stage) %>% mutate(use_win = as.numeric(ifelse(game_winner == 1, 1, 0)))
    g2 <- filter(dat, p1 == opp_char & p2 == use_char & stage_pick == stage) %>% mutate(use_win = as.numeric(ifelse(game_winner == 2, 1, 0)))
    g <- union_all(g1, g2)
    return(sum(g$use_win))
}

get_game_count <- function(dat, use_char, opp_char, stage) {
    g1 <- filter(dat, p1 == use_char & p2 == opp_char & stage_pick == stage) 
    g2 <- filter(dat, p1 == opp_char & p2 == use_char & stage_pick == stage) 
    g <- union_all(g1, g2)
    return(nrow(g))
}

stage_win_dat <- stage_win_dat %>% rowwise() %>% mutate(win_count = get_win_count(dat_final, use_char, opp_char, stage), game_count = get_game_count(dat_final, use_char, opp_char, stage))

sw_trim <- filter(stage_win_dat, game_count >= 10) %>% mutate(win_rate = win_count/game_count)

char_mu_baselines <- sw_trim %>% group_by(use_char, opp_char) %>% summarize(mu_baseline = mean(win_rate), games_played = sum(game_count))

sw_joined <- inner_join(sw_trim, char_mu_baselines, by=c('use_char', 'opp_char')) %>% mutate(stage_diff = win_rate - mu_baseline) %>% select(-games_played)

char_stage_adv <- sw_joined %>% group_by(use_char, stage) %>% summarize(stage_adv = mean(stage_diff), games_played = sum(game_count))

########
# generate plots
########

# 1) character matchup (with and without game counts)
char_mu_baselines


# 2) overall stage advantages (with and without game counts)
char_stage_adv

# 3) one plot per character: opponent character x stage full grid (with and without counts)
# filter on use_char for each plot
sw_joined



