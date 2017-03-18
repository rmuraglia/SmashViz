# scrub-busters.r

# look at character win rate differential between silver and below games versus gold and above games.
# if a character wins a lot in silver-, then they are a scrub  buster -- good at defeating weak players

library(tidyverse)

an_raw <- read_delim('/Users/rmuraglia/GitHub/SmashViz/stage_advantage/data/anthers_12_21_2016_wiiuranked_patch1.1.6_matches.tsv', delim='\t', na=c('NULL'))
an_dat <- an_raw %>% filter(!is.na(stage_pick)) %>% filter(game_winner != 3 | is.na(game_winner)) %>% mutate(game_winner = coalesce(game_winner, set_winner))

character_map <- read_delim('/Users/rmuraglia/GitHub/SmashViz/stage_advantage/data/characters.tsv', delim='\t', col_names=TRUE)
an_dat <- an_dat %>% mutate(p1_charid = factor(p1_charid, levels=character_map$id, labels=character_map$name), p2_charid = factor(p2_charid, levels=character_map$id, labels=character_map$name))

# filter to close matches
gap_cutoff <- 100
an_filter <- filter(an_dat, abs(p1_rating - p2_rating) < gap_cutoff)

get_win_rate <- function(df) {
    usage <- bind_rows(select(df, char = p1_charid), select(df, char = p2_charid)) %>% group_by(char) %>% summarize(game_count = n()) 
    wins1 <- df %>% filter(game_winner == 1) %>% select(char = p1_charid)
    wins2 <- df %>% filter(game_winner == 2) %>% select(char = p2_charid)
    wins <- bind_rows(wins1, wins2) %>% group_by(char) %>% summarize(win_count = n())
    win_rate <- inner_join(filter(usage, game_count > 500), wins, by='char') %>% mutate(win_rate = win_count/game_count)
    return(list(usage, wins, win_rate))
}

# get scrub win rates 
scrub_max <- 1225

an_filter_scrub <- filter(an_filter, p1_rating < scrub_max & p2_rating < scrub_max)
scrub_dfs <- get_win_rate(an_filter_scrub)

# get pro win rates
pro_min <- 1400
an_filter_pro <- filter(an_filter, p1_rating > pro_min & p2_rating > pro_min)
pro_dfs <- get_win_rate(an_filter_pro)

# join win rates and get difference
win_diff <- inner_join(scrub_dfs[[3]], pro_dfs[[3]], by='char') %>% select(char, scrub_wr = win_rate.x, pro_wr = win_rate.y) %>% mutate(win_diff = scrub_wr - pro_wr) %>% arrange(win_diff)

printA <- filter(win_diff, win_diff < -0.05) %>% filter(char != 'Random') %>% select(char, win_diff)
printB <- filter(win_diff, win_diff > 0.035) %>% select(char, win_diff) %>% arrange(desc(win_diff))