# stage_adv.r

library(tidyverse)
library(grid)
library(gridExtra)
library(png)
library(scales)

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

# initial look at matchups for which there is high quality data
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

baseline_wrs <- data_frame(p1 = character_map$name, p2 = character_map$name) %>% expand(p1, p2) %>% rowwise() %>% mutate(p1_wr = baseline_mu_ratio(dat_clean, p1, p2, min_players, min_matches, min_rating, max_spread))

baseline_plot_df <- filter(baseline_wrs, p1 != 'Random' & p2 != 'Random') %>% mutate(p1 = factor(p1), p2 = factor(p2)) %>% mutate(p1 = factor(p1, levels=rev(levels(p1))))

# plot main panel
p1_main <- ggplot(baseline_plot_df, aes(x=p2, y=p1)) + 
    geom_tile(aes(fill=is.na(p1_wr)), colour='grey50') + 
    scale_fill_manual(name = 'Matchups that have been explored at a high level on Anther\'s ladder since patch 1.1.6. \nAre there 50+ matches involving 5+ unique players for each character at gold level or above (players within 100 ranking points)?\n', labels=c('yes', 'no'), values=c('dodgerblue', 'white')) + 
    scale_x_discrete(position='top') + 
    theme(legend.position='top')

# extract legend
p1_legend_g <- ggplotGrob(p1_main)$grobs
p1_legend <- p1_legend_g[[which(sapply(p1_legend_g, function(x) x$name) == "guide-box")]]

# make vertical character icon strip
imgpre <- '/Users/rmuraglia/GitHub/SmashViz/StockIcons/stock_90_'
imgpost <- '_01.png'

make_rasters <- function(char_vec, alpha = 1) {
    raster_list <- list()
    for (char_string in char_vec) {

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
        } else if (char_string=='Dark Pit') {
            char_string <- 'pitb'
        }

        # force classic Wario skin
        if (char_string=='Wario') { img <- readPNG(paste(imgpre, tolower(char_string), '_05.png', sep='')) 
        } else { img <- readPNG(paste(imgpre, tolower(char_string), imgpost, sep='')) }

        # alpha controls transparency (0 = transparent, 1 = opaque)
        pre_raster <- matrix(rgb(img[,,1], img[,,2], img[,,3], img[,,4]*alpha), nrow=dim(img)[1])

        # turn into raster object
        raster_list[[char_string]] <- rasterGrob(pre_raster, interpolate=TRUE)
    }
    return(raster_list)
}

make_annotation <- function(raster, x, y, img_offset) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

p1_y_offset <- 1
p1_y_chars <- levels(baseline_plot_df$p1)
p1_y_rasters <- make_rasters(p1_y_chars)
p1_y_plot <- seq(from = p1_y_offset, to = length(p1_y_chars) * p1_y_offset, by = p1_y_offset)

p1_y_strip <- ggplot(data=as.data.frame(p1_y_plot), aes(y=p1_y_plot)) + 
    geom_blank() +
    mapply(make_annotation, p1_y_rasters, 0, p1_y_plot, p1_y_offset) + 
    theme_classic() + 
    theme(axis.ticks=element_blank()) +
    theme(axis.line=element_blank()) +
    scale_x_continuous(limits=c(-p1_y_offset, p1_y_offset), expand=c(0, 0)) + 
    scale_y_discrete(expand=c(0, 0.6)) +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
    theme(axis.title.x=element_blank()) + ylab('') +
    theme(plot.margin=unit(c(0, 0, 6, 6), 'pt'))

# make horizontal character icon strip
p1_x_offset <- 1
p1_x_chars <- levels(baseline_plot_df$p2)
p1_x_rasters <- make_rasters(p1_x_chars)
p1_x_plot <- seq(from = p1_x_offset, to = length(p1_x_chars) * p1_x_offset, by = p1_x_offset)

p1_x_strip <- ggplot(data=as.data.frame(p1_x_plot), aes(x=p1_x_plot)) + 
    geom_blank() +
    mapply(make_annotation, p1_x_rasters, p1_x_plot, 0, p1_x_offset) + 
    theme_classic() + 
    theme(axis.ticks=element_blank()) +
    theme(axis.line=element_blank()) +
    scale_y_continuous(limits=c(-p1_x_offset, p1_x_offset), expand=c(0, 0)) + 
    scale_x_discrete(expand=c(0, 0.6)) +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
    theme(axis.title.x=element_blank()) + xlab('') +
    theme(plot.margin=unit(c(6, 6, 0, 0), 'pt'))

# make totally blank plot to take up lower left corner space
blank_plot<-ggplot(data=as.data.frame(0)) + geom_blank() + theme_classic() + labs(x='', y='') + theme(plot.margin=unit(c(0,0,0,0), 'pt'))

# suppress features on main plot and add diagonal line
p1_main <- p1_main + theme(legend.position='none', axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    theme(plot.margin=unit(c(0,6,6,0), 'pt')) +
    geom_abline(slope=-1, intercept=length(p1_y_chars)+1)

# combine for final plot
p1_all <- arrangeGrob(blank_plot, p1_legend, blank_plot, p1_x_strip, p1_y_strip, p1_main, nrow=3, ncol=2, heights=c(2,1,24), widths=c(1,25))
ggsave(file='figures/01_explored_mus.png', width=14, height=10, dpi=100, plot=p1_all)

# get max number of bans
# p1_ban_counts <- sapply(dat_clean$p1_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
# p2_ban_counts <- sapply(dat_clean$p2_bans, function(x) { length(unlist(strsplit(x, split='-'))) })
# p1_ban_max <- max(p1_ban_counts)
# p2_ban_max <- max(p2_ban_counts)

# split bans into separate fields
# dat_final <- dat_clean %>% separate(p1_bans, into=paste('p1_ban', c(1:p1_ban_max), sep='_'), sep='-', remove=TRUE) %>% separate(p2_bans, into=paste('p2_ban', c(1:p2_ban_max), sep='_'), sep='-', remove=TRUE)

choose_list <- c('Bayonetta', 'Diddy Kong', 'Cloud', 'Sheik', 'Mario', 'Sonic', 'Fox', 'Rosalina And Luma', 'Mewtwo', 'Marth', 'Zero Suit Samus', 'Ryu', 'Corrin', 'Pikachu', 'Megaman', 'Villager', 'Toon Link', 'Captain Falcon')

stage_list <- c('Smashville', 'Battlefield', 'Final Destination', 'Town and City', 'Dreamland', 'Lylat Cruise', 'Duck Hunt')

dat_final <- dat_clean %>% filter(p1_rating > min_rating & p2_rating > min_rating) %>% filter(abs(p1_rating - p2_rating) < max_spread) %>% filter(p1_charid %in% choose_list & p2_charid %in% choose_list) %>% select(p1 = p1_charid, p2 = p2_charid, stage_pick, game_winner)

##########
# show stage selection distribution
##########
stage_counts <- dat_final %>% group_by(stage_pick) %>% summarize(count = n()) %>% mutate(percent = count/sum(count)*100) %>% mutate(stage_pick = factor(stage_pick, levels=stage_list))
p3 <- ggplot(stage_counts, aes(x=stage_pick, y=percent)) + geom_col() + 
    scale_y_continuous(breaks = seq(from=0, to=40, by=5), sec.axis=sec_axis(name='Game counts', trans=~./100*sum(stage_counts$count), breaks = round(seq(from=0, to=40, by=5)/100*sum(stage_counts$count)))) +
    labs(y='Percentage of games played on stage', title='Stage selection breakdown for matches between selected characters', x='')
ggsave(file='figures/03_stage_selections.png', width=7.5, height=7, dpi=150, plot=p3)

p3_alt_df <- dat_clean %>% filter(p1_rating > min_rating & p2_rating > min_rating) %>% filter(abs(p1_rating - p2_rating) < max_spread) %>% group_by(stage_pick) %>% summarize(count=n()) %>% mutate(percent = count/sum(count)*100) %>% mutate(stage_pick = factor(stage_pick, levels=stage_list))
p3_alt <- ggplot(p3_alt_df, aes(x=stage_pick, y=percent)) + geom_col() + 
    scale_y_continuous(breaks = seq(from=0, to=35, by=5), sec.axis=sec_axis(name='Game counts', trans=~./100*sum(p3_alt_df$count), breaks = round(seq(from=0, to=35, by=5)/100*sum(p3_alt_df$count)))) +
    labs(y='Percentage of games played on stage', title='Stage selection breakdown for matches between all characters', x='')
ggsave(file='figures/03_stage_selections_alt.png', width=7.5, height=7, dpi=150, plot=p3_alt)

##########
# compute stage advantage and character matchup metrics
##########

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
stage_win_dat <- stage_win_dat %>% mutate(use_char = factor(use_char, levels=rev(sort(choose_list))), opp_char = factor(opp_char, levels=sort(choose_list)))

#######
# make a raw MU chart
#######

raw_mu_chart <- stage_win_dat %>% group_by(use_char, opp_char) %>% summarize(win_count = sum(win_count), game_count = sum(game_count), win_rate = win_count/game_count)
raw_mu_chart <- left_join(expand(data_frame(use_char = choose_list, opp_char = choose_list), use_char, opp_char), raw_mu_chart, by=c('use_char', 'opp_char')) %>% mutate(use_char = factor(use_char, levels=rev(sort(choose_list))), opp_char = factor(opp_char, levels=sort(choose_list)))
raw_mu_chart <- raw_mu_chart %>% mutate(win_rate = ifelse(game_count < min_matches, NA, win_rate))

y_offset <- 1
x_offset <- 1
y_chars <- levels(raw_mu_chart$use_char)
x_chars <- levels(raw_mu_chart$opp_char)
y_rasters <- make_rasters(y_chars)
x_rasters <- make_rasters(x_chars)
y_plot <- seq(from = y_offset, to = length(y_chars) * y_offset, by = y_offset)
x_plot <- seq(from = x_offset, to = length(x_chars) * x_offset, by = x_offset)

y_strip <- ggplot(data=as.data.frame(y_plot), aes(y=y_plot)) +
    geom_blank() + 
    mapply(make_annotation, y_rasters, 0, y_plot, y_offset) +
    theme_classic() +
    theme(axis.ticks=element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), plot.margin=unit(c(0,0,6,6), 'pt')) +
    scale_x_continuous(limits = c(-y_offset, y_offset), expand=c(0,0)) +
    scale_y_discrete(expand=c(0, 0.6)) +
    ylab('P1 character')

x_strip <- ggplot(data=as.data.frame(x_plot), aes(x=x_plot)) +
    geom_blank() +
    mapply(make_annotation, x_rasters, x_plot, 0, x_offset) +
    theme_classic() +
    theme(axis.ticks=element_blank(), axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), plot.margin=unit(c(6,6,0,0), 'pt')) +
    scale_y_continuous(limits = c(-x_offset, x_offset), expand=c(0,0)) +
    scale_x_discrete(expand=c(0, 0.6), position='top') +
    xlab('P2 character')

blank_plot<-ggplot(data=as.data.frame(0)) + geom_blank() + theme_classic() + labs(x='', y='') + theme(plot.margin=unit(c(6,0,0,6), 'pt'))

p2_main <- ggplot(raw_mu_chart, aes(x=opp_char, y=use_char)) + 
    geom_tile(aes(fill=win_rate), colour='grey50') + 
    scale_fill_gradientn(name = 'P1 win rate', colours=c('blue4', 'blue3', 'blue2', 'blue1', 'grey90', 'red1', 'red2', 'red3', 'red4')) +
    scale_x_discrete(position='top') + 
    theme(legend.position='right') +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    geom_abline(slope=-1, intercept=length(y_chars)+1) 

p2_legend_g <- ggplotGrob(p2_main)$grobs
p2_legend <- p2_legend_g[[which(sapply(p2_legend_g, function(x) x$name) == "guide-box")]]

p2_all <- arrangeGrob(blank_plot, x_strip + ggtitle('Raw matchup win matrix'), blank_plot, y_strip, p2_main + theme(legend.position='none') + theme(plot.margin=unit(c(0, 6, 6, 0), 'pt')), p2_legend, nrow=2, heights=c(1.5, 12), widths=c(1, 12, 1.5))

ggsave(file='figures/02_raw_mu_matrix.png', plot=p2_all, dpi=100, width=12, height=10)

p2_alt <- arrangeGrob(blank_plot, x_strip + ggtitle('Raw matchup win matrix (game count listed in box)'), blank_plot, y_strip, p2_main + theme(legend.position='none') + theme(plot.margin=unit(c(0, 6, 6, 0), 'pt')) + geom_text(aes(label=game_count)), p2_legend, nrow=2, heights=c(1.5, 12), widths=c(1, 12, 1.5))
ggsave(file='figures/02_raw_mu_matrix_alt.png', plot=p2_alt, dpi=100, width=12, height=10)

##########
# mu chart corrected by stage frequency
##########

sw_trim <- filter(stage_win_dat, game_count >= 15) %>% mutate(win_rate = win_count/game_count)

char_mu_baselines <- sw_trim %>% group_by(use_char, opp_char) %>% summarize(mu_baseline = mean(win_rate), games_played = sum(game_count))

plot4_df_L <- expand.grid(use_char = choose_list, opp_char = choose_list) %>% as_data_frame
plot4_df_R <- inner_join(char_mu_baselines, raw_mu_chart, by=c('use_char', 'opp_char')) %>% filter(game_count >= min_matches) %>% select(use_char, opp_char, mu_baseline, games_played)
plot4_df <- left_join(plot4_df_L, plot4_df_R, by=c('use_char', 'opp_char')) %>% mutate(use_char = factor(use_char, levels=rev(sort(choose_list))), opp_char = factor(opp_char, levels=sort(choose_list)))

p4_main <- ggplot(plot4_df, aes(x=opp_char, y=use_char)) +
    geom_tile(aes(fill=mu_baseline), colour='grey50') +
    scale_fill_gradientn(name = 'P1 win rate', colours=c('blue4', 'blue3', 'blue2', 'blue1', 'grey90', 'red1', 'red2', 'red3', 'red4')) +
    scale_x_discrete(position='top') + 
    theme(legend.position='right') +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    geom_abline(slope=-1, intercept=length(y_chars)+1) 

p4_legend_g <- ggplotGrob(p4_main)$grobs
p4_legend <- p4_legend_g[[which(sapply(p4_legend_g, function(x) x$name) == "guide-box")]]

p4_all <- arrangeGrob(blank_plot, x_strip + ggtitle('Stage frequency adjusted matchup win matrix'), blank_plot, y_strip, p4_main + theme(legend.position='none') + theme(plot.margin=unit(c(0, 6, 6, 0), 'pt')), p4_legend, nrow=2, heights=c(1.5, 12), widths=c(1, 12, 1.5))

ggsave(file='figures/04_stage_adj_mu_matrix.png', plot=p4_all, dpi=100, width=12, height=10)

p4_alt <- arrangeGrob(blank_plot, x_strip + ggtitle('Stage freq adjusted MU win matrix (game counts listed -- char x char x stage combinations with <15 matches dropped)'), blank_plot, y_strip, p4_main + theme(legend.position='none') + theme(plot.margin=unit(c(0, 6, 6, 0), 'pt')) + geom_text(aes(label=games_played)), p4_legend, nrow=2, heights=c(1.5, 12), widths=c(1, 12, 1.5))

ggsave(file='figures/04_stage_adj_mu_matrix_alt.png', plot=p4_alt, dpi=100, width=12, height=10)

#########
# one plot per char: opposing char x stages full breakdown
#########

plot5_df_L <- expand.grid(use_char = choose_list, opp_char = choose_list, stage = stage_list) %>% as_data_frame
plot5_df_R <- inner_join(sw_trim, char_mu_baselines, by=c('use_char', 'opp_char')) %>% mutate(stage_diff = win_rate - mu_baseline) %>% select(-games_played)
plot5_df <- left_join(plot5_df_L, plot5_df_R, by=c('use_char', 'opp_char', 'stage')) %>% mutate(opp_char = factor(opp_char, levels=rev(sort(choose_list))), stage = factor(stage, levels=stage_list))

p5_y_strip <- y_strip + ylab('Opponent character') + coord_cartesian(ylim=c(1, 18.9))

for (i in choose_list) {
    sub_df <- filter(plot5_df, use_char==i)
    max_lim <- sub_df$stage_diff %>% max(abs(.), na.rm=TRUE)

    p5_main <- ggplot(sub_df, aes(x=stage, y=opp_char)) + 
    geom_tile(aes(fill=stage_diff), colour='grey50') +
    scale_fill_gradientn(name = paste(i, '\nstage win\nenrichment\nper MU', sep=''), colours=c('blue4', 'blue3', 'blue2', 'blue1', 'grey90', 'red1', 'red2', 'red3', 'red4'), limits=c(-max_lim, max_lim)) +
    scale_x_discrete(position='top') +
    theme(legend.position='right') +
    theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank()) 

    p5_legend_g <- ggplotGrob(p5_main)$grobs
    p5_legend <- p5_legend_g[[which(sapply(p5_legend_g, function(x) x$name) == "guide-box")]]

    p5_all <- arrangeGrob(p5_y_strip, p5_main + theme(legend.position='none') + ggtitle(paste(i, ' stage advantage matrix', sep='')), p5_legend, nrow=1, widths=c(1, 12, 1.8))
    ggsave(file=paste('figures/05_', i, '_stage_adv.png', sep=''), plot=p5_all, dpi=100, width=9, height=10)

    p5_alt <- arrangeGrob(p5_y_strip, p5_main + theme(legend.position='none') + ggtitle(paste(i, ' stage advantage matrix (with game counts)', sep='')) + geom_text(aes(label=game_count)), p5_legend, nrow=1, widths=c(1, 12, 1.8))
    ggsave(file=paste('figures/05_', i, '_stage_adv_alt.png', sep=''), plot=p5_alt, dpi=100, width=9, height=10)
}

##########
# overall character advantage
##########

plot6_df <- plot5_df %>% group_by(use_char, stage) %>% summarize(stage_adv = mean(stage_diff, na.rm=TRUE), games_played = sum(game_count, na.rm=TRUE)) %>% group_by(use_char) %>% mutate(stage_adv = stage_adv - mean(stage_adv, na.rm=TRUE)) %>% ungroup() %>% mutate(use_char = factor(use_char, levels=rev(sort(choose_list))), stage = factor(stage, levels=stage_list))

p6_lim <- max(abs(plot6_df$stage_adv), na.rm=TRUE)
p6_y_strip <- y_strip + theme(axis.title.y=element_blank()) + coord_cartesian(ylim=c(1, 18.9))

p6_main <- ggplot(plot6_df, aes(x=stage, y=use_char)) +
geom_tile(aes(fill=stage_adv), colour='grey50') +
scale_fill_gradientn(name = 'Stage win\nenrichment', colours=c('blue4', 'blue3', 'blue2', 'blue1', 'grey90', 'red1', 'red2', 'red3', 'red4'), limits=c(-p6_lim, p6_lim)) +
scale_x_discrete(position='top') +
theme(legend.position='right') +
theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank())

p6_all <- arrangeGrob(p6_y_strip, p6_main + ggtitle('Aggregate stage advantages'), nrow=1, widths=c(1,15)) 
ggsave(file='figures/06_aggregate_stage_adv.png', plot=p6_all, dpi=100, width=9, height=10)

p6_alt <- arrangeGrob(p6_y_strip, p6_main + ggtitle('Aggregate stage advantages (with game counts)') + geom_text(aes(label=games_played)), nrow=1, widths=c(1,15)) 
ggsave(file='figures/06_aggregate_stage_adv_alt.png', plot=p6_alt, dpi=100, width=9, height=10)

