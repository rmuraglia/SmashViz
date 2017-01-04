# char_use_counts_viz.r

library(ggplot2)
library(tidyverse)
library(scales)

####
# PART 1
# analysis on all ranked matches (3ds + wii u)
####

# create output directory
ifelse(!dir.exists('all_ranked'), dir.create('all_ranked'), FALSE)

## based on game participation count
all_counts <- read.table('char_use_counts_all_ranked.csv', header=T, sep=',')
all_counts[,1]<-as.Date(all_counts[,1])

# drop 'random' character stats
all_counts <- select(all_counts, -Random)

# raw fractional usage
all_total_counts<-rowSums(all_counts[,-1]) # total number of character picks for each week
all_frac_use<-data.frame(Dates=all_counts[,1], all_counts[,-1]/all_total_counts)

# prune out zero values preceding character release
prune_zeros<-function(X) {
    first_nonzero<-which(X!=0)[1]
    X[1:(first_nonzero-1)]<-NA
    return(X)
}

all_frac_use <- all_frac_use %>% 
    mutate(Bayonetta = prune_zeros(Bayonetta), Cloud = prune_zeros(Cloud), Corrin = prune_zeros(Corrin), Lucas = prune_zeros(Lucas),Mewtwo = prune_zeros(Mewtwo), Roy = prune_zeros(Roy), Ryu = prune_zeros(Ryu))

# individual character plot by name
# ggplot(all_frac_use, aes(x=Dates, y=Mario)) + geom_line() + geom_point() + geom_smooth(span=0.75) + scale_x_date(breaks=pretty_breaks(10))

# individual character plot by index
# ggplot(all_frac_use, aes_string(x='Dates', y=colnames(all_frac_use)[27])) + geom_line() + geom_point() + geom_smooth() + scale_x_date(breaks=pretty_breaks(10))

# initial look at each character with no annotations
ifelse(!dir.exists('all_ranked/frac_counts/'), dir.create('all_ranked/frac_counts/'), FALSE)

for (i in colnames(all_frac_use)[-1]) {
    all_frac_plot<-ggplot(all_frac_use, aes_string(x='Dates', y=i)) + geom_line() + geom_point() + geom_smooth(span=0.5) + scale_x_date(breaks=pretty_breaks(10)) + labs(y='Fractional usage rate (per week)', title=i) 
    ggsave(file=paste('all_ranked/frac_counts/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=all_frac_plot)
}

# fold over/under-representation, corrected by number of available characters

num_avail_chars<-apply(all_frac_use[,-1], 1, function(x) sum(!is.na(x)))
expected_usage<-1/num_avail_chars

# for each observed usage rate, you can turn it into a fold change as such:
# goal: if obs = exp -> 0, obs = 2*exp -> 1, obs = exp/2 -> -1, obs = 3*exp -> 2
# for positive fold changes: obs = exp * (fold + 1)
# for negative fold changes: obs = exp * (1/(-fold + 1))
calc_fold<-function(x, expected) {
    if (is.na(x)) {
        fold <- NA
    } else if (x < expected) { 
        fold <- 1 - expected/x
    } else { 
        fold <- x/expected - 1
    }
    return(fold)
}

# remember that the period is a placeholder for whatever you're piping in
all_fold_use <- all_frac_use %>% select(-Dates) %>% data.matrix(.) %>% 
    mapply(calc_fold, ., expected_usage) %>%
    matrix(., nrow=nrow(all_frac_use), dimnames=dimnames(select(all_frac_use, -Dates))) %>%
    data.frame(Dates=all_counts[,1], .)

ifelse(!dir.exists('all_ranked/fold_use/'), dir.create('all_ranked/fold_use'), FALSE)

# see range of min/max multipliers to determine reasonable colorbar
# all_fold_use %>% select(-Dates) %>% summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE))) %>% sort(.)

# example line plot style
# ggplot(all_fold_use, aes(x=Dates, y=Little.Mac)) + geom_point(aes(colour=Little.Mac)) + geom_line(aes(colour=Little.Mac)) + scale_colour_gradientn(colours=c('blue', 'blue1', 'blue2', 'blue3', 'grey30', 'red3', 'red2', 'red1', 'red'), limits=c(-5, 5), name='fold usage')

# example bar plot style (better color representation)
# ggplot(all_fold_use, aes(x=Dates, y=Little.Mac)) + geom_bar(aes(fill=Little.Mac), stat='identity') + scale_fill_gradientn(name='fold usage', colours=c('blue', 'blue1', 'blue2', 'blue3', 'grey30', 'red3', 'red2', 'red1', 'red'), limits=c(-5, 5))

# set params for limits of colorbar and plotting window
colorbar_lims<-c(-5, 5)
colorbar_cols<-c('blue1', 'blue2', 'blue3', 'blue4', 'grey30', 'red4', 'red3', 'red2', 'red1')
gradcols<-scale_fill_gradientn(name='fold usage', colours=colorbar_cols, limits=colorbar_lims)
ylim_floor <- -10
low_col<-'blue'
high_col<-'red'


for (i in colnames(all_fold_use)[-1]) {

    # subset to character of interest and create base plot
    plot_df<-select(all_fold_use, Dates, one_of(i))
    outplot<-ggplot(plot_df, aes_string(x='Dates', y=i)) + geom_bar(aes_string(fill=i), stat='identity') + scale_x_date(breaks=pretty_breaks(10)) + coord_cartesian(xlim=range(plot_df[,1])) + labs(y='Fold usage relative to uniform (per week)', title=i) + gradcols

    # make adjustments for extremes if necessary
    fold_range<-range(plot_df[,2], na.rm=TRUE)
    adj_bool<-c(fold_range[1]<colorbar_lims[1], fold_range[2]>colorbar_lims[2])
    if (any(adj_bool)) {
        if (adj_bool[1]) { # need to fix low extremes
            if (fold_range[1]<ylim_floor) { # need to fix plot limits
                outplot <- outplot + coord_cartesian(ylim=c(ylim_floor, max(c(0, plot_df[,2]))))
            }
            low_dat<-plot_df
            low_dat[which(!(low_dat[,2]<colorbar_lims[1])), 2] <- 0
            outplot <- outplot + geom_bar(data=low_dat, fill=low_col, stat='identity')
        }
        if (adj_bool[2]) { # need to fix high extremes
            high_dat<-plot_df
            high_dat[which(!(high_dat[,2]>colorbar_lims[2])), 2] <- 0
            outplot <- outplot + geom_bar(data=high_dat, fill=high_col, stat='identity')
        }
    }
    ggsave(file=paste('all_ranked/fold_use/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=outplot)
}


## based on unique users count

ifelse(!dir.exists('all_ranked/frac_users/'), dir.create('all_ranked/frac_users/'), FALSE)

## based on number of users with significant usage (greater than 20% of plays)

ifelse(!dir.exists('all_ranked/signif_users/'), dir.create('all_ranked/signif_users/'), FALSE)

####
# PART 2
# analysis on wii u ranked matches only
####

# create output directory
ifelse(!dir.exists('wiiu_ranked'), dir.create('wiiu_ranked'), FALSE)

## based on game participation count
wiiu_counts <- read.table('char_use_counts_wiiu_ranked.csv', header=T, sep=',')
wiiu_counts[,1]<-as.Date(wiiu_counts[,1])

# drop 'random' character stats
wiiu_counts <- select(wiiu_counts, -Random)

# drop weeks preceding wii u release
wiiu_counts <- wiiu_counts %>% filter(rowSums(.[,-1])!=0)

# raw fractional usage
wiiu_total_counts<-rowSums(wiiu_counts[,-1]) # total number of character picks for each week
wiiu_frac_use<-data.frame(Dates=wiiu_counts[,1], wiiu_counts[,-1]/wiiu_total_counts)


# prune out zero values preceding character release
wiiu_frac_use <- wiiu_frac_use %>% 
    mutate(Bayonetta = prune_zeros(Bayonetta), Cloud = prune_zeros(Cloud), Corrin = prune_zeros(Corrin), Lucas = prune_zeros(Lucas),Mewtwo = prune_zeros(Mewtwo), Roy = prune_zeros(Roy), Ryu = prune_zeros(Ryu))

# initial look at each character with no annotations
ifelse(!dir.exists('wiiu_ranked/frac_counts/'), dir.create('wiiu_ranked/frac_counts/'), FALSE)

for (i in colnames(wiiu_frac_use)[-1]) {
    wiiu_frac_plot<-ggplot(wiiu_frac_use, aes_string(x='Dates', y=i)) + geom_line() + geom_point() + geom_smooth(span=0.5) + scale_x_date(breaks=pretty_breaks(10)) + labs(y='Fractional usage rate (per week)', title=i) 
    ggsave(file=paste('wiiu_ranked/frac_counts/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=wiiu_frac_plot)
}

# fold over/under-representation, corrected by number of available characters

wiiu_num_avail_chars<-apply(wiiu_frac_use[,-1], 1, function(x) sum(!is.na(x)))
wiiu_expected_usage<-1/wiiu_num_avail_chars

wiiu_fold_use <- wiiu_frac_use %>% select(-Dates) %>% data.matrix(.) %>% 
    mapply(calc_fold, ., wiiu_expected_usage) %>%
    matrix(., nrow=nrow(wiiu_frac_use), dimnames=dimnames(select(wiiu_frac_use, -Dates))) %>%
    data.frame(Dates=wiiu_counts[,1], .)

ifelse(!dir.exists('wiiu_ranked/fold_use/'), dir.create('wiiu_ranked/fold_use'), FALSE)

for (i in colnames(wiiu_fold_use)[-1]) {

    # subset to character of interest and create base plot
    plot_df<-select(wiiu_fold_use, Dates, one_of(i))
    outplot<-ggplot(plot_df, aes_string(x='Dates', y=i)) + geom_bar(aes_string(fill=i), stat='identity') + scale_x_date(breaks=pretty_breaks(10)) + coord_cartesian(xlim=range(plot_df[,1])) + labs(y='Fold usage relative to uniform (per week)', title=i) + gradcols

    # make adjustments for extremes if necessary
    fold_range<-range(plot_df[,2], na.rm=TRUE)
    adj_bool<-c(fold_range[1]<colorbar_lims[1], fold_range[2]>colorbar_lims[2])
    if (any(adj_bool)) {
        if (adj_bool[1]) { # need to fix low extremes
            if (fold_range[1]<ylim_floor) { # need to fix plot limits
                outplot <- outplot + coord_cartesian(ylim=c(ylim_floor, max(c(0, plot_df[,2]))))
            }
            low_dat<-plot_df
            low_dat[which(!(low_dat[,2]<colorbar_lims[1])), 2] <- 0
            outplot <- outplot + geom_bar(data=low_dat, fill=low_col, stat='identity')
        }
        if (adj_bool[2]) { # need to fix high extremes
            high_dat<-plot_df
            high_dat[which(!(high_dat[,2]>colorbar_lims[2])), 2] <- 0
            outplot <- outplot + geom_bar(data=high_dat, fill=high_col, stat='identity')
        }
    }
    ggsave(file=paste('wiiu_ranked/fold_use/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=outplot)
}


### testing area




### old annotations

# luigi nerfs:
# 1.1.0 - July 30 2015 - fireball nerf - 42
# 1.1.1 - Sept 30 2015 - downthrow change - 51
qplot(1:114, dat_shares[26,], geom=c('point', 'line'), ylab='Luigi usage shares') + geom_vline(xintercept = 42, linetype = 'dashed', colour='red') + geom_vline(xintercept = 51, linetype = 'dashed', colour='red') + geom_smooth()

# diddy nerfs:
# 1.0.6 - Apr 15 2015 - 27
# 1.0.8 - June 14 2015 - 36

qplot(1:114, dat_shares[9,], geom=c('point', 'line'), ylab='Diddy usage shares') + geom_vline(xintercept = 27, linetype = 'dashed', colour='red') + geom_vline(xintercept = 36, linetype = 'dashed', colour='red') + geom_smooth()


for (i in 1:nrow(dat)) {
    p_out <- qplot(1:114, dat_shares[i,], geom=c('point', 'line'), ylab=rownames(dat)[i]) + geom_smooth()
    ggsave(p_out, file=paste('first_out/', rownames(dat)[i], '.png', sep=''))
}


# doctor mario:
# nairo uses doc vs esam at MLG worlds (10/19/2015) - 53
qplot(1:114, dat_shares[11,], geom=c('point', 'line'), ylab='Dr. Mario usage shares') + geom_vline(xintercept = 53, linetype = 'dashed', colour='red') + geom_smooth()

