# char_use_counts_viz.r

# visualize character usage count data
# choose between different data sources (raw counts, or unique user counts)
# choose between different data transformations (fractional usage, fold usage relative to uniform)

library(ggplot2)
library(tidyverse)
library(scales)

viz_char_counts <- function(ladder_type, rank_type, count_type, plot_type) {

    # usage:
    # ladder_type = { 'all', 'wiiu', '3ds' }
    # rank_type = { 'all', 'ranked', 'unranked' }
    # count_type = { 'char_use', 'unique_user', 'signif_user' }
    # plot_type = { 'raw', 'frac_use', 'fold_use' }

    # ensure that output directory exists
    ifelse(!dir.exists('figures'), dir.create('figures'), FALSE)
    level1_dir <- paste('figures/', ladder_type, '_', rank_type, sep='')
    ifelse(!dir.exists(level1_dir), dir.create(level1_dir), FALSE)
    level2_dir <- paste(level1_dir, '/', count_type, '-', plot_type, sep='')
    ifelse(!dir.exists(level2_dir), dir.create(level2_dir), FALSE)

    # load data
    dat_file <- paste('parsed_char_counts/', count_type, '_counts_', ladder_type, '_', rank_type, '.csv', sep='')
    if (file.exists(dat_file)) {
        raw_dat <- read.table(dat_file, header=T, sep=',', stringsAsFactors=F)
        raw_dat[,1] <- as.Date(raw_dat[,1])
        print('Parsed data read in successfully')
    } else {
        print('The parsed data file does not appear to exist. Double check your values for ladder_type, rank_type and count-type, and possibly run the appropriate python script for game records parsing.')
        quit('no')
    }

    # drop 'random' character stats and weeks with no matches played
    trim_dat <- raw_dat %>% select(-Random) %>% filter(rowSums(.[,-1])!=0)

    # prune out zero values preceding character release
    trim_dat <- trim_dat %>% mutate(Bayonetta = prune_zeros(Bayonetta), 
        Cloud = prune_zeros(Cloud), Corrin = prune_zeros(Corrin), Lucas = prune_zeros(Lucas),
        Mewtwo = prune_zeros(Mewtwo), Roy = prune_zeros(Roy), Ryu = prune_zeros(Ryu))
    
    # use appropriate data transformation depending on plot choice then generate and save plots
    if (plot_type == 'raw') {
        xfo_dat <- trim_dat
        plot_raw(xfo_dat, level2_dir)
    } else if (plot_type == 'frac_use') {
        xfo_dat <- xfo_to_frac_use(trim_dat)
        plot_frac_use(xfo_dat, level2_dir)
    } else if (plot_type == 'fold_use') {
        xfo_dat <- xfo_to_fold_use(trim_dat)
        plot_fold_use(xfo_dat, level2_dir, 5, -10)
    }
}

prune_zeros<-function(X) {
    first_nonzero<-which(X!=0)[1]
    X[1:(first_nonzero-1)]<-NA
    return(X)
}

plot_raw<-function(X, fig_dir) {
    for (i in colnames(X)[-1]) {
        outplot <- ggplot(X, aes_string(x='Dates', y=i)) +
            geom_line() + geom_point() + geom_smooth(span=0.5) +
            scale_x_date(breaks=pretty_breaks(10)) +
            labs(y='Raw use counts (per week)', title=i)
        ggsave(file=paste(fig_dir, '/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=outplot)
    }
}

xfo_to_frac_use<-function(X) {
    # turn into fractional usage (all character contributions sum to 1)
    week_totals <- rowSums(X[,-1], na.rm=TRUE)
    X_xfo <- data.frame(Dates = X[,1], X[,-1]/week_totals)
    return(X_xfo)
}

plot_frac_use<-function(X, fig_dir) {
    for (i in colnames(X)[-1]) {
        outplot <- ggplot(X, aes_string(x='Dates', y=i)) +
            geom_line() + geom_point() + geom_smooth(span=0.5) +
            scale_x_date(breaks=pretty_breaks(10)) +
            labs(y='Fractional usage rate (per week)', title=i)
        ggsave(file=paste(fig_dir, '/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=outplot)
    }
}

xfo_to_fold_use<-function(X) {
    X_frac <- xfo_to_frac_use(X)
    num_avail_chars <- apply(X_frac[,-1], 1, function(x) sum(!is.na(x)))
    expected_usage <- 1/num_avail_chars
    X_xfo <- X_frac %>% select(-Dates) %>% data.matrix(.) %>% mapply(calc_fold, ., expected_usage) %>%
        matrix(., nrow=nrow(X_frac), dimnames=dimnames(select(X_frac, -Dates))) %>%
        data.frame(Dates=X_frac[,1], .)
    return(X_xfo)
}

calc_fold<-function(x, expected) {
    # for an observed usage rate (x), you can turn it into a fold change from an expected baseline
    # ex: if obs = exp -> 0, obs = 2*exp -> 1, obs=exp/2 -> -1, obs = 3*exp -> 2
    # for positive fold changes: obs = exp * (fold + 1)
    # for negative fold changes: obs = exp * (1/(-fold + 1))
    if (is.na(x)) {
        fold <- NA
    } else if (x < expected) { 
        fold <- 1 - expected/x
    } else { 
        fold <- x/expected - 1
    }
    return(fold)
}

plot_fold_use<-function(X, fig_dir, cbar_lim, y_floor) {
    # cbar_lim gives the limits for the colorbar
    # y_floor gives the lowest y value to plot
    # for char_use, 5 and -10 are resonably values
    # for unique_user, STILL NEED TO DETERMINE VALS
    # for signif_user, STILL NEED TO DETERMINE VALS
    
    # if want to see range to inform param guesses, use:
    # X %>% select(-Dates) %>% summarise_each(funs(min(., na.rm=T), max(., na.rm=T))) %>% sort(.)

    # make colorbar for majority of values
    cbar_lims <- c(-cbar_lim, cbar_lim)
    cbar_cols <- c('blue1', 'blue2', 'blue3', 'blue4', 'grey30', 'red4', 'red3', 'red2', 'red1')
    cbar <- scale_fill_gradientn(name='fold usage', colours=cbar_cols, limits=cbar_lims)

    # set extreme colors
    low_col <- 'blue'
    high_col <- 'red'

    for (i in colnames(X)[-1]) {

        # subset to character of interest and create base plot
        plot_df <- select(X, Dates, one_of(i))
        outplot <- ggplot(plot_df, aes_string(x='Dates', y=i)) + geom_bar(aes_string(fill=i), stat='identity') +
            scale_x_date(breaks=pretty_breaks(10)) + coord_cartesian(xlim=range(plot_df[,1])) +
            labs(y='Fold usage relative to uniform (per week)', title=i) + cbar

        # make adjustments for extremes if necessary
        fold_range<-range(plot_df[,2], na.rm=TRUE)
        adj_bool<-c(fold_range[1]<(-cbar_lim), fold_range[2]>cbar_lim)
        if (any(adj_bool)) {
            if (adj_bool[1]) { # need to fix low extremes
                if (fold_range[1]<y_floor) { # need to fix plot limits
                    outplot <- outplot + coord_cartesian(ylim=c(y_floor, max(c(0, plot_df[,2]))))
                }
                low_dat <- plot_df
                low_dat[which(!(low_dat[,2]<(-cbar_lim))), 2] <- 0
                outplot <- outplot + geom_bar(data=low_dat, fill=low_col, stat='identity')
            }
            if (adj_bool[2]) { # need to fix high extremes
                high_dat<-plot_df
                high_dat[which(!(high_dat[,2]>cbar_lim)), 2] <- 0
                outplot <- outplot + geom_bar(data=high_dat, fill=high_col, stat='identity')
            }
        } # close adjustments
        ggsave(file=paste(fig_dir, '/', i, '.png', sep=''), width=10, height=5, dpi=150, plot=outplot)
    } # cloose loop over characters
}

viz_char_counts('all', 'ranked', 'char_use', 'raw')
viz_char_counts('all', 'ranked', 'char_use', 'frac_use')
viz_char_counts('all', 'ranked', 'char_use', 'fold_use')
viz_char_counts('wiiu', 'ranked', 'char_use', 'raw')
viz_char_counts('wiiu', 'ranked', 'char_use', 'frac_use')
viz_char_counts('wiiu', 'ranked', 'char_use', 'fold_use')
viz_char_counts('all', 'ranked', 'unique_user', 'raw')
viz_char_counts('all', 'ranked', 'unique_user', 'frac_use')
viz_char_counts('all', 'ranked', 'unique_user', 'fold_use')
viz_char_counts('wiiu', 'ranked', 'unique_user', 'raw')
viz_char_counts('wiiu', 'ranked', 'unique_user', 'frac_use')
viz_char_counts('wiiu', 'ranked', 'unique_user', 'fold_use')



## based on unique users count

ifelse(!dir.exists('all_ranked/frac_users/'), dir.create('all_ranked/frac_users/'), FALSE)

## based on number of users with significant usage (greater than 20% of plays)

ifelse(!dir.exists('all_ranked/signif_users/'), dir.create('all_ranked/signif_users/'), FALSE)


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

