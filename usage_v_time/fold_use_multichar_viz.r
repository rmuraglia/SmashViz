# fold_use_multichar_viz.r

library(ggplot2)
library(tidyverse)
library(scales)
library(gplots)

prune_zeros<-function(X) {
    first_nonzero<-which(X!=0)[1]
    X[1:(first_nonzero-1)]<-NA
    return(X)
}

xfo_to_frac_use<-function(X) {
    # turn into fractional usage (all character contributions sum to 1)
    week_totals <- rowSums(X[,-1], na.rm=TRUE)
    X_xfo <- data.frame(Dates = X[,1], X[,-1]/week_totals)
    return(X_xfo)
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

dat <- read.table('/Users/rmuraglia/GitHub/SmashViz/usage_v_time/parsed_char_counts/unique_user_counts_wiiu_ranked.csv', sep=',', header=T, stringsAsFactors=F)
dat[,1]<-as.Date(dat[,1])

trim_dat <- dat %>% select(-Random) %>% filter(rowSums(.[,-1])!=0)

trim_dat <- trim_dat %>% mutate(Bayonetta = prune_zeros(Bayonetta), 
        Cloud = prune_zeros(Cloud), Corrin = prune_zeros(Corrin), Lucas = prune_zeros(Lucas),
        Mewtwo = prune_zeros(Mewtwo), Roy = prune_zeros(Roy), Ryu = prune_zeros(Ryu))

xfo_dat <- xfo_to_fold_use(trim_dat)

plot_dat <- gather(xfo_dat, key=Character, value=Fold_Usage, -Dates)

SA_chars <- c('Diddy.Kong', 'Cloud', 'Sheik', 'Rosalina.And.Luma', 'Mario', 'Sonic', 'Fox', 'Zero.Suit.Samus', 'Ryu', 'Mewtwo', 'Bayonetta')
SA_dat <- filter(plot_dat, Character %in% SA_chars) # following line is probably faster, but this is more readable
# SA_dat <- xfo_dat %>% select(one_of(SA_chars)) %>% bind_cols(select(xfo_dat, Dates)) %>% gather(key=Character, value=Fold_Usage, -Dates)

all_cols <- rich.colors(length(unique(plot_dat[,2])))
SA_cols <- rich.colors(length(SA_chars))

SA_plot <- ggplot(SA_dat, aes(x=Dates, y=Fold_Usage, colour=Character)) + geom_line() + scale_colour_manual(values=SA_cols) + scale_x_date(breaks=pretty_breaks(10))
ggsave(file='/Users/rmuraglia/GitHub/SmashViz/usage_v_time/figures/wiiu_ranked/multichar-fold_use/SA_chars.png', width=10, height=5, dpi=150, plot=SA_plot)

all_plot <- ggplot(plot_dat, aes(x=Dates, y=Fold_Usage, colour=Character)) + geom_line() + scale_colour_manual(values=all_cols) + scale_x_date(breaks=pretty_breaks(10)) + coord_cartesian(ylim=c(-10, 6))
ggsave(file='/Users/rmuraglia/GitHub/SmashViz/usage_v_time/figures/wiiu_ranked/multichar-fold_use/all_chars.png', width=10, height=5, dpi=150, plot=all_plot)


SAB_chars <- c(SA_chars, c('Pikachu', 'Metaknight', 'Villager', 'Megaman', 'Ness'))
SAB_cols <- rich.colors(length(SAB_chars))
SAB_dat <- filter(plot_dat, Character %in% SAB_chars)
SAB_plot <- ggplot(SAB_dat, aes(x=Dates, y=Fold_Usage, colour=Character)) + geom_line() + scale_colour_manual(values=SAB_cols) + scale_x_date(breaks=pretty_breaks(10))
ggsave(file='/Users/rmuraglia/GitHub/SmashViz/usage_v_time/figures/wiiu_ranked/multichar-fold_use/SAB_chars.png', width=10, height=5, dpi=150, plot=SAB_plot)