# char_use_counts_viz.r

library(ggplot2)
library(tidyverse)
library(scales)

# raw counts
# current data source uses all matches - consider stratifying by ranked/unranked
# currently shows number of games character participated in. perhaps consider having a more user-centric metric (ie number of players using X as main during this week) - this can help control for low tier variance if the cause is a given player having a week with many matches
counts <- read.table('char_use_counts.csv', header=T, sep=',')
counts[,1]<-as.Date(counts[,1])

# fractional usage
total_week_counts<-rowSums(counts[,-1])
use_shares<-cbind(Dates=counts[,1], counts[,-1]/total_week_counts)

# fractional usage shifted to represent over/under usage relative to uniform distribution
# (frac_use - exp_use)/exp_use unbounded on top but min is -1
# kinda want more like fold off: if exp usage is 10%, then 20% is +1, 5% is -1
# if neg, multiply by reciprocal?
# not complete - need to account for number of characters available
# see http://www.ssbwiki.com/Downloadable_content#Characters for character introduction dates
chars_avail<-rep(NA, length.out=nrow(use_shares))

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



### testing area
tim<-counts[1:5,1:6]
total_week_counts<-rowSums(tim[,-1])

use_shares<-cbind()

tim <- tim %>% select(-Dates) %>% mutate(total_use = rowSums(.))
tim<-tim/tim$total_use


ggplot(use_shares, aes(x=Dates, y=Bayonetta)) + geom_line() + geom_point() + geom_smooth() + scale_x_date(breaks = pretty_breaks(10))