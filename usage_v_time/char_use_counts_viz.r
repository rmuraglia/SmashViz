# char_use_counts_viz.r

library(ggplot2)

dat <- read.table('char_use_counts.csv', header=T, sep=',', row.names=1)

csums <- colSums(dat)

dat_shares<-t(t(dat)/colSums(dat)) # have to transpose because otherwise divides in column major order

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
