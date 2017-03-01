library(ggplot2)
library(scales)

# http://stackoverflow.com/questions/29869862/ggplot2-how-to-add-percentage-or-count-labels-above-percentage-bar-plot

# select player_id, rating, rank from player_ladder_stats where season_id = 58 and games_played >= 8;

tim <- read.table('profane_melee58_ratings.csv', header=T, sep=',')
qplot(tim[,2], geom='histogram', binwidth=50, xlab='Rating', col=I('grey20'), fill=I('grey40'), main='Melee 2016 Q4 (season 58)') + scale_x_continuous(breaks=pretty_breaks(10))

ggplot(data=tim, aes(rating)) + geom_histogram(binwidth=50, col='grey20', fill='grey40') + labs(title='Melee 2016 Q4 (season 58)') + scale_x_continuous(breaks=pretty_breaks(10))

profane_breaks <- c(800, 950, 1100, 1245, 1277, 1314, 1355, 1388, 1421, 1455, 1505, 1555, 1610, 1665, 1715, 1770)
profane_colors <- c()
ggplot(data=tim, aes(rating)) + geom_histogram(breaks = profane_breaks, col='grey20', fill='grey40') + labs(title='Melee 2016 Q4 (season 58)') + scale_x_continuous(breaks=pretty_breaks(10)) + stat_bin(breaks=profane_breaks, geom='text', aes(label=..count..), vjust=-1, size=3) + ylim(c(0, 1000))

