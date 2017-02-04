# pgr_tier.r

library(ggplot2)
library(tidyverse)

pg_dat <- read.table('~/Downloads/PGRv2 Survey - Tier Lists Results - Data.tsv', header=TRUE, sep='\t') %>% select(-Your.Tag)

char_scores <- colMeans(pg_dat, na.rm=TRUE)
char_order <- names(sort(char_scores))
pg_plot <- pg_dat %>% gather(character, rating) %>% mutate(character=factor(character, levels=char_order))

outplot <- ggplot(data=pg_plot, aes(x=character, y=rating)) + scale_y_continuous(breaks=seq(0,10), sec.axis=sec_axis(name='', trans=~., breaks=seq(0,10))) + coord_flip() +
    # geom_violin(scale='count', adjust=0.25)
    geom_boxplot()
# + geom_point(position=position_jitter(w=0.1, h=0.1))

ggsave('~/Downloads/PGR.png', width=4, height=10, plot=outplot)


# note: the sec.axis portion allows for creating a seconary axis. to just make a duplicate, use dup_axis()