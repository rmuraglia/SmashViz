# ladder_contribs.r

library(ggplot2)
library(scales)
library(tidyverse)

dat_all_all <- read.table('char_use_counts_all_all.csv', header=T, sep=',')
dat_all_ranked <- read.table('char_use_counts_all_ranked.csv', header=T, sep=',')
dat_all_unranked <- read.table('char_use_counts_all_unranked.csv', header=T, sep=',')
dat_wiiu_all <- read.table('char_use_counts_wiiu_all.csv', header=T, sep=',')
dat_wiiu_ranked <- read.table('char_use_counts_wiiu_ranked.csv', header=T, sep=',')
dat_wiiu_unranked <- read.table('char_use_counts_wiiu_unranked.csv', header=T, sep=',')
dat_3ds_all <- read.table('char_use_counts_3ds_all.csv', header=T, sep=',')
dat_3ds_ranked <- read.table('char_use_counts_3ds_ranked.csv', header=T, sep=',')
dat_3ds_unranked <- read.table('char_use_counts_3ds_unranked.csv', header=T, sep=',')

all_all_counts <- rowSums(dat_all_all[,-1])/2

compare_counts <- data.frame(Dates=as.Date(dat_all_all[,1]),
    AA=rowSums(dat_all_all[,-1])/2,
    AR=rowSums(dat_all_ranked[,-1])/2,
    AU=rowSums(dat_all_unranked[,-1])/2,
    WA=rowSums(dat_wiiu_all[,-1])/2,
    WR=rowSums(dat_wiiu_ranked[,-1])/2,
    WU=rowSums(dat_wiiu_unranked[,-1])/2,
    DA=rowSums(dat_3ds_all[,-1])/2,
    DR=rowSums(dat_3ds_ranked[,-1])/2,
    DU=rowSums(dat_3ds_unranked[,-1])/2)

plot_df <- gather(compare_counts, key=Game_Type, value=Games_Played, -Dates)


ggplot(plot_df, aes(x=Dates, y=Games_Played, color=Game_Type)) + geom_line() + geom_point()