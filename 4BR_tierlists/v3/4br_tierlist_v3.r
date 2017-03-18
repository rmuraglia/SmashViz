# 4br_tierlist_v3.r

library(tidyverse)
library(rvest)
library(png)
library(grid)
library(ggplot2)
library(scales)

# fetch data
if (file.exists('4BR-v3.txt')) {
    D <- read_delim('4BR-v3.txt', delim='\t', col_names=TRUE)
} else {
    url<-'https://smashboards.com/threads/4br-smash-for-wii-u-tier-list-v3.445995/'
    html<-read_html(url)
    D <- html %>% html_nodes('table') %>% .[[1]] %>% html_table()
    D <- D[,-1] # drop row number column
    write.table(D, file='4BR-v3.txt', quote=FALSE, sep='\t', col.names=TRUE, row.names=FALSE)
}

D[30,1] <-  'Pit' # rename Pit/Dark Pit to just Pit

###
# load stock icons as rasters
###
imgpre<-'../../StockIcons/stock_90_'
imgpost<-'_01.png'

make_rasters<-function(alpha=1) {
    raster_list<-list()
    for (i in 1:nrow(D)) {
        char_string<-D[i,1]

        # go through exceptions for character naming
        if (char_string=='Captain Falcon') { char_string<-'captain' }
        if (char_string=='King Dedede') { char_string<-'dedede' }
        if (char_string=='Diddy Kong') { char_string<-'diddy' }
        if (char_string=='Donkey Kong') { char_string<-'donkey' }
        if (char_string=='Dr. Mario') { char_string<-'drmario' }
        if (char_string=='Duck Hunt') { char_string<-'duckhunt' }
        if (char_string=='Mr. Game & Watch') { char_string<-'gamewatch' }
        if (char_string=='Ganondorf') { char_string<-'ganon' }
        if (char_string=='Greninja') { char_string<-'gekkouga' }
        if (char_string=='Corrin') { char_string<-'kamui' }
        if (char_string=='Bowser') { char_string<-'koopa' }
        if (char_string=='Bowser Jr.') { char_string<-'koopajr' }
        if (char_string=='Little Mac') { char_string<-'littlemac' }
        if (char_string=='Charizard') { char_string<-'lizardon' }
        if (char_string=='Meta Knight') { char_string<-'metaknight' }
        if (char_string=='Mii Brawler') { char_string<-'miienemyf' }
        if (char_string=='Mii Gunner') { char_string<-'miigunner' }
        if (char_string=='Mii Swordsman') { char_string<-'miiswordsman' }
        if (char_string=='Villager') { char_string<-'murabito' }
        if (char_string=='Pac-Man') { char_string<-'pacman' }
        if (char_string=='Olimar') { char_string<-'pikmin' }
        if (char_string=='Dark Pit') { char_string<-'pitb' }
        if (char_string=='Jigglypuff') { char_string<-'purin' }
        if (char_string=='Robin') { char_string<-'reflet' }
        if (char_string=='R.O.B.') { char_string<-'robot' }
        if (char_string=='Mega Man') { char_string<-'rockman' }
        if (char_string=='Rosalina') { char_string<-'rosetta' }
        if (char_string=='Zero Suit Samus') { char_string<-'szerosuit' }
        if (char_string=='Toon Link') { char_string<-'toonlink' }
        if (char_string=='Wii Fit Trainer') { char_string<-'wiifit' }

        # go through exceptions for character icons
        if (char_string=='kamui' | char_string=='reflet') { img<-readPNG(paste(imgpre, char_string, '_02.png', sep='')) }
        else if (char_string=='Wario') { img<-readPNG(paste(imgpre, tolower(char_string), '_05.png', sep='')) }
        else { img<-readPNG(paste(imgpre, tolower(char_string), '_01.png', sep=''))}

        # alpha controls transparency (0=transparent, 1=opaque). default value is 1
        pre_raster<-matrix(rgb(img[,,1], img[,,2], img[,,3], img[,,4]*alpha), nrow=dim(img)[1])

        # turn into raster object
        raster_list[[i]]<-rasterGrob(pre_raster, interpolate=TRUE)
    }
    return(raster_list)
}

rasters<-make_rasters()

make_annotation<-function(raster, x, y) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

####
# rankings with standard dev
####

# make x range based on img_offset (image size)
img_offset<-2
xplot<-seq(from=img_offset, to=(img_offset*2*nrow(D)), by=img_offset*2)

# make plotting dataframe
df <- bind_cols(D, as.data.frame(xplot))
df <- df %>% mutate(rank3 = nrow(D) - `Ordered Score`) %>% rename(stddev = `Standard Deviation`) %>% select(Character, xplot, rank3, stddev)

# plotting call

plot1 <- ggplot(data=df, aes(x=xplot, y=rank3, ymin=rank3-stddev, ymax=rank3+stddev)) + geom_linerange(colour='black', size=0.5) + mapply(make_annotation, rasters, df$xplot, df$rank3) + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + labs(title='4BR tier list - v3', y='Score') + scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-v3-01.png', width=12, height=4.5, dpi=200, plot=plot1)

#######
# sorted stddev
#######

sd_order<-order(df$stddev)
sd_df <- mutate(df, Baseline=0) %>% arrange(stddev) %>% mutate(xplot = seq(from=img_offset, to=(img_offset*2*nrow(D)), by=img_offset*2))

plot2 <- ggplot(data=sd_df, aes(x=xplot, y=stddev)) +
    geom_segment(aes(x=xplot, xend=xplot, y=Baseline, yend=stddev)) +
    mapply(make_annotation, rasters[sd_order], sd_df$xplot, sd_df$stddev) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') +
    labs(title='4BR tier list v3 - StdDevs') +
    scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-v3-sorted-stddev.png', width=12, height=4.5, dpi=200, plot=plot2)


###
# character movement from v2 to v3
###

D_2 <- read_delim('../v2/4BR-1.1.6.txt', delim='\t', col_names=TRUE)
D_2 <- filter(D_2, Character != 'Dark Pit') 
D_2 <- D_2 %>% mutate(rank2 = nrow(D_2) - `Ordered Average`) %>% select(Character, rank2, stddev = `Standard Deviation (Order)`)

# make function to rescale scores
rescale_rank <- function(rank_prev, rank_new) {
    maxmin_prev <- range(rank_prev)
    range_prev <- maxmin_prev[2] - maxmin_prev[1]
    maxmin_new <- range(rank_new)
    range_new <- maxmin_new[2] - maxmin_new[1]
    rescaled <- (rank_prev - maxmin_prev[1]) * range_new/range_prev + maxmin_new[1]
    return(rescaled)
}

rank2_rescaled <- rescale_rank(D_2$rank2, df$rank3)

# put rescaled scores in current tier list order
rank2_final <- rep(NA, length.out=nrow(D))
char_vec_v3 <- D$Character
char_vec_v2 <- D_2$Character

for (i in 1:nrow(D_2)) {
    if (char_vec_v2[i]=='Mii Swordfighter') {
        ind <- which(char_vec_v3=='Mii Swordsman')
    } else {
        ind <- which(char_vec_v3==char_vec_v2[i])
    }
    rank2_final[ind] <- rank2_rescaled[i]
}

# combine to plotting df
df2 <- bind_cols(df, as.data.frame(rank2_final))
df2 <- mutate(df2, delta_2_3 = rank3 - rank2_final)

# plot with flat arrows
ghosts2 <- make_rasters(0.5)

plot3 <- ggplot(data=df2, aes(x=xplot, y=rank3)) + geom_segment(aes(x=xplot, xend=xplot, y=rank2_final, yend=rank3, colour=delta_2_3), arrow=arrow(length=unit(0.3, 'cm'))) + mapply(make_annotation, rasters, df2$xplot, df2$rank3) + mapply(make_annotation, ghosts2, df2$xplot, df2$rank2_final) + scale_colour_gradient2(low='blue', high='red') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') + labs(title='4BR tier list v3 - diff from v2', y='Score')  + scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-v3-delta-from-v2.png', width=12, height=4.5, dpi=200, plot=plot3)

#####
# v2 to v3 sorted by diff
#####

df3 <- arrange(df2, delta_2_3) %>% mutate(baseline = 0) %>% mutate(xplot = seq(from=img_offset, to=(img_offset*2*nrow(D)), by=img_offset*2))
d23_order <- order(df2$delta_2_3)

plot4 <- ggplot(data=df3, aes(x=xplot, y=delta_2_3)) + geom_segment(aes(x=xplot, xend=xplot, y=baseline, yend=delta_2_3, colour=delta_2_3), arrow=arrow(length=unit(0.3, 'cm'))) + mapply(make_annotation, rasters[d23_order], df3$xplot, df3$delta_2_3) + mapply(make_annotation, ghosts2[d23_order], df3$xplot, df3$baseline) + scale_colour_gradient2(low='blue', high='red') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') + labs(title='4BR tier list v3 - diff from v2 sorted', y='Score')  + scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-v3-delta-from-v2-sorted.png', width=12, height=4.5, dpi=200, plot=plot4)


####
# show both deltas (v1 to v2) and (v2 to v3)
####

D_1 <- read_delim('../v1/4BR-1.1.3.txt', delim='\t', col_names=TRUE)
D_1 <- filter(D_1, Character != 'Dark Pit')

rank1_rescaled <- rescale_rank(D_1$`Average Placement`, df$rank3)
rank1_final <- rep(NA, length.out=nrow(D))
char_vec_v1 <- D_1$Character

for (i in 1:nrow(D_1)) {
    if (char_vec_v1[i]=='Duck Hunt Dog') {
        ind <- which(char_vec_v3=='Duck Hunt')
    } else {
        ind <- which(char_vec_v3==char_vec_v1[i])
    }
    rank1_final[ind] <- rank1_rescaled[i]
}

df4 <- bind_cols(df2, as.data.frame(rank1_final))
df4 <- mutate(df4, rank1_final = ifelse(is.na(rank1_final), rank2_final, rank1_final), delta_1_2 = rank2_final - rank1_final)

ghosts1<-make_rasters(0.25)

# only plot lines that are greater than the minimum length
# keep lines straight if they go in the same direction
# curve arrows in opposite directions if necessary to avoid overlap
diff_min <- 2.5

df5 <- bind_rows(select(df4, xplot, rank = rank1_final, delta = delta_1_2), select(df4, xplot, rank = rank2_final, delta = delta_2_3))
df5_samedir <- df5 %>% group_by(xplot) %>% summarize(prod = prod(delta)) %>% mutate(straight = prod>=0)
df5 <- inner_join(df5, df5_samedir, by='xplot')
df5_curve <- filter(df5, !straight & abs(delta) > diff_min)
df5_straight <- filter(df5, straight & abs(delta) > diff_min)


plot5 <- ggplot(df5, aes(x=xplot, y=rank)) + geom_segment(data=df5_straight, aes(x=xplot, xend=xplot, y=rank, yend=(rank+delta), colour=delta), arrow=arrow(length=unit(0.3, 'cm'))) + geom_curve(data=df5_curve, aes(x=xplot, xend=xplot, y=rank, yend=(rank+delta), colour=delta), arrow=arrow(length=unit(0.3, 'cm')), curvature=0.1) + mapply(make_annotation, ghosts1, df4$xplot, df4$rank1_final) + mapply(make_annotation, ghosts2, df4$xplot, df4$rank2_final) + mapply(make_annotation, rasters, df4$xplot, df4$rank3) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') + labs(title='4BR tier list v3 - diff from v2 and v1', y='Score') + scale_x_continuous(expand=c(0.03, 0)) + scale_colour_gradientn(colors=c('blue', 'royalblue', 'grey90', 'tomato', 'red'), limits=c(-max(abs(df5$delta)), max(abs(df5$delta))))

ggsave(file='4br-v3-delta-from-v2-v1.png', width=12, height=4.5, dpi=200, plot=plot5)

####
# sort by total delta
####

df6 <- mutate(df4, total_delta = delta_1_2 + delta_2_3) %>% arrange(total_delta) %>% mutate(baseline=0) %>% mutate(xplot = seq(from=img_offset, to=(img_offset*2*nrow(D)), by=img_offset*2))
df6_order <- order(df4$delta_1_2 + df4$delta_2_3)

df7_1 <- select(df6, xplot, rank = rank1_final, delta = delta_1_2) %>% mutate(rank = 0)
df7_2 <- select(df6, xplot, rank = delta_1_2, delta = delta_2_3)

df7 <- bind_rows(df7_1, df7_2)
df7_samedir <- df7 %>% group_by(xplot) %>% summarize(prod = prod(delta)) %>% mutate(straight = prod>=0)
df7 <- inner_join(df7, df7_samedir, by='xplot')
df7_curve <- filter(df7, !straight & abs(delta) > diff_min)
df7_straight <- filter(df7, straight & abs(delta) > diff_min)

plot6 <- ggplot(df6, aes(x=xplot, y=total_delta)) + geom_segment(data=df7_straight, aes(x=xplot, xend=xplot, y=rank, yend=(rank+delta), colour=delta), arrow=arrow(length=unit(0.3, 'cm'))) + geom_curve(data=df7_curve, aes(x=xplot, xend=xplot, y=rank, yend=(rank+delta), colour=delta), arrow=arrow(length=unit(0.3, 'cm')), curvature=0.1) + mapply(make_annotation, ghosts1[df6_order], df6$xplot, df6$baseline) + mapply(make_annotation, ghosts2[df6_order], df6$xplot, df6$delta_1_2) + mapply(make_annotation, rasters[df6_order], df6$xplot, df6$total_delta) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') + labs(title='4BR tier list v3 - diff from v2 and v1 sorted', y='Score') + scale_x_continuous(expand=c(0.015, 0)) + scale_colour_gradientn(colors=c('blue', 'royalblue', 'grey90', 'tomato', 'red'), limits=c(-max(abs(df5$delta)), max(abs(df5$delta))))

ggsave(file='4br-v3-delta-from-v2-v1-sorted.png', width=12, height=4.5, dpi=200, plot=plot6)