# 4br_tierlist_v3.r

library(tidyverse)
library(rvest)
library(png)
library(grid)
library(ggplot2)
library(scales)

if (file.exists('4BR-v3.txt')) {
    D <- read.table('4BR-v3.txt', sep='\t', header=T, stringsAsFactors=FALSE)
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
# make plotting objects
####

# make x range based on img_offset (image size)
img_offset<-2
xplot<-seq(from=img_offset, to=(img_offset*2*nrow(D)), by=img_offset*2)

# make plotting dataframe
df<-cbind(xplot, D)
df<-as.data.frame(df)
df[,3]<-nrow(D)-df[,3] # invert score to place high rank at top
colnames(df)[c(3, 4)]<-c('AvgPlacement', 'StdDev') # make names more ggplot friendly

####
# plotting call
####

outplot<-ggplot(data=df, aes(x=xplot, y=AvgPlacement, ymin=AvgPlacement-StdDev, ymax=AvgPlacement+StdDev)) +
    geom_linerange(colour='black', size=0.5) + theme_grey() +
    mapply(make_annotation, rasters, df[,1], df[,3]) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
    labs(title='4BR tier list - v03') +
    # theme(axis.ticks=element_line(colour='grey50')) + # looks more like old ggplot default
    scale_x_continuous(expand=c(0.015, 0))
    # coord_cartesian(xlim=c(min(xplot)-img_offset, max(xplot)+img_offset)) # with default flag expand=T and new ggplot defaults this has too much white space

ggsave(file='4br-v3-01.png', width=12, height=4.5, dpi=200, plot=outplot)


# sort rank orders by stddev
sd_order<-order(df[,4])
sd_df<-cbind(Baseline=0, df[,4])
sd_df<-sd_df[sd_order, ]
sd_df<-as.data.frame(cbind(xplot, sd_df))
colnames(sd_df)[3]<-'StdDev'

plot4<-ggplot(data=sd_df, aes(x=xplot, y=StdDev)) +
    geom_segment(aes(x=xplot, xend=xplot, y=Baseline, yend=StdDev)) +
    mapply(make_annotation, rasters[sd_order], sd_df[,1], sd_df[,3]) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') +
    labs(title='4BR tier list v3 - StdDevs') +
    scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-v3-sorted-stddev.png', width=12, height=4.5, dpi=200, plot=plot4)


###
# note character movement
###

D_2 <- read.table('../v2/4BR-1.1.6.txt', sep='\t', header=T)
D_2 <- D_2[-27,] # remove dark pit
D_2[,2] <- nrow(D_2) - D_2[,2]

D_1 <- read.table('../v1/4BR-1.1.3.txt', sep='\t', header=T)
D_1 <- D_1[-17,] # remove dark pit

ghosts2<-make_rasters(0.7)
ghosts1<-make_rasters(0.4)

# rescale old scores to current score scale
maxmin3 <- range(df[,3])
range3 <- maxmin3[2] - maxmin3[1]
maxmin2 <- range(D_2[,2])
range2 <- maxmin2[2] - maxmin2[1]
maxmin1 <- range(D_1[,2])
range1 <- maxmin1[2] - maxmin1[1]

rank2_vals <- (D_2[,2] - maxmin2[1]) * range3/range2 + maxmin3[1]
rank1_vals <- (D_1[,2] - maxmin1[1]) * range3/range1 + maxmin3[1]

# put rescaled scores in current tier list order
rank2 <- rep(NA, length.out=nrow(D))
rank1 <- rep(NA, length.out=nrow(D))
char_vec_v3 <- as.character(D[,1])
char_vec_v2 <- as.character(D_2[,1])
char_vec_v1 <- as.character(D_1[,1])

for (i in 1:nrow(D_2)) {
    if (char_vec_v2[i]=='Mii Swordfighter') {
        ind <- which(char_vec_v3=='Mii Swordsman')
    } else {
        ind <- which(char_vec_v3==char_vec_v2[i])
    }
    rank2[ind] <- rank2_vals[i]
}

for (i in 1:nrow(D_1)) {
    if (char_vec_v1[i]=='Duck Hunt Dog') {
        ind <- which(char_vec_v3=='Duck Hunt')
    } else {
        ind <- which(char_vec_v3==char_vec_v1[i])
    }
    rank1[ind] <- rank1_vals[i]
}

df2 <- cbind(xplot, df[,3], rank2, rank1)
df2<-as.data.frame(df2)
colnames(df2)[2] <- 'rank3'

df2 <- mutate(df2, delta_1_2 = rank2 - rank1, delta_2_3 = rank3 - rank2)

