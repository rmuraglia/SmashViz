# tierlist-1.1.6.r

############
# revisualize 4BR tier list to de-emphasize arbitrary groupings and emphasize uncertainty on characters between voters
############

library(rvest)
library(ggplot2)
library(grid)
library(png)
library(scales)

###
# if data table doesn't exist locally, fetch, load and write. else, just load
###
if (file.exists('4BR-1.1.6.txt')) {
    D <- read.table('4BR-1.1.6.txt', sep='\t', header=T)
} else {
    url<-'https://smashboards.com/threads/4br-smash-for-wii-u-tier-list-v2-0.440779/'
    html<-read_html(url)
    D <- html %>% html_nodes('table') %>% .[[1]] %>% html_table()
    D <- D[,-1] # drop row number column
    write.table(D, file='4BR-1.1.6.txt', quote=FALSE, sep='\t', col.names=TRUE, row.names=FALSE)
}

###
# load stock icons as rasters
###
imgpre<-'../StockIcons/stock_90_'
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
        if (char_string=='Mii Swordfighter') { char_string<-'miiswordsman' }
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
df[,3]<-nrow(D)-df[,3]
colnames(df)[c(3, 4)]<-c('AvgPlacement', 'StdDev') # make names more ggplot friendly

####
# plotting call
####

outplot<-ggplot(data=df, aes(x=xplot, y=AvgPlacement, ymin=AvgPlacement-StdDev, ymax=AvgPlacement+StdDev)) +
    geom_linerange(colour='black', size=0.5) + theme_grey() +
    mapply(make_annotation, rasters, df[,1], df[,3]) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
    labs(title='4BR tier list - patch 1.1.6') +
    # theme(axis.ticks=element_line(colour='grey50')) + # looks more like old ggplot default
    scale_x_continuous(expand=c(0.015, 0))
    # coord_cartesian(xlim=c(min(xplot)-img_offset, max(xplot)+img_offset)) # with default flag expand=T and new ggplot defaults this has too much white space

ggsave(file='4br-1.1.6.png', width=12, height=4.5, dpi=200, plot=outplot)

###############
# Second figure: note character movement
###############

if (file.exists('../tierlist-1.1.3/4BR-1.1.3.txt')) {
    D_old <- read.table('../tierlist-1.1.3/4BR-1.1.3.txt', sep='\t', header=T)
} else {
    url<-'http://smashboards.com/threads/first-official-4br-smash-for-wii-u-tier-list.429817/'
    html<-read_html(url)
    D_old <- html %>% html_nodes('table') %>% .[[1]] %>% html_table()
    D_old <- D_old[,-1] # drop row number column
    write.table(D_old, file='../tierlist-1.1.3/4BR-1.1.3.txt', quote=FALSE, sep='\t', col.names=TRUE, row.names=FALSE)
}

ghosts<-make_rasters(0.5)

## don't use internal rank delta - doesn't account for true voting score position
# rank_delta<-as.numeric(as.character(D[,7]))
# rank_delta[is.na(rank_delta)]<-0
# prev_rank<-df[,3]-rank_delta

## use previous data instead to reconstruct rank
maxmin6<-range(df[,3])
range6<-maxmin6[2]-maxmin6[1]
maxmin3<-range(D_old[,2])
range3<-maxmin3[2]-maxmin3[1]
scale_f<-range6/range3
fixed_f<-maxmin6[1]
prev_rank_vals<-(D_old[,2]-maxmin3[1])*scale_f + fixed_f

# insert rank to appropriate slot
prev_rank<-rep(NA, length.out=nrow(D))
char_vec_new<-as.character(D[,1])
char_vec_old<-as.character(D_old[,1])
for (i in 1:nrow(D_old)) {
    if (char_vec_old[i]=='Duck Hunt Dog') {
        ind<-which(char_vec_new=='Duck Hunt')
    } else if (char_vec_old[i]=='Mii Swordsman') { 
        ind<-which(char_vec_new=='Mii Swordfighter')
    } else { 
        ind<-which(char_vec_new==char_vec_old[i])
    }
    prev_rank[ind]<-prev_rank_vals[i]
}


df2<-cbind(xplot, D, prev_rank)
df2<-as.data.frame(df2)
df2[,3]<-nrow(D)-df2[,3]
colnames(df2)[c(3, 4)]<-c('AvgPlacement', 'StdDev')

# denote no change for bayo and corrin
bc_ind<-is.na(df2[,9])
df2[bc_ind, 9]<-df2[bc_ind, 3]

true_delta<-df2[,3]-df2[,9]
df2<-cbind(df2, true_delta)

plot2<-ggplot(data=df2, aes(x=xplot, y=AvgPlacement)) +
    geom_segment(aes(x=xplot, xend=xplot, y=prev_rank, yend=AvgPlacement, colour=true_delta), arrow=arrow(length=unit(0.3, 'cm'))) +
    mapply(make_annotation, rasters, df2[,1], df2[,3]) +
    mapply(make_annotation, ghosts, df2[,1], df2[,9]) +
    scale_colour_gradient2(low="blue", high="red") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') +
    labs(title='4BR tier list - patch 1.1.6') +
    scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-1.1.6-withdeltas.png', width=12, height=4.5, dpi=200, plot=plot2)


####
# sort by delta
####

delta_order<-order(true_delta)
sort_df<-cbind(as.character(df2$Character), Baseline=0, df2$true_delta)
sort_df<-sort_df[delta_order, ]
sort_df<-as.data.frame(cbind(xplot, sort_df))
colnames(sort_df)[c(2,4)]<-c('Character', 'true_delta')

plot3<-ggplot(data=sort_df, aes(x=xplot, y=true_delta)) +
    geom_segment(aes(x=xplot, xend=xplot, y=Baseline, yend=true_delta, colour=true_delta), arrow=arrow(length=unit(0.3, 'cm'))) +
    mapply(make_annotation, rasters[delta_order], sort_df[,1], sort_df[,4]) + 
    mapply(make_annotation, rasters[delta_order], sort_df[,1], sort_df[,3]) + 
    scale_colour_gradient2(low="blue", high="red") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), legend.position='none') +
    labs(title='4BR tier list - patch 1.1.6') +
    scale_x_continuous(expand=c(0.015, 0))

ggsave(file='4br-1.1.6-sorted-deltas.png', width=12, height=4.5, dpi=200, plot=plot2)
