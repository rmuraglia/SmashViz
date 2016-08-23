# tierlist-1.1.3.r

############
# revisualize 4BR tier list to de-emphasize arbitrary groupings and emphasize uncertainty on characters between voters
############

library(rvest)
library(ggplot2)
library(grid)
library(png)

###
# if data table doesn't exist locally, fetch, load and write. else, just load
###
if (file.exists('4BR-1.1.3.txt')) {
    D <- read.table('4BR-1.1.3.txt', sep='\t', header=T)
} else {
    url<-'http://smashboards.com/threads/first-official-4br-smash-for-wii-u-tier-list.429817/'
    html<-read_html(url)
    D <- html %>% html_nodes('table') %>% .[[1]] %>% html_table()
    D <- D[,-1] # drop row number column
    write.table(D, file='4BR-1.1.3.txt', quote=FALSE, sep='\t', col.names=TRUE, row.names=FALSE)
}

###
# load stock icons as rasters
###
imgpre<-'../StockIcons/stock_90_'
imgpost<-'_01.png'

raster_list<-list()
for (i in 1:nrow(D)) {
    char_string<-D[i,1]

    # go through exceptions for character naming
    if (char_string=='Captain Falcon') { char_string<-'captain' }
    if (char_string=='King Dedede') { char_string<-'dedede' }
    if (char_string=='Diddy Kong') { char_string<-'diddy' }
    if (char_string=='Donkey Kong') { char_string<-'donkey' }
    if (char_string=='Dr. Mario') { char_string<-'drmario' }
    if (char_string=='Duck Hunt Dog') { char_string<-'duckhunt' }
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

    # turn into raster object
    raster_list[[i]]<-rasterGrob(img, interpolate=TRUE)
}

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
colnames(df)[c(3, 4)]<-c('AvgPlacement', 'StdDev') # make names more ggplot friendly

####
# plotting call
####

outplot<-ggplot(data=df, aes(x=xplot, y=AvgPlacement, ymin=AvgPlacement-StdDev, ymax=AvgPlacement+StdDev)) +
    geom_linerange(colour='black', size=0.5) + theme_grey() +
    mapply(make_annotation, raster_list, df[,1], df[,3]) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
    labs(title='4BR tier list - patch 1.1.3') +
    # theme(axis.ticks=element_line(colour='grey50')) + # looks more like old ggplot default
    scale_x_continuous(expand=c(0.015, 0))
    # coord_cartesian(xlim=c(min(xplot)-img_offset, max(xplot)+img_offset)) # with default flag expand=T and new ggplot defaults this has too much white space

ggsave(file='4br-1.1.3.png', width=12, height=4.5, dpi=200, plot=outplot)

