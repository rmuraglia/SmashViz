# 4BR_v_runspeed.r

library(ggplot2)
library(png)
library(grid)
library(gridExtra)

# load data
D <- read.table('4BR-1.1.6.txt', sep='\t', header=T)

# add runspeed vector (sorted by 4br order)
RunSpeed<-c(1.824, 1.97, 2.016, 1.632, 1.6, 3.5, 2.184, 
    2.1, 1.6, 2.05, 1.6, 1.85325, 1.9, 1.27, 1.456, 
    1.46265, 1.7325, 1.45, 1.785, 2.32, 2.08, 1.55, 
    1.86, 1.66215, 1.7031, 1.4175, 1.66215, 1.568, 
    1.5, 1.47, 1.15, 1.792, 1.5, 1.5, 1.5, 1.785, 
    1.5264, 1.52, 2.24, 1.888, 1.57, 1.63, 1.3944, 
    1.52, 1.504, 1.424, 1.696, 1.312, 1.472, 1.95, 2, 
    1.36, 1.3, 1.72, 1.3, 1.218, 1.5, 1.155)

D <- cbind(D, RunSpeed)
speedorder<-order(RunSpeed)

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
# 4BR placement vs RunSpeed
####

img_offset<-1.2
plot1_df<-as.data.frame(D[speedorder, c(1, 2, 8)])
colnames(plot1_df)<-c('Character', 'AvgPlacement', 'RunSpeed')
plot1_df[,2]<-nrow(plot1_df)-plot1_df[,2]

plot1 <- ggplot(data=plot1_df, aes(x=RunSpeed, y=AvgPlacement)) +
    geom_smooth(method='lm', formula=y~x+log(x), se=FALSE, colour='grey30') +
    mapply(make_annotation, rasters[speedorder], plot1_df[,3], plot1_df[,2])

# normal view
ggsave(file='4BR_v_runspeed_nozoom.png', width=8, height=8, dpi=100, plot=plot1)

# zoomed because sonic is a jerk
ggsave(file='4BR_v_runspeed_zoom.png', width=8, height=8, dpi=100, plot=plot1 + coord_cartesian(xlim=range(RunSpeed[-6]), expand=TRUE))

# zoom with x + log(x) fit with sonic removed
ggsave(file='4BR_v_runspeed_zoom_log_trunc.png', width=8, height=8, dpi=100, plot=plot1 + scale_x_continuous(limits=range(RunSpeed[-6])))

# zoom with linear fit with sonic removed
plot2 <- ggplot(data=plot1_df, aes(x=RunSpeed, y=AvgPlacement)) +
    geom_smooth(method='lm', se=FALSE, colour='grey30') +
    mapply(make_annotation, rasters[speedorder], plot1_df[,3], plot1_df[,2]) +
    scale_x_continuous(limits=range(RunSpeed[-6]))

ggsave(file='4BR_v_runspeed_zoom_lm_trunc.png', width=8, height=8, dpi=100, plot=plot2)



# other option for fit formula - polynomial 
# geom_smooth(method='lm', formula=y~poly(x, 2), se=FALSE)

# GAM (generalized additive model) smoothing example: http://www.ats.ucla.edu/STAT/r/faq/smooths.htm




