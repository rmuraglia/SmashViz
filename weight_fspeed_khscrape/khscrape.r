# khscrape.r

library(jsonlite)
library(ggplot2)
library(png)
library(grid)

# check attribute ID mapping with
# http://api.kuroganehammer.com/api/smashattributetypes

# fetch weights
url_weight<-'http://api.kuroganehammer.com/api/smashattributetypes/25/characterattributes'
dat_weight<-fromJSON(url_weight)

# parse weights to dataframe
getWeight<-function(x) { x[8][[1]][4] }
weights<-unlist(apply(dat_weight, 1, getWeight))
w_order<-dat_weight[,5]
w_df<-cbind(w_order, weights)
w_df<-w_df[order(w_df[,1]),]

# fetch fall speeds
url_fspeed<-'http://api.kuroganehammer.com/api/smashattributetypes/8/characterattributes'
dat_fspeed<-fromJSON(url_fspeed)

# parse fall speed (if want fast fall speed, use [2,4]) to dataframe
getFSpeed<-function(x) { x[8][[1]][1,4] }
fspeeds<-unlist(apply(dat_fspeed, 1, getFSpeed))
fs_order<-dat_fspeed[,5]
fs_df<-cbind(fs_order, fspeeds)
fs_df<-fs_df[order(fs_df[,1]),]

# make sure ordering is correct
if (!all(fs_df[,1]==w_df[,1])) { print('Character ordering does not seem consistent. Double check the w_df and fs_df dataframes') }

# make plotting DF
plot_df<-cbind(fs_df, w_df[,2])
colnames(plot_df)<-c('name', 'FallSpeed', 'Weight')
plot_df<-data.frame(plot_df)
plot_df[,2]<-as.numeric(as.character(plot_df[,2]))
plot_df[,3]<-as.numeric(as.character(plot_df[,3]))

# collect img rasters
imgpre<-'../StockIcons/stock_90_'
imgpost<-'_01.png'

make_rasters<-function(alpha=1) {
    raster_list<-list()
    for (i in 1:nrow(plot_df)) {
        char_string<-plot_df[i,1]

        # go through character naming exceptions
        if (char_string=='Bowser') { char_string<-'koopa' }
        if (char_string=='Bowserjr') { char_string<-'koopajr' }
        if (char_string=='Captainfalcon') { char_string<-'captain' }
        if (char_string=='Charizard') { char_string<-'lizardon' }
        if (char_string=='Corrin') { char_string<-'kamui' }
        if (char_string=='Darkpit') { char_string<-'pitb' }
        if (char_string=='Diddykong') { char_string<-'diddy' }
        if (char_string=='Donkeykong') { char_string<-'donkey' }
        if (char_string=='Ganondorf') { char_string<-'ganon' }
        if (char_string=='Greninja') { char_string<-'gekkouga' }
        if (char_string=='Jigglypuff') { char_string<-'purin' }
        if (char_string=='Kingdedede') { char_string<-'dedede' }
        if (char_string=='Megaman') { char_string<-'rockman' }
        if (char_string=='Miibrawler') { char_string<-'miienemyf' }
        if (char_string=='Miigunner') { char_string<-'miigunner' }
        if (char_string=='Miiswordfighter') { char_string<-'miiswordsman' }
        if (char_string=='Mrgamewatch') { char_string<-'gamewatch' }
        if (char_string=='Olimar') { char_string<-'pikmin' }
        if (char_string=='Rob') { char_string<-'robot' }
        if (char_string=='Robin') { char_string<-'reflet' }
        if (char_string=='Rosalinaluma') { char_string<-'rosetta' }
        if (char_string=='Villager') { char_string<-'murabito' }
        if (char_string=='Wiifittrainer') { char_string<-'wiifit' }
        if (char_string=='Zerosuitsamus') { char_string<-'szerosuit' }

        # use classic Wario
        if (char_string=='Wario') { img<-readPNG(paste(imgpre, tolower(char_string), '_05.png', sep='')) }
        else { img<-readPNG(paste(imgpre, tolower(char_string), imgpost, sep='')) }

        # alpha controls transparency (0=transparent, 1=opaque). default value is 1
        pre_raster<-matrix(rgb(img[,,1], img[,,2], img[,,3], img[,,4]*alpha), nrow=dim(img)[1])

        # turn into raster object
        raster_list[[i]]<-rasterGrob(pre_raster, interpolate=TRUE)
    }
    return(raster_list)
}

rasters<-make_rasters()
img_offset<-1.25

make_annotation<-function(raster, x, y) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

# plotting call
outplot<-ggplot(data=plot_df, aes(x=Weight, y=FallSpeed)) + mapply(make_annotation, rasters, plot_df[,3], plot_df[,2])
ggsave(file='FallSpeed_v_Weight.png', width=9, height=8, dpi=200, plot=outplot)

# qplot(plot_df[,2], plot_df[,3])