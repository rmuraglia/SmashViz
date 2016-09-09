# pop_v_results.r

# revisualization of smashboards users Das Koopa and Thinkaman's work
# character usage rates vs tournament placings as a measure of popularity vs results
# from: https://smashboards.com/threads/4br-tier-list-v1-0-competitive-impressions.429826/page-442#post-21235085
# with some additional model fits to help discern trends

library(ggplot2)
library(png)
library(grid)

dat<-read.table('pop_v_results.txt', header=T, sep=',')

imgpre<-'../StockIcons/stock_90_'
imgpost<-'_01.png'
img_offset<-0.15

make_rasters<-function(D, alpha=1) {
    raster_list<-list()
    for (i in 1:nrow(D)) {
        char_string<-D[i,1]

        # go through exceptions for character icons
        if (char_string=='Kamui' | char_string=='Reflet') { img<-readPNG(paste(imgpre, tolower(char_string), '_02.png', sep='')) 
        } else if (char_string=='Wario') { img<-readPNG(paste(imgpre, tolower(char_string), '_05.png', sep='')) 
        } else { img<-readPNG(paste(imgpre, tolower(char_string), '_01.png', sep=''))}

        # alpha controls transparency (0=transparent, 1=opaque). default value is 1
        pre_raster<-matrix(rgb(img[,,1], img[,,2], img[,,3], img[,,4]*alpha), nrow=dim(img)[1])

        # turn into raster object
        raster_list[[i]]<-rasterGrob(pre_raster, interpolate=TRUE)
    }
    return(raster_list)
}

make_annotation<-function(raster, x, y) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

rasters<-make_rasters(dat)

# fig 1: no fit
plot1 <- ggplot(data=dat, aes(x=Popularity, y=Results)) + mapply(make_annotation, rasters, dat[,2], dat[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank())

ggsave(file='pop_v_res_nofit.png', width=8, height=8, dpi=100, plot=plot1)

# fig 2: loess fit
plot2 <- ggplot(data=dat, aes(x=Popularity, y=Results)) +
    stat_smooth(method='loess', level=0.99) + 
    mapply(make_annotation, rasters, dat[,2], dat[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +
    coord_cartesian(ylim=c(-0.2,6.2))
ggsave(file='pop_v_res_loess.png', width=8, height=8, dpi=100, plot=plot2)

# fig 3: lm fit
plot3 <- ggplot(data=dat, aes(x=Popularity, y=Results)) +
    stat_smooth(method='lm', level=0.99) + 
    mapply(make_annotation, rasters, dat[,2], dat[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +
    coord_cartesian(ylim=c(-0.2,6.2))
ggsave(file='pop_v_res_lm.png', width=8, height=8, dpi=100, plot=plot3)

# fig 4: poly(x,2) fit
plot4 <- ggplot(data=dat, aes(x=Popularity, y=Results)) +
    stat_smooth(method='lm', formula=y~poly(x, 2), level=0.99) + 
    mapply(make_annotation, rasters, dat[,2], dat[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +
    coord_cartesian(ylim=c(-0.2,6.2))
ggsave(file='pop_v_res_x2.png', width=8, height=8, dpi=100, plot=plot4)

# figs 5 and 6: repeat 2 and 3, but with zeros removed
zero_indices <- which(dat$Results==0)
dat_zero_rm <- dat[dat$Results!=0, ]
plot5 <- ggplot(data=dat_zero_rm, aes(x=Popularity, y=Results)) +
    stat_smooth(method='loess', level=0.99) + 
    mapply(make_annotation, rasters[-zero_indices], dat_zero_rm[,2], dat_zero_rm[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +
    coord_cartesian(ylim=c(0.5,6.2))
ggsave(file='pop_v_res_loess_zero_rm.png', width=8, height=8, dpi=100, plot=plot5)

plot6 <- ggplot(data=dat_zero_rm, aes(x=Popularity, y=Results)) +
    stat_smooth(method='lm', level=0.99) + 
    mapply(make_annotation, rasters[-zero_indices], dat_zero_rm[,2], dat_zero_rm[,3]) +
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank()) +
    coord_cartesian(ylim=c(0.5,6.2))
ggsave(file='pop_v_res_lm_zero_rm.png', width=8, height=8, dpi=100, plot=plot6)