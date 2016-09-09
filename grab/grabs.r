# grabs.r

# visualization of grab frame data for each character
# maximum grab range data taken from this video: https://youtu.be/DOXQJKrx_wQ
# other grab attributes taken from kurogane hammer

library(tidyverse)
library(png)
library(grid)
library(gridExtra)
library(gplots)

dat<-read.table('grabs.txt', header=T, sep='\t', stringsAsFactors=FALSE)
dat<-mutate(dat, EndLag = FAF - HitboxEnd)

imgpre<-'../StockIcons/stock_90_'
imgpost<-'_01.png'

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

make_annotation<-function(raster, x, y, img_offset) {
    annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
}

make_rect<-function(x, y, rect_offset, fill, alpha=0.3, x_scale=1, y_scale=1) {
    annotate('rect', xmin=x-rect_offset/x_scale, xmax=x+rect_offset/x_scale, ymin=y-rect_offset/y_scale, ymax=y+rect_offset/y_scale, alpha=alpha, fill=fill)
}

make_legend_rect<-function(xl, xr, yd, yu, rect_offset, fill, alpha=0.3, x_scale=1, y_scale=1) {
    annotate('rect', xmin=xl-rect_offset/x_scale, xmax=xr+rect_offset/x_scale, ymin=yd-rect_offset/y_scale, ymax=yu+rect_offset/y_scale, alpha=alpha, fill=fill)
}

rasters<-make_rasters(dat)


#################
# Max grab range vs FAF
#################

# identical coords list:
# kirby, sheik (28, 71)
# luigi, mario, gunner, sword (29, 79)
# gnw, roy, brawler (30, 74)
# lucina, marth, pit, dpit (30, 78)
# falcon, megaman (31, 76)

# no zoom
p1_offset<-4
p1 <- ggplot(data=dat, aes(x=FAF, y=GrabPixels)) +
    mapply(make_annotation, rasters, dat[,5], dat[,2], p1_offset) + 
    ylab('Max Standing Grab Range (in pixels)')

ggsave(file='GrabRange_v_FAF_nozoom.png', width=10, height=8, dpi=100, plot=p1)

# left panel: zoom to crowded area
# get overlapping values (create unique pair key, get ones with more than one copy present)
p2_dupes <- dat %>% mutate(RangeFAF = paste(GrabPixels, FAF)) %>%
    group_by(RangeFAF) %>% filter(n()>1) %>% ungroup
p2_unique_coords <- p2_dupes %>% distinct(FAF, GrabPixels)
p2_plot<-cbind(p2_unique_coords, colors=rich.colors(nrow(p2_unique_coords)))
p2_offset<-1.5

p2_L <- ggplot(data=dat, aes(x=FAF, y=GrabPixels)) +
    mapply(make_annotation, rasters, dat[,5], dat[,2], p2_offset) +
    mapply(make_rect, p2_plot[,1], p2_plot[,2], p2_offset, p2_plot[,3], x_scale=3) +
    coord_cartesian(xlim=c(27, 40), ylim=c(62, 102)) + 
    ylab('Max Standing Grab Range (in pixels)')

# right panel: legend for colored boxes
legend_ncol<-2 # fix legend width at 2
legend_floor<-0 # bottom y coord for legend
legend_2<-data.frame(Character=character(), x=numeric(), y=numeric())

p2_temp<-p2_dupes

# pop entries out of matrix until none remain
while (nrow(p2_temp)>0) {
    # for new match, put on new row in legend
    legend_floor<-legend_floor+1

    # get first entry as comparison benchmark
    key<-p2_temp$RangeFAF[1]

    # get entries that match this key
    pop_inds<-which(p2_temp$RangeFAF == key)

    # add values to legend_2 dataframe
    for (i in 1:length(pop_inds)) {
        new_entry<-data.frame(p2_temp[pop_inds[i], 1], (i-1)%%legend_ncol, legend_floor)
        legend_2<-rbind(legend_2, new_entry)

        # if needed, increment legend height to add another row for group
        if (i %% legend_ncol == 0) {
            if (i==length(pop_inds)) { next }
            legend_floor<-legend_floor+1
        }
    }

    # pop values from matrix
    p2_temp<-p2_temp[-pop_inds,]
}

colnames(legend_2)<-c('Character', 'x', 'y')
l2_rast_ind<-match(legend_2[,1], dat[,1])
legend_offset<-0.5

# make legend annotation rectangles
df_init<-rep(NA, length.out=5)
legend_2_rect<-data.frame(xl=df_init, xr=df_init, yd=df_init, yu=df_init, color=df_init, stringsAsFactors=FALSE)
legend_2_rect[,5]<-rich.colors(nrow(legend_2_rect))
legend_2_rect[,1]<-0
legend_2_rect[,2]<-1
legend_2_rect[,3]<-c(1, 2, 4, 5, 7)
legend_2_rect[,4]<-c(1, 3, 4, 6, 8)

p2_R <- ggplot(data=legend_2, aes(x=x, y=y)) +
    mapply(make_annotation, rasters[l2_rast_ind], legend_2[,2], legend_2[,3], legend_offset) + 
    mapply(make_legend_rect, legend_2_rect[,1], legend_2_rect[,2], legend_2_rect[,3], legend_2_rect[,4], legend_offset, legend_2_rect[,5]) +
    coord_cartesian(xlim=c(-0.5, 1.5), ylim=c(0.5, 8.5)) + 
    theme(axis.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) 

p2<-arrangeGrob(p2_L, p2_R, nrow=1, widths=c(8,1))
# grid.draw(p2)

ggsave(file='GrabRange_v_FAF_zoom.png', width=12, height=8, dpi=100, plot=p2)


##################
# Max grab range vs active frames
##################


##################
# Endlag vs startup
##################




