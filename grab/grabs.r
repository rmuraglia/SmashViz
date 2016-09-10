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

# assumptions/notes: 
# regular grab reaches max range on hitboxstart
# tether grabs reach max range on hitboxend
# assume tether grab first active frame max range is average of max ranges of regular grabs
# assume linear growth for tether range over course of active frames
# tether grabs defined as grabs with more than one active frame
# when removing 3rd element, this is because that is pacman, who requires special treatment

dat3 <-  dat %>% mutate(Tether = (HitboxEnd - HitboxStart != 1))
tethers <- select(dat3, Tether)
tether_inds<-which(tethers[,])
avg_tether_start<- dat3 %>% filter(Tether==FALSE) %>% summarize(mean(GrabPixels)) %>% .[1,1]

p3_offset<-5

# set up plotting frame and some layers
p3_base <- ggplot(data=dat3, aes(x=HitboxEnd, y=GrabPixels))
p3_reg_icons <- mapply(make_annotation, rasters[-tether_inds], dat3[-tether_inds, 3], dat3[-tether_inds, 2], p3_offset)
# p3_tether_solid <- mapply(make_annotation, rasters[tether_inds[-3]], dat3[tether_inds[-3],4], dat3[tether_inds[-3],2], p3_offset)
p3_tether_solid <- mapply(make_annotation, rasters[tether_inds], dat3[tether_inds,4], dat3[tether_inds,2], p3_offset)
p3_ax <- labs(x='Hitbox Active (Frame)', y='Max Grab Range (in pixels)')
p3_nozoom_coords<-coord_cartesian(xlim=c(6, 39))


# add transparent best guess icons for beginning of tethers
ghosts<-make_rasters(dat3, 0.5)
p3_tether_ghost <- mapply(make_annotation, ghosts[tether_inds[-3]], dat3[tether_inds[-3],3], avg_tether_start, p3_offset)

# add segments for tether linear fits
tether_cols<-rep(NA, length.out=nrow(dat))
tether_cols[tether_inds[-3]]<-c('green4', 'magenta', 'khaki4', 'orangered', 'dodgerblue', 'chartreuse2', 'green')
make_segment<-function(tether_ind) {
    annotate('segment', x=dat3[tether_ind, 3], xend=dat3[tether_ind, 4], y=avg_tether_start, yend=dat3[tether_ind, 2], colour=tether_cols[tether_ind], size=2, alpha=0.5)
}
p3_segments <- mapply(make_segment, tether_inds[-3])

# pacman has a discontinuous, three phase grab. handle manually
# add transparent icons for pac. grob bug makes it such that I need a separate rasterGrob for each annotation
pac_img<-readPNG(paste(imgpre, tolower(dat3[tether_inds[3], 1]), imgpost, sep=''))
pac_trans<-matrix(rgb(pac_img[,,1], pac_img[,,2], pac_img[,,3], pac_img[,,4]*0.5), nrow=dim(pac_img)[1])
pg1<-rasterGrob(pac_trans, interpolate=TRUE)
pg2<-rasterGrob(pac_trans, interpolate=TRUE)
pg3<-rasterGrob(pac_trans, interpolate=TRUE)
pg4<-rasterGrob(pac_trans, interpolate=TRUE)
pac_ghost1<-make_annotation(pg1, 12, avg_tether_start, p3_offset)
pac_ghost2<-make_annotation(pg2, 14, avg_tether_start, p3_offset)
pac_ghost3<-make_annotation(pg3, 22, mean(c(avg_tether_start, dat3[32,2])), p3_offset)
pac_ghost4<-make_annotation(pg4, 24, mean(c(avg_tether_start, dat3[32,2])), p3_offset)
pac_ghost5<-make_annotation(ghosts[[32]], 32, dat3[32,2], p3_offset)

# add segments for pac
pac_line1<-annotate('segment', x=12, xend=14, y=avg_tether_start, yend=avg_tether_start, colour='gold', size=2, alpha=0.5)
pac_line2<-annotate('segment', x=22, xend=24, y=mean(c(avg_tether_start, dat3[32,2])), yend=mean(c(avg_tether_start, dat3[32,2])), colour='gold', size=2, alpha=0.5)
pac_line3<-annotate('segment', x=32, xend=39, y=dat3[32,2], yend=dat3[32,2], colour='gold', size=2, alpha=0.5)

p3_nozoom <- p3_base + pac_line1 + pac_line2 + pac_line3 +
    pac_ghost1 + pac_ghost2 + pac_ghost3 + pac_ghost4 + pac_ghost5 +
    p3_segments + p3_tether_ghost + p3_tether_solid + 
    p3_reg_icons + p3_ax + p3_nozoom_coords

ggsave(file='GrabRange_v_Frames_nozoom.png', width=10, height=6, dpi=100, plot=p3_nozoom)

## zoomed version with legend in right panel
p4_offset<-1.5
p4_zoom_coords<-coord_cartesian(xlim=c(5, 13), ylim=c(60, 105))
p4_reg_icons<-mapply(make_annotation, rasters[-tether_inds], dat3[-tether_inds, 3], dat3[-tether_inds, 2], p4_offset)
p4_tether_ghosts<-mapply(make_annotation, ghosts[tether_inds], dat3[tether_inds, 3], avg_tether_start, p4_offset)
p4_rect1<-make_legend_rect(6.6, 7.4, 72.5, 82.5, 0, alpha=0.3, fill='lightskyblue')
p4_rect2<-make_legend_rect(5.6, 6.4, 69.5, 80.5, 0, alpha=0.3, fill='lightcoral')

p4_L<-p3_base + p4_reg_icons + p4_tether_ghosts + p4_zoom_coords + p4_rect1 + p4_rect2

legend_4U<-array(NA, dim=c(9,3))
legend_4U[1,]<-c('Shulk', 1, 81)
legend_4U[2,]<-c('Bayonetta', 2, 81)
legend_4U[3,]<-c('Kamui', 0.5, 78)
legend_4U[4,]<-c('Lucina', 1.5, 78)
legend_4U[5,]<-c('Marth', 2.5, 78)
legend_4U[6,]<-c('Captain', 1, 76)
legend_4U[7,]<-c('Mewtwo', 2, 76)
legend_4U[8,]<-c('Reflet', 1, 74)
legend_4U[9,]<-c('Roy', 2, 74)
legend_4U<-as.data.frame(legend_4U)
legend_4U[,2]<-as.numeric(as.character(legend_4U[,2]))
legend_4U[,3]<-as.numeric(as.character(legend_4U[,3]))
colnames(legend_4U)<-c('characer','x','y')
l4U_vec<-c(48, 1, 15, 24, 27, 2, 29, 40, 44)
l4U_offset<-0.75

l4U<-ggplot(data=legend_4U, aes(x=x, y=y)) +
    mapply(make_annotation, rasters[l4U_vec], legend_4U[,2], legend_4U[,3], l4U_offset) +
    coord_cartesian(xlim=c(-0.5, 3.5), ylim=c(73, 82), expand=F) +
    theme(axis.text.x = element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    make_legend_rect(-0.5, 3.5, 73, 82, 0, 'lightskyblue', 0.3)

legend_4D<-array(NA, dim=c(19, 3))
legend_4D[1,]<-c('Luigi', 1, 79)
legend_4D[2,]<-c('Mario', 2, 79)
legend_4D[3,]<-c('Gunner', 3, 79)
legend_4D[4,]<-c('Sword', 4, 79)
legend_4D[5,]<-c('Pit', 2, 78)
legend_4D[6,]<-c('DPit', 3, 78)
legend_4D[7,]<-c('Diddy', 2, 77)
legend_4D[8,]<-c('Ness', 3, 77)
legend_4D[9,]<-c('GnW', 1, 74)
legend_4D[10,]<-c('Peach', 2, 74)
legend_4D[11,]<-c('WFT', 3, 74)
legend_4D[12,]<-c('Brawler', 4, 74)
legend_4D[13,]<-c('Fox', 2, 73)
legend_4D[14,]<-c('Pikachu', 3, 73)
legend_4D[15,]<-c('Kirby', 2, 71)
legend_4D[16,]<-c('Sheik', 3, 71)
legend_4D[17,]<-c('Rockman', 2.5, 76)
legend_4D[18,]<-c('Sonic', 2.5, 75)
legend_4D[19,]<-c('Ryu', 2.5, 72)
legend_4D<-as.data.frame(legend_4D)
legend_4D[,2]<-as.numeric(as.character(legend_4D[,2]))
legend_4D[,3]<-as.numeric(as.character(legend_4D[,3]))
colnames(legend_4D)<-c('characer','x','y')
l4D_vec<-c(25, 26, 57, 58, 37, 38, 5, 31, 11, 34, 53, 56, 10, 35, 16, 47, 42, 49, 45)

l4D<-ggplot(data=legend_4D, aes(x=x, y=y)) +
    mapply(make_annotation, rasters[l4D_vec], legend_4D[,2], legend_4D[,3], legend_offset) +
    coord_cartesian(xlim=c(0,5), ylim=c(70.5, 79.5), expand=F) + 
    theme(axis.text.x = element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
    make_legend_rect(0, 5, 70.5, 79.5, 0, 'lightcoral', 0.3)

p4_legends<-arrangeGrob(l4U, l4D, nrow=2, heights=c(8,9))
p4<-arrangeGrob(p4_L, p4_legends, nrow=1, widths=c(3,1))

ggsave(file='GrabRange_v_Frames_zoom.png', width=10, height=6, dpi=100, plot=p4)


##################
# Endlag vs startup
##################




