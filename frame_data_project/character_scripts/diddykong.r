# diddykong.r

library(jsonlite)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)
library(gridExtra)

source('../fdp_common.r')

moves_df <- import_from_kh('http://api.kuroganehammer.com/api/Characters/name/Diddykong/detailedmoves')

# filter out uninteresting or problematic moves
moves_df <-  filter(moves_df, !grepl('Explosion|Failed|throw|^Rapid Jab$|Throw)$', moveName))

skip_moves <- c('Peanut Popgun', 'Monkey Flip', 'Rocketbarrel Boost', 'Banana Peel')
skip_string <- paste(skip_moves, collapse='|')

# get general 'all move' information 
# frame_max <- max(as.numeric(moves_df$FAF), na.rm=TRUE) + 1
frame_max <- 60
unique_moves <- sapply(moves_df$moveName, stem_move_names) %>% unique(.)

# split monkey flip into grab and attack variants
unique_moves[21] <- 'Monkey Flip (Grab)'
unique_moves <- append(unique_moves, 'Monkey Flip (Attack Input)', 21) 

num_unique_moves <- length(unique_moves)

# initialize information stores
all_frames <- as.data.frame(matrix(NA, nrow=num_unique_moves, ncol=frame_max))
colnames(all_frames) <- c(1:frame_max)
rownames(all_frames) <- unique_moves
all_attribs <- all_frames

# populate active hitboxes
all_frames <- get_active_frames(moves_df, skip_string, all_frames)

# populate recovery frames
all_frames <- get_recovery_frames(moves_df, skip_string, all_frames)

# populate inactive frames
all_frames <- get_inactive_frames(all_frames, skip_string)

# populate autocancel frames
all_attribs <- get_ac_frames(moves_df, skip_string, all_attribs)

# process Peanut Popgun
all_frames['Peanut Popgun', 18] <- 4
all_frames['Peanut Popgun', 1:17] <- 1
all_frames['Peanut Popgun', 19:48] <- 1

# process Monkey Flip (grab)
all_frames['Monkey Flip (Grab)', 20:42] <- 2
all_frames['Monkey Flip (Grab)', 1:19] <- 1
all_frames['Monkey Flip (Grab)', 42:43] <- 1

# process Monkey Flip (kick)
all_frames['Monkey Flip (Attack Input)', 1:5] <- 1
all_frames['Monkey Flip (Attack Input)', 6:10] <- 2
all_frames['Monkey Flip (Attack Input)', 11:25] <- 3
all_frames['Monkey Flip (Attack Input)', 26:29] <- 1

# process Rockets
all_frames['Rocketbarrel Boost', 1:7] <- 1
all_frames['Rocketbarrel Boost', 8:16] <- 2
all_frames['Rocketbarrel Boost', 17:33] <- 3

# process Banana
all_frames['Banana Peel', 1:19] <- 1
all_frames['Banana Peel', 20]<-4
all_frames['Banana Peel', 21:39]<-1



##############
# GENERATE FIGURE
##############

# transform into ggplot friendly format
plot_frames <- all_frames %>% mutate(MoveNames=rownames(.)) %>% gather(key=Frame, value=Type, -MoveNames) %>% mutate(Frame=as.integer(Frame)) %>% mutate(Type=as.factor(Type)) %>% mutate(MoveNames=factor(MoveNames, levels=rev(unique_moves)))
plot_attribs <- all_attribs %>% mutate(MoveNames=rownames(.)) %>% gather(key=Frame, value=Type, -MoveNames) %>% mutate(Frame=as.integer(Frame)) %>% mutate(Type=as.factor(Type)) %>% mutate(MoveNames=factor(MoveNames, levels=rev(unique_moves)))


# load in icons for attributes
img_list <- c('../icons/shield2.png', '../icons/spark.png', '../icons/arrow-cropped.png', '../icons/hexagon_effect_abstract_sign-512.png')

# create icon plotting object for each cell
img_offset<-0.4
raster_list<-vector('list', nrow(plot_attribs))
for (i in 1:nrow(plot_attribs)) {
    if (!is.na(plot_attribs[i,3])) {
        img <- readPNG(img_list[as.numeric(as.character(plot_attribs[i,3]))])
        raster_list[[i]] <- rasterGrob(img, interpolate=TRUE)
    }
}

make_annotation<-function(raster, x, y) {
    if (!is.null(raster)) {
        annotation_custom(raster, xmin=x-img_offset, xmax=x+img_offset, ymin=y-img_offset, ymax=y+img_offset)
    }
}

# to identify line positions to separate classes of attacks, inspect manually (add 0.5 to end of class)
# cbind(rev(unique_moves), 1:num_unique_moves)
line_positions <- c(5.5, 10.5, 13.5, 16.5, 19.5)

# main panel plot
p1 <- ggplot(plot_frames, aes(x=Frame, y=MoveNames)) + 
    geom_tile(aes(fill=Type), colour='grey50') + 
    scale_x_continuous(position='bottom', breaks=seq(0,frame_max, 5), expand=c(0.01,0), 
        sec.axis=sec_axis(name='', trans=~., breaks=seq(0, frame_max, 5))) + 
    mapply(make_annotation, raster_list, plot_attribs[,2], as.numeric(plot_attribs[,1])) +  
    scale_fill_manual(values=c('gold', 'red', 'red4', 'purple'), na.value='grey80', labels=c('None', 'Main', 'Secondary', 'Item\nspawn'), name='Direct\nhitboxes') + 
    labs(title='Diddy Kong\'s Smash 4 frame data by @Quappo_ for Floor\'s frame data viz project') + 
    theme(axis.title.y=element_blank()) +
    geom_hline(yintercept=line_positions)

# extract legend
g <- ggplotGrob(p1 + theme(legend.position='right'))$grobs
color_legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

# create fake plot for icon legend
img_legend_rasters <- list()
for (i in 1:length(img_list)) {
    img_legend_rasters[[i]] <- rasterGrob(readPNG(img_list[i]), interpolate=TRUE)
}

legend_y_vals <- data.frame(y_vals=c(0:5))
icon_legend <- ggplot(data=legend_y_vals, aes(y=y_vals)) + 
    geom_point(x=0, alpha=0) + # dummy points help for framing
    mapply(make_annotation, img_legend_rasters, 0, c(1:length(img_legend_rasters))) + 
    coord_cartesian(xlim=c(-0.5, 0.5)) +  
    scale_y_continuous(labels=c('', 'Armor/\nInvuln./\nIntangibility', 'Autocancel', 'Projectile\nactive', 'Reflect', ''), position='right', expand=c(0,0)) + 
    theme_classic() + theme(axis.ticks=element_blank(), axis.title.y=element_blank(), axis.line=element_blank(), axis.text.y=element_text(color='black')) #+ ggtitle('Other\nAttributes')

# plot together
legends <- arrangeGrob(color_legend, icon_legend, nrow=2)
p_out <- arrangeGrob(p1 + theme(legend.position='none'), legends, nrow=1, widths=c(9,1))
# grid.draw(p_out) # preview

ggsave(file='../figures/DiddyKong_fdp.png', width=12, height=4, dpi=200, plot=p_out)