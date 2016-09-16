# season1_co-usage_heatmap.r

# calculate character co-usage rates for each character based on anther's ladder wii u season 1 data
# co-usage rate is defined as [...]

library(tidyverse)
library(png)
library(grid)
library(gridExtra)
library(RMySQL)

cnx <- dbConnect(MySQL(), dbname='anthers_02_02_2016')

df <- dbGetQuery(cnx, 'select player_ladder_stat_characters.player_id, characters.name, player_ladder_stat_characters.times_used, player_ladder_stat_characters.wins from player_ladder_stat_characters join characters on player_ladder_stat_characters.character_id = characters.id where season_id = 7;')

dbDisconnect(cnx)

# make character name field a factor for easy indexing
df_backup<-df # original copy in case of needed reversion to untouched query
df$name<-as.factor(df$name)

# create matrix showing cumulative co-usage shares
nchars<-nlevels(df$name)
use_mat<-matrix(0, nrow=nchars, ncol=nchars)
dimnames(use_mat)<-list(levels(df$name), levels(df$name))

players<-unique(df$player_id) # or distinct(df, player_id)

# define a character being used as a main if it was used in the majority of a given user's matches
main_count<-rep(0, length.out=nchars)

for (player in players) {

    # get individual player data
    sub_df<-subset(df, df$player_id==player) # or filter(df, player_id==player)

    # get fractional usage rates per character
    use_prc<-sub_df[,3]/sum(sub_df[,3])

    # get most used character and increment global mains vector
    main_ind<-which.max(use_prc)
    main_char<-sub_df$name[main_ind]
    main_count[main_char]<-main_count[main_char]+1

    # for each character used, add usage rate to main character's entry
    for (i in 1:nrow(sub_df))  {
        use_char<-sub_df$name[i]
        use_mat[main_char, use_char]<-use_mat[main_char, use_char] + use_prc[i]
    }
}

use_mat_backup<-use_mat # copy in case of reversion
self_use_rate<-diag(use_mat)/main_count
diag(use_mat)<-0 # remove dominating self-use values on diagonal
diag(use_mat)<-apply(use_mat, 1, median) # reset self-use as baseline median for secondaries to better illustrate +/- relative to other secondaries

# scale operates on columns, so double transpose
use_mat<-t(scale(t(use_mat), center=diag(use_mat), scale=rowSums(use_mat))) # center on per-character medians, scale by overall secondary usage per character to normalize
use_mat[is.nan(use_mat)]<-0

# tidy data for plotting
plot_df<-as.data.frame(use_mat) %>% mutate(Mains=rownames(use_mat)) %>%
    gather(key=Secondaries, value=Usage_Rate, -Mains)
# alternate version for matrices with var names in dimnames: reshape2:::melt.matrix(use_mat)

######
# Plotting calls
######

# plot1: basic plot
p1 <- ggplot(plot_df, aes(x=Secondaries, y=Mains)) + 
    geom_tile(aes(fill=Usage_Rate), colour='grey50') +
    scale_fill_gradient2(low='white', high='black') +
    theme(legend.position='none') + 
    # theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) # perpendicular
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) # slanted

ggsave(file='anther_s1_cusage_hmap01.png', width=12, height=8, dpi=100, plot=p1)

# plot 2: icon version
# note for plot margin: default is 6 all around. order is clockwise from top

imgpre<-'../StockIcons/stock_90_'
imgpost<-'_01.png'

make_rasters<-function(char_vec, alpha=1) {
    raster_list<-list()
    for (i in char_vec) {

        # go through exceptions for character naming
        if (i=='Captain Falcon') { i<-'captain' }
        if (i=='King Dedede') { i<-'dedede' }
        if (i=='Diddy Kong') { i<-'diddy' }
        if (i=='Donkey Kong') { i<-'donkey' }
        if (i=='Dr. Mario') { i<-'drmario' }
        if (i=='Duck Hunt Duo') { i<-'duckhunt' }
        if (i=='Mr. Game And Watch') { i<-'gamewatch' }
        if (i=='Ganondorf') { i<-'ganon' }
        if (i=='Greninja') { i<-'gekkouga' }
        if (i=='Corrin') { i<-'kamui' }
        if (i=='Bowser') { i<-'koopa' }
        if (i=='Bowser Jr.') { i<-'koopajr' }
        if (i=='Little Mac') { i<-'littlemac' }
        if (i=='Charizard') { i<-'lizardon' }
        if (i=='Mii Brawler') { i<-'miienemyf' }
        if (i=='Mii Gunner') { i<-'miigunner' }
        if (i=='Mii Swordsman') { i<-'miiswordsman' }
        if (i=='Villager') { i<-'murabito' }
        if (i=='Olimar') { i<-'pikmin' }
        if (i=='Dark Pit') { i<-'pitb' }
        if (i=='Jigglypuff') { i<-'purin' }
        if (i=='Robin') { i<-'reflet' }
        if (i=='R.O.B.') { i<-'robot' }
        if (i=='Megaman') { i<-'rockman' }
        if (i=='Rosalina And Luma') { i<-'rosetta' }
        if (i=='Zero Suit Samus') { i<-'szerosuit' }
        if (i=='Toon Link') { i<-'toonlink' }
        if (i=='Wii Fit Trainer') { i<-'wiifit' }
        if (i=='Random') { i<-'omakase' }

        # go through exceptions for character icons
        if (i=='kamui' | i=='reflet') { img<-readPNG(paste(imgpre, i, '_02.png', sep='')) }
        else if (i=='Wario') { img<-readPNG(paste(imgpre, tolower(i), '_05.png', sep='')) }
        else { img<-readPNG(paste(imgpre, tolower(i), '_01.png', sep=''))}

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

rasters<-make_rasters(rownames(use_mat))

# make vertial strip for 'mains' axis
y_offset<-1
yplot<-seq(from=y_offset, to=nchars*y_offset, by=y_offset)
y_strip <- ggplot(data=as.data.frame(yplot), aes(y=yplot)) + 
    mapply(make_annotation, rasters, 0, yplot, y_offset) +
    theme_classic() + 
    theme(axis.ticks=element_blank()) +
    scale_x_continuous(limits=c(-y_offset, y_offset), expand=c(0, 0)) + 
    scale_y_discrete(expand=c(0.01,0)) +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
    theme(axis.title.x=element_blank()) + ylab('Mains') +
    theme(plot.margin=unit(c(6, 0, 0, 6), 'pt'))

# make horizontal strip for 'secondaries' axis
x_offset<-1
xplot<-seq(from=x_offset, to=nchars*x_offset, by=x_offset)
x_strip<-ggplot(data=as.data.frame(xplot), aes(x=xplot)) +
    mapply(make_annotation, rasters, xplot, 0, x_offset) +
    theme_classic() +
    theme(axis.ticks=element_blank()) +
    scale_y_continuous(limits=c(-x_offset, x_offset), expand=c(0,0)) +
    scale_x_discrete(expand=c(0.01, 0)) +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
    theme(axis.title.y=element_blank()) + xlab('Secondaries') +
    theme(plot.margin=unit(c(0,6,6,0), 'pt'))

# make totally blank plot to take up lower left corner space
blank_plot<-ggplot(data=as.data.frame(0)) + geom_blank() + theme_classic() + labs(x='', y='') + theme(plot.margin=unit(c(0,0,6,6), 'pt'))

# suppress axes from previous plot
p2_hmap <- ggplot(plot_df, aes(x=Secondaries, y=Mains)) + 
    geom_tile(aes(fill=Usage_Rate), colour='grey50') +
    scale_fill_gradient2(low='white', high='black') +
    theme(legend.position='none') + 
    theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    theme(plot.margin=unit(c(6,6,0,0), 'pt'))

# combine for final plot
p2 <- arrangeGrob(y_strip, p2_hmap, blank_plot, x_strip, nrow=2, ncol=2, widths=c(1, 25), heights=c(13, 1))
# grid.draw(p2)
ggsave(file='anther_s1_cusage_hmap02.png', width=12, height=8, dpi=100, plot=p2)


# plot 3: emphasize small differences. illustrate rare usage too
# all values are between -1 and 1, with the vast majority concentrated very close to 0, esp for negative values.
# to bring out differences, we can take a fractional root to separate out values near 0

exp_pow<-3
plot_df <- plot_df %>% mutate(temp = abs(Usage_Rate)^(1/exp_pow)) %>%
    mutate(PowUsage = ifelse(Usage_Rate<0, -temp, temp)) %>%
    select(-temp)

p3 <- ggplot(plot_df, aes(x=Secondaries, y=Mains)) + 
    geom_tile(aes(fill=PowUsage), colour='grey50') +
    scale_fill_gradient2(low='blue', high='red') +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) # slanted

ggsave(file='anther_s1_cusage_hmap03.png', width=12, height=8, dpi=100, plot=p3)





#### testing area
# plot showing similarity between mains
tim<-dist(use_mat, method='manhattan')
hc<-hclust(tim)
plot(hc)

# similarity between secondaries
# this doesn't make a ton of sense since these don't necessarily add to a similar value - general high use can cluster with general high use even if proportions are off
tom<-dist(t(use_mat), method='manhattan')
h2<-hclust(tom)
plot(h2)

# factor_key preserves column order for Secondaries
# must manually add factor levels (in order) for mains 
test_df<-as.data.frame(use_mat[hc$order, h2$order]) %>% mutate(Mains=rownames(use_mat)[hc$order]) %>% gather(key=Secondaries, value=Usage_Rate, -Mains, factor_key=TRUE) %>% mutate(Mains=factor(Mains, levels=rownames(use_mat)[hc$order]))

tplot<- ggplot(test_df, aes(x=Secondaries, y=Mains)) + 
    geom_tile(aes(fill=Usage_Rate), colour='grey50') +
    scale_fill_gradient2(low='white', high='black') +
    theme(legend.position='none') + 
    # theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) # perpendicular
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) # slanted

# try differences based on exp pow
pow_mat <- plot_df %>% select(-Usage_Rate) %>% spread(key=Secondaries, value=PowUsage) %>% select(-Mains)
rownames(pow_mat)<-rownames(use_mat)

pow_dist_main<-dist(pow_mat, method='manhattan')
pow_dist_secd<-dist(t(pow_mat), method='manhattan')
pow_ord_main<-hclust(pow_dist_main)$order
pow_ord_secd<-hclust(pow_dist_secd)$order

pow_df<-as.data.frame(pow_mat[pow_ord_main, pow_ord_secd]) %>%
    mutate(Mains=rownames(use_mat)[pow_ord_main]) %>%
    gather(key=Secondaries, value=PowUsage, -Mains, factor_key=TRUE) %>%
    mutate(Mains=factor(Mains, levels=rownames(use_mat)[pow_ord_main]))

pow_plot<-ggplot(pow_df, aes(x=Secondaries, y=Mains)) + 
    geom_tile(aes(fill=PowUsage), colour='grey50') +
    scale_fill_gradient2(low='blue', high='red') +
    theme(legend.position='none') + 
    # theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) # perpendicular
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) # slanted






