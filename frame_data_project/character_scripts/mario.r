# mario.r

library(jsonlite)
library(ggplot2)
library(tidyverse)

# fetch information from KH API
moves_url <- 'http://api.kuroganehammer.com/api/Characters/name/Mario/detailedmoves'
moves_raw <- fromJSON(moves_url)
moves_all <- jsonlite::flatten(moves_raw)
moves_df <- select(moves_all, moveName, FAF=firstActionableFrame, h1=hitbox.hitbox1, h2=hitbox.hitbox2, h3=hitbox.hitbox3, h4=hitbox.hitbox4, h5=hitbox.hitbox5, h6=hitbox.hitbox6, notes=hitbox.notes, ac1=autocancel.cancel1, ac2=autocancel.cancel2) %>%
    filter(!grepl('throw', moveName))

# get maximum FAF for plotting purposes
frame_max <- max(as.numeric(moves_df$FAF), na.rm=TRUE)

# get parsed move list, so we can combine things like 'Dash Attack' and 'Dash Attack (Late)'
parse_move <- function(x) {
    strsplit(x, split='[\\(\\)]') %>% unlist(.) %>% trimws(.) %>% ifelse(is.character(.), ., .[1])
}

unique_moves <- sapply(moves_df$moveName, parse_move) %>% unique(.)
num_unique_moves <- length(unique_moves)

# create frame data matrix: one row per move, one column per frame (plus one more for move name)
all_frames <- as.data.frame(matrix(NA, nrow=num_unique_moves, ncol=frame_max))
colnames(all_frames) <- c(1:frame_max)
rownames(all_frames) <- unique_moves

all_attribs <- all_frames # also make a frame for special attributes (invincibility, reflect, )

## go through returned move list, populating frame table
# initialize some variables that will be used to detect/react to a new unique move
get_active_frames <- function(x) {
    f1 <- one_hitbox_frames(x$h1s, x$h1e)
    f2 <- one_hitbox_frames(x$h2s, x$h2e)
    f3 <- one_hitbox_frames(x$h3s, x$h3e)
    f4 <- one_hitbox_frames(x$h4s, x$h4e)
    f5 <- one_hitbox_frames(x$h5s, x$h5e)
    f6 <- one_hitbox_frames(x$h6s, x$h6e)
    f_all <- c(f1, f2, f3, f4, f5, f6)
    f_out <- f_all[which(!is.na(f_all))]
    return(f_out)
}

one_hitbox_frames <- function(start, end) {
    if (nchar(start)==0) { return(NA)
    } else if (is.na(end)) { return(as.numeric(start))
    } else { return(c(start:end)) }
}

get_armor_frames <- function(x) {
    armor_vec <- strsplit(x, split='[[:punct:]]') %>% unlist(.) %>% as.numeric(.)
    a_frames <- armor_vec[which(!is.na(armor_vec))]
    return(a_frames)
}

get_ac_frame <- function(x) {
    strsplit(as.character(x), split='>') %>% unlist(.) %>% as.numeric(.)
}

# first pass through to place active hitboxes
for (i in 1:nrow(moves_df)) {

    # grab one move's info
    move_detail <- moves_df[i,]

    # skip landing hitboxes
    if (grepl('(Landing)', move_detail$moveName)) { next } 

    # get trimmed name
    parsed_name <- parse_move(move_detail$moveName)
    if (parsed_name=='Cape') { next } # handle cape separately
    if (parsed_name=='Fireball') { next } # handle fireball separately
    if (parsed_name=='F.L.U.D.D') { next }  # handle fludd separately

    # get active hitbox info
    hitbox_detail <- move_detail %>% separate(h1, into=c('h1s', 'h1e'), sep='-') %>%
        separate(h2, into=c('h2s', 'h2e'), sep='-') %>%
        separate(h3, into=c('h3s', 'h3e'), sep='-') %>%
        separate(h4, into=c('h4s', 'h4e'), sep='-') %>%
        separate(h5, into=c('h5s', 'h5e'), sep='-') %>%
        separate(h6, into=c('h6s', 'h6e'), sep='-') 
    active_frames <- get_active_frames(hitbox_detail)

    # fill in frames df with these hitboxes
    if (grepl('(Early)', move_detail$moveName) | grepl('(Late)', move_detail$moveName)) {
        # if early or late hitbox, assign type 4
        all_frames[parsed_name, active_frames] <- 4 
    } else {
        # otherwise, assume normal hitbox (type 2)
        all_frames[parsed_name, active_frames] <- 2
    }

    # check if move has intangibility -- if it does, add attribute to all_attribs, type 1
    if (grepl('Intangible', move_detail$notes)) {
        armor_range <- get_armor_frames(move_detail$notes)
        armor_frames <- c(armor_range[1]: armor_range[2])
        all_attribs[parsed_name, armor_frames] <- 1
    }

    # check for autocancel frames -- first autocancel frame = type 2
    ac_bool <- grepl('>', move_detail)
    if (any(ac_bool)) {
        ac_frame <- get_ac_frame(move_detail[ac_bool])
        all_attribs[parsed_name, ac_frame] <- 2
    }
}

# second pass through to place recovery frames
for (i in 1:nrow(moves_df)) {

    move_detail <- moves_df[i,]

    # if FAF isn't NA, then fill in from last active frame until FAF
    if (!is.na(move_detail$FAF)) {
        parsed_name <- parse_move(move_detail$moveName)
        if (parsed_name=='Cape') { next } # handle cape separately
        if (parsed_name=='Fireball') { next } # handle fireball separately
        if (parsed_name=='F.L.U.D.D') { next }  # handle fludd separately
        last_active <- which(!is.na(all_frames[parsed_name,])) %>% max(.)
        recovery_frames <- c((last_active+1) : (as.numeric(move_detail$FAF) -1))
        all_frames[parsed_name, recovery_frames] <- 3
    }
}



###################
plot_df1 <- all_frames %>% mutate(MoveNames=rownames(.)) %>% gather(key=Frame, value=Type, -MoveNames) %>% mutate(Frame=as.integer(Frame)) %>% mutate(Type=as.integer(Type)) # do as factor for Type in actual plot for discrete types
plot_df2 <- all_attribs %>% mutate(MoveNames=rownames(.)) %>% gather(key=Frame, value=Type, -MoveNames) %>% mutate(Frame=as.integer(Frame)) %>% mutate(Type=as.factor(Type))

ggplot(plot_df1, aes(x=Frame, y=MoveNames)) + geom_tile(aes(fill=Type), colour='grey50') + scale_x_continuous(position='top') + geom_point(data=plot_df2, aes(shape=Type))


# for projectiles just note when it becomes active
