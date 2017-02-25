# fdp_common.r

# functions to deal with regular hitboxes for the frame data project

# to do: change early and late in get active frames to be a generic string that is passed like the blacklist

###############
# CORE FUNCTIONS
###############

import_from_kh <- function(URL) {
    # use the KH API to access a character's detailed moves (e.g.: http://api.kuroganehammer.com/api/Characters/name/Mario/detailedmoves)
    # select out just the relevant fields and rename/process them to something convenient
    # remove the moves with 'throw' in the name - those don't have relevant hitboxes (we are about the grabs)
    raw <- fromJSON(URL)
    flat <- jsonlite::flatten(raw)
    df_out <- flat %>% select(moveName, FAF=firstActionableFrame, 
        h1=hitbox.hitbox1, h2=hitbox.hitbox2, h3=hitbox.hitbox3, 
        h4=hitbox.hitbox4, h5=hitbox.hitbox5, h6=hitbox.hitbox6, 
        notes=hitbox.notes, ac1=autocancel.cancel1, ac2=autocancel.cancel2) 
    return(df_out)
}

get_active_frames <- function(kh_data, blacklist, frame_df) {
    # populate the frame_df with active hitboxes
    for (i in 1:nrow(kh_data)) {

        # grab one set of move info from the raw KH data
        move_detail <- kh_data[i,]

        # if the moveName matches something on the blacklist (requires manual processing), skip it
        if (grepl(blacklist, move_detail$moveName)) { next }

        # get the trimmed name
        stemmed_name <- stem_move_names(move_detail$moveName)

        # get active hitbox info
        hitbox_detail <- split_hitboxes(move_detail)
        active_frames <- all_active_frames(hitbox_detail)

        # fill in frame_df with these hitboxes
        if (grepl('Early', move_detail$moveName) | grepl('Late', move_detail$moveName)) {
            # if early or late, assign type 3
            frame_df[stemmed_name, active_frames] <- 3
        } else {
            # otherwise, assume normal hitbox (type 2)
            frame_df[stemmed_name, active_frames] <- 2
        }
    }
    return(frame_df)
}

get_recovery_frames <- function(kh_data, blacklist, frame_df) {
    # populate the frames df with recovery frames (last melee hitbox until FAF)
    for (i in 1:nrow(kh_data)) {
        move_detail <- kh_data[i,]
        if (grepl(blacklist, move_detail$moveName)) { next }
        
        if (!is.na(move_detail$FAF)) {

            # fill in FAF and recovery frames
            stemmed_name <- stem_move_names(move_detail$moveName)
            last_active <- which(!is.na(frame_df[stemmed_name,])) %>% max(.)
            recovery_frames <- c((last_active+1) : (as.numeric(move_detail$FAF) - 1))
            frame_df[stemmed_name, recovery_frames] <- 1
        }
    }
    return(frame_df)
}

get_inactive_frames <- function(frame_df, blacklist) {
    # populate the frames df with inactive frames (start up and gaps during melee hitboxes)
    for (i in 1:nrow(frame_df)) {
        if (grepl(blacklist, rownames(frame_df[i,]))) {  next }
        last_active <- which(!is.na(frame_df[i,])) %>% max(.)
        inactive_frames <- which(is.na(frame_df[i, 1:last_active]))
        frame_df[i, inactive_frames] <- 1
    }
    return(frame_df)
}

get_ac_frames <- function(kh_data, blacklist, attrib_df) {
    # populate the attributes df with autocancel frames
    for (i in 1:nrow(kh_data)) {
        move_detail <- kh_data[i,]
        if (grepl(blacklist, move_detail$moveName)) { next }
        stemmed_name <- stem_move_names(move_detail$moveName)

        # check if ac1 has any info
        if (grepl('-', move_detail$ac1) & nchar(move_detail$ac1)>1) {
            ac_frames <- early_autocancel(move_detail$ac1)
            attrib_df[stemmed_name, ac_frames] <- 2
        } else if (grepl('>', move_detail$ac1)) {
            ac_frame <- late_autocancel(move_detail$ac1)
            attrib_df[stemmed_name, ac_frame] <- 2
        }

        # check if ac2 has any info
        if (grepl('>', move_detail$ac2)) {
            ac_frame <- late_autocancel(move_detail$ac2)
            attrib_df[stemmed_name, ac_frame:frame_max] <- 2
        }
    }
    return(attrib_df)
}

#################
# HELPER FUNCTIONS
#################

stem_move_names <- function(full_name) {
    # given a moveName entry, parse the name down to core move name 
    # e.g.: combine 'Dash Attack' and 'Dash Attack (Late)'
    strsplit(full_name, split='[\\(\\)]') %>% unlist(.) %>% trimws(.) %>% ifelse(is.character(.), ., .[1])
}

split_hitboxes <- function(move_info) {
    # input: one line of move info with hitbox fields as is
    # output: one line of move info with hitbox fields split by '-' for parsing ranges
    move_info %>% 
        separate(h1, into=c('h1s', 'h1e'), sep='-') %>%
        separate(h2, into=c('h2s', 'h2e'), sep='-') %>%
        separate(h3, into=c('h3s', 'h3e'), sep='-') %>%
        separate(h4, into=c('h4s', 'h4e'), sep='-') %>%
        separate(h5, into=c('h5s', 'h5e'), sep='-') %>%
        separate(h6, into=c('h6s', 'h6e'), sep='-') 
}

one_hitbox_frames <- function(start, end) {
    if (nchar(start)==0) { 
        # no hitbox data
        return(NA) 
    } else if (is.na(end)) {
        # only single active frame
        return(as.numeric(start))
    } else {
        # return a range
        return(c(start:end))
    }
}

all_active_frames <- function(move_info) {
    # get the active frames for each hitbox, then combine into one set of active frames for the whole move
    f1 <- one_hitbox_frames(move_info$h1s, move_info$h1e)
    f2 <- one_hitbox_frames(move_info$h2s, move_info$h2e)
    f3 <- one_hitbox_frames(move_info$h3s, move_info$h3e)
    f4 <- one_hitbox_frames(move_info$h4s, move_info$h4e)
    f5 <- one_hitbox_frames(move_info$h5s, move_info$h5e)
    f6 <- one_hitbox_frames(move_info$h6s, move_info$h6e)
    f_all <- c(f1, f2, f3, f4, f5, f6)
    f_out <- f_all[which(!is.na(f_all))]
    return(f_out)
}

early_autocancel <- function(ac_info) {
    # if grepl('-', ac_info) it is an early autocancel frame
    frames <- strsplit(as.character(ac_info), split='-') %>% unlist(.) 
    return(frames[1]:frames[2])
}

late_autocancel <- function(ac_info) {
    # if grepl('>', ac_info) it is a late autocancel frame
    strsplit(as.character(ac_info), split='>') %>% unlist(.) %>% as.numeric(.)
}

