# game_records_to_unique_user_char_counts.py

"""
convert raw game records to per-week character usage counts
character usage determined as number of unique users character was used by in a week
optionally, can threshold users by 'significant usage' of a character (e.g. used in at least 20% of matches)
"""

import numpy as np
import pandas as pd 
import datetime as dt 
import mysql.connector

# open connection to database
cnx = mysql.connector.connect(database='anthers_12_21_2016', option_files='/Users/rmuraglia/.my.cnf')
cur = cnx.cursor()

# create dictionary to map character ID to character string
cur.execute('select id, name from characters where game_id in (3,4);')
all_characters = cur.fetchall()
char_dict = {}

for x in all_characters :
    char_dict[x[0]] = x[1]

cur.execute('select distinct name from characters where game_id in (3,4) order by name;')
char_strings = [x[0] for x in cur.fetchall()]
char_num = len(char_strings) # or select count(distinct name) from characters where game_id in (3,4);

# get list of seasons for each game for game-specific filtering
cur.execute('select id from game_seasons where ladder_id = 3;')
seasons_3ds = [x[0] for x in cur.fetchall()]
cur.execute('select id from game_seasons where ladder_id = 4;')
seasons_wiiu = [x[0] for x in cur.fetchall()]

# close connection to db
cur.close()
cnx.close()

# generate game_records table in SQL
"""
query:
select ladder_matches.id as set_id, match_games.id as game_id, season_id, ladder_matches.created_at, type, match_count, game_number, group_concat(match_game_character_selections.player_id order by match_game_character_selections.id separator '-') as player_ids, group_concat(distinct match_players.player_id) as team1_id, group_concat(match_game_character_selections.character_id order by match_game_character_selections.id separator '-') as character_ids, stage_pick, final_result as game_result, results_finalized as set_result 
from ladder_matches join match_games join match_game_character_selections join match_players 
on ladder_matches.id = match_games.match_id and match_games.id = match_game_character_selections.match_game_id and ladder_matches.id = match_players.match_id 
where results_finalized in (1,2) and ladder_id in (3,4) and team_number = 1 
group by game_id
order by ladder_matches.created_at;

export in sequel pro, or use `select into outfile` syntax
"""

game_records_filename = 'raw_game_records/anthers_12_21_2016_wiiu3ds_game_records_pulled_1-1-17_9-32 PM.csv'
# game_records_filename = 'raw_game_records/testmini.csv'


# shared helper function
def gen_pass_vec(game_info, ladder_id, rank_type) :
    # if any of the 'pass' variables are True, then we skip the record

    # improper date
    pass_date = game_info[3] == 'NULL'

    # improper number of participating characters
    pass_chars = len(game_info[9].split('-')) != 2

    # improper number of participating users
    pass_users = len(game_info[7].split('-')) != 2

    # improper season_id for the desired game
    if ladder_id == 'wiiu' :
        pass_ladder = int(game_info[2]) not in seasons_wiiu
    elif ladder_id == '3ds' :
        pass_ladder = int(game_info[2]) not in seasons_3ds
    else : 
        pass_ladder = False

    # improper type for ranked or unranked matches
    if rank_type == 'ranked' :
        pass_type = not int(game_info[4]) == 2
    elif rank_type == 'unranked' :
        pass_type = not int(game_info[4]) == 1
    else :
        pass_type = False

    return [pass_date, pass_chars, pass_users, pass_ladder, pass_type]

###
# generate counts based on number of unique users
###

def unique_user_counts(ladder_id, rank_type) :
    """
    produce a table for the number of unique users that used a character in a given week
    ladder_id = {'all', 'wiiu', '3ds'}
    rank_type = {'all', 'ranked', 'unranked'}
    """

    if ladder_id not in ['all', 'wiiu', '3ds'] or rank_type not in ['all', 'ranked', 'unranked'] :
        print "Please enter valid values for ladder_id and rank_type."
        return None

    # initiate empty objects to track time and hold all time character counts
    week_start = dt.date(2014, 10, 15)
    num_day_skip = 7
    all_counts = pd.DataFrame(index = char_strings)

    # initiate empty object to hold unique character users
    char_user_sets = gen_char_user_sets(char_strings)

    # loop through each match record
    with open(game_records_filename) as f :
        header = f.readline() # skip first line
        for line in f :
            game_info = line.split(',')
            pass_vec = gen_pass_vec(game_info, ladder_id, rank_type)
            if any(pass_vec) : continue
            game_date = dt.datetime.strptime(game_info[3].split()[0], '%Y-%m-%d').date()
            game_chars = game_info[9].split('-')
            game_char1 = int(game_chars[0])
            game_char2 = int(game_chars[1])
            game_users = game_info[7].split('-')
            game_user1 = int(game_users[0])
            game_user2 = int(game_users[1])

            # if the game did not take place during the 'current week', then tally results from previous week and keep on moving time forward until we're in the appropriate week for the game to start new counts
            while not week_start <= game_date < week_start + dt.timedelta(days=num_day_skip) :
                week_counts = tally_unique_users(char_user_sets, week_start)
                all_counts = pd.concat([all_counts, week_counts], axis=1)
                week_start = week_start + dt.timedelta(days = num_day_skip)
                char_user_sets = gen_char_user_sets(char_strings)

            # if it did take place during the 'current week', add users to the character's set
            char_user_sets[char_dict[game_char1]].add(game_user1)
            char_user_sets[char_dict[game_char2]].add(game_user2)

    # add last set of records to table
    week_counts = tally_unique_users(char_user_sets, week_start)
    all_counts = pd.concat([all_counts, week_counts], axis=1)

    # save results to file
    all_counts = all_counts.transpose()
    outfilename = 'parsed_char_counts/unique_user_counts_' + ladder_id + '_' + rank_type + '.csv'
    all_counts.to_csv(outfilename, index_label='Dates')
    return all_counts

def gen_char_user_sets(character_list) :
    X = {}
    for x in character_list :
        X[x] = set()
    return X

def tally_unique_users(char_user_sets, week_start) :
    week_counts = pd.Series(0, index = char_strings, name = week_start)
    for i in char_user_sets.keys() :
        week_counts[i] = len(char_user_sets[i])
    return week_counts

# be careful! next lines generate new tables
unique_all = unique_user_counts('all', 'ranked')
unique_wiiu = unique_user_counts('wiiu', 'ranked')


###
# generate counts based on unique number of SIGNIFICANT users
###

def signif_user_counts(ladder_id, rank_type, alpha) :
    """
    produce a table for the number of unique users that used a character in a given week
    ladder_id = {'all', 'wiiu', '3ds'}
    rank_type = {'all', 'ranked', 'unranked'}
    alpha = [0, 1] : fraction of games played by a user on a character to be considered significant usage of that character
    """

    if ladder_id not in ['all', 'wiiu', '3ds'] or rank_type not in ['all', 'ranked', 'unranked'] or not 0 <= alpha <= 1 :
        print "Please enter valid values for ladder_id, rank_type and alpha."
        return None

    # initiate empty objects to hold character counts per week and all time
    week_start = dt.date(2014, 10, 15)
    num_day_skip = 7
    all_counts = pd.DataFrame(index = char_strings)

    # initiate empty object to hold character use counts per user
    users_dict = {}

    # loop through each match record
    with open(game_records_filename) as f :
        header = f.readline() # skip first line
        for line in f :
            game_info = line.split(',')
            pass_vec = gen_pass_vec(game_info, ladder_id, rank_type)
            if any(pass_vec) : continue
            game_date = dt.datetime.strptime(game_info[3].split()[0], '%Y-%m-%d').date()
            game_chars = game_info[9].split('-')
            game_char1 = int(game_chars[0])
            game_char2 = int(game_chars[1])
            game_users = game_info[7].split('-')
            game_user1 = int(game_users[0])
            game_user2 = int(game_users[1])

            # if the game did not take place during the 'current week', then tally results from previous week and keep moving time forward until we're in the appropriate week for the game to start new counts
            while not week_start <= game_date < week_start + dt.timedelta(days=num_day_skip) :
                week_counts = tally_signif_users(users_dict, week_start, alpha)
                all_counts = pd.concat([all_counts, week_counts], axis=1)
                week_start = week_start + dt.timedelta(days = num_day_skip)
                users_dict = {}

            # if it did take place during the 'current week', increment that use count for the character for that user
            users_dict = increment_users_dict(users_dict, game_user1, game_char1)
            users_dict = increment_users_dict(users_dict, game_user2, game_char2)

    # add last set of records to table
    week_counts = tally_signif_users(users_dict, week_start, alpha)
    all_counts = pd.concat([all_counts, week_counts], axis=1)

    # save results to file
    all_counts = all_counts.transpose()
    outfilename = 'parsed_char_counts/signif' + str(int(alpha*100)) + '_user_counts_' + ladder_id + '_' + rank_type + '.csv'
    all_counts.to_csv(outfilename, index_label='Dates')
    return all_counts


def tally_signif_users(users_dict, week_start, alpha) :
    week_counts = pd.Series(0, index = char_strings, name=week_start)
    for i in users_dict.values() :
        # for each user, determine which characters were used beyond the signficance threshold 
        signif_chars = i.index[i/sum(i) >= alpha]

        for j in signif_chars :
            # for each significant character, increment their weekly user count by one
            week_counts[j] += 1
    return week_counts

def increment_users_dict(users_dict, game_user, game_char) :
    if game_user not in users_dict.keys() :
        users_dict[game_user] = pd.Series(0, index=char_strings)
    users_dict[game_user][char_dict[game_char]] += 1
    return users_dict


# be careful! next lines generate new tables
# signif10_all = signif_user_counts('all', 'ranked', 0.1)
signif20_all = signif_user_counts('all', 'ranked', 0.2)
signif30_all = signif_user_counts('all', 'ranked', 0.3)
# signif10_wiiu = signif_user_counts('wiiu', 'ranked', 0.1)
signif20_wiiu = signif_user_counts('wiiu', 'ranked', 0.2)
signif30_wiiu = signif_user_counts('wiiu', 'ranked', 0.3)

"""
notes and testing area

    # dict keyed by user where each entry is a series where the index are characters
    # can then iterate through dict, determining which characters to add count to with series.index[series>threshold]

tim = some dict
tom = pd.DataFrame(tim.items(), columns=['character', 'counts'])
carl = tom['counts']/sum(tom['counts'])
tom['character'][np.where(carl>=0.1)[0]]

set_id,game_id,season_id,created_at,type,match_count,game_number,player_ids,team1_id,character_ids,stage_pick,game_result,set_result
303562,94446,4,2014-10-15 15:45:06,2,3,1,8245-7366,8245,82-104,54,2,2
303562,94470,4,2014-10-15 15:45:06,2,3,2,8245-7366,8245,82-104,49,2,2
303659,94482,4,2014-10-15 16:02:04,2,3,1,2210-7366,2210,119-104,53,2,2
303659,94499,4,2014-10-15 16:02:04,2,3,2,2210-7366,2210,119-104,50,2,2
303665,94483,4,2014-10-15 16:03:08,2,3,1,8640-8245,8640,108-82,52,1,1
303665,94504,4,2014-10-15 16:03:08,2,3,2,8640-8245,8640,108-82,49,1,1
303740,94526,4,2014-10-15 16:25:12,2,3,1,7366-8640,7366,104-108,52,1,1
303740,94541,4,2014-10-15 16:25:12,2,3,2,7366-8640,7366,104-108,54,2,1
303740,94553,4,2014-10-15 16:25:12,2,3,3,7366-8640,7366,104-108,54,1,1
"""