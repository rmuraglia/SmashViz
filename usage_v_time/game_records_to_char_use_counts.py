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

game_records_filename = 'anthers_12_21_2016_wiiu3ds_game_records_pulled_1-1-17_9-32 PM.csv'

# count character participation

def gen_pass_vec(game_info, ladder_id, rank_type) :
    # if any of the 'pass' variables are True, then we skip the record

    # improper date
    pass_date = game_info[3] == 'NULL'

    # improper number of participating characters
    pass_chars = len(game_info[9].split('-')) != 2

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

    return [pass_date, pass_chars, pass_ladder, pass_type]

def char_counts(ladder_id, rank_type) :
    """
    produce a table for character usage counts on a weekly basis
    ladder_id variable takes values { 'all', 'wiiu', '3ds' } for game based filtering
    rank_type takes values { 'all', ranked', 'unranked' } for match type filtering
    """

    if ladder_id not in ['all', 'wiiu', '3ds'] or rank_type not in ['all', 'ranked', 'unranked'] :
        print "Please enter valid values for ladder_id and rank_type."
        return None

    # initiate empty objects to hold counts for all time and week
    week_start = dt.date(2014, 10, 15)
    all_counts = pd.DataFrame(index = char_strings)
    week_counts = pd.Series(0, index = char_strings, name = week_start)
    num_day_skip = 7

    # loop through each match record to count character participation
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
            while not week_start <= game_date < week_start + dt.timedelta(days=num_day_skip) :
                all_counts = pd.concat([all_counts, week_counts], axis=1)
                week_start = week_start + dt.timedelta(days=num_day_skip)
                week_counts = pd.Series(0, index = char_strings, name = week_start)
            week_counts[char_dict[game_char1]] = week_counts[char_dict[game_char1]] + 1
            week_counts[char_dict[game_char2]] = week_counts[char_dict[game_char2]] + 1

    # add last set of records to table 
    all_counts = pd.concat([all_counts, week_counts], axis=1)

    # save results to file
    all_counts = all_counts.transpose()
    outfilename = 'char_use_counts_' + ladder_id + '_' + rank_type + '.csv'
    all_counts.to_csv(outfilename, index_label='Dates')
    return all_counts

counts_df = char_counts('all', 'all')

