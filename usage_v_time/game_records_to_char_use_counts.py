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

# close connection to db
cur.close()
cnx.close()

# initiate empty objects to hold counts for all time and week
week_start = dt.date(2014, 10, 15)
all_counts = pd.DataFrame(index = char_strings)
week_counts = pd.Series(0, index = char_strings, name = week_start)
num_day_skip = 7

# generate game_records table in SQL
# query: select ladder_matches.id as set_id, match_games.id as game_id, season_id, ladder_matches.created_at, match_count, game_number, search_user_id, reply_user_id, search_user_character_unused, reply_user_character_unused, stage_pick, final_result as game_result, results_finalized as set_result from ladder_matches join match_games on ladder_matches.id = match_games.match_id where results_finalized in (1,2) and ladder_id in (3,4) order by ladder_matches.created_at; 
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

# loop through each match record to count character participation
with open('anthers_12_21_2016_wiiu3ds_game_records_pulled_1-1-17_9-32 PM.csv') as f :
    header = f.readline() # skip first line
    for line in f :
        game_info = line.split(',')
        # skip invalid games
        pass_vec = [game_info[3]=='NULL', len(game_info[9].split('-')) != 2]
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
all_counts.to_csv('char_use_counts.csv', index_label='Dates')
