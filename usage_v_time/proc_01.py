import numpy as np
import pandas as pd
import datetime as dt
import mysql.connector

# open connection to database
cnx = mysql.connector.connect(database='anthers_02_02_2016', option_files='/Users/rmuraglia/.my.cnf')
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

# initiate empty objects to hold counts for all time and week
week_start = dt.date(2014, 10, 15)
all_counts = pd.DataFrame(index = char_strings)
week_counts = pd.Series(0, index = char_strings, name = week_start)

# loop through each match record to count character participation
with open('usage_01.csv') as f :
    header = f.readline() # skip first line
    for line in f :
        game_info = line.split(',')
        game_date = dt.datetime.strptime(game_info[3].split()[0], '%Y-%m-%d').date()
        game_char1 = int(game_info[8])
        game_char2 = int(game_info[9])
        while not week_start <= game_date < week_start + dt.timedelta(days=7) :
            all_counts = pd.concat([all_counts, week_counts], axis=1)
            week_start = week_start + dt.timedelta(days=7)
            week_counts = pd.Series(0, index = char_strings, name = week_start)
        week_counts[char_dict[game_char1]] = week_counts[char_dict[game_char1]] + 1
        week_counts[char_dict[game_char2]] = week_counts[char_dict[game_char2]] + 1

# add last set of records to table
all_counts = pd.concat([all_counts, week_counts], axis=1) 

# save results to file
all_counts.to_csv('char_use_counts.csv')