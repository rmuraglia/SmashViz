expect 1040964 results from
select ladder_matches.id as set_id, match_games.id as game_id, season_id, created_at, match_count, game_number, search_user_id, reply_user_id, search_user_character, reply_user_character, stage_pick, final_result as game_result, results_finalized as set_result from ladder_matches join match_games on ladder_matches.id = match_games.match_id where ladder_id in (3, 4) and results_finalized in (1, 2) order by created_at;
exported as usage_01.csv
