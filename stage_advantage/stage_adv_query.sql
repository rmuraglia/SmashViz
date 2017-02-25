-- stage_adv_query.sql

-- set a date cutoff for games to be considered in analysis. choose to focus on games after patch 1.1.6
set @GAMES_CUTOFF = '2016-05-20';

select p1.*, p2_id, p2_rating, p2_sigma, p2_charid, p2_bans
    from (
    select p1_left.*, p1_right.p1_bans
        from (
        select sets.id as set_id, 
            games.id as game_id, 
            sets.created_at,
            sets.match_count,
            games.game_number,
            games.stage_pick,
            games.final_result as game_winner,
            sets.results_finalized as set_winner,
            mp.player_id as p1_id,
            pls.rating as p1_rating,
            pls.rating_standard_deviation as p1_sigma,
            mgcs.character_id as p1_charid
            from match_games as games
            join ladder_matches as sets
            join match_players as mp
            join player_ladder_stats as pls
            join match_game_character_selections as mgcs
            on games.match_id = sets.id
            and games.match_id = mp.match_id
            and sets.season_id = pls.season_id
            and mp.player_id = pls.player_id
            and games.id = mgcs.match_game_id
            and mp.player_id = mgcs.player_id
            where sets.results_finalized in (1,2)
            and sets.ladder_id = 4
            and sets.season_id != 67
            and sets.type = 2
            and date(sets.created_at) >= @GAMES_CUTOFF
            and mp.team_number = 1
        ) as p1_left
    left join (
        select games.id as game_id,
            group_concat(gss.stage_id separator '-') as p1_bans
            from match_games as games
            join ladder_matches as sets
            join match_players as mp
            join game_stage_strikes as gss
            on games.match_id = sets.id
            and games.match_id = mp.match_id
            and games.id = gss.game_id
            and mp.player_id = gss.user_id
            where sets.results_finalized in (1,2)
            and sets.ladder_id = 4
            and sets.season_id != 67
            and sets.type = 2
            and date(sets.created_at) >= @GAMES_CUTOFF
            and mp.team_number = 1
            group by game_id
        ) as p1_right
    on p1_left.game_id = p1_right.game_id
    ) as p1
join (
    select p2_left.*, p2_right.p2_bans
        from (
        select games.id as game_id,
            mp.player_id as p2_id,
            pls.rating as p2_rating,
            pls.rating_standard_deviation as p2_sigma,
            mgcs.character_id as p2_charid
            from match_games as games
            join ladder_matches as sets
            join match_players as mp
            join player_ladder_stats as pls
            join match_game_character_selections as mgcs
            on games.match_id = sets.id
            and games.match_id = mp.match_id
            and sets.season_id = pls.season_id
            and mp.player_id = pls.player_id
            and games.id = mgcs.match_game_id
            and mp.player_id = mgcs.player_id
            where sets.results_finalized in (1,2)
            and sets.ladder_id = 4
            and sets.season_id != 67
            and sets.type = 2
            and date(sets.created_at) >= @GAMES_CUTOFF
            and mp.team_number = 2
        ) as p2_left
    left join (
        select games.id as game_id,
        group_concat(gss.stage_id separator '-') as p2_bans
        from match_games as games
            join ladder_matches as sets
            join match_players as mp
            join game_stage_strikes as gss
            on games.match_id = sets.id
            and games.match_id = mp.match_id
            and games.id = gss.game_id
            and mp.player_id = gss.user_id
            where sets.results_finalized in (1,2)
            and sets.ladder_id = 4
            and sets.season_id != 67
            and sets.type = 2
            and date(sets.created_at) >= @GAMES_CUTOFF
            and mp.team_number = 2
            group by game_id
        ) as p2_right
    on p2_left.game_id = p2_right.game_id
    ) as p2
on p1.game_id = p2.game_id
;
