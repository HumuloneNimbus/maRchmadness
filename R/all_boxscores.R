#Uses ncaahoopR to retrieve boxscores for each team in a vector, then condenses them into one row per game, adding that row to a dataframe for that team. These dataframes are combined into a list for the given season.

#' @export
all_boxscores <- function(team_list, season){
  
  teams <- team_list
  boxscores <- list()
  drops <- c(2,3,19)
  
  #Get Season stats for all teams in 'teams' vector
  for (i in teams)
  {
    games <- get_game_ids(i, season = season)
    
    #Initialize list of games for this team, to be converted into a dataframe later
    game_list <- list()
    team_name <- str_replace(i, " ", "_")
    #Gets boxscore, condenses season to one dataframe, skipping any games that produce errors
    for(game in games)
    {
      #Initiallizing skipping condition if error is encountered
      skip_game <- FALSE
      if(skip_game){next}
      
      #Gets the boxscore of each game, combining into a single dataframe with one row per game
      tryCatch({
        
        #Get boxscore for game
        both_teams <- get_boxscore(game)
        
        #Extract opponent name for use later
        opponent <- names(both_teams[!names(both_teams) %in% i])
        
        #Extract boxscore of team being examined, then condenses indvidual player stats into  TEAM stats
        team_box <- both_teams[[i]][,-drops] %>% filter(player == "TEAM") %>% mutate(game_id = as.character(game), Opp = opponent) %>% subset(select = -player)
        
        #Repeats above process for opponent
        opponent_box <- both_teams[[opponent]][,-drops]
        colnames(opponent_box) <- paste0("Opp.", colnames(opponent_box))
        opponent_box <- opponent_box %>% filter(Opp.player == "TEAM") %>% subset(select = -Opp.player)
        
        #Adds this game to the list of games for this team
        game_list[[paste0(opponent, "_", game)]] <- cbind.data.frame(team_box, opponent_box)
      }, error = function(e){skip_game <<- TRUE}
      )
      if(skip_game){next}
    }
    
    #Assigns this teams game list to the boxscores list, then converts game_list into a dataframe for each team
    boxscores[[team_name]] <- assign(team_name, bind_rows(game_list))
    rm(list = team_name)
  }
  return(boxscores)
  rm(game)
  rm(games)
  rm(i)
  rm(opponent)
  rm(skip_game)
  rm(both_teams)
  rm(opponent_box)
  rm(team_box)
  rm(game_list)
  rm(team_name)
}

