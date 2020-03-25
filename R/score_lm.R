#Estimate score by opponent stats, then add resulting list to score_lm list

#' @export
score_lm <- function(boxscore_list){
  #Initialize linear models list
  lm_list <- list()
  teams <- names(boxscore_list)
  for (team in teams){
    df <- osu_psu_box_19_20[[team]]
    lm_list[[team]] <- lm(PTS ~ Opp.FGM + Opp.FGA + Opp.3PTM + Opp.3PTA + Opp.FTM + Opp.FTA + Opp.OREB + Opp.DREB + Opp.AST + Opp.STL + Opp.BLK + Opp.TO + Opp.PF, df)
  }
  #Clean up
  return(lm_list)
  rm(df)
  rm(team)
  rm(teams)
}
