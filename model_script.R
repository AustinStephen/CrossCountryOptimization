## Contains scripts that are used to make recommendations for teams
## Austin Stephen
## 6/23/2020

#' Finds the runner closest scoring runner to the median of the field and returns their name
#' 
#' An input of meet results and the school of interest
#' 
#' @param meet.results should be a df with the meet results, must have a column named runner.time
#' @param school should be the name of school of interest
#' 
#' @retrun the output is a numeric time value
#' 
#' @examples 
#' school <- Harvard
#' runner.recommend <- runner_recommend(meet.results, school)
#' print(runner.recommend)
#' James Elliot

runner_recomend <- function(meet.results, school)
{
  median_time <- median(meet.results$runner_time, TRUE)
  team <- filter(meet.results, meet.results$runner_school == school)
  team <- team %>% mutate(off_median= abs(team$runner_time - median_time))
  team <- team[order(team$off_median),]
  #team <- filter(team, team$off_median == min(team$off_median, na.rm=T))
  return(team$runner_name)
}



