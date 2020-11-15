## Functions for cleaning and manipulating results data
## Austin Stephen 
## 7/20/2020

## packages use to build functions and document 
install.packages("roxygen2")
library(tidyverse)
library(roxygen2)


#' Convert min:sec.milisec time format to seconds.
#' 
#' An input of time in min:sec.milisec output a total of seconds 
#' 
#' @param time this should be a factor or string in the format min:sec:milsec
#' 
#' @retrun the output is a numeric time value
#' 
#' @examples 
#' time <- 30:15.5
#' time.sec <- time.to.sec(time)
#' print(time.sec)
#' 1815.5

time.to.sec <- function(time){
  min <- substr(time,start =1, stop=2)
  sec <- substr(time, start =4, stop=5)
  hund <- substr(time, start=6, stop=7)
  min <- as.numeric(min)
  sec <- as.numeric(sec)
  hund <- as.numeric(hund)
  total <- (min * 60)+(sec)+hund
  return(total)
}

#' Remove athlete year from school name.
#' 
#' Omits the last 5 characters of string which removes the athletes year from the
#' school they compete under. This enables the athletes to be sorted by school 
#' 
#' @param school this should be a factor or string in the format with the students 
#'  year on the end.
#'  
#' @retrun the output is a string without the athletes year
#' 
#' @examples 
#' school.origional <- "Harvard {SR}"
#' school.new <- remove.year(school.original)
#' print(school.new)
#' "Harvard"
#' 
remove.year <- function(school){
  school.name <- substr(school, start=1, stop=(nchar(as.character(school))-5))
  return(school.name)
}

#' Takes in meet results outputs df of schools, team place and team scores.
#' 
#' Produces the reuslts of a meet from a list of the indiviudal results including: team place,
#' team score and school name.
#' 
#' @param results 2d df with columns titled runner_school and runner_points
#'  
#' @return the output is an ordered 2d df that includes the school and their team score
#' 
#' @examples 
#' meet.results <- calculate.team.results(meet.data)
#' 

calculate.team.results <- function(results){
  team.list <- unique(results$runner_school)
  team.scores <- data.frame(score=integer(),school=factor(),place=integer())
  names(team.scores) <- c("score","school","place")
  for(team in team.list){
    team.results <- filter(results, runner_school == team)
    team.results<- team.results[-c(6:7),]
    team.total <- sum(team.results$runner_points)
    team.scores <- team.scores %>% add_row(score = team.total,school=team,place=1)
  }
  team.scores <- na.omit(team.scores)
  numb.of.teams <- nrow(team.scores)
  team.scores <- team.scores[order(team.scores$score),]
  team.scores$place <- c(1:numb.of.teams)
  return(team.scores) 
}

#' Applies a time imporvement to a specific runner on a team and returns updated meet results
#' 
#' Takes in a runners posistion on their team, their team name, the ammount of time they improve by, 
#'   and the current meet results. Output is the same meet results recalculated with the runners 
#'   time improvement factored in and rescored   
#' 
#' @param runner an integer value 1-7 that reprsents which place they took on thier team
#' 
#' @param school a string of the school name
#' 
#' @param time a numeric value of how much to decrease the runners time by
#' 
#' @param meet.results the results of the meet to modify and return
#'  
#' @return the meet.results input with the runners new postion and score
#' 
#' @examples 
#' meet.results <- calculate.team.results(3,"Alabama",15.00,meet.results)
#'

improve <- function(runner,school,time,meet.results){
  non.scored.runners <- filter(meet.results,is.na(runner_points))
  meet.results <- na.omit(meet.results)
  team <- filter(meet.results, meet.results$runner_school == school)
  athlete <- team[runner,] 
  athlete$runner_time <- athlete$runner_time-time
  meet.results <- meet.results[meet.results$runner_name != athlete$runner_name,]
  meet.results <- meet.results %>% add_row(runner_name=athlete$runner_name, runner_time = athlete$runner_time, runner_school= athlete$runner_school)
  meet.results <- meet.results[order(meet.results$runner_time),]
  
  total.runners <- nrow(meet.results)
  meet.results$runner_points <- c(1:total.runners)
  
  meet.results <- rbind(non.scored.runners,meet.results)
  total.runners <- nrow(meet.results)
  meet.results <- meet.results[order(meet.results$runner_time),]
  meet.results$runner_place <- c(1:total.runners)
  return(meet.results)
}

