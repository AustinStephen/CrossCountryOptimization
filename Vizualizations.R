## Visualizations
## Austin Stephen
##7/20/2020

install.packages(extrafont)
## packages used in graphics 
library(extrafont)

## reading in functions
source("Functions_cleaning_manipulating.R")

## loading in the font used for the graphics
font_import()
loadfonts(device = "win")

# Loading the datasets
NCAA.Mens.National.2019 <-  read.csv("./data/NCAA_mens_2019_individual.csv")
NCAA.Womens.National.2019 <-  read.csv("./data/NCAA_womens_2019_individual.csv")
NCAA.Womens.GreatLakes.2019 <-  read.csv("./data/NCAA_womens_2019_GreatLakes_individual.csv")
NCAA.Mens.GreatLakes.2019 <-  read.csv("./data/NCAA_mens_2019_GreatLakes.csv")

## Converting the time to seconds in all data sets
NCAA.Mens.National.2019$runner_time <- time.to.sec(NCAA.Mens.National.2019$runner_time)
NCAA.Womens.National.2019$runner_time <- time.to.sec(NCAA.Womens.National.2019$runner_time)
NCAA.Womens.GreatLakes.2019$runner_time <- time.to.sec(NCAA.Womens.GreatLakes.2019$runner_time)
NCAA.Mens.GreatLakes.2019$runner_time <- time.to.sec(NCAA.Mens.GreatLakes.2019$runner_time)

## Removing runner year abreviation so runners can be grouped by school
NCAA.Mens.National.2019$runner_school <- remove.year(NCAA.Mens.National.2019$runner_school)
NCAA.Womens.National.2019$runner_school <- remove.year(NCAA.Womens.National.2019$runner_school)
NCAA.Womens.GreatLakes.2019$runner_school <- remove.year(NCAA.Womens.GreatLakes.2019$runner_school)
NCAA.Mens.GreatLakes.2019$runner_school <- remove.year(NCAA.Mens.GreatLakes.2019$runner_school)

## Turns the factor runner_place to a numeric 
NCAA.Mens.National.2019$runner_place<- as.numeric(as.character(NCAA.Mens.National.2019$runner_place))
NCAA.Womens.National.2019$runner_place<- as.numeric(as.character(NCAA.Womens.National.2019$runner_place))
NCAA.Womens.GreatLakes.2019$runner_place<- as.numeric(as.character(NCAA.Womens.GreatLakes.2019$runner_place))
NCAA.Womens.GreatLakes.2019$runner_place<- as.numeric(as.character(NCAA.Womens.GreatLakes.2019$runner_place))

## Computes all team results 
team.results.Mens.GL <- calculate.team.results(NCAA.Mens.GreatLakes.2019)
team.results.Mens.Nat <- calculate.team.results(NCAA.Mens.National.2019)
team.results.Womens.GL <- calculate.team.results(NCAA.Womens.GreatLakes.2019)
team.results.Womens.Nat <- calculate.team.results(NCAA.Womens.National.2019)


## Figures

# Men's National Meet Place as a Function of Time
# Figure 1
ggplot(data=NCAA.Mens.National.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .5)+
  theme_classic()+
  ylim(0,250)+
  labs(title= "Men's National Meet Place as a Function of Time",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"), 
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=30, family="times"))

# Men's National Meet Place as a Function of Time w/ regression line
# Figure 2
ggplot(data=NCAA.Mens.National.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .5)+
  theme_classic()+
  geom_smooth(method='lm',formula=y~x, color="darkred", se=FALSE, size=1.25)+
  ylim(0,250)+
  labs(title= "Men's National Meet Place as a Function of Time",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"),
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=30, family="times"))

# Women's national meet 2019 place as a function of time
# Figure 3
ggplot(data=NCAA.Womens.National.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .5)+
  theme_classic()+
  ylim(0,250)+
  labs(title= "Women's National Meet",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"), 
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=30, family="times"))

# Women's great lakes regional meet 2019 place as a function of time
# Figure 3
ggplot(data=NCAA.Womens.GreatLakes.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .5)+
  theme_classic()+
  ylim(0,250)+
  labs(title= "Women's Great Lakes Meet",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"), 
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=30, family="times"))

# Men's Great Lakes regional meet 2019 place asa function of time
# Figure 3
ggplot(data=NCAA.Mens.GreatLakes.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .5)+
  theme_classic()+
  ylim(0,250)+
  labs(title= "Men's Great Lakes Meet",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"), 
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=30, family="times"))


# Men's national meet place as a function of time Virginia Tech highligheted
# Figure 4


NCAA.Mens.National.2019.VT <- filter(NCAA.Mens.National.2019,runner_school=="Virginia Tech")

ggplot(data=NCAA.Mens.National.2019, aes(x=runner_time, y=runner_place)) +
  geom_point(alpha = .05)+
  geom_point(data=NCAA.Mens.National.2019.VT[-c(6,7),], aes(x=runner_time, y=runner_place), color="darkorange1",size=4.0)+
  geom_point(data=NCAA.Mens.National.2019.VT[-c(1:5),], aes(x=runner_time, y=runner_place), color="darkorange1",size=2.5)+
  theme_classic()+
  labs(title="Virginia Tech Runner Distribution",x="Time (sec)",y="Place")+
  theme(axis.text=element_text(size=20, family="times"),
        axis.title=element_text(size=22, family="times"),
        plot.title=element_text(size=28, family="times"))

# Computations for Virgina Tech runner selection 

first.runner <- improve(1,"Virginia Tech",30,NCAA.Mens.National.2019)

results.first.runner <- calculate.team.results(first.runner)

fourth.runner <- improve(4,"Virginia Tech",30,NCAA.Mens.National.2019)

results.fourth.runner <-  calculate.team.results(fourth.runner)






