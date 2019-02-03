library(tidyverse)

#this code calculates Elo scores for test cricket
#the starting dataset is the results downloaded in script 1.

testData <- read_csv("testData.csv")

#two records have "tied" instead of "draw"
#technically different - but not from a win/lost perspective
#so replace for now

#also delete any ongoing games

testData %>% 
  filter(Winner!="-") %>%
  mutate(Winner_Type=if_else(Winner_Type=="Check","Draw",Winner_Type)) -> testData

#Functions used in Elo calculation:

#calculation for the expected win rate
#this is a probability of a win
expectedWin <- function(eloDif){
  1/(1+10^(eloDif/400))
}

#the elo update formula
# k is a parameter - somewhere in the 10-60 region depending on the sport 
# oldElo - previous value
# result - 1 = win, 0 = loss, 0.5 is draw
# expResult = expected result from expectedWin formula

updateElo <- function(oldElo,result,expResult,k=30) {
  oldElo + (k*(result-expResult))
}


# This function takes the dataset of wins / losses 
# and adds columns representing Elo scores and corresponding win probabilities for each side

# Currently uses an explicity loop through rows in the dataset
# Feels that there is probably a better / faster way to do this



calculateElo <- function(data,firstYear,k) {
 
  #filter dataset basaed on starting year passed as a parameter
  data %>% 
    mutate(year=lubridate::year(mDate)) %>%
    filter(year>=firstYear) %>%
    as.data.frame()-> temp
  
  #extract a list of teams from the dataset
  teams <- union(temp$`Team 1`,temp$`Team 2`)
  
  #initially see the elo of each team to 1000
  elo<- rep(1000,length(teams))
  #label the elo values by country
  names(elo) <- teams
  #add the initial elo scores to every row in the dataset
  temp[,teams] <- elo
  
  #1. Do vectorised computations:
  # these are all the calculations which can be done without knowing Elo scores
  # these are done for all games / matches similtaneously
  
  #this extracts the position of the column containing the elo score of each team in the game
  temp[,"col1"] <- match(temp$`Team 1`,names(temp))
  temp[,"col2"] <- match(temp$`Team 2`,names(temp))
  
  #This gives a score to each of the teams based on the result of the game
  temp[,"r1"] <- case_when(temp$`Team 1`==temp$Winner ~ 1,
                           temp$`Team 2`==temp$Winner ~ 0,
                           TRUE ~ 0.5)
  
  temp[,"r2"] <- case_when(temp$`Team 1`==temp$Winner ~ 0,
                           temp$`Team 2`==temp$Winner ~ 1,
                           TRUE ~ 0.5)
  
  #Then do row specific calcs - for row 1 just do in place
  
  
  
 
  for (row in 1:dim(temp)[1]){
    
    #if not the first row in the dataset, need to carry down previous elo values
    if(row>1){
      #first set the elo value in this row to the same value as the previous row
      temp[row,teams] <- temp[row-1,teams]
    }
    #This extracts the Elo of the two playing teams and puts into columns elo1/elo2
    temp[row,"elo1"] <-as.numeric(temp[row,match(temp$`Team 1`[row],names(temp))])
    temp[row,"elo2"] <-as.numeric(temp[row,match(temp$`Team 2`[row],names(temp))])
    
    #This calculates the elo gap between the two teams before the game happened
    temp[row,"dif1"] <- temp[row,"elo2"]-temp[row,"elo1"]
    temp[row,"dif2"] <- temp[row,"elo1"] - temp[row,"elo2"]
    
    #Calculate each teams chance of winning before the game started based on Elo
    temp[row,"e1"] <- expectedWin(temp[row,"dif1"])
    temp[row,"e2"] <- expectedWin(temp[row,"dif2"])
    
    #Update each teams elo based on the result of the game and pre game probs
    #Save this result in "newElo1"/"newElo2"
    temp[row,"newElo1"] <- updateElo(temp[row,"elo1"],temp[row,"r1"],temp[row,"e1"],k=k)
    temp[row,"newElo2"] <- updateElo(temp[row,"elo2"],temp[row,"r2"],temp[row,"e2"],k=k)
    
    #save the new elo values in the columns which hold current Elos for all teams
    temp[row,temp[row,"col1"]] <- temp[row,"newElo1"]
    temp[row,temp[row,"col2"]] <- temp[row,"newElo2"]
  }
  
  #save the parameters passed to the function as columns in the output dataset:
  temp$k <- k
  temp$firstYear <- firstYear
  return(temp)
}
  

#run a number of tests to calibrate

#use England as the benchmark to chart as play a lot of Cricket (and always have)

ks <- c(15,30,45,60)
years <- c(1946,1960,1980)

params <- expand.grid(ks=ks,years=years)

outputs <- map2_dfr(params$years,params$ks,~calculateElo(testData,.x,.y))

#extract england / group by year

outputs %>%
  select(England, mDate, year, firstYear,k) %>%
  group_by(firstYear,k,year) %>% #keep last elo in the year
  arrange(firstYear,k,year,mDate) %>% 
  filter(n()==1:n()) %>%
  filter(k==30) %>%
  ggplot(aes(x=year,y=England,colour=as.factor(firstYear))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Elos calculated from different start points quickly converge",
       subtitle="Therefore, no reason not to use all data from 1946")

ggsave("calibration1.png",dpi=72,units="in",width=700/72,height=700/72)


#No reason not to use all years

outputs %>%
  select(England, mDate, year, firstYear,k) %>%
  group_by(firstYear,k,year) %>% #keep last elo in the year
  arrange(firstYear,k,year,mDate) %>% 
  filter(n()==1:n()) %>%
  filter(firstYear==1946) %>%
  ggplot(aes(x=year,y=England,colour=as.factor(k))) +
  geom_line() +
  theme_minimal() +
  labs(title="A k value of 30 is a nice balance of changing a lot vs not changing much")

ggsave("calibration2.png",dpi=72,units="in",width=700/72,height=700/72)

#30 feels ok - a good balance

write_csv(outputs,"cricket_elos.csv")
