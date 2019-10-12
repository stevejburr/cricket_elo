#code for analysis for second folowup blog post

# focussing on unexpected results / modelling

# but also looking at results by position in series (game 1 vs later)

library(tidyverse)
library(ggrepel)


#load previous data and filter to prefered k values

read_csv("cricket_elos.csv") %>%
  filter(k==30 & firstYear==1946) %>%
  select(-c(k,firstYear)) -> data


head(data)

#series defined based on Team 1/Team 2/mDate
#can't use only year as series might cross a year
#create a gapbetween matches in days
data %>%
  arrange(`Team 1`,`Team 2`, mDate) %>%
  group_by(`Team 1`,`Team 2`) %>%
  mutate(days_since_last=mDate-lag(mDate),
         days_since_last=if_else(!is.na(days_since_last),days_since_last,as.difftime(0,units="days")),
         days_since_last=as.numeric(days_since_last)) -> data

#create exploratory plot to understand best cut off to use
data %>% filter(days_since_last<200) %>%
  pull(days_since_last) %>% 
  density() %>% 
  plot()

abline(v=20)
abline(v=30)
abline(v=40)

#using a gap of 40 seems safe enough


data$series_id <- 2
for (row in (2:dim(data)[1])) {
  if(data[row,]$days_since_last ==0 || data[row,]$days_since_last >40){
    #if first ever game between teams or
    #more than 40 days since last played
    #then increment the series counter
    data[row,]$series_id=(data[row-1,]$series_id)+1
  }else{
    #otherwise, the series_id is the previous one
    data[row,]$series_id=data[row-1,]$series_id
  }
}

#identify game count by series id

data %>%
  group_by(`Team 1`,`Team 2`,series_id) %>%
  arrange(`Team 1`,`Team 2`,series_id, mDate) %>% 
  mutate(Match=1:n())->data

data %>%
  filter(year<2019) %>%
  mutate(`Home Advantage`=r1-e1,
         `Match Type`=if_else(Match==1,"First Tests","Subsequent Tests")) %>% 
  group_by(`Match Type`,year) %>%
  summarise(`Home Advantage`=mean(`Home Advantage`)) -> home_away_elo

png("home_advantage_elo_first.png",width=700,height=700,res=72,type="cairo-png")

ggplot(home_away_elo) +
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=0,yend=0.15),
               colour="grey80")+
  geom_smooth(aes(x=year,y=`Home Advantage`,colour=`Match Type`),
              se=FALSE,size=1.5) +
  scale_y_continuous("Home win rate advantage / % points",
                     labels=scales::percent_format(accuracy=1))+
  scale_x_continuous("",
                     breaks=seq(1960,2020,20))+
  scale_colour_discrete("Match type")+
  theme_minimal() +
  theme(panel.grid= element_blank(),
        text=element_text(colour="grey50",size = 20),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Home advantage is bigger for the first game in a test series",
       subtitle="The difference between first and subsequent tests has shrunk as the \ntotal effect has increased",
       caption="Trends are smoothed based on yearly averages using loess\nData source: ESPNcricinfo - Design and Analysis by @stevejburr")

dev.off()

#now find most unexpected results
#these are biggest absolute changes in Elo score


data %>%
  ungroup() %>%
  mutate(deltaElo=abs(elo1-newElo1)) %>%
  top_n(10,deltaElo) %>%
  mutate(Loser=if_else(`Team 1`==Winner,`Team 2`,`Team 1`)) %>%
  mutate(description=paste0(Winner," beat ",Loser, " in ",Ground, " - ",`Match Date`)) %>%
  select(description,deltaElo,Winner_Type) %>%
  mutate(description=as.factor(description),
         description=fct_reorder(description,deltaElo)) %>%
  ggplot(aes(x=description,y=deltaElo,fill=Winner_Type)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous("Elo score change")+
  scale_x_discrete("")+
  scale_fill_discrete("",labels=c("Away win","Home win"))+
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text=element_text(colour="grey50"),
        text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Top 10 unexpected test cricket match results since 1946",
       subtitle="Based on change in Elo score - not accounting for home advantage",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") -> p

#have to do manual fudge of title due to size of labels and alignment
#this extracts the table of alignments and overwrites the left value of title/subtitle
g <- ggplotGrob(p)
g$layout$l[g$layout$name%in%c("title","subtitle")] <- 4

png("unexpexted_text_results.png",width=700,height=700,res=72,type="cairo-png")
grid::grid.draw(g)
dev.off()
#note that most of the unexpected results are home wins...


#now calculate unexpected series results by dividing this delta by number of games in a series

data %>%
  ungroup() %>%
  mutate(deltaElo=abs(elo1-newElo1)) %>%
  group_by(`Team 1`,`Team 2`, series_id) %>%
  summarise(mDate=min(mDate),
            deltaElo=sum(deltaElo),
            count=n()) %>%
  mutate(avgDeltaElo=deltaElo/count,
         mDate=lubridate::year(mDate)) %>%
  ungroup() %>%
  filter(count>=3) %>%
  top_n(10,avgDeltaElo) %>%
  arrange(-avgDeltaElo) -> unexpected_series

#need to seperately compute a "year" for the series and the result (wins per side)
data %>%
  group_by(series_id) %>%
  mutate(t1Win=if_else(`Winner`==`Team 1`,1,0)) %>%
  summarise(t1Win=sum(t1Win)) -> t1Win

data %>%
  group_by(series_id) %>%
  mutate(t2Win=if_else(`Winner`==`Team 2`,1,0)) %>%
  summarise(t2Win=sum(t2Win)) -> t2Win

#combine these
unexpected_series %>%
  left_join(t1Win) %>%
  left_join(t2Win) %>%
  mutate(description=paste0(`Team 1`," vs ",`Team 2`,
                            " ",t1Win,"-",t2Win," - ",mDate),
         description=as.factor(description),
         description=fct_reorder(description,avgDeltaElo)) %>%
  ggplot(aes(x=description,y=avgDeltaElo))+
  geom_col(fill="grey70")+
  coord_flip() +
  scale_y_continuous("Avg Elo score change")+
  scale_x_discrete("")+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text=element_text(colour="grey50"),
        text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Top 10 unexpected test cricket series results since 1946 - 3 match+ series",
       subtitle="Based on change in Elo score - not accounting for home advantage",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") ->p

g <- ggplotGrob(p)
g$layout$l[g$layout$name%in%c("title","subtitle")] <- 4

png("unexpexted_text_series_results.png",width=700,height=700,res=72,type="cairo-png")
grid::grid.draw(g)
dev.off()


#now try to improve using a simple ordinal regression approach
#accounts for home advantage + draw %age
#https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
data %>%
  ungroup() %>%
  select(`Team 1`, `Team 2`, Winner_Type,dif1) %>%
  mutate(Winner_Type=factor(
    Winner_Type, levels=c("Away","Draw","Home"))
    ) -> data_mod

model<- MASS::polr(Winner_Type ~ dif1,data=data_mod, Hess = TRUE)

summary(model)

predictions <- predict(model,data_mod,type="probs")

data <- cbind(as.data.frame(data),as.data.frame(predictions))

#check for correlation with Elo approach
plot(data$e1,data$Home)

#look at gap - but this doesn't quite work due to net change in number of results
plot(data$mDate,data$Home-data$e1)

#if we "renormalise" (i.e. remove draws) then it's a fair comparison
plot(data$mDate,(data$Home/(data$Home+data$Away))-data$e1)

#make nicer ggplot version of these diagnostic plots
data %>%
  mutate(normHome=Home/(Home+Away),
         normAway=Away/(Home+Away)) %>%
  ggplot() +
  geom_point(aes(x=e1,y=normHome),colour="steelblue")+
  geom_point(aes(x=e2,y=normAway),colour="grey50") +
  annotate("text",x=0.3,y=0.6,label="Home team",colour="steelblue",size=8)+
  annotate("text",y=0.3,x=0.6,label="Away team",colour="grey50",size=8)+
  scale_x_continuous("Elo based win probability",
                     breaks=seq(0,1,0.1),
                     limits=c(0,1.01))+
  scale_y_continuous("Model based win probability",
                     breaks=seq(0,1,0.1),
                     limits=c(0,1.01))+
  theme_minimal() +
  theme(text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid=element_blank()) +
  labs(title="Elo win probabilites vs Ordinal logistic regress probabilities",
       subtitle="The model predicts result (Home win/lose/draw) using Elo gap.\nThe gap between the sets of lines v.roughly shows modelled home advantage.\nIn %point terms, this effect shrinks as the teams become more inbalanced.\nTo enable a fair comparison with the pure Elo approach draws are ignored here.",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") -> p
#this commentary is wrong / needs updating
png("model_vs_elo1.png",width=700,height=700,res=72,type="cairo-png")
p
dev.off()

data %>%
  mutate(normHome=Home/(Home+Away),
         HA=normHome-e1) %>%
  ggplot() +
  #geom_point(aes(x=mDate,y=HA),colour="steelblue")+
  geom_smooth(aes(x=mDate,y=HA),colour="steelblue",se=F)+
  theme_minimal() +
  theme(text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid=element_blank())

#still needs some tidying here ^^^


#nb direction of HA is not consistent with previous results
#but note this isn't being modelled explictly
#and not strictly correct to just compare elo to model, but good to sense check


#ACTUALLY CREATE A DATASET WHICH REVERSES EVERY PREDICTION TO CALCULATE HA
#create a second modelling dataset which swaps differences to represent if game happened elsewhere

data %>%
  ungroup() %>%
  select(`Team 1`, `Team 2`, Winner_Type,dif2) %>%
  rename(dif1="dif2") %>%
  mutate(Winner_Type=factor(
    Winner_Type, levels=c("Away","Draw","Home"))
  ) -> data_mod2


predictions2 <- predict(model,data_mod2,type="probs")
colnames(predictions2) <- c("Away_t","Draw_t","Home_t")
head(predictions2)

data <- cbind(as.data.frame(data),as.data.frame(predictions2))

data %>% 
  mutate(normHome=Home/(Home+Away),
         normAway=Away/(Home+Away),
         normHome_t=Home_t/(Home_t+Away_t),
         normAway_t=Away_t/(Home_t+Away_t)) %>%
  ggplot()+
  #geom_line(aes(x=-dif1,y=e1,group=1),colour="grey50") +
  geom_line(aes(x=-dif1,y=Home,group=1),colour="red") +
  geom_line(aes(x=-dif1,y=Away_t,group=1),colour="steelblue") +
  #geom_line(aes(x=-dif1,y=Draw, group=1),colour="green") +
  #geom_line(aes(x=-dif1,y=Draw_t,group=1),colour="grey") +
  scale_x_continuous("Home team Elo advantage",limits=c(-450,450),breaks=seq(-400,400,100)) +
  scale_y_continuous("Probability of winning") +
  labs(title="Modelled home advantage vs Elo score difference",
       subtitle="The blue line shows how likely the home team would have been to win if the game actually took place away")


#now going through + flagging the probability of each result happening:
data %>%
  mutate(resultProb=case_when(
    Winner_Type=="Home" ~ Home,
    Winner_Type=="Draw" ~ Draw,
    Winner_Type=="Away" ~ Away
  )) -> data

#this would give modelled unlikely results
data %>%
  top_n(10,-resultProb)


#get modelled unlikely series results
data %>% 
  group_by(`Team 1`,`Team 2`,series_id) %>%
  mutate(resultProb=cumsum(log10(resultProb)),
         resultProb=10^resultProb) %>%
  filter(Match==max(Match) & Match>=3) %>% 
  select(`Team 1`,`Team 2`,year,resultProb) %>%
  ungroup() %>%
  #top_n(100,-resultProb) %>%
  left_join(t1Win) %>%
  left_join(t2Win) %>%
  mutate(description=paste0(`Team 1`," vs ",`Team 2`,
                            " ",t1Win,"-",t2Win," - ",year)) %>%
  arrange(resultProb) %>% 
  mutate(rank=1:n()) %>% View()

#not really properly validated these models here, or thought about alternatives
#but feels that it's doing about the right thing
#challenge is that draw gets less likely with time, but is middle point in scale
#definitely possible to improve on this

#do a quick test by reversing the difference + getting predictions
#ideally the win %age should change by approximately the same as other approahces (10-20%)


#APPLY MODEL PREDICTIONS TO EACH GAME IN A SERIES
#NEED TO WRITE A NOTE IN WRITE UP EXPLAINING THAT IT UPDATES AFTER EACH GAME AND ISN'T PERFECTLY THE SAME AS A PREGAME EXPECTATION
#I.E. IF YOU LOSE FIRST TEST WHEN YOU SHOULDN'T, THIS APPROACH WOULD THEN EXPECT YOU TO BE MORE LIKELY TO LOSE SUBSEQUENT MATCHES

#DO A FEW RECENT CASE STUDIES (E.G. INDIA WINNING IN AUSTRALIA, ENGLAND WI) TO COMPUTE HA / SEE LIKELIHOOD OF SERIES RESULT


#potential build on ordinal regression by adding (home/away coefficients by team)
#ideally these might need to vary across time...
#perhaps too much?

data %>%
  ungroup() %>%
  select(`Team 1`, `Team 2`, Winner_Type,dif1,mDate) %>%
  mutate(Winner_Type=factor(
    Winner_Type, levels=c("Away","Draw","Home"))
  ) %>%
  MASS::polr(Winner_Type ~ dif1+mDate,data=., Hess = TRUE) -> model_middle

data %>%
  ungroup() %>%
  select(`Team 1`, `Team 2`, Winner_Type,dif1,mDate) %>%
  mutate(Winner_Type=factor(
    Winner_Type, levels=c("Away","Draw","Home"))
  ) %>%
  MASS::polr(Winner_Type ~ dif1+mDate+`Team 1`+`Team 2`,data=., Hess = TRUE) -> model_complex

data %>%
  ungroup() %>%
  select(`Team 1`, `Team 2`, Winner_Type,dif1,mDate) %>%
  mutate(Winner_Type=factor(
    Winner_Type, levels=c("Away","Draw","Home"))
  ) %>%
  MASS::polr(Winner_Type ~ dif1+mDate*`Team 1`+mDate*`Team 2`,data=., Hess = TRUE) -> model_v_complex

anova(model,model_middle,model_complex,model_v_complex)
#sense check time dependence of this



# 12/10/2019 - apply per game model to the number of games in the series

predictions_complex <- predict(model_complex,data,type="probs")
colnames(predictions_complex) <- c("Away Complex","Draw Complex","Home Complex")

# first extract the number of games, the number of

data %>%
  group_by(series_id,`Team 1`,`Team 2`) %>%
  summarise(year=min(year)) %>%
  left_join(t1Win) %>%
  left_join(t2Win) %>%
  mutate(description=paste0(`Team 1`," vs ",`Team 2`," ",t1Win,"-",t2Win," - ",year)) %>%
  ungroup() %>%
  select(series_id, description) -> series_description

data %>%
  cbind(predictions_complex) %>%
  left_join(series_description) %>%
  group_by(description, series_id) %>%
  mutate(Matches=max(Match)) %>% 
  filter(Match==1) %>%
  select(Matches,Away, Draw, Home,`Away Complex`,`Draw Complex`,`Home Complex`) %>%
  left_join(t1Win) %>%
  left_join(t2Win) -> start_of_series_probs

#check correlation
start_of_series_probs %>%
  ggplot(aes(x=Home,y=`Home Complex`)) + geom_point()


#check if number of predicted draws is ok
data %>%
  mutate(draw=if_else(Winner=="drawn",1,0)) %>%
  summarise(draws=sum(draw),
            sum_prob_draws=sum(Draw))

# yes - at atop line level it does fine

#get most probablable results
#track modelled P(T1 WIn) / P(T2 WIN) / P(Drawn Series) + get these macro probs

set.seed(123)
start_of_series_probs %>%
  filter(Matches>=3) %>%
  # filter(series_id<=3) %>%
  mutate(reps=10000) %>%
  #filter(description=="Australia vs England 5-0 - 2013") %>%
  group_by(reps,description,Matches,t1Win,t2Win) %>%
  #nest() %>%
  uncount(reps,.id="rep") %>%
  uncount(Matches,.remove=F) %>%
  mutate(prob=map_dbl(1:n(),~runif(1))) %>%
  mutate(t1Win_sim=if_else(prob>Away+Draw,1,0),
         t2Win_sim=if_else(prob<= Away,1,0)) %>%
  group_by(description,Matches,t1Win,t2Win,Away,Draw,Home,rep) %>%
  summarise(t1Win_sim=sum(t1Win_sim),
            t2Win_sim=sum(t2Win_sim))%>%
  mutate(matched=if_else(t1Win==t1Win_sim & t2Win==t2Win_sim,1,0),
         most_likely_result=paste0(t1Win_sim,"-",t2Win_sim)) -> raw_sims

raw_sims %>%
  group_by(description,most_likely_result) %>%
  summarise(times_seen=n()) %>% 
  mutate(times_seen=times_seen/sum(times_seen)) %>%
  arrange(description,-times_seen) %>%
  filter(1:n()==1) -> most_likely_results

# use raw_sims to get to series wins, losses and draws...
# add this to simulation results.
  
raw_sims %>%
  group_by(description,Away,Draw,Home) %>%
  summarise(matched=sum(matched)/n()) %>%
  left_join(most_likely_results) %>%
  arrange(matched) -> simulation_results

saveRDS(simulation_results,"simulation_results.rds")


# use model to get "probability of winning series" at outset

# try to adjust ELO for home advantage??!

# %>%
#   summarise(t1Win_sim=sum(t1Win_sim),
#             reps=n()) %>%
#   mutate(total=reps*Matches,
#          home_perc = t1Win_sim/total)
