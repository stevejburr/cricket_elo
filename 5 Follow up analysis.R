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
  MASS::polr(Winner_Type ~ dif1+mDate,data=., Hess = TRUE) %>%
  confint()
