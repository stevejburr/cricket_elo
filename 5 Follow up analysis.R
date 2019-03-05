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