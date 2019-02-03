library(tidyverse)
library(ggrepel)

#code for producing final charts

#load previous data and filter to prefered k values

read_csv("cricket_elos.csv") %>%
  filter(k==30 & firstYear==1946) %>%
  select(-c(k,firstYear)) -> data


#chart Home / win losses over time

data %>%
  filter(year<2019) %>%
  group_by(year,Winner_Type) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)) %>%
  ggplot() +
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=0,yend=0.6),
               colour="grey80")+
  geom_smooth(aes(x=year,y=perc,colour=Winner_Type),se=F,size=1.5) +
  scale_y_continuous("% of games",labels = scales::percent_format(accuracy=1))+
  scale_x_continuous("")+
  scale_color_discrete("Winner")+
  theme_minimal() +
  theme(text=element_text(colour="grey50",size=20),
        axis.text=element_text(colour="grey50"),
        axis.title=element_text(colour="grey50"),
        panel.grid=element_blank())+
  labs(title="Since the 1980s there are fewer draws in test cricket",
       subtitle="Based on simple trends, it looks like this favours the home team",
       caption="Trends are smoothed based on yearly averages using loess\nData source: ESPNcricinfo - Design and Analysis by @stevejburr")

ggsave("win_rates_over_time.png",units="in",dpi=72,width=700/72,height=700/72)

data %>%
  filter(year<2019) %>%
  group_by(year,Winner_Type) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)) %>%
  select(-count) %>%
  group_by(year) %>%
  spread(key=Winner_Type,value=perc,fill=0) %>%
  transmute(gap=Home-Away) %>%
  ggplot(aes(x=year,y=gap)) +
  geom_line()

data %>%
  filter(year<2019) %>%
  group_by(year,Winner_Type) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)) %>%
  select(-count) %>%
  group_by(year) %>%
  spread(key=Winner_Type,value=perc,fill=0) %>%
  transmute(gap=Home-Away) -> home_away_raw
  
ggplot() +
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=0,yend=0.25),
               colour="grey80")+
  geom_smooth(data=home_away_raw,aes(x=year,y=gap),se=F,colour="blue",size=1.5) +
  scale_y_continuous("Home win rate advantage / % points",
                     breaks=seq(0,0.25,0.05),
                     labels=scales::percent_format(accuracy=1)) +
  coord_cartesian(ylim=c(0,0.25)) +
  scale_x_continuous("",
                     breaks=seq(1960,2020,20))+
  theme_minimal() +
  theme(panel.grid= element_blank(),
        text=element_text(colour="grey50",size = 20),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Home advantage in test cricket has increased over time",
       caption="Trends are smoothed based on yearly averages using loess\nData source: ESPNcricinfo - Design and Analysis by @stevejburr")

ggsave("home_advantage_simple.png",units="in",dpi=72,width=700/72,height=700/72)




ggplot() +
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=0,yend=0.45),
               colour="grey80")+
  geom_point(data=home_away_raw,aes(x=year,y=gap),size=2,colour="blue",alpha=0.5)+
  geom_smooth(data=home_away_raw,aes(x=year,y=gap),se=F,colour="blue",size=1.5) +
  scale_y_continuous("Home win rate advantage / % points",
                     breaks=seq(0,0.45,0.05),
                     labels=scales::percent_format(accuracy=1)) +
  coord_cartesian(ylim=c(0,0.45)) +
  scale_x_continuous("",
                     breaks=seq(1960,2020,20))+
  theme_minimal() +
  theme(panel.grid= element_blank(),
        text=element_text(colour="grey50",size = 20),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Home advantage in test cricket has increased over time",
       caption="Trends are smoothed based on yearly averages using loess\nData source: ESPNcricinfo - Design and Analysis by @stevejburr") 
ggsave("home_advantage_simple_points.png",units="in",dpi=72,width=700/72,height=700/72)

#chart team Elo over time (for major countries)

teams <- union(data$`Team 1`,data$`Team 2`)

#identify first year where each country played and if year is less then we don't plot Elo...
data %>%
  group_by(year,`Team 1`) %>%
  summarise(count=n()) %>%
  group_by(`Team 1`) %>%
  summarise(m1=min(year)) %>%
  rename(Country=`Team 1`) -> t1
  
data %>%
  group_by(year,`Team 2`) %>%
  summarise(count=n()) %>%
  group_by(`Team 2`) %>%
  summarise(m2=min(year)) %>%
  rename(Country=`Team 2`) -> t2

t1 %>% full_join(t2) %>%
  ungroup() %>%
  mutate(m1=if_else(is.na(m1),2019,m1),
         m2=if_else(is.na(m2),2019,m2),
         min=if_else(m1<m2,m1,m2)) %>%
  select(Country,min) -> mins

data %>%
  group_by(year) %>%
  arrange(year,mDate) %>% 
  mutate(year_game=1:n(),year_games=n()) %>%
  filter(year_game==year_games) %>%
  select(-starts_with("year_game")) %>%
  mutate(`South Africa`=if_else((year>1970&year<1991),as.double(NA),`South Africa`)) %>%
  select_at(teams)%>% 
  gather(key="Country",value="Elo",-year) %>%
  filter(Country %in% c("Australia","Bangladesh","England","India",
                        "New Zealand","Pakistan","South Africa",
                        "Sri Lanka","West Indies","Zimbabwe")) %>%
  left_join(mins) %>%
  mutate(Elo=if_else(year<min,as.double(NA),Elo)) -> country_elos_over_time


ggplot(country_elos_over_time) +
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=600,yend=1400),
               colour="grey80")+
  geom_line(aes(x=year,y=Elo,colour=Country),show.legend = F,size=1.5) +
  geom_text_repel(data=(country_elos_over_time %>% filter(year==2019)),
            aes(x=2022,y=Elo,colour=Country,label=Country),
            direction="y",segment.size=NA,show.legend = F,
            hjust="left",size=8) +
  scale_x_continuous("",breaks=seq(1960,2020,20)) +
  scale_y_continuous("Team Elo Rating")+
  scale_colour_manual("Team",values=c("Australia"="#dddd0f",
                                      "Bangladesh"="#005600",
                                      "England"="#013166",
                                      "India"="#00b8ff",
                                      "New Zealand"="#000000",
                                      "Pakistan"="#008080",
                                      "South Africa"="#006651",
                                      "Sri Lanka"="#0000FF",
                                      "West Indies"="#800000",
                                      "Zimbabwe"="#FF0000")
  )+
  coord_cartesian(xlim=c(1946,2040))+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text=element_text(colour="grey50",size=20),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="In 2007 Australia reached the highest ever Test Elo Rating",
  caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") 
  
ggsave("test_cricket_elo.png",units="in",dpi=72,width=700/72,height=700/72)
#look @ different calculations
data %>%
  filter(year<2019) %>%
  mutate(`Home Advantage`=r1-e1) %>% 
  group_by(year) %>%
  summarise(`Home Advantage`=mean(`Home Advantage`)) -> home_away_elo

ggplot(home_away_elo) +
  geom_smooth(aes(x=year,y=`Home Advantage`),se=FALSE) +
  labs(title="Smoothed trend on year average")

#combine this with raw proportions from earlier
home_away_elo %>%
  left_join(home_away_raw) %>%
  rename(`Home Advantage (Elo)`=`Home Advantage`,
         `Home Advantage (Raw)`=gap) %>%
  ggplot()+
  geom_segment(data=data.frame(x=seq(1950,2020,10)),
               aes(x=x,xend=x,y=0,yend=0.25),
               colour="grey80")+
  geom_smooth(aes(x=year,y=`Home Advantage (Elo)`), se=FALSE,colour="red",size=1.5) +
  geom_smooth(aes(x=year,y=`Home Advantage (Raw)`),se=FALSE,colour="blue",size=1.5) +
  annotate("text",x=2020,y=0.23,
           label="Home advantage - \nunadjusted",
           colour="blue",hjust="left",
           size=7) +
  annotate("text",x=2020,y=0.12,
           label="Home advantage - \nElo adjusted",
           colour="red",hjust="left",
           size=7)+
  coord_cartesian(xlim=c(1946,2040)) +
  scale_y_continuous("Home win rate advantage / % points",
                     labels=scales::percent_format(accuracy=1))+
  scale_x_continuous("",
                     breaks=seq(1960,2020,20))+
  theme_minimal() +
  theme(panel.grid= element_blank(),
        text=element_text(colour="grey50",size = 20),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Home advantage in test cricket has grown over time",
       subtitle="The trend is similar in shape when we control for team strength",
  caption="Trends are smoothed based on yearly averages using loess\nData source: ESPNcricinfo - Design and Analysis by @stevejburr")


ggsave("home_advantage_both.png",units="in",dpi=72,width=700/72,height=700/72)

#get average home advantage by country 
data %>%
  filter(year<2019) %>%
  mutate(`Home Advantage`=r1-e1) %>% 
  filter(`Team 1` %in% c("Australia","Bangladesh","England","India",
                         "New Zealand","Pakistan","South Africa",
                         "Sri Lanka","West Indies","Zimbabwe")) %>%
  ggplot(aes(x=mDate,y=`Home Advantage`,colour=`Team 1`)) +
  #geom_point()+
  geom_smooth(se=F) +
  scale_colour_manual("Team",values=c("Australia"="#dddd0f",
                                      "Bangladesh"="#005600",
                                      "England"="#013166",
                                      "India"="#00b8ff",
                                      "New Zealand"="#000000",
                                      "Pakistan"="#008080",
                                      "South Africa"="#006651",
                                      "Sri Lanka"="#0000FF",
                                      "West Indies"="#800000",
                                      "Zimbabwe"="#FF0000")
  )+
  labs(title="Smoothed trend by country on individual games")


#who is the worst traveller?
data %>%
  mutate(`Away Disadvantage`=-1*(r2-e2)) %>% 
  filter(`Team 2` %in% c("Australia","Bangladesh","England","India",
                         "New Zealand","Pakistan","South Africa",
                         "Sri Lanka","West Indies","Zimbabwe")) %>%
  ggplot(aes(x=mDate,y=`Away Disadvantage`,colour=`Team 2`)) +
  scale_colour_manual("Team",values=c("Australia"="#dddd0f",
                                      "Bangladesh"="#005600",
                                      "England"="#013166",
                                      "India"="#00b8ff",
                                      "New Zealand"="#000000",
                                      "Pakistan"="#008080",
                                      "South Africa"="#006651",
                                      "Sri Lanka"="#0000FF",
                                      "West Indies"="#800000",
                                      "Zimbabwe"="#FF0000")
  )+
  #geom_point()+
  geom_smooth(se=F) +
  labs(title="Smoothed trend by country on individual games")

