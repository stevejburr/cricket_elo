library(tidyverse)
library(ggrepel)

#code for producing final charts

#load previous data and filter to prefered k values

read_csv("cricket_elos.csv") %>%
  filter(k==30 & firstYear==1946) %>%
  select(-c(k,firstYear)) -> data



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
  group_by(mDate) %>%
  mutate(`South Africa`=if_else((year>1970&year<1991),as.double(NA),`South Africa`)) %>%
  select_at(c("mDate",teams))%>% 
  gather(key="Country",value="Elo",-mDate) %>%
  mutate(year=lubridate::year(mDate)) %>%
  filter(Country %in% c("Australia","Bangladesh","England","India",
                        "New Zealand","Pakistan","South Africa",
                        "Sri Lanka","West Indies","Zimbabwe")) %>%
  left_join(mins) %>%
  mutate(Elo=if_else(year<min,as.double(NA),Elo)) -> country_elos_by_date


#build top elo in year field:
country_elos_by_date %>%
  filter(!is.na(Elo)) %>%
  group_by(mDate) %>%
  summarise(benchmark=max(Elo)) %>%
  right_join(country_elos_by_date) -> country_elos_by_date

png("test_cricket_elo_by_game_facet.png",width=700,height=700,res=72,type="cairo-png")

ggplot(country_elos_by_date) +
  facet_wrap(.~Country,nrow=2)+
  geom_line(aes(x=mDate,y=benchmark),
            colour="grey50",show.legend = F,size=1) +
  geom_line(aes(x=mDate,y=Elo,colour=Country),show.legend = F,size=1) +
  scale_x_date("")+
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
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text=element_text(colour="grey50"),
        text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="In 2007 Australia reached the highest ever Test Elo Rating",
       subtitle="Grey line shows the rating of the best team in the world",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") 

dev.off()

max(country_elos_by_date$mDate)

png("test_cricket_elo_by_game.png",width=700,height=700,res=72,type="cairo-png")

ggplot(country_elos_by_date) +
   geom_line(aes(x=mDate,y=Elo,colour=Country),show.legend = F,size=1) +
  geom_text_repel(data=(country_elos_by_date %>% filter(mDate==as.Date("2019-01-31"))),
                  aes(x=as.Date("2022-01-31"),y=Elo,colour=Country,label=Country),
                  direction="y",segment.size=NA,show.legend = F,
                  hjust="left",size=8) +
  scale_x_date("") +
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
  coord_cartesian(xlim=c(as.Date("1946-01-01"),as.Date("2040-01-01")))+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="In 2007 Australia reached the highest ever Test Elo Rating",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") 

dev.off()



png("test_cricket_elo_by_game_2012.png",width=700,height=700,res=72,type="cairo-png")

ggplot(country_elos_by_date) +
  geom_line(aes(x=mDate,y=Elo,colour=Country),show.legend = F,size=1) +
  geom_text_repel(data=(country_elos_by_date %>% filter(mDate==as.Date("2019-01-31"))),
                  aes(x=as.Date("2022-01-31"),y=Elo,colour=Country,label=Country),
                  direction="y",segment.size=NA,show.legend = F,
                  hjust="left",size=5) +
  scale_x_date("") +
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
  coord_cartesian(xlim=c(as.Date("2012-01-01"),as.Date("2020-01-01")))+
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text=element_text(colour="grey50",size=17),
        axis.title=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"))+
  labs(title="Test Cricket Elo ratings by game since 2012",
       subtitle="England's poor series in the West Indies has made a big difference to their ranking",
       caption="Data source: ESPNcricinfo - Design and Analysis by @stevejburr") 

dev.off()