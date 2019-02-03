#code to download test criket match results data from espn cricinfo:

#read in libraries
library(tidyverse)
library(XML)

#first get cricket test series data
#data is held by year...


getTestResultsYear <- function(year) {
  #url to get data from espncricinfo
  url <- paste0("http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=1;id=",
                year,";type=year")
  
  #grab a list contain all tables in the page defined by URL
  tables <-readHTMLTable(url, stringsAsFactors = F)
  
  #extract only the results table
  tables <- tables$`Match results`
  
  #need a date value to stamp on, use the first day as easiest
  #extract month, first day and year by position and convert to date
  
  #there are two date formats depending on whether the match crosses a year
  #first deal with this unusual case using stringr functions
  matches <- str_match(tables$`Match Date`,"(^[A-Za-z]{3}) ([0-9]{1,2}), ([0-9]{4})")
  
  tables$mDate <- as.Date(NA)
  tables$mDate[!is.na(matches[,1])] <- as.Date(paste(matches[!is.na(matches[,1]),2],
                                                     matches[!is.na(matches[,1]),3],
                                                     matches[!is.na(matches[,1]),4]),
                                               "%b %d %Y")
  
  tables %>%
    mutate(temp_len=nchar(`Match Date`)) %>%
    mutate(temp_pos=regexpr("-",`Match Date`)) %>%
    mutate(temp_monthday=substr(`Match Date`,1,temp_pos-1)) %>%
    mutate(temp_year= substr(`Match Date`,temp_len-3,temp_len)) %>%
    mutate(mDate=if_else(is.na(mDate),as.Date(paste(substr(`Match Date`,1,temp_pos-1),
                                                    substr(`Match Date`,temp_len-3,temp_len)),"%b %d %Y"),mDate)) %>% 
    select(-starts_with("temp_")) -> tables
  
  #format it so we understand both the winner (country) but also home vs lost
  tables %>%
    mutate(Winner_Type=case_when(Winner==`Team 1` ~ "Home",
                                 Winner==`Team 2` ~ "Away",
                                 Winner=="drawn" ~ "Draw",
                                 TRUE ~ "Check")) -> tables
  
  return(tables)
  
}



years <- 1946:2019
#map the download process across all the years
testData<-map_dfr(years,getTestResultsYear)

write_csv(testData,"testData.csv")

