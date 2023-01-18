library(tidyverse)
library(baseballr)
library(Lahman)

#xWOBA is dropped from years 2015-2022 since it isn't included in our data

import_batting <- function(year, df){
  filepath = str_c("C:/Users/B/Desktop/Projects/2023-WAR-Projections/stats-tables/batters/batters-", year, ".csv")
  df1 <- read.csv(filepath) %>%
    mutate(Year = year) %>%
    select(!xwOBA)
  df <- df1 %>% rows_append(df)
  return(df)
}

#intial dataframe using csv files from fangraphs (Qualified)
batting <- data.frame()
batting <- import_batting(2022, df = batting)
batting <- import_batting(2021, df = batting)
#batting <- import_batting(2020, df = batting)
batting <- import_batting(2019, df = batting)
batting <- import_batting(2018, df = batting)
batting <- import_batting(2017, df = batting)
batting <- import_batting(2016, df = batting)
batting <- import_batting(2015, df = batting)
batting <- import_batting(2014, df = batting)
batting <- import_batting(2013, df = batting)
batting <- import_batting(2012, df = batting)
colnames(batting) <- c("Name","Team","G","PA", "HR", "R", "RBI", "SB", "BB%", 
                       "K%", "ISO", "BABIP", "AVG", "OBP", "SLG", "wOBA", "wRC+",
                       "BsR", "Off", "Def", "WAR", "key_fangraphs", "Year")

# join with chadwick, retrieve birth month & year to calculate age for season
batting <- batting %>%
  inner_join(chadwick_player_lu(), by = "key_fangraphs") %>%
  select(colnames(batting),birth_month, birth_year, key_fangraphs) %>%
  mutate(birthyear = ifelse(birth_month >= 7,
                            birth_year + 1, birth_year),
         Age = Year - birth_year)

Fielding %>%
  mutate(key_bbref = playerID) %>%
  left_join(chadwick_player_lu(), by = "key_bbref") %>%
  group_by(key_fangraphs, POS) %>%
  inner_join(batting, by = "key_fangraphs") %>%
  rename(G = G.x) %>%
  summarize(Games = sum(G)) %>%
  arrange(key_fangraphs, desc(Games)) %>%
  filter(POS == first(POS)) %>%
  select(!Games) %>%
  right_join(.,batting, by = "key_fangraphs") %>%
  filter(POS != "P") %>%
  drop_na() %>%
  group_by(key_fangraphs)-> batting
   
  







  
  
  
  
  
  
  
  