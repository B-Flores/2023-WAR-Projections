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

batting <- data.frame()
batting <- import_batting(2022, df = batting)
batting <- import_batting(2021, df = batting)
batting <- import_batting(2020, df = batting)
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

people <- batting %>%
  inner_join(chadwick_player_lu(), by = "key_fangraphs") %>%
  select(-colnames(batting), key_fangraphs)


age_To_war <- batting %>%
  inner_join(people, by = "key_fangraphs") %>%
  mutate(birthyear = ifelse(birth_month >= 7,
                            birth_year + 1, birth_year),
         Age = Year - birth_year) %>%
  select(Age, WAR) %>%
  distinct()

ggplot(age_To_war, aes(Age, WAR)) + geom_point()

fit <- lm(WAR ~ I(Age-30) + I((Age - 30)^2), data = age_To_war)
b <- coef(fit)
Age.max <- 30 - b[2] / b[3] / 2
Max <- b[1] - b[2]^2 / b[3] / 4
list(fit = fit, Age.max = Age.max, Max = Max)

ggplot(age_To_war, aes(Age, WAR)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
              formula = y ~ poly(x, 2, raw = TRUE)) 

################################################################################

positions <- Fielding %>%
  mutate(key_bbref = playerID) %>%
  inner_join(chadwick_player_lu(), by = "key_bbref") %>%
  select(POS, key_fangraphs)

positions <- positions %>%
  inner_join(batting, by = "key_fangraphs") %>%
  group_by(key_fangraphs, POS) %>%
  summarize(Games = sum(G)) %>%
  arrange(key_fangraphs, desc(Games))

age_war_pos <- batting %>%
  inner_join(people, by = "key_fangraphs") %>%
  mutate(birthyear = ifelse(birth_month >= 7,
                            birth_year + 1, birth_year),
         Age = Year - birth_year) %>%
  inner_join(positions, by = "key_fangraphs") %>%
  select(Age, WAR, POS, Year) %>%
  distinct() %>%
  filter(POS != "P")

ggplot(age_war_pos, aes(x = POS)) +
  geom_histogram(bins = 50, col= "white", stat = "count")

# Filter by position and look at histogram faceted by Year

age_war_pos %>%
  filter(POS == "OF") %>%
  ggplot(aes(x = Age)) +
    geom_histogram(stat = "count") +
    facet_wrap(~round(Year))
    

  
# Career WAR Trajectories by Position (Quadratic Regression)
ggplot(age_war_pos, aes(Age, WAR)) + 
  geom_point(aes(color=POS)) +
#  facet_grid(Age > 30~POS) +
  facet_wrap(~POS)+
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
              formula = y ~ poly(x, 2, raw = TRUE))




# model <- lm(WAR~Age + POS, data = age_war_pos)
# summary(model)
# anova(model)
# model
# 
# model2 <- lm(WAR~Age + POS + Age:POS, data = age_war_pos)
# summary(model2)
# anova(model2)
# model2
# 
# 
# 
# 
# ggplot(age_war_pos, aes(Age, WAR)) +
#   geom_point(aes(color=POS)) +
#   geom_abline(aes(intercept = 4.367, slope = -0.059, color = "1B")) +
#   geom_abline(aes(intercept = 4.367 + model2$coefficients[3], slope = -0.059 + model2$coefficients[8]+ 0.00276, color = "2B")) +
#   geom_abline(aes(intercept = 4.367 + model2$coefficients[4], slope = -0.059 + model2$coefficients[9], color = "3B")) +
#   geom_abline(aes(intercept = 4.367 + model2$coefficients[5], slope = -0.059 + model2$coefficients[10], color = "C")) +
#   geom_abline(aes(intercept = 4.367 + model2$coefficients[6], slope = -0.059 + model2$coefficients[11], color = "OF")) +
#   geom_abline(aes(intercept = 4.367 + model2$coefficients[7], slope = -0.059 + model2$coefficients[12], color = "SS")) +
#   facet_wrap(~POS)
# 
# ggplot(age_war_pos, aes(Age, WAR)) +
#   geom_point(aes(color=POS)) +
# #  facet_grid(Age > 30~POS) +
#   facet_wrap(~POS)+
#   geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
#               formula = y ~ x, aes(color = POS))
# 
# ggplot(age_war_pos, aes(Age, WAR)) +
#   geom_point(aes(color=POS)) +
# #  facet_grid(Age > 30~POS) +
#   facet_wrap(~POS)+
#   geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
#               formula = y ~ poly(x, 2, raw = TRUE)) +
#   geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
#               formula = y ~ x)

