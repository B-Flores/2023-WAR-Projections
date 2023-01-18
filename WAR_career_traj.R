source("importData.R")

# Career WAR Trajectories by Position (Quadratic Regression)
ggplot(batting, aes(Age, WAR)) + 
  geom_point(aes(color=POS)) +
  #  facet_grid(Age > 30~POS) +
  facet_wrap(~POS)+
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
              formula = y ~ poly(x, 2, raw = TRUE))


batting %>%
  group_by(key_fangraphs) %>%
  summarise(POS, WAR, Age, Years = n()) %>%
  filter(Years >= 5) %>%
  ggplot(aes(Age,WAR)) +
  geom_point(aes(color=POS)) +
  #  facet_grid(Age > 30~POS) +
  facet_wrap(~POS)+
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
              formula = y ~ poly(x, 2, raw = TRUE))
  
test_data <- batting %>%
  group_by(key_fangraphs) %>%
  summarise(POS = as_factor(POS), WAR, Age = as_factor(Age), Years = n()) %>%
  filter(Years >= 5)

# Polynomial Regression of the form :
#   A + B(Age - 30) + C(Age - 30)^2
#     * A = Predicted value of WAR at Age = 30
#     * max(WAR) occurs at Age = 30 - (B/2C)
#     * max(WAR) = A - B^2/4C

poly_reg <- test_data %>% lm(WAR ~ I(Age - 30) + I((Age-30)^2), data = .) 
summary(poly_reg)

mod1 <- test_data %>% aov(WAR ~ Age + POS, data = .)
summary(mod1) 
mod1 %>% TukeyHSD() 

mod1 %>% TukeyHSD() %>%
  plot()




career_war <- batting %>% 
  group_by(key_fangraphs) %>%
  summarize(Name, POS, CarWAR = sum(WAR), Years = max(Year) - min(Year)) %>%
  mutate(Avg_WAR = CarWAR / Years) %>%
  filter(Years >= 7) %>%
  distinct() %>%
  arrange(desc(CarWAR), desc(Years))

career_war %>% 
  group_by(POS) %>%
  summarise(Avg_WAR = mean(Avg_WAR)) %>%
  aov(Avg_WAR ~ POS, .) -> mod1

summary(mod1)



####################### FUNCTIONS #############################################

plot_stat <- function(df = batting, temp = batting$WAR){
  df %>% ggplot(aes(Age, temp)) + 
    geom_point(aes(color=POS)) +
    #  facet_grid(Age > 30~POS) +
    facet_wrap(~POS)+
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
                formula = y ~ poly(x, 2, raw = TRUE))
  
}

plot_stat(temp = batting$`wRC+`)


# Plot individual player career trajectory by name
plot_player <- function(df = batting, name = "Christian Yelich"){
  df %>% 
    filter(Name == name) %>%
    ggplot(aes(Age, WAR)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.5,
                formula = y ~ poly(x, 2, raw = TRUE))
    
}


# Filter by position and look at histogram faceted by Year
pos_hist <- function(data = batting, position = "C"){
  data %>% 
    filter(POS == position) %>%
    ggplot(aes(x = Age)) +
    geom_histogram(stat = "count") +
    facet_wrap(~round(Year))
  
  
}
pos_hist(position = "OF")

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
