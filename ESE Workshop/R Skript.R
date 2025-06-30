# Pakete laden

library(lme4)
library(tidyverse)
library(sjPlot)

# Visualisierung

DATA %>%
  ggplot(aes(x = team_sports_activities, y = social_inclusion, color = CLASS)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Multilevel-Regression

## Random intercept

lmer(social_inclusion ~ team_sports_activities + (1 | CLASS), data = DATA)

lmer(social_inclusion ~ team_sports_activities + (1 | CLASS), data = DATA) %>% 
  tab_model()

## Random intercept and slope

lmer(social_inclusion ~ team_sports_activities + (1 + team_sports_activities | CLASS), data = DATA)

lmer(social_inclusion ~ team_sports_activities + (1 + team_sports_activities | CLASS), data = DATA) %>% 
  tab_model()

# Visualisierung

DATA %>%
  ggplot(aes(x = team_sports_activities %>% scale(scale = FALSE), y = social_inclusion, color = CLASS)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Multilevel-Regression mit Zentrierung von X (Prädktor)

## Random intercept

lmer(social_inclusion ~ team_sports_activities %>% scale(scale = FALSE) + (1 | CLASS), data = DATA)

lmer(social_inclusion ~ team_sports_activities %>% scale(scale = FALSE) + (1 | CLASS), data = DATA) %>% 
  tab_model()

## Random intercept and slope

lmer(social_inclusion ~ team_sports_activities %>% scale(scale = FALSE) + (1 + team_sports_activities %>% scale(scale = FALSE) | CLASS), data = DATA)

lmer(social_inclusion ~ team_sports_activities %>% scale(scale = FALSE) + (1 + team_sports_activities %>% scale(scale = FALSE) | CLASS), data = DATA) %>% 
  tab_model()

# Ausblick: 3-Level

lmer(social_inclusion ~ team_sports_activities + (1 + team_sports_activities | SCHOOL:CLASS), data = DATA)


