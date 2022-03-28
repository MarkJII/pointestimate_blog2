library(tidyverse)
library(ggridges)
library(hrbrthemes)

set.seed(2022)

team <- c('white_sox', 'twins', 'guardians', 'tigers', 'royals')
proj_win_percent <- c(.545, .503, .476, .468, .461)

al_central <- tibble(team, proj_win_percent)

al_central_sim <- al_central %>%
  group_by(team) %>%
  mutate(simmed_wins = list(rbinom(10000, 162, proj_win_percent))) %>%
  unnest(simmed_wins)

ggplot(data = al_central_sim, aes(x = simmed_wins, y = team, color = team, fill = team))+
  geom_density_ridges(alpha = 0.5)+
  theme_ipsum()

al_central_sim_summary <- al_central_sim %>%
  group_by(team) %>%
  summarise(mean_wins = mean(simmed_wins),
            median_wins = median(simmed_wins),
            lower_twenty_fifth = quantile(simmed_wins, .25),
            upper_seventy_fifth = quantile(simmed_wins, .75))

al_central_sim_wide <- al_central_sim %>%
  group_by(team) %>%
  mutate(sim_id = row_number()) %>%
  select(-proj_win_percent) %>%
  pivot_wider(names_from = team, values_from = simmed_wins) %>%
  mutate(white_sox_win = white_sox > twins & white_sox > guardians 
         & white_sox > tigers & white_sox > royals) %>%
  mutate(twins_win = twins > white_sox & twins > guardians 
         & twins > tigers & twins > royals) %>%
  mutate(tigers_win = tigers > twins & tigers > guardians 
         & tigers > white_sox & tigers > royals) %>%
  mutate(royals_win = royals > twins & royals > guardians 
         & royals > white_sox & royals > tigers) %>%
  mutate(guardians_win = guardians > twins & guardians > tigers 
         & guardians > white_sox & guardians > royals) %>%
  group_by('All') %>%
  summarise(white_sox_win = mean(white_sox_win),
            twins_win = mean(twins_win),
            tigers_win = mean(tigers_win),
            royals_win = mean(royals_win),
            guardians_win = mean(guardians_win))
