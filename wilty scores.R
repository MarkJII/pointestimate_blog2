library(rvest)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(htmltab)
library(purrr)
library(stringr)

url <- "https://en.wikipedia.org/wiki/List_of_Would_I_Lie_to_You%3F_episodes"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")

df <- html_table(tbls[grep("Series",tbls,ignore.case = T)],fill = T)

tbls <- map2(url, 2:18, htmltab)
tbls <- do.call(rbind, tbls)

wilty_prepped <- tbls %>%
  as_tibble() %>%
  select(-`First broadcast`) %>%
  mutate(dm_score = str_extract(Scores, "^\\d+"),
         lm_score = str_extract(Scores, "\\d+$"),
         season = str_extract(Episode, "^\\d+"),
         ep = str_extract(Episode, "\\d+$"),
         dm_wins = case_when(dm_score > lm_score ~ 1, TRUE ~ 0),
         dm_ties = case_when(dm_score == lm_score ~ 1, TRUE ~ 0),
         dm_losses = case_when(dm_score < lm_score ~ 1, TRUE ~ 0)) %>%
  filter(!is.na(dm_score)) %>%
  mutate(dm_team = str_split(`David's team`, " and "),
         lm_team =str_split(`Lee's team`, " and " ))

dm_team_list <- wilty_prepped %>%
  select(Episode, dm_team) %>%
  tidyr::unnest(dm_team) %>%
  rename(name = dm_team) %>%
  mutate(team = 'David')

lm_team_list <- wilty_prepped %>%
  select(Episode, lm_team) %>%
  tidyr::unnest(lm_team) %>%
  rename(name = lm_team) %>%
  mutate(team = 'Lee')

team_list <- dm_team_list %>%
  rbind(lm_team_list) %>%
  mutate(name = str_trim(name))

team_list %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

wilty_prepped %>%
  group_by(season) %>%
  summarise(dm_wins = sum(dm_wins),
            dm_ties = sum(dm_ties),
            dm_losses = sum(dm_losses),
            dm_score = mean(as.numeric(dm_score), na.rm = TRUE),
            lm_score = mean(as.numeric(lm_score), na.rm = TRUE))

wilty_prepped %>%
    mutate(score_dif = as.integer(dm_score) - as.integer(lm_score)) %>%
    ggplot(aes(x = score_dif))+
    geom_histogram(binwidth = 1, color="black", fill="white")+
    theme_ipsum()

write.csv(team_list, "wilty_teams.csv", row.names = TRUE)

wilty_prepped %>%
    select(-dm_team, -lm_team, -`David's team`, -`Lee's team`, -Scores) %>%
    write.csv('wilty.csv', row.names = TRUE)