library(tidyverse)
library(rvest)

url <- "https://fbref.com/en/comps/9/history/Premier-League-Seasons"
pl_seasons <- read_html(url)

pl_season_scraped <- pl_seasons %>%
  html_node('#div_seasons') %>%
  html_nodes("th a")

pl_url_and_seasons <- tibble(URL = html_attr(pl_season_scraped, "href"),
                            season = html_text(pl_season_scraped)) %>%
  mutate(URL = paste("https://fbref.com", URL, sep = "")) %>%
  filter(URL != 'https://fbref.com/en/comps/9/Premier-League-Stats')

get_season <- function(URL, season){
  season_id <- str_extract(URL, "(?<=[.]com/en/comps/9/)[^/]+")
  node_id <- paste('#div_results', season_id, '1_overall', sep = "")
  season_data <- read_html(URL)
  season_table <- season_data %>%
    html_node(node_id) %>%
    html_table(fill = TRUE) %>%
    select(Rk, Squad, MP, W, D, L, GF, GA, GD, Pts) %>%
    add_column(Season = season) 
}

final_data <- pmap_df(pl_url_and_seasons, get_season)

write.csv(final_data,'pl.csv')

final_data %>%
  group_by(Season) %>%
  summarise(avg_pts = mean(Pts),
         sd_pts = sd(Pts),
         teams = n()) %>%
  ggplot(aes(x = Season, y = sd_pts))+
  geom_point()


##test stuff
test_url <- 'https://fbref.com/en/comps/9/3232/2019-2020-Premier-League-Stats '
season_id = str_extract(test_url, "(?<=[.]com/en/comps/9/)[^/]+")
node_id = paste('#div_results', season_id, '1_overall', sep = "")
season_data <- read_html(test_url)
season_data %>%
  html_node(node_id) %>%
  html_table(fill = TRUE) %>%
  select(Rk, Squad, MP, W, D, L, GF, GA, GD, Pts, xG, xGA, xGD, 'xGD/90', Attendance)