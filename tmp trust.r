
simulate_data <- function(n = n, min_trust = min_trust) {
  mean(replicate(1000, prod(runif(n = n, min = min_trust, max = .9999))))
}


n_people <- seq(1:500)
min_trust <- seq(.9:.99, by = .01)

people_grid <- crossing(n_people, min_trust) %>%
  rowwise() %>%
  mutate(avg_sucess = simulate_data(n = n_people, min_trust = min_trust),
         min_trust_avg = (1 + min_trust) / 2,
         power_odds = min_trust_avg ^ n_people,
         power_odds_dif = power_odds - avg_sucess) %>%
  filter(min_trust != 1)

ggplot(people_grid, aes(x = n_people, y = avg_sucess, color = as.factor(min_trust)))+
         geom_line()

ggplot(people_grid, aes(x = n_people, y = power_odds_dif, color = as.factor(min_trust)))+
  geom_line()+
  facet_wrap(~as.factor(min_trust))

ggplot(people_grid, aes(x = n_people, y = power_odds_dif, color = as.factor(min_trust)))+
  geom_point()

#https://www.worldometers.info/world-population/
#https://data.worldbank.org/indicator/SP.POP.1564.TO.ZS?contextual=population-by-age
world_population <- 7900000000
population_15_to_64 <- .65191 * world_population

world_trust_levels <- seq(0:.10, by = .01)
world_willing_to_partner_levels <- seq(0:.10, by = .01)

crossing(world_trust_levels, world_willing_to_partner_levels) %>%
  filter(world_trust_levels <= .10 & world_willing_to_partner_levels <= 0.05) %>%
  rowwise() %>%
  mutate(odds_of_successful_partnership = world_trust_levels * world_willing_to_partner_levels,
         pop_willing = odds_of_successful_partnership * population_15_to_64) %>%
  ggplot(aes(x = world_trust_levels, y = pop_willing, color = as.factor(world_willing_to_partner_levels)))+
  geom_line()