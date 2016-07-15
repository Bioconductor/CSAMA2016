suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(gapminder)
library(purrr)

gapminder %>%
  group_by(continent) %>%
  summarize(n_obs = n())

gapminder %>%
  group_by(continent) %>%
  tally()

gapminder %>%
  group_by(continent) %>%
  summarize(n_obs = n(),
            n_countries = n_distinct(country))

gapminder %>%
  group_by(continent) %>%
  summarise(avg_lifeExp = mean(lifeExp))

gapminder %>%
  group_by(continent) %>%
  summarise_each(funs(mean, median), lifeExp, gdpPercap)

## just Asia, min and max life expectancy for each year
gapminder %>%
  filter(continent == "Asia") %>%
  group_by(year) %>%
  summarize_each(funs(min, max), lifeExp)

## it would be nicer to know which country exhibits the min and max
## don't filter to asia
gapminder %>%
  select(year, country, lifeExp) %>%
  arrange(year) %>%
  group_by(year) %>%
  #top_n(1)               ## gets the min
  top_n(1, desc(lifeExp)) ## gets the max

(df <- gapminder %>%
  group_by(continent, country) %>%
  select(country, year, continent, lifeExp) %>%
  mutate(le_delta = lifeExp - lag(lifeExp)) %>%
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
  filter(min_rank(worst_le_delta) < 2) %>%
  arrange(worst_le_delta))

library(ggplot2)

ggplot(gapminder %>% filter(country %in% df$country),
       aes(x = year, y = lifeExp)) +
  facet_wrap(~ country) +
  geom_smooth(lwd = 1.3, se = FALSE, method = "lm") +
  geom_point()

## group_by() + nest()

gapminder <- gapminder %>%
  mutate(year1950 = year - 1950)

gapminder %>%
  group_by(continent, country) %>%
  nest()

country_model <- function(df) {
  lm(lifeExp ~ year1950, data = df)
}

gapminder %>%
  filter(country == "Italy") %>%
  country_model()

(models <- gapminder %>%
  group_by(continent, country) %>%
  nest() %>%
  mutate(lm_fit = map(data, country_model)))

models %>%
  mutate(coef_fit = map(lm_fit, coef))

tidy_models <- models %>%
  mutate(tidy = lm_fit %>% map(broom::tidy))
tidy_models
tidy_models[[1, "tidy"]]

tidy_models %>%
  select(-data, -lm_fit) %>%
  unnest()

coefs <- tidy_models %>%
  select(-data, -lm_fit) %>%
  unnest()

ggplot(coefs %>% filter(term == "(Intercept)"), aes(estimate)) +
  geom_density()

coefs %>%
  select(continent, country, term, estimate) %>%
  mutate(term = ifelse(term == "(Intercept)", "intercept", "slope")) %>%
  spread(term, estimate) %>%
  ggplot(aes(x = intercept, y = slope)) + geom_point()
