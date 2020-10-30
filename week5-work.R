library(tidyverse)

file <- "data-raw/Human-development-index.csv"

HDI <- read_csv(file)

hdi <- read_csv(file) %>% 
  janitor::clean_names()

hdi <- hdi %>%
  pivot_longer(names_to = "year",
               values_to = "index",
               cols = -c(hdi_rank_2018, country))
hdi

hdi_summary <- hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(index))

hdi_summary <- hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(index),
            n = length(index))
