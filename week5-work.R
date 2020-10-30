library(tidyverse)

file <- "data-raw/Human-development-index.csv"

#HDI <- read_csv(file)

hdi <- read_csv(file) %>% 
  janitor::clean_names()

hdi <- hdi %>%
  pivot_longer(names_to = "year",
               values_to = "index",
               cols = -c(hdi_rank_2018, country))
hdi

hdi_no_na <- hdi %>% 
  filter(!is.na(index)) #gives the Na without the !. so include the !

hdi_no_na <- hdi_no_na %>%
  mutate(year =  str_replace(year, "x", "") %>% 
           as.numeric()) #removes the x from in front of the years

hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(index))

hdi_summary <- hdi_no_na %>% 
  group_by(country) %>%
  summarise(mean_index = mean(index),
            n = length(index),
            sd_index = sd(index),     #calculating standard deviation 
            se_index = sd_index/sqrt(n)) #calculating standard error

hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#create a plot for hdi dataset
#without creating any intermediate data structures
#have no clue!!!!

hdi %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = index)) +
  geom_errorbar(aes(x = country,
                    ymin = index - index,
                    ymax = index + index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()


#####data from buoy#####
