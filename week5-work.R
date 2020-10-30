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
#will need to try again! 

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
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4) #read in the data and we can look at the first few lines so that we can see what type of data this is 
#first two rows contain the measure and the units of the data 

#data starts on line 3, so when we read it in we need to skip the first two rows

buoy44025 <- read_table(file,
                        col_names = FALSE, #the names of cols are not correct?
                        skip = 2)
#strict way of reading in data. each line needs to be the same length and each field and in the same position in every line.

#measure is a vector that contains the new column names that you want to paste into the buoy44025
#these come from the original 'file' that we wrote in at the beginning
#we want to extract and edit them so we can use them in the new buoy44025 dataset
#saves them as characters
measure <- scan(file, 
                nlines = 1, #one line of data to be used
                what = character()) %>%
  str_remove("#") #removes the hashtags that are at the start of each row

#units is also a vector that comes from the original 'file' that we read in at the beginning
#units that the measurements are taken in 
#they are listed in the second line of data so we want to skip the first row 
units <- scan(file,
              skip = 1, 
              nlines = 1, #one line of data to be used
              what = character()) %>% #character is the type of data to be read
  str_remove("#") %>% #again remove the hashtag at the start
  str_replace("/", "_per_") #replacing them with / in the data and _per_ to display the units correctly

names(buoy44025) <- paste(measure, units, sep = "_") 
names(buoy44025)
#now we are pasting in the measure and units that we extracted from 'file' and putting them 
#change the names of the columns and replace them with new names
#sep is used to denote the symbol that goes between measure and unit when they are pasted in

