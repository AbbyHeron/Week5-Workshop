############################################TASK 1############################################

#Loading in tidyverse
library(tidyverse)

#Reading in sample data
hdi <- read_csv("data/Human-development-index.csv") %>% 
  janitor::clean_names()

hdi

#Converting the data to tidy format
hdi2 <- hdi %>% 
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country))

#Removing all values with NA
no_na_hdi <- filter(hdi2, !is.na(value))
no_na_hdi

#Summarising to get the mean index by country
hdi_summary <- no_na_hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(value))

hdi_summary

#Creating a column in the summary data showing how many values have gone into the mean - n/indices
hdi_summary <- no_na_hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(value),
            n = length(value))

#Creating a column in the summary data showing the standard deviation
hdi_summary <- no_na_hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(value),
            n = length(value),
            sd = sd(value))

#Creating a column in the summary data showing the standard error
hdi_summary <- no_na_hdi %>% 
  group_by(country) %>% 
  summarise(mean_index = mean(value),
            n = length(value),
            sd = sd(value),
            se = plotrix::std.error(value))

#Filtering the summary data to get 10 countries with the lowest mean HDI
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

#Creating a plot of the 10 countries with the lowest mean HDI
hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se,
                    ymax = mean_index + se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#Combining this into one chunk of code with no intermediates
hdifinal <- read_csv("data/Human-development-index.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country)) %>% 
  filter(!is.na(value)) %>%
  group_by(country) %>% 
  summarise(mean_index = mean(value),
            n = length(value),
            sd = sd(value),
            se = plotrix::std.error(value)) %>% 
  filter(rank(mean_index) < 11) %>%
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_index)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_index - se,
                    ymax = mean_index + se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

hdifinal

############################################TASK 2############################################



