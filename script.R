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

#Reading in the data - Only reading in the first 4 lines so we can decide how to read in the data
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
buoy <- readLines("data/buoy.txt", n = 4)
buoy

#Reading in the data using read_table and skipping the first 2 lines as they contain the names and units
buoy2 <- read_table("data/buoy.txt", 
              col_names = FALSE,
              skip = 2)

#Using scan() to read in appropriate lines, tidy the data and name the columns measure_units
?scan()
?str_remove()
buoy3 <- scan("data/buoy.txt", what = "character", n = 4)

#Reading in the variable names and removing the hash from the first variable name - #YY becomes YY
names <- scan(file, nlines = 1, what = character()) %>% 
  str_remove("#")

#Reading in the units in the second row, removing the hash from the first unit and replacing / with "_per_" as / is treated as a special character
units <- scan(file, skip = 1, nlines = 1, what = character()) %>% 
  str_remove("#") %>% 
  str_replace("/", "_per_")

#Pasting variable names and units together separated by _ to create the column names
names(buoy2) <- paste(names, units, sep = "_") 

############################################TASK 3############################################

#Importing data but the headers are mainly in the 3rd row so must skip the first 2 rows

#Reading in the data
filesol <- "data/Y101_Y102_Y201_Y202_Y101-5.csv"

#Skip first 2 rows as they dont include important info and tidy
sol <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()

#Filter the data to keep rows of human proteins identified by more than one peptide
sol <- sol %>% 
  filter(str_detect(description, "OS=Homo sapiens")) %>% 
  filter(x1pep == "x")

#Extract genename from the description and put it into a column
sol <- sol %>%
  mutate(genename =  str_extract(description,"GN=[^\\s]+") %>% 
           str_replace("GN=", ""))

#Extract top protein identifier and put in column protid
sol <- sol %>%
  mutate(protid =  str_extract(accession, ".::[^|;]+") %>% 
           str_replace(".::", ""))

#Reading in a new dataframe to create a column for protein abundance and cell lineage and replicate
filesol <- "data/Y101_Y102_Y201_Y202_Y101-5.csv"

#Skip first 2 rows as they dont include important info and tidy
sol2 <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()

#Converting the data to tidy format - giving abundance and lineage_rep their own column
sol2 <- sol2 %>% 
  pivot_longer(names_to = "lineage_rep",
               values_to = "abundance",
               cols = -c(accession, peptide_count, unique_peptides, confidence_score, anova_p, q_value,
                         max_fold_change, power, highest_mean_condition, lowest_mean_condition, mass, description, x1pep))

#Creating unique columns for lineage and rep
#NOT FINISHED
names <- scan(sol2, nlines = 1, what = character()) %>% 
  str_remove("_")

#NOT FINISHED
units <- scan(file, skip = 1, nlines = 1, what = character()) %>% 
  str_remove("#") %>% 
  str_replace("/", "_per_")

#NOT FINISHED
names(buoy2) <- paste(names, units, sep = "_") 
