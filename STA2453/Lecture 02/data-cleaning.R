
# Load the required libraries
# To install, use install.packages('tidyverse')
library(tidyverse)


# THE ISSUE:
# Column headers are values, not variable names-

# Look at the first 6 observations
head(relig_income)

# The observation of a row should be a religion/income level
# e.g. The number of Agnostic people with <10K income

# We can use the pivot_longer function from the tidyr package
# for this purpose
# We specify that we want to pivot all columns except for religion
# we want the names of those specified variables to become a new
# column called income. We want the values from each of those
# columns to become a new variable called count
relig_income %>% 
  pivot_longer(-religion, 
               names_to = "income",
               values_to = 'count')



# THE ISSUE:
# An observation should be an artist track week 
# Essentially, each wk column should be an observation for
# a given artist and track. 
# we can again use the pivot_longer function. 
# We specify that we want to pivot on the all the columns
# from wk1 to wk76. We want the names of those colums
# to become a new variable named week, and the values
# from those columns to become a new variable called rank.
billboard %>% 
  pivot_longer(wk1:wk76, 
               names_to = "week",
               values_to = "rank", 
               values_drop_na = T) %>% 
  # transform the week variable to numeric by removing 'wk'
  mutate(week = as.numeric(gsub('wk', '', week)),
         date = as.Date(date.entered) + 7*(week - 1)) %>% 
  arrange(artist, track, week)



# THE ISSUE:
# Multiple variables stored in one column
# This data  comes from the World Health Organisation, 
# and records the counts of confirmed tuberculosis cases 
# by country, year, age group and gender
# Each variable name is storing information for two groups (an age and gender)
# A row in the current format stores data for every available age/gender combination
# A tidy row observation should represent a single country, year, age/gender

tb <- readr::read_csv(here::here('static', 'slides', 'data', 'tb.csv'))

# we use pivot_longer and choose to pivot every column except
# iso2 and year. We move the names of the pivoted columns
# to a variable named age_sex and their values to a variable
# named count. We also drop any missing values
# we then split the age_sex variable into two variables
# sex and age by pulling the first character from the 
# age_sex variable (i.e. either an 'f' or 'm'). The
# remaining characters are the age groupings. 
tb %>% 
  pivot_longer(-c(iso2, year), 
               names_to = "age_sex",
               values_to = 'count', 
               values_drop_na = T) %>% 
  mutate(gender = str_sub(age_sex, 1, 1),
         age = str_sub(age_sex, 2, -1))


# THE ISSUE:
# Variables are stored in both rows and columns
# The raw data can be accessed here (https://github.com/tidyverse/tidyr/tree/master/vignettes)
# Download the data and store somewhere on your local computer

weather <- readr::read_csv(here::here('path to your downloaded weather data', 'weather.csv'))

# an observation in the raw data stores the maximum temperature or 
# minimum temperature for a weather radar for every day of a 
#
# a tidy observation with store the minimum and maximum temperature
# for a single day
# we can pivot_longer and select the day columns (d1 to d31)
# and we move the names of those columns to a variable called
# 'day' and we move their values to a variable called 'temp'

# we then use pivot_wider to to collapse the minimum and maximum
# temperature rows into a single row.
# we are going to colapse the values from the 'element' variable
# which are either tmax or tmin into two new variables called
# 'tmax' and 'tmin'. We are going to use the values from 
# the 'temp' variable to populate 'tmax' and 'tmin'
weather %>% 
  pivot_longer(d1:d31, 
               values_drop_na = T,
               names_to = "day",
               values_to = "temp") %>% 
  # transform day variable to numeric
  mutate(day = as.numeric(gsub('d', '', day))) %>% 
  pivot_wider(names_from = "element",
              values_from = "temp")

# for more info on pivot_longer and
# pivot_wider see their help files
# ?pivot_longer or ?pivot_wider

