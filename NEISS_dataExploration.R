# Created by ER 4/28/25

# Load packages
library(shiny)
library(vroom) # Fast file reading 
library(tidyverse)

# Get 2017 NEISS data
dir.create("neiss")
download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

# Call injuries dataset
# 255,000 observations with 10 variables:
  # trmt_date: date person was seen in hospital
  # age, sex, and race: demographic info
  # body_part: location of injury
  # diag: diagnosis
  # prod_code: primary product assoc with injury
  # weight: statistical weight 
  # narrative: how accident occured 

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

# Pair with products data frame to look up product name from code
# Pair with population data frame to get total pop in 2017 for each age/sex

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")

# Data Exploration 
# Explore product 649 (toilets) 
selected <- injuries %>% 
  filter(prod_code == 649)
nrow(selected)

# Summary of toilet injuries, weighted by weight variable so counts = estimated
# total injuries acorss US 
selected %>% count(location, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)
selected %>% count(diag, wt = weight, sort = TRUE)

# Plot pattern of toilet injuries by age and sex
summary <- selected %>% 
  count(age, sex, wt = weight)
summary

summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

# Calculate injury rate to control for population size for each age group
summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4) #per 10,000 people
summary

# Plot injury rate per 10,000 people
summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

# Look at narratives 
selected %>% 
  sample_n(10) %>% 
  pull(narrative)
