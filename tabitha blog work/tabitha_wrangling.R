#Tabitha Tatusko
#Covid Publications Analysis Data Wrangling

#import libraries
library(tidyverse)
library(janitor)
library(datasets)
library(tidytext)
library(textdata)

#import data
publications_info <- read.csv("data/covid19_publications.csv")

#clean column names
pubs_info <- publications_info %>%
  clean_names()

#wrangle data
pubs_data <- pubs_info %>%
  select(pub_year, country_of_research_organization, title) %>%
  filter(complete.cases(.)) %>%
  filter(!is.na(country_of_research_organization)) %>%
  filter(country_of_research_organization != "")

#save wrangled data
saveRDS(pubs_data, file = "data/covid_pubs.RDs")