#Tabitha Tatusko
#Covid Publications Analysis Data Wrangling

#import libraries
library(tidyverse)
library(janitor)
library(datasets)
library(tidytext)
library(textdata)
library(stringr)

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


#create publication prevalence by country barchart
country_pubs <- pubs_data %>%
  separate(country_of_research_organization
           , into=c('Country 1', 'Country 2', 'Country 3', 'Country 4')
           , sep = ";")
  


#create top words barchart
data(stop_words)

#seperate words from titles
common_topics_all <- pubs_data %>%
  select(!country_of_research_organization) %>%
  unnest_tokens(output = word, input = title)

#remove common stop words and additional coivd related stop words
common_topics <- common_topics_all %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = T) %>%
  filter(!word %in% c("covid" , "19", "pandemic", "sars", "cov", "2", "coronavirus", "2020", "2019",
                      "de", "la")) #also removed spanish stop words de/la

#create bar cgart of top 15 most common words in titles
common_topics %>%
  slice(1:15) %>%
  ggplot(aes(x = fct_reorder(word, n), y = n, color = word, fill = word)) +
  geom_col() +
  # Rotate graph
  coord_flip() +
  guides(color = "none", 
         fill = "none") +
  labs(
    title = "Common Topics in Covid Publications",
    y = "Number of Occurences",
    x = "Topic Word"
  )
