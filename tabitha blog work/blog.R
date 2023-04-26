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
  rename(country = country_of_research_organization) %>%
  unnest_tokens(output = country, input = country, 
                token = 'regex', pattern=";", to_lower = F) %>%
  mutate(across(where(is.character), str_trim)) %>%
  #this removes duplicate countries for one publication
  #ex: first title had 5 Kuwait authors, but still only 1 pub coming from Kuwait
  group_by(country) %>%
  filter(title!=lag(title) | is.na(lag(title))) %>%
  group_by(pub_year) %>%
  count(country, sort = T)

country_pubs %>%
  slice(1:10) %>%
  ggplot(aes(x = fct_reorder(country, n), y = n, color = country, fill = country)) +
  geom_col() +
  geom_label(aes(label = n), color = "black", fill = "white", 
             label.padding = unit(0.1, "lines"), label.size = NA) +
  facet_grid(. ~ pub_year) +
  # Rotate graph
  coord_flip() +
  guides(color = "none", 
         fill = "none") +
  labs(
    title = "Top Publication Producing Countries in 2020 and 2021",
    x = "Publication Count",
    y = "Country"
  )
  


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
