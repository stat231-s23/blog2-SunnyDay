#Tabitha Tatusko
#Covid Publications Analysis Data Wrangling

#import libraries
library(tidyverse)
library(janitor)
library(datasets)
library(tidytext)
library(textdata)
library(stringr)
library(plotly)

#import wrangled data
pubs_data <- readRDS(file = "data/covid_pubs.RDs")

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
  count(country, sort = T) %>%
  rename(count = n)

#save wrangled data
saveRDS(country_pubs, file = "data/pubs_by_country.RDs")


#create top words barchart
data(stop_words)

#seperate words from titles
common_topics_all <- pubs_data %>%
  select(!country_of_research_organization) %>%
  unnest_tokens(output = word, input = title)

#remove common stop words and additional covid related stop words
common_topics <- common_topics_all %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = T) %>% #also removed spanish stop words 
  filter(!word %in% c("covid" , "19", "pandemic", "sars", "cov", "2", "coronavirus", "2020", "2019",
                      "based", "disease", "de", "la", "en")) %>% 
  rename(count = n)

#save wrangled data
saveRDS(common_topics, "data/common_topics.RDs")
