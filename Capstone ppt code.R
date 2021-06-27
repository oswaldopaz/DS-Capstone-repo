
#### CODE FOR CAPSTONE PROJECT, DATA SCIENCE SPECIALIZATION (COURSERA)

## Libraries:

library(tm)
library(readr)
library(tidyr)
library(dplyr)
library(qdap)
library(tidytext)
library(markovchain)
library(stringr)

## Data importing and cleaning

Blogs <- readLines("en_US.blogs.txt",skipNul = TRUE, warn = F)
News <- readLines("en_US.news.txt",skipNul = TRUE, warn = F)
Tweets <- readLines("en_US.twitter.txt",skipNul = TRUE, warn = F)

Blogs2k <- Blogs[1:2000]
News2k <- News[1:2000]
Tweets2k <- Tweets[1:2000]

Data6k <- rbind(Blogs2k,News2k,Tweets2k)

## text cleaning ##
Data6k_clean <- Data6k %>% tolower() %>% 
  
  str_replace(pattern = "--", " ") %>% 
  removeNumbers() %>% 
  replace_contraction() %>% 
  stripWhitespace() %>% 
  str_replace_all("mrs[.]", "mistress") %>% 
  str_replace_all("mr[.]", "mister") %>% 
  removePunctuation() 

Data6k_clean %>% tail(5)

## Tokenizing (1 - grams)

token1 <- Data6k_clean %>% 
  strsplit(" ") %>% 
  unlist() %>% 
  head(2000)

token1 %>% tail(15)

## Tokenizing (2 - grams)

token2 <- Data6k_clean %>% as.data.frame() %>% 
  mutate(text=Data6k_clean) %>% 
  head(2000) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  pull(bigram) %>% 
  tail(2000)

token2 %>% tail(5)

## Tokenizing (3 - grams)

token3 <- Data6k_clean %>% as.data.frame() %>% 
  mutate(text=Data6k_clean) %>% 
  head(2000) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>% 
  pull(bigram) %>% 
  tail(2000)

head(token3)

## Sentence prediction/make-up using monograms

Markov_fit_1 <- markovchainFit(token1)

for (i in 1:5) {
  
  set.seed(1984+i)
  markovchainSequence(n = 9,
                      markovchain = Markov_fit_1$estimate, 
                      t0 = "the", include.t0 = T) %>%  
    paste(collapse = " ") %>% 
    paste0(".") %>% 
    print()
}

## Monogram prediction
predictiveMonogram <- function(text, num_word){
  text <- strsplit(text, " ") %>% unlist() %>% tail(1)
  
  punctuation <- which(Markov_fit_1$estimate[ tolower(text), ] %>% names() %>% str_detect("[:punct:]"))
  
  suggest <- Markov_fit_1$estimate[ tolower(text), -punctuation] %>%
    sort(decreasing = T) %>% 
    head(num_word) 
  
  suggest <- suggest[suggest > 0] %>% 
    names()
  
  return(suggest)
}

predictiveMonogram("For",10)

## Bigram prediction

Markov_fit_2 <- markovchainFit(token2)

predictiveBigram <- function(text, num_word){
  
  suggest <- Markov_fit_2$estimate[ tolower(text), ] %>%
    sort(decreasing = T) %>% 
    head(num_word) 
  
  suggest <- suggest[ suggest > 0 ] %>% 
    names() 
  
  return(suggest)
}

predictiveBigram("I am",5)

## Trigram prediction

Markov_fit_3 <- markovchainFit(token3)

predictiveTrigram <- function(text, num_word){
  
  suggest <- Markov_fit_3$estimate[ tolower(text), ] %>%
    sort(decreasing = T) %>% 
    head(num_word) 
  
  suggest <- suggest[ suggest > 0 ] %>% 
    names() 
  
  return(suggest)
}

predictiveTrigram("We know what",5)

## END ##


























