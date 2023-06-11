#Installing Packages
install.packages('tidyverse')
install.packages('tidytext')
install.packages('wordcloud')
install.packages('janeausten')
install.packages('textdata')
install.packages('reshape2')

#Loading Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidytext)
library(readr)
library(wordcloud)
library(RColorBrewer)
library(janeaustenr)
library(textdata)
library(reshape2)

#Deploying Gutenberg package
install.packages('gutenbergr')
library(gutenbergr)

AuthorsData <- gutenberg_metadata

n_distinct(AuthorsData$author)
n_distinct(AuthorsData$title)

#Lets check id=32325 adventure of Huckleberry Finn
#and id=50109 the mysterious stranger
#and id=102 The Tragedy of Pudd'nhead Wilson

#Lets make a accronoym for each

HF <- gutenberg_download(gutenberg_id = 32325)
MS <- gutenberg_download(gutenberg_id = 50109)
PW <- gutenberg_download(gutenberg_id = 102)

#Lets Tokenize all three data frames
HF <- HF %>%
  unnest_tokens(word, text)

MS <- MS %>%
  unnest_tokens(word, text)

PW <- PW %>%
  unnest_tokens(word, text)

#Lets remove stopwords
HF <- HF %>%
  anti_join(stop_words)

MS <- MS %>%
  anti_join(stop_words)

PW <- PW %>%
  anti_join(stop_words)

#Lets create a plot
HF %>%
  count(word, sort=TRUE) %>%
  filter(n > 120) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="darkgrey") +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()

MS %>%
  count(word, sort=TRUE) %>%
  filter(n > 35) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="orange") +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()

PW %>%
  count(word, sort=TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="darkred") +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()

#Creating a wordcloud
HF_Wordccloud <- HF %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50))

MS_Wordccloud <- MS %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50))

PW_Wordccloud <- PW %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50))

#Check sentiment
NRC <- get_sentiments("nrc")
Bing <- get_sentiments("bing")
Afinn <- get_sentiments("afinn")

#Lets take one book to do a sentiment analysis using all three lexicons
#Let's choose The Mysterios Stranger MS

#Lets reload it to add a line number
MS <- gutenberg_download(gutenberg_id = 50109)

#Tokenization
MS <- MS %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Joining the Lexicons
MS_Afinn <- MS %>%
  inner_join(Afinn) %>%
  group_by(index= linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "afinn")

MS_Bing <- MS %>%
  inner_join(Bing) %>%
  mutate(method = "bing")

MS_NRC <- MS %>%
  inner_join(NRC) %>%
  filter(sentiment %in% c("positive","negative")) %>%
  mutate(method = "NRC")

#Binding NRC and Bing
Bing_NRC <- bind_rows(MS_Bing, MS_NRC) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill= 0) %>%
  mutate(sentiment = positive - negative)

#Plotting
Plot <- bind_rows(MS_Afinn, Bing_NRC) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~method, ncol=1 , scales = "free_y")
Plot

#Creating a wordcloud with the positive and negative words
MS <- MS %>%
  anti_join(stop_words)

MS_WordcloudNEW <- MS %>%
  inner_join(Bing) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(colors = c("grey20","grey60"), max.words = 50, scale=c(4,.2))