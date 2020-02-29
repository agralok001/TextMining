  #Basic Text Mining on Customer Comments

#Housekeeping

choose.dir()
getwd()
setwd("C:\\Users\\Alok\\Google Drive\\Analytics\\Github\\TextMining\\TextMining")
#load the dataset
list.files()

data<-read.csv("Customer_comment.csv",fill = TRUE)
data
trim(data)

data_3<-apply(data,2,function(x)gsub('\\s+', ' ',x))
data_4<-apply(data_3,2,function(x)gsub('.', ' ',x))

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(tidyr)
library(scales)
library(janeaustenr)

#rlang::last_error()
text_df<-data_frame(line=1:63, text=data_3)
text_df
#last_error()

TDF_1 <-text_df %>% unnest_tokens(word,text)
#View(nwe_data)


data(stop_words)
stop_words

TDF_1 %>% anti_join(stop_words)

#wORD cOUNTS
TDF_1 %>%
  count(word, sort = TRUE) 

TDF_1 %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) 


TDF_1 %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


TDF_1 %>% anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


###Sentiment Analysis
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

TDF_1

sentiment_bing <- TDF_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

sentiment_bing

#ggplot(sentiment_bing, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word, ncol = 2, scales = "free_x")


  afinn <- TDF_1 %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = line %/% 80) %>% 
    summarise(sentiment = sum(value)) %>% 
    mutate(method = "AFINN")
  
  bing_and_nrc <- bind_rows(TDF_1 %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            TDF_1 %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
    count(method, index = line %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

  bind_rows(afinn, 
            bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")

  
  bing_word_counts <- TDF_1 %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts  
table(bing_word_counts$sentiment,bing_word_counts$n)

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#Wordcloud
library(wordcloud)

TDF_1 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Bigrams
TDF_1 %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

