############
### PATH ###
############

# getwd()
setwd("/Users/alexvlima/Documents/GitHub/Text-Mining-Twitter/")

#################
### LIBRARIES ###
#################

library(twitteR)
library(tidytext)
library(tidyverse)
library(tm)
library(stringr)
library(wordcloud)
library(lexiconPT)
library(ptstem)
library(topicmodels)

#################
### API - KEY ###
#################

source("~/Library/Mobile Documents/com~apple~CloudDocs/Pessoal/twitter-key.R")

####################
### TREND TOPICS ###
####################

# woeid -> where on earth id
location <- availableTrendLocations()

BR <- location %>%
  filter(country == "Brazil")

# 23424768 - codigo do Brazil
trendsBrazil <- getTrends(woeid = 23424768)$name
# 20 primeiros apenas
trendsBrazil[1:20]

rm(list = ls())

###############
### DATASET ###
###############

tweets_carlos_bolsonaro <- userTimeline("CarlosBolsonaro",
                                        n = 3200, 
                                        excludeReplies = T,
                                        includeRts = F)
df_cb <- twListToDF(tweets_carlos_bolsonaro)

tweets_marcelo_freixo <- userTimeline("MarceloFreixo",
                                        n = 3200, 
                                        excludeReplies = T,
                                        includeRts = F)
df_mf <- twListToDF(tweets_marcelo_freixo)

tweets_rodrigo_maia <- userTimeline("RodrigoMaia",
                                      n = 3200, 
                                      excludeReplies = T,
                                      includeRts = F)
df_rm <- twListToDF(tweets_rodrigo_maia)

rm(list = ls(pattern = "tweets*"))
   
df <- 
  bind_rows(df_cb, df_mf, df_rm) %>%
  select(text, screenName, created)

rm(list = ls(pattern = "df_"))

#################
### WRANGLING ###
#################

glimpse(df)
source("rm_accent.R")

### Tokenization ###
df_words <- df %>%
  unnest_tokens(words, text, to_lower = T) %>%
  mutate(words = removePunctuation(words)) %>%
  filter(!is.na(words))
                                 
df_words2 <- df_words

### Stopwords ###
df_words2$words <- removeWords(df_words2$words, c("https*", "tco", "pra", "sobre", 
                                                  "a454nb9gfs","0001f3fb","0001f449",
                                                  "2jj24ruot8","egzbclv8kc","u90ffrryw5",
                                                  "c2wll0sjt9","0001f602","0001f44d","0001f4f8",
                                                  stopwords("en"), stopwords("pt")))

### Remove words with 1 character e empty fields ###
df_words2 <-
  df_words2 %>%
  mutate(count_letters = nchar(df_words2$words)) %>%
  filter(df_words2$words != "", count_letters > 1) 
  
#################
### WORDCLOUD ###
#################

df_wordcloud <- 
  df_words2 %>%
  group_by(words,screenName) %>%
  summarise(n = n()) 

set.seed(123)
df_wordcloud %>% 
  filter(screenName == "CarlosBolsonaro") %>%
  with(wordcloud(words, n, max.words = 100, 
                 scale = c(4,0.5), random.order = F,
                 colors = brewer.pal(8,"Dark2")))

df_wordcloud %>% 
  filter(screenName == "MarceloFreixo") %>%
  with(wordcloud(words, n, max.words = 100, 
                 scale = c(4,0.5), random.order = F,
                 colors = brewer.pal(8,"Dark2")))

df_wordcloud %>% 
  filter(screenName == "RodrigoMaia") %>%
  with(wordcloud(words, n, max.words = 100, 
                 scale = c(4,0.5), random.order = F,
                 colors = brewer.pal(8,"Dark2")))

rm(df_wordcloud)

############################
### SENTIMENTAL ANALYSIS ###
############################

### Dictionaries in PT ###
data("sentiLex_lem_PT02") 
sentiLex_lem_PT02

sentiLex_lem_PT02$words <- gsub(pattern = "-se", replacement = "", sentiLex_lem_PT02$term)

### Stemming ###
df_words2$stem <- ptstem(df_words2$words, algorithm = "rslp", complete = FALSE)
sentiLex_lem_PT02$stem <- ptstem(sentiLex_lem_PT02$words, algorithm = "rslp", complete = FALSE)

df_sentiment <- 
  df_words2 %>%
  inner_join(sentiLex_lem_PT02, by = c("stem" = "stem")) 
glimpse(df_sentiment)

df_sentiment %>% 
  filter(screenName == "CarlosBolsonaro") %>%
  group_by(created) %>%
  summarise(sentimento = sum(polarity)) %>%
  mutate(data = lubridate::floor_date(created, "week")) %>%
  ggplot(aes(x = data, y = sentimento, fill = sentimento > 0)) +
  geom_col() +
  # ggtitle("Gráfico do Sentimento dos Tweets do Carlos Bolsonaro") +
  ylab ("") +
  xlab("") +
  ylim(-60, 60) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

df_sentiment %>%
  filter(screenName == "MarceloFreixo") %>%
  group_by(created) %>%
  summarise(sentimento = sum(polarity)) %>%
  mutate(data = lubridate::floor_date(created, "week")) %>%
  ggplot(aes(x = data, y = sentimento, fill = sentimento > 0)) +
  geom_col() +
  # ggtitle("Gráfico do Sentimento dos Tweets do Marcelo Freixo") +
  ylab ("") +
  xlab("") +
  ylim(-60,60) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

df_sentiment %>%
  filter(screenName == "RodrigoMaia") %>%
  group_by(created) %>%
  summarise(sentimento = sum(polarity)) %>%
  mutate(data = lubridate::floor_date(created, "week")) %>%
  ggplot(aes(x = data, y = sentimento, fill = sentimento > 0)) +
  geom_col() +
  # ggtitle("Gráfico do Sentimento dos Tweets do Rodrigo Maia") +
  ylab ("") +
  xlab("") +
  ylim(-60,60) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")

df_sentiment %>%
  group_by(created, screenName) %>%
  summarise(sentimento = sum(polarity)) %>%
  mutate(data = lubridate::floor_date(created, "week")) %>%
  ggplot(aes(x = data, y = sentimento, fill = sentimento > 0)) +
  geom_col() +
  # ggtitle("Gráfico do Sentimento dos Tweets do Rodrigo Maia") +
  ylab ("") +
  xlab("") +
  ylim(-60,60) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") +
  facet_wrap(~screenName, scales = "free_x")

rm(rm_accent, df_sentiment, sentiLex_lem_PT02)

##############
### TF-IDF ###
##############

df_words3 <- df_words2 %>% count(screenName) %>% mutate(total = n) %>% select(-n)
df_words4 <- df_words2 %>% count(screenName, words)

df_words4 <- df_words4 %>%
  left_join(df_words3, by=c("screenName"="screenName"))

### Count versus Frequency ###
ggplot(df_words4, aes(x=n/total, fill=screenName)) +
  geom_histogram(show.legend=FALSE) + 
  facet_wrap(~screenName, ncol=3, scales="free") + 
  xlim(0,0.01) +
  theme_minimal()

### Term Frequency versus Using Ranking ###
### (TF = 1/rank) ###

df_words4 <- df_words4 %>%
  mutate(tf=n/total) %>% arrange(screenName, desc(n)) %>% 
  group_by(screenName) %>% mutate(rank=row_number()) %>% ungroup() %>%
  arrange(desc(n))

fit <- lm(data = filter(df_words4, rank >= 10 & rank <= 500),
             formula = log10(tf) ~ log10(rank))

ggplot(df_words4, aes(x = rank, y = tf, color = screenName)) +
  geom_line(alpha = 0.8, size = 1.1, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10() +
  xlab("rank") + ylab("term frequency") +
  geom_abline(intercept = fit$coefficients[[1]], 
              slope = fit$coefficients[[2]],
              color = "gray50", lty = 2) +
  theme_minimal()

### IDF ###
df_words4 <- bind_tf_idf(tbl = df_words4,
                       term = words,
                       document = screenName,
                       n = n)

arrange(df_words4, desc(tf_idf))

df_words4 %>% group_by(screenName) %>% top_n(10, tf_idf) %>%
  ungroup() %>% mutate(word = reorder(words, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf, fill = screenName)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~screenName, ncol = 3, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "tf-idf") + 
  theme_minimal()

rm(df_words3, df_words4, fit)

######################
### TOPIC MODELING ###
######################

df_words3 <- 
  df_words2 %>%
  group_by(screenName, words) %>%
  summarise(n = n())

dtm_words <- df_words3 %>%
  cast_dtm(document = screenName, term = words, value = n)
dtm_words

fit_lda <- LDA(dtm_words, k = 3, control = list(seed = 123))

words_by_politician <- tidy(fit_lda, matrix = "beta")

top_terms <- words_by_politician %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() 
