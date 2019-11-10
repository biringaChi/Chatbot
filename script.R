# Building a chatbot using Twitter Customer Response data from major companies
library(tidyverse)
library(quanteda)
library(ggplot2)
library(stringi)
library(stringr)
library(e1071)
twitter.df <- read_csv("data/twcs.csv", col_names = TRUE)

# Building action-reaction pairs
# Action

action <- twitter.df %>% 
  filter(inbound == TRUE) %>% 
  select(text) %>% 
  mutate(obs_measure = nchar(text, type = "chars", allowNA = TRUE)) %>% 
  as_tibble()

action$brand <- stri_extract_all_regex(action$text, "^@[a-zA-z_]+")
action$brand <- stri_replace_all_regex(action$brand,"@", "")

colSums(is.na(action))
unique(action$brand)

# Distribution of observation measure
action %>% 
  ggplot(aes(x = obs_measure)) + 
  theme_grey() +
  geom_histogram(binwidth = 5) +
  labs(y = "Count", x = "Tweet length",  title="Distribution of Tweet Length")

# Action experimentaion
bag.of.words <- function(data, word, ngram) {
  corpus <- tokens(data, what = word, remove_numbers = TRUE,  remove_punct = TRUE, 
                       remove_symbols = TRUE, ngrams = ngram, remove_url = TRUE)
  corpus <- tokens_tolower(corpus)
  corpus.dfm <- dfm(corpus)
  corpus.matrix <- as.matrix(corpus.dfm)
}

result.matrix <- bag.of.words(action$text[1:1000], "word",  ngram = 2L)

plot.prep <- function() {
  result.matrix.df <- as.data.frame(word = colnames(result.matrix), freq = result.matrix[1,])
  result.matrix.df <- result.matrix.df[order(-result.matrix.df$freq), ][1:20, ]
  result.matrix.df$word <- factor(result.matrix.df$word, as.character(result.matrix.df$word))
}

plot.prep.df <- plot.prep()


ggplot(plot.prep.df, aes(x=word, y=freq)) +
  geom_bar(stat="Identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Frequency", x = "n-Grams = 2",
       title="Top 10 most frequent word-Level n-Grams")



ggplot(plot.prep.df, aes(x = word, y = freq.row)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = "Frequency", x = "n-Grams = 2",
       title="Top 10 most frequent word-Level n-Grams")

# Reaction
reaction <- twitter.df %>% 
  filter(inbound == FALSE) %>% 
  select(text)

# Model sampling
sample.action <- action[1:1000, 1]
sample.reaction <- reaction[1:1000, 1]

# Model building
sample.action$text <- str_remove_all(sample.action$text, "[^\x01-\x7F]")
sample.action.tokens <- tokens(sample.action$text, what="word", remove_numbers = TRUE,  remove_punct = TRUE, 
                        remove_symbols = TRUE, remove_url = TRUE)
sample.action.tokens <- tokens_tolower(sample.action.tokens)
sample.action.tokens <- tokens_wordstem(sample.action.tokens, language = quanteda_options("language_stemmer"))
sample.action.tokens.dfm <- dfm(sample.action.tokens)
action.reaction.df <- cbind(reaction = sample.reaction$text, convert(sample.action.tokens.dfm, to = "data.frame"))

svm.model <-  svm(reaction ~., action.reaction.df, kernel = "linear", cost = 100, scale = FALSE)

# Started training by 4:28
















