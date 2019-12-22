# Building a chatbot using Twitter Customer Response data from major companies
# More string cleanig to be done 

library(tidyverse)
library(quanteda)
library(ggplot2)
library(stringi)
library(stringr)
library(doSNOW)
library(e1071)

twitter.df <- read_csv("data/twcs.csv", col_names = TRUE, n_max = 10000)

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

#----------------------------------------------------------------------------------------
# Action experimentaion
bag.of.words <- function(data, word, ngram, corpus.dfm = FALSE) {
  corpus <- tokens(data, what = word, remove_numbers = TRUE,  remove_punct = TRUE, 
                       remove_symbols = TRUE, ngrams = ngram, remove_url = TRUE)
  corpus <- tokens_tolower(corpus)
  corpus.dfm <- dfm(corpus)
  corpus.dfm <<- as.matrix(corpus.dfm)
}

bag.of.words(action$text[1:1000], "word",  ngram = 2L)
#----------------------------------------------------------------------------------------

# for character
char.result.matrix <- bag.of.words(action$text[1:1000], "char",  ngram = 4L)   

plot.prep <- function(data) {
  result.matrix.df <- data.frame(word = colnames(data), frequency= data[1,])
  result.matrix.df <- result.matrix.df[order(-result.matrix.df$frequency), ][1:10, ]
  result.matrix.df$word <- factor(result.matrix.df$word, as.character(result.matrix.df$word))
  return(result.matrix.df)
}

#word.plot <- plot.prep(result.matrix)
#char.plot <- plot.prep(char.result.matrix)


ggplot(plot.prep.df, aes(x = word, y = frequency)) +
  geom_bar(stat="Identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Frequency", x = "n-Grams = 2",
       title="Top 10 most frequent word-Level n-Grams")

ggplot(char.plot, aes(x = word, y = frequency)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = "Frequency", x = "n-Grams = 4",
       title="Top 10 most frequent char-Level n-Grams")

#----------------------------------------------------------------------------------------
# Reaction
reaction <- twitter.df %>% 
  filter(inbound == FALSE) %>% 
  select(text)
#----------------------------------------------------------------------------------------

# Model sampling  # main train 
sample.action <- action[1:1000, 1]
sample.reaction <- reaction[1:1000, 1]

# Model building
sample.action$text <- str_remove_all(sample.action$text, "[^\x01-\x7F]")
sample.action.tokens <- tokens(sample.action$text, what="word", remove_numbers = TRUE,  remove_punct = TRUE, 
                        remove_symbols = TRUE, remove_url = TRUE)
sample.action.tokens <- tokens_tolower(sample.action.tokens)
sample.action.tokens <- tokens_wordstem(sample.action.tokens, language = quanteda_options("language_stemmer"))
sample.action.tokens.dfm <- dfm(sample.action.tokens)
train_action_df <- as.data.frame(as.matrix(sample.action.tokens.dfm))
#action.reaction.df <- cbind(reaction = sample.reaction$text, convert(sample.action.tokens.matrix, to = "data.frame"))

# experiment from vids
# Adding reaction data
reaction.df <- as.data.frame(as.matrix(sample.reaction$text)) 

# incoporate the response column to the training matrix and convert to a dataframe
data_train <- cbind(sample.reaction$text, train_action_df)
data_train <- as.data.frame(data_train) %>% rename(response = "sample.reaction$text")

set.seed(1234)
start <- Sys.time()
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
model <-  svm(response ~., data_train, kernel = "linear", cost = 100, scale = FALSE)
stopCluster(cl)
end <- Sys.time() - start
end

model
#----------------------------------------------------------------------------------------
# propose a testing question and build the prediction model
test <- "Hey There"
test.tokens <- tokens(test, what="word", remove_numbers = TRUE,  remove_punct = TRUE, 
                      remove_symbols = TRUE, remove_url = TRUE)
test.tokens <- tokens_tolower(test.tokens)
test.tokens <- tokens_wordstem(test.tokens, language = quanteda_options("language_stemmer"))
test.tokens.dfm <- dfm(test.tokens)
test.tokens.df <- as.data.frame(as.matrix(test.tokens.dfm)) 

# Merge the tesing dtm(df) with the training dtm(df)
test_data <- test.tokens.df[1,]
test_data[test_data == 1] =  0
test_train <- data_train %>% select(-response)
test_data <- cbind(test_train, test_data)

p <- predict(model, test_data)
answer <- as.character(p)

# predict
paste("Answer:", answer)



# Test data
sample.action.test <- action[1000:1050, 1]
test.tokens <- tokens(sample.action.test$text, what="word", remove_numbers = TRUE,  remove_punct = TRUE, 
                               remove_symbols = TRUE, remove_url = TRUE)
test.tokens <- tokens_tolower(test.tokens)
test.tokens <- tokens_wordstem(test.tokens, language = quanteda_options("language_stemmer"))
test.tokens.dfm <- dfm(test.tokens)
test.tokens.dfm <- dfm_select(test.tokens.dfm, pattern = sample.action.tokens.dfm, selection = "keep")
test.tokens.matrix <- as.matrix(test.tokens.dfm)


test.tokens.df <- cbind(reaction = sample.reaction$text, convert(test.tokens.dfm, to = "data.frame"))

predictions <- predict(svm.model, test.tokens.matrix)
response <- as.character(predictions)

# Predict
paste("Response:", response)
}













