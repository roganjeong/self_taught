############################################
# Statistical Modelling & Machine Learning #
#               R Example6                 #
############################################

install.packages('quanteda')
install.packages('readtext')
install.packages('tidyverse')
install.packages('tidytext')

library(quanteda)
library(readtext)
library(tidyverse)
library(tidytext)


# Bar plot function
facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
} 


# Read external document files
dat <- readtext("txt/*")

# Select top 10 words for each doc.
doc_common <- dat %>%   
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(doc_id, word) %>% 
  group_by(doc_id) %>% 
  top_n(10) %>% 
  ungroup()
# unnest_tokens: split input(text) into tokens(word).
# anti_join: Filtering.
# count: count the unique values of one or more variables.
# group_by: convert an existing table into a grouped table.
# top_n: select n highest values.
# ungroup: remove grouping.


facet_bar(doc_common,
          y = word,
          x = n,
          by = doc_id,
          nrow = 3, ncol = 3)+
  labs(title = "Top 10 common words in articles",x = "")


# Sentiment analysis

# get_sentiments(): Recall a dictionary for sentiment.
# Dictionaries are based on unigram.
# Type of dictionary: 
# 1. 'nrc': positive, negative, anger, anticipation, disgust, 
#           fear, joy, sadness, surprise, and trust. 
# 2. 'bing': positive and negative.
# 3. 'AFINN': score that runs between -5 and 5, 
#             negative scores -> negative sentiment
#             positive scores -> positive sentiment.

# bing lexicon with positive words
bing_pos <- get_sentiments('bing') %>%
  filter(sentiment == 'positive')

# Tokenizing: unigram.
doc <- dat %>%   
  ungroup() %>%
  unnest_tokens(word, text)

# List and count positive words in doc2.txt.
doc %>%
  filter(doc_id == 'doc2.txt') %>%
  inner_join(bing_pos) %>%
  count(word, sort = TRUE)

# Positive and negative word frequencies for each doc.
doc_sentiment <- doc %>%
  inner_join(get_sentiments('bing')) %>%
  count(doc_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, 
              values_fill = list(n = 0)) %>%
  mutate(pos_diff = positive - negative)

doc_sentiment


# Word Cloud
install.packages('ggwordcloud')
library(ggwordcloud)

# Word cloud for words with high frequencies.
wc <- doc %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(sentiment, word, sort = T) %>% 
  top_n(20)

print(as_tibble(wc), n = 25)

wc %>%
  ggplot() + 
  geom_text_wordcloud_area(aes(label = word, size = n)) +
  scale_size_area(max_size = 15)


# TF-IDF
doc_words <- dat %>%
  unnest_tokens(word, text) %>% 
  add_count(doc_id, name = "total_words") %>%
  group_by(doc_id, total_words) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

doc_words
# total_words: # of all words in a document.

# Calculate TF-IDF.
doc_words <- doc_words %>% 
  select(-total_words) %>%
  bind_tf_idf(term = word, document = doc_id, n = n)

# Plot for 10 words with highest TF-IDF values.
doc_words %>% 
  group_by(doc_id) %>% 
  top_n(10) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = doc_id, 
            nrow = 3, ncol = 3)

# Tokenizing by n gram
doc_bigrams <- dat %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

doc_bigrams %>%
  count(bigram, sort = TRUE)

# Filtering stop words
# If there is a stop word in a bigram token, remove it.
doc_separated <- doc_bigrams %>%  
  separate(bigram, into = c("word1", "word2"), sep = " ")

doc_united <- doc_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, c(word1, word2), sep = " ")

doc_united %>% count(bigram, sort = TRUE)

# Plot for top 2 bigram words
doc_united %>% 
  count(doc_id, bigram, sort = TRUE) %>% 
  bind_tf_idf(term = bigram, document = doc_id, n = n) %>% 
  group_by(doc_id) %>% 
  top_n(2) %>% 
  ungroup() %>%
  facet_bar(y = bigram, x=tf_idf, by=doc_id, nrow=3, ncol=3)


# Network of bigram
install.packages('tidygraph')
install.packages('ggraph')

library(tidygraph)
library(ggraph)

bigram_counts <- doc_separated %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) 

bigram_graph <- bigram_counts %>% 
  filter(n > 5) %>%
  as_tbl_graph()

ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# Construct a corpus
cop <- corpus(dat)

# Document term matrix (Document feature matrix)
dfmat <- cop %>% 
  tokens(remove_punct=T, remove_symbols=T) %>%
  tokens_select(pattern = stopwords("en"), selection = "remove") %>%
  dfm()

# TF-IDF matrix
tf_idfmat <- dfm_tfidf(dfmat)


# Clustering documents using dtm
install.packages('quanteda.textstats')
library(quanteda.textstats)

text_dist <- as.dist(textstat_dist(dfmat))
clust <- hclust(text_dist, method = 'average')
plot(clust, xlab = "Distance", ylab = "")

# Clustering documents using tf-idf
text_dist <- as.dist(textstat_dist(tf_idfmat))
clust <- hclust(text_dist, method = 'average')
plot(clust, xlab = "Distance", ylab = "")


# Topic model: LDA

install.packages('tm')
install.packages('topicmodels')

library(tm)
library(topicmodels)

docs <- Corpus(DirSource('./txt/'))  

docs <- docs %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

# Document term matrix.
dtm = DocumentTermMatrix(docs)

# LDA
doc_lda <- LDA(dtm, k = 2, control = list(seed = 1234))

# Word-topic probability
doc_topics <- tidy(doc_lda, matrix = "beta")
doc_topics

# Top ten words by topic
library(ggplot2)
library(dplyr)
top_terms <- doc_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Document-topic probability
doc_documents <- tidy(doc_lda, matrix = "gamma")
doc_documents

# Top terms for each document
tidy(dtm) %>%
  filter(document == 'doc6.txt') %>%
  arrange(desc(count))
## Topic 1: Hardware of iphone

tidy(dtm) %>%
  filter(document == 'doc3.txt') %>%
  arrange(desc(count))
## Topic 2: Apps in iphone
