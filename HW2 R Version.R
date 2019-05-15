#Q1 Data Collection
##1.1 Scraping

setwd("~/GitHub/MMSS_311_2")
library("xml2")
library("rvest")
library("dplyr")
pg <- read_html("https://en.wikipedia.org/wiki/Category:Member_states_of_the_Association_of_Southeast_Asian_Nations")


countrytext <- pg %>%
  html_nodes(".mw-category-group+ .mw-category-group a") %>%
  html_text()
countrytext

countryurl <- pg %>%
  html_nodes(".mw-category-group+ .mw-category-group a") %>%
  html_attr("href")
countryurl

data <- data.frame(
  countrytext, countryurl, 
  stringsAsFactors = F) %>%
  mutate(countryurl = paste0("https://en.wikipedia.org", countryurl))
head(data)

for(i in 1:nrow(data)) { 
  data$text[i] <- data$countryurl[1] %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = "\n\n")
}

#Q2 Pre-Processing & Word Frequency Analysis
##Q2.1 Pre-Process

library(tm)
tweets <- read.csv("trumptweets.csv")
trumptweets <- Corpus(VectorSource(tweets$text))
trumptweets <- tm_map(trumptweets, removePunctuation)
trumptweets <- tm_map(trumptweets, tolower)
trumptweets <- tm_map(trumptweets, removeWords, stopwords("en"))
trumptweets <- tm_map(trumptweets,stemDocument)

dtm <- DocumentTermMatrix(trumptweets)
dtm <- removeSparseTerms(dtm,.99)

library("tidyverse")
library("tidytext")
tidy_dtm <- tidy(dtm)
dtm.mat <- as.matrix(dtm)

dtm_tfidf <- DocumentTermMatrix(trumptweets, control = list(weighting = weightTfIdf))

##Q2.2 Word Frequency/Dictionary Methods
###(a)

tidy_dtm %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq))

tidy_dtm %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(reorder(term, -freq), freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("word")

###(b)
library(lubridate)
tweets <- mutate(tweets, created_at = mdy_hm(created_at))
tweets_post <- filter(tweets, created_at >= as.Date("2016-12-08"))
tweets_pre <- filter(tweets, created_at < as.Date("2016-12-08"))

trumptweets_pre <- Corpus(VectorSource(tweets_pre$text))
trumptweets_pre <- tm_map(trumptweets_pre, removePunctuation)
trumptweets_pre <- tm_map(trumptweets_pre, tolower)
trumptweets_pre <- tm_map(trumptweets_pre, removeWords, stopwords("en"))
trumptweets_pre <- tm_map(trumptweets_pre,stemDocument)
dtm_pre <- DocumentTermMatrix(trumptweets_pre)
dtm_pre <- removeSparseTerms(dtm_pre,.99)
tidy_dtm_pre <- tidy(dtm_pre)

trumptweets_post <- Corpus(VectorSource(tweets_post$text))
trumptweets_post <- tm_map(trumptweets_post, removePunctuation)
trumptweets_post <- tm_map(trumptweets_post, tolower)
trumptweets_post <- tm_map(trumptweets_post, removeWords, stopwords("en"))
trumptweets_post <- tm_map(trumptweets_post,stemDocument)
dtm_post <- DocumentTermMatrix(trumptweets_post)
dtm_post <- removeSparseTerms(dtm_post,.99)
tidy_dtm_post <- tidy(dtm_post)

tidy_dtm_pre %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq))

tidy_dtm_pre %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(reorder(term, -freq), freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("word")

tidy_dtm_post %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq))

tidy_dtm_post %>% group_by(term) %>%
  summarize(freq = sum(count)) %>%
  top_n(20, freq) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(reorder(term, -freq), freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("word")

#Prior to the election, the top words used in the tweets revolved around realdonaldtrump, trump, thank, great, will. These words occured with the highest frequency. After the election, the words with the highest frequency are will, great, amp. We note that prior to the election, Trump uses words that promises certain goals, such as "will" and "great". He also asserts his brand by tweeting trump and realdonaldtrump frequently. After the election, Trump started using less words to assert his brand, presumably because he already has recognition. Instead, Trump used more words to describe his policies, such as "tax".

###(c) + (d) + (e)
removeMostPunctuation<-
  function (x, preserve_intra_word_dashes = FALSE) 
  {
    rmpunct <- function(x) {
      x <- gsub("#", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      gsub("\002", "#", x, fixed = TRUE)
    }
    if (preserve_intra_word_dashes) { 
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- rmpunct(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      rmpunct(x)
    }
  }

trumptweets_hashtags <- Corpus(VectorSource(tweets$text))
trumptweets_hashtags <- tm_map(trumptweets_hashtags, tolower)
trumptweets_hashtags <- tm_map(trumptweets_hashtags, removeWords, stopwords("en"))
trumptweets_hashtags <- tm_map(trumptweets_hashtags, content_transformer(removeMostPunctuation),
                               preserve_intra_word_dashes = TRUE)


trumptweets_hashtags <- tm_map(trumptweets_hashtags,stemDocument)
dtm_hashtags <- DocumentTermMatrix(trumptweets_hashtags)
dtm_hashtags <- removeSparseTerms(dtm_hashtags,.99)
tidy_dtm_hashtags <- tidy(dtm_hashtags)
dtm.mat.hashtags <- as.matrix(dtm_hashtags)

tidy_dtm_hashtags_filt %>% group_by(term) %>% 
  summarize(freq = sum(count)) %>%
  top_n(5, freq) %>%
  arrange(desc(freq)) %>%
  ggplot(aes(reorder(term, -freq), freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("word")

###(f)
bigram.docs <- tweets %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigram.docs <- filter(bigram.docs, bigram == "crooked hillary")
bigram.docs <- mutate(bigram.docs, created_at = mdy_hms(created_at))

bigram.docs <- mutate(bigram.docs, n = 1)
bigram.docs %>% 
  group_by(month=format(as.Date(created_at),format="%m")) %>% summarize(freq=sum(n)) %>%
  ungroup() -> df2
df2