# Twitter Sentiment Analysis Project --- MGIS 650 
# Data - Spotify_tweets_2023
# Author - Dhwani Doshi 
# Naming convention - snake case 

# ----- 1) Install all required packages -----

#install.packages("readxl") 
#install.packages("openxlsx")
#install.packages('textstem') 
#install.packages("reshape2") 
#install.packages("ggplot2") 
#install.packages("syuzhet") 
#install.packages("tm")


# ----- 2) set working directory -----

setwd("/Users/dhwani/Desktop/sentiment_analysis")
getwd()

# ----- 3) import data -----

library(readxl)
spotify_raw_data <- read_excel("Spotify_tweets_2023.xlsx")

# ----- 4) sentiment analysis -----

# ----- 4.1) syuzhet method -----

library(syuzhet)
spotify_sent_syuzhet <- get_sentiment(spotify_raw_data$text, method = "syuzhet")

# ----- 4.2) afinn method -----

spotify_sent_afinn <- get_sentiment(spotify_raw_data$text, method = "afinn")

# ----- 4.2) NRC method -----

spotify_sent_nrc <- get_sentiment(spotify_raw_data$text, method = "nrc")
spotify_nrc_data <- get_nrc_sentiment(spotify_raw_data$text)

# ----- 5) Final dataset -----

# ----- 5.1) Combine all sentiment scores with raw dataset -----

spotify_sent <- cbind(spotify_raw_data, spotify_sent_syuzhet, spotify_sent_afinn, spotify_sent_nrc,spotify_nrc_data)
#View(spotify_sent)

# ----- 5.2) Export this data in excel format -----

library(openxlsx)
write.xlsx(spotify_sent, file = "spotify_sent.xlsx", rowNames = FALSE)

# ----- 6) Cleaning tweets -----

# ----- 6.1) create a dataframe with only "status_id" and "text" column -----

attach(spotify_sent)
data_tweet <- spotify_sent[,c("status_id","text")] 
data_tweet <- data.frame(doc_id = status_id, text = text, stringsAsFactors = FALSE)

# ----- 6.2) Construct the corpus -----

library(tm)
tweets_1 = Corpus(DataframeSource(data_tweet))
#tweets_1[[1]]$content   

# ----- 6.3) Noise removal -----

# ----- 6.3.1) Retweet removal -----

removeRT <- function(x){gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)}
tweets_2 = tm_map(tweets_1,content_transformer(removeRT))
#tweets_2[[1]]$content

# ----- 6.3.2) Hashtag removal -----

removeHashtag <- function(x){gsub("#\\S+", "", x)}
tweets_3 = tm_map(tweets_2,content_transformer(removeHashtag))
#tweets_3[[1]]$content

# ----- 6.3.3) URL removal -----

removeURL <- function(x){gsub("http[^[:space:]]*", "", x)}
tweets_4 = tm_map(tweets_3,content_transformer(removeURL))
#tweets_4[[1]]$content

# ----- 6.3.4) HTML removal -----

unescapeHTML <- function(str) {return(gsub("<.*?>", "", str))}
tweets_5 = tm_map(tweets_4,content_transformer(unescapeHTML))
#tweets_5[[1]]$content

# ----- 6.3.5) Mention removal -----

removeMention <- function(x){gsub("@\\w+", "", x)}
tweets_6 = tm_map(tweets_5,content_transformer(removeMention))
#tweets_6[[1]]$content

# ----- 6.3.6) Carriage removal -----

removeCarriage <- function(x){gsub("[\r\n]", "", x)}
tweets_7 = tm_map(tweets_6,content_transformer(removeCarriage))
#tweets_7[[1]]$content

# ----- 6.3.7) Emoticon removal -----

removeEmoticon <- function(x){gsub("[^\x01-\x7F]", "", x)}
tweets_8 = tm_map(tweets_7,content_transformer(removeEmoticon))
#tweets_8[[1]]$content

# ----- 6.4) general preprocessing procedures -----

# ----- 6.4.1) lowercase -----

tweets_9 = tm_map(tweets_8,content_transformer(tolower))
#tweets_9[[1]]$content

# ----- 6.4.2) remove punctuation -----

tweets_10 <- tm_map(tweets_9, removePunctuation)
#tweets_10[[1]]$content

# ----- 6.4.3) remove numbers -----

tweets_11 <- tm_map(tweets_10,removeNumbers)
#tweets_11[[1]]$content

# ----- 6.4.4) remove stopwords -----

tweets_12 <- tm_map(tweets_11,removeWords,stopwords("english"))
#tweets_12[[1]]$content

# ----- 6.4.5) remove specific words -----

tweets_13 <- tm_map(tweets_12,removeWords,c("https", "t.co", "0001f33b"))
#tweets_13[[1]]$content

# ----- 6.4.6) remove whitespaces -----

tweets_14 <- tm_map(tweets_13,stripWhitespace)
#tweets_14[[1]]$content

# ----- 6.4.7) word stemming -----

tweets_15<-tm_map(tweets_14,stemDocument)
#tweets_15[[1]]$content

# ----- 6.4.8) word lemmatization -----

library(textstem)
tweets_16 <- tm_map(tweets_15, lemmatize_strings)
#tweets_16[[1]]$content

# ----- 7) transform clean tweets data -----

# convert tweets from list to a structured data frame 

tweets17 <- data.frame(text = sapply(tweets_16, as.character), stringsAsFactors = FALSE)
tweets18 <- cbind(Index = rownames(tweets17),status_id, tweets17)
rownames(tweets18) <- 1:nrow(tweets18)

# ----- 8) Create a word data frame -----

# Load required libraries
library(tidyr)
library(tidytext)
library(dplyr) 

# Extract individual words from the text column
word_data <- tweets18 %>%
  unnest_tokens(word, text) %>%
  mutate(word = tolower(word))  # Convert words to lowercase to match with stop words

# Load stop words
stop_words <- stopwords("en")

# Remove rows with stop words
word_data <- word_data %>%
  filter(!word %in% stop_words)

library(openxlsx)
write.xlsx(word_data, file = "word_data.xlsx", rowNames = FALSE)
# ----- 9) Create a word count data frame -----

word_count_data <- word_data %>%
  count(word, sort = TRUE)
View(word_count_data)

library(openxlsx)
write.xlsx(word_count_data, file = "word_count_data.xlsx", rowNames = FALSE)

# ----- 10) clear unwanted words from dataframe -----

# Define a list of meaningless words
meaningless_words <- c("goamazongrammarlyquillbotscribdzoom","nfqbviucanva","phnetflixvivamaxyoutubehbo","proturnitinquizletcourseherocanvapicsart","im","alway","permiss","realli","lfb","ph","wanna","fuck","anoth","celebr","ive","lf","dday","tri","viu","agad","php","de","wtb","s") 

# Filter out rows containing meaningless words 
# Modify word_data
word_data <- word_data %>%
  filter(!word %in% meaningless_words)

# Modify word_data_count
word_count_data <- word_count_data %>%
  filter(!word %in% meaningless_words)

View(word_count_data)

# ----- 11) modify required words from dataframe -----

# Define a dictionary of corrections (to replace incorrect strings with correct versions)
corrections_dict <- c("spotifi" = "spotify", "etcspotifydisney" = "disney","accountonhand" = "account", "youtub" = "youtube","favorit" = "favorite","appl" = "apple", "singl" = "single","happi" = "happy", "episod" = "episode", "offici" = "official")

# Function to correct misspelled or incorrect strings
correct_string <- function(word) {
  if (word %in% names(corrections_dict)) {
    return(corrections_dict[word])
  } else {
    return(word)
  }
}

# Modify word_data
word_data$word <- sapply(word_data$word, correct_string)

# Modify word_data_count
word_count_data$word <- sapply(word_count_data$word, correct_string)

# ----- 12) create wordcloud -----

library(wordcloud)

word_data %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(6,"Dark2")))

#save wordcloud
png("spotify_wordcloud.png", width = 10, height = 8, units = 'in', res = 300)
word_data %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(6,"Dark2")))
dev.off();

# ----- 13) create plot of emotion frequency -----

library(ggplot2)

#get the emotions using the NRC dictionary
emotions <- get_nrc_sentiment(word_count_data$word)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
# create graph
png("emotion_spotify_tweets.png", width = 12, height = 8, units = "in", res = 300)
ggplot(emo_sum, aes(x = emotion, y = count, fill = emotion)) +
  geom_bar(stat = "identity") +
  xlab("Emotion") +
  ylab("Count") +
  ggtitle("Spotify Tweets emotion")
dev.off()