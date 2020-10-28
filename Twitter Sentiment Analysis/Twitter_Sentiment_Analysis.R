library(twitteR)
library(dplyr)
library(lubridate)
library(stringr)
library(tm)
library(SentimentAnalysis)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(tidytext)
library(ggplot2)

consumer_key <- "eAnyisrocHc70WM36lV19s4Oj"
consumer_secret <- "5zThdZxAybhOdypTpBtINtjC6O4c7NchPsb5f0MqHilBORluTt"
access_token <- "1319262410759237637-ro9T8KQCxSaHJrUqyBevO0Xyypbwx2"
access_secret <- "oWpZvc1wGUvi1LCh11kJchNeAtXcjMP853LCuFRZmmtXq"


setup_twitter_oauth(consumer_key, consumer_secret, 
                    access_token, access_secret)

tweets <- searchTwitter("Trump", n = 100)
tweets_df <- twListToDF(tweets)

# seperating text column and converting text to character
tweets_text <- tweets_df$text
tweets_text_df <- data.frame(Text=unique(tweets_text))


## remove letters, digits, and punctuation haracters starting with @ 
## remove usernames and replace with ""
tweets_text_df$Text <- gsub("@\\w*"," ", tweets_text_df$Text)

##Remove website links and replace with ""
tweets_text_df$Text  <- gsub("http[[:alnum:][:punct:]]*"," ",
                          tolower(tweets_text_df$Text ))
tweets_text_df$Text  <- gsub("www[[:alnum:][:punct:]]*"," ",
                          tolower(tweets_text_df$Text ))


#remove html entitties like &quot; starting with 
#note perfect but we will remove remaining punctation at later step
tweets_text_df$Text<-gsub("\\&\\w*;","", tweets_text_df$Text)

#remove any letters repeated more than twice (eg. hellooooooo -> helloo)
tweets_text_df$Text  <- gsub('([[:alpha:]])\\1+', '\\1\\1', tweets_text_df$Text)

#additional cleaning removing leaving only letters numbers or spaces
tweets_text_df$Text <- gsub("[^a-zA-Z0-9 ]","",tweets_text_df$Text)

#review tweets now
head(tweets_text_df$Text,10)


## Corpus Creation
# create corpus and clean up text before creating document term matrix

Twitter_Corpus <- Corpus(VectorSource(tweets_text_df$Text))

Twitter_Corpus <- tm_map(Twitter_Corpus, stemDocument)
Twitter_Corpus <- tm_map(Twitter_Corpus, removePunctuation)
Twitter_Corpus <- tm_map(Twitter_Corpus, removeNumbers)
Twitter_Corpus <- tm_map(Twitter_Corpus, removeWords, stopwords("english"))
Twitter_Corpus <- tm_map(Twitter_Corpus, stripWhitespace) 

#create term document matrix (terms as rows, documents as columns)
tdm <- TermDocumentMatrix(Twitter_Corpus)

# define tdm as matrix
m = as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud2(df)


# Most frequent words

barplot(df[1:10,]$freq, las = 2, names.arg = df[1:10,]$word,
        col ="steelblue", main ="Most frequent words",
        ylab = "Word frequencies")


mysentiment <- get_nrc_sentiment(df$word)
sentimentScores <- data.frame(Score= colSums(mysentiment))
sentimentScores <- cbind(Sentiment = rownames(sentimentScores), sentimentScores)
rownames(sentimentScores) <- NULL

wordcloud2(sentimentScores)


positive_senti <- get_sentiments("bing") %>%
    filter(sentiment == "positive")

bing <- get_sentiments("bing")
df2 <- df %>% inner_join(bing)
df2 <- df2[-1,]

df2 %>%
    filter(freq > 5) %>%
    mutate(n = ifelse(sentiment == "negative", -freq, freq)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment))+
    geom_col() +
    coord_flip() +
    labs(y = "Sentiment Score")


