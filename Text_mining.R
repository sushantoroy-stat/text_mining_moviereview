# Text Mining using R 
# install and Load ####
# install.packages("tm")
# install.packages("textclean")
# install.packages("wordcloud") 
# install.packages("SnowballC") 
# install.packages("ggplot2")
library(tm) 
library(textclean) 
library(wordcloud) 
library(SnowballC) 
library(ggplot2)

# Read data ####
text_data <- read.csv("IMDB Dataset.csv")
head(text_data)
str(text_data)

# Basic before Preprocessing ####
# Read text column
# Create a corpus( structured collection of text documents, 
# Once the corpus is created, we can preprocess the text)
# Display the first line of the corpus 
text_column <- text_data$review
corpus1 <- Corpus(VectorSource(text_column))
corpus<-VCorpus(VectorSource(text_column))
corpus[[1]]$content
is.list(corpus)

# Text Preprocessing ####
## using tm+textclean packages ####
# Convert text to lowercase 
# removed punctuation and numbers
# Remove stopwords ("the", "is", "and", "in", "of", etc.)
# Apply stemming (the process of reducing words to their base or root form) 
# Remove white space(extra spaces and normalizes the spacing between words)

corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, removeWords, stopwords("en")) 
corpus<- tm_map(corpus, stemDocument) 
corpus<- tm_map(corpus, stripWhitespace)
corpus[[1]]$content

# Creating DTM (Document-Term Matrix) after Preprocessing ####. 
# A DTM is a table that counts the frequency of terms in the text.
# View matrix summary 
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)


# word frequencies and data frame ####
# word_freq<-sort(colSums(as.matrix(dtm)))
library(slam)
word_freq <- sort(col_sums(dtm),decreasing = FALSE) #as vector
word_freq_df <- data.frame(term = names(word_freq), frequency = word_freq)
head(word_freq_df)

# Again Preprocessing and arrange(descending, top words)
word_freq_df$term <- trimws(word_freq_df$term)
word_freq_df_sorted <- word_freq_df[order(word_freq_df$frequency,decreasing = TRUE),]
word_freq_df_sorted
top_words  <- head(word_freq_df_sorted, 5) 
top_words

# visualization ####
## Bar Chart using ggplot2 ####
# Create a bar chart of the top worde 
ggplot(top_words, aes(x = reorder(term, frequency), y = frequency)) + 
  geom_bar(stat = "identity", fill = "steelblue") +  
    coord_flip() +  
      theme_minimal() + 
        labs(x = "Terms", y = "Frequency")


## Topic(cluster) Modeling ####
# two tpyes most popular: Latent Dirichlet Allocation (LDA) 
# and Non-negative Matrix Factorization (NMF).

# Topic Modeling with LDA
# Create dtm
# Apply LDA
# Get the top 10 terms for each topic(cluster)

library(topicmodels)
#dtm  <- DocumentTermMatrix(corpus)
inspect(dtm)
lda_model  <-  LDA(dtm, k = 5)
topics <- terms(lda_model, 10)
print(topics)


## wordcloud ####
data_wc  <- head(word_freq_df_sorted, 10000) 
head(data_wc)
wordcloud(words = data_wc$term, 
          freq = data_wc$frequency, 
          max.words = 1000, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))


