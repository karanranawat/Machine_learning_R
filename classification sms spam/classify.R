
# Collecting data
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)

# Data preparation 
library(tm) #text mining package

#The corpus function is extremely useful and can read many different 
#sources such as PDFs and Microsoft word documents
sms_corpus <- Corpus(VectorSource(sms_raw$text))

print(sms_corpus)
inspect(sms_corpus[1:3])

# transforming the corpus using the tm_map function
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #create a wrapper to get and set contents of a text document
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# comparison before cleaning and after cleaning
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# Tokenization - converting a sentence into tokens(words)
sms_dtm <- DocumentTermMatrix(corpus_clean)

# Creating training and test datasets

sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))

prop.table(table(sms_raw_test$type))

# Visualizing text data
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)

spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# creating indicator features

sms_dict <- findFreqTerms(sms_dtm_train, 5) # Find words in DTM that occur atleat 5 times
# Dictionary is a data structure that allows us to specify which words should appear in the DTM

sms_train <- DocumentTermMatrix(sms_corpus_train, list(sms_dict))


