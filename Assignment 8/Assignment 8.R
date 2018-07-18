                                      #ASSIGNMENT 8: SENTIMENT ANALYSIS

#PART 1:
#Scrape at least 5000 customer_reviews/blogs from the web using R and then apply the sentiment analysis technique 
#that you have already learnt using a downloaded positive/negative lexicon. 
#Stem both lexicon and customer_reviews using openNLP. There are many R scraping tools such as rvest. 
#Prepare a labeled dataset with positive and negative labels.

#Products to test:
#Echo dot (Second Generation): https://www.amazon.com/dp/B01DFKC2SO
#Kindle Paperwhite: https://www.amazon.com/dp/B00OQVZDJM

install.packages("stringr") #Provides clean, modern interface to common string operations
install.packages("qdap") #Aggregates data by any number of grouping variables
install.packages("rvest") #Makes it easy to scrape/harvest data from html web pages
library(rvest)

#Creating a Dataframe to store product customer_reviews
review=data.frame(ReviewContent=character(),stringsAsFactors=FALSE)

for(i in 1:500) #Scraping through 500 webpages
{
  url=paste("https://www.amazon.com/dp/B01DFKC2SO",i,sep="")
  
  review_data <- read_html(url)
  
  text <- review_data %>% 
    html_nodes("#cm_cr-review_list .review .review-data .review-text") %>%
    html_text() 
  #text #Displayes the product name
  
  for(j in 1:length(text))
  {
    review[nrow(review)+1,] <- c(text[j])
  }
}

#Export to a CSV file:
write.csv(review,file = "desktop/customer_reviews.csv",row.names = FALSE)

library(tm)
library(NLP)
library(qdap)
library(stringr)

#Positive Lexicons

pos_lex <- read.csv(file="positive-words.csv", header=TRUE, sep= ',')
pos_lex <- Corpus(VectorSource(pos_lex)) #Converting to corpus
pos_lex <- tm_map(pos_lex,stemDocument) #Stemming
pos_lex <- data.frame(text=sapply(pos_lex, `[[`, "content"), stringsAsFactors=FALSE) #Converting to dataframe
pos_lex <- unique(pos_lex) #Fetching only unique positive words
pos_lex<-data.frame(Words=unlist(pos_lex)) #Unlisting

#Negative Lexicons

neg_lex <- read.csv(file="negative-words.csv", header=TRUE, sep= ',')
neg_lex <- Corpus(VectorSource(neg_lex)) #Converting to corpus
neg_lex <- tm_map(neg_lex,stemDocument) #Stemming
neg_lex <- data.frame(text=sapply(neg_lex, `[[`, "content"), stringsAsFactors=FALSE) #Converting to dataframe
neg_lex <- unique(neg_lex) #Fetching only unique positive words
neg_lex<-data.frame(Words=unlist(neg_lex)) #Unlisting

#Result Dataframe to store review, sentimental score:
df <- data.frame("Review"=character(),"Positive Word Count"=integer(),"Negative Word Count"=integer(),"Total Word Count"=integer(),"Positivity Percentage"=integer(),"Negativity Percentage"=integer(),"Result"=character(),stringsAsFactors = FALSE)

#Review Data
review <- read.csv(file="customer_reviews.csv", header=TRUE, sep= ',')
NROW(review) #Number of rows of dataframe 'review'

for(k in 1:NROW(review))
{
  
  review_data=review[k,]  #To extract data from each row and all columns
  review_original<-review_data
  
  df[nrow(df)+1,1]<-c(toString(review_original))
  
  review_data = tolower(review_data) #Making it lower case
  review_data = gsub('[[:punct:]]', '', review_data) #Removing  punctuation
  review_data = gsub("[[:digit:]]", "", review_data) #Removing numbers
  review_data <- Corpus(VectorSource(review_data)) #Converting into corpus
  review_data = tm_map(review_data, removeWords, stopwords('english'))  #Removing stop words
  review_data=tm_map(review_data,stemDocument) #Stemming 
  #strwrap(b[[1]]) #To view the stemmed data
  
  review_data <- data.frame(text=sapply(review_data, `[[`, "content"), stringsAsFactors=FALSE)#Converting corpus to dataframe
  #review_data
  #typeof(review_data)
  
  review_data<-str_trim(clean(review_data)) #Removing extra white spaces 
  review_data<- as.String(review_data)
  review_words <- strsplit(review_data, " ") #Splitting a sentence into words
  length(review_words)
  review_words<-data.frame(Words=unlist(review_words)) #Unlisting 
  
  review_words<-as.matrix(review_words,nrow=NROW(review_words),ncol=NCOL(review_words)) #Matrix
  NROW(review_words)
  
  positive_count=0
  negative_count=0
  total_word_count=NROW(review_words)
  
  for(i in 1:NROW(review_words)) #Parsing every word of the review
  {
    if(review_words[i][1] %in% pos_lex$Words)
      positive_count=positive_count+1
    else if (review_words[i][1] %in% neg_lex$Words)
      negative_count=negative_count+1
  }
  
  positive_count
  negative_count
  total_word_count
  positivity_percentage=(positive_count/total_word_count)*100
  negativity_percentage=(negative_count/total_word_count)*100
  result=""
  
  if(positivity_percentage>negativity_percentage)
  {result='Positive'
  }else
  {result='Negative'}
  
  result
  
  df[nrow(df),2:7]<- c(positive_count,negative_count,total_word_count,positivity_percentage,negativity_percentage,result)
}

#Writing to CSV
write.csv(df,"Sentiments_Result.csv")

#-----------------------------------------------------------------------------------

#PART 2:
#Divide the labeled dataset into training and test datasets (e.g. 80%/20%). 
#Run NBC in R on the labeled training dataset for sentiment classification and then show the performance using the test dataset. 
#Your input corresponding to a review is its sparse representation.
#Web scraping


install.packages("snowballC") #Collapses words to a common root to aid comparison of vocabulary
install.packages("e1071") #Naive Bayes Classifier

#Reading the customer_reviews data:
data <- read.csv(file="customer_reviews.csv", header=TRUE, sep= ',')

#Converting dataframe to corpus
review_corpus <- Corpus(DataframeSource(data))
review_corpus

getTransformations() #Prepare the corpus

#Create a custom transformation by content_transformer():
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
review_corpus <- tm_map(review_corpus, toSpace, "/|@|\\|")

review_corpus <- tm_map(review_corpus, content_transformer(tolower)) # Conversion to Lower Case
review_corpus <- tm_map(review_corpus, removeNumbers) #Remove Numbers
review_corpus <- tm_map(review_corpus, removePunctuation) #Remove Punctuation
review_corpus <- tm_map(review_corpus, removeWords, stopwords("english")) #Remove English Stop Words
review_corpus <- tm_map(review_corpus, removeWords, c("hmmmm", "can", "say", "im", "your", "get", "asap","let", "ok","ugh"))
review_corpus <- tm_map(review_corpus, stripWhitespace) #Strip Whitespace
review_corpus <- tm_map(review_corpus, stemDocument) #Stemming

#Creating a Document Term Matrix
doc_term_matrix <- DocumentTermMatrix(review_corpus)
dim(doc_term_matrix) #Dimension

doc_term_matrix = removeSparseTerms(doc_term_matrix,0.9)
dim(doc_term_matrix)

#Converting document term matrix to dataframe
df <- as.data.frame(as.matrix(doc_term_matrix),row.names = FALSE)

#Writing to CSV
write.csv(df,"desktop/Input.csv")

library(e1071)

labeled_customer_reviews <- read.csv("desktop/Sentiments_Result.csv")
head(labeled_customer_reviews)
dim(labeled_customer_reviews)
str(labeled_customer_reviews)

sparse_rep <- read.csv("desktop/Input.csv")
dim(sparse_rep)
str(sparse_rep)

input_data <- cbind(sparse_rep[1:94], labeled_customer_reviews$Result)
head(input_data)
dim(input_data)

colnames(input_data)[95] <- "Decision"

#Creating Test and Train dataset:
index <- sample(1:nrow(input_data),round(0.8*nrow(input_data)))
train_data <- input_data[index,]
test_data <- input_data[-index,]
head(test_data)


#Create an NBC classifier:
help("naiveBayes")
model <- naiveBayes( Decision ~ . , data=train_data)
class(model)
summary(model)
print(model)

#Predicting the test data:
pred <- predict(model, newdata = test_data)

#Displaying a confusion matrix:
confusion_matrix <- table(pred, test_data$Decision)
confusion_matrix