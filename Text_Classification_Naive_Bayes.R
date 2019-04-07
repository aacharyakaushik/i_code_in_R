library(RJSONIO)
library(httr)
library(jsonlite)
library(tokenizers)
library(SnowballC)
library(tm)
library(e1071)
library(caret)
library(stopwords)

#Data Collection
API_Key = "e69ff3f3-ecee-4b5c-87ac-df8746c90246" 
base_url ="https://content.guardianapis.com/search?"
sections<-c("sport","politics","world","technology","artanddesign","business")
fields='body'
article<-data.frame()
for(i in 1:7)
{
  for(j in sections)
  {
    APIurl<-paste(base_url, "api-key=", API_Key, "&section=",j, "&show-fields=wordcount%2C", fields,"&page-size=200") 
    json=fromJSON(APIurl) 
    article_data<-as.data.frame(json$response$results,flatten=TRUE) 
    article_body<-as.data.frame(json$response$results$fields$body,flatten=TRUE) 
    article_data=subset(article_data,select=-c(fields)) 
    article_res=cbind(article_data,article_body) 
    article=rbind(article,article_res) 
  } 
}
names(article)[12]="body" 
article<-subset(article, select=c(id,webTitle,body,sectionId)) 
nrow(article)


# Data Cleaning
article$body = gsub("[0-9]*","", article$body) 
article$body = gsub("<[^>]*>", "", article$body) 
article$body = gsub( "http://t.co/[a-z,A-Z,0-9]*{8}", "", article$body) 
article$body = gsub("[[:punct:]]", "", article$body) 
article$body = gsub("[[:digit:]]", "", article$body) 
article$body = gsub("[ \t]{2,}", "", article$body) 
article$body = gsub("^\\s+|\\s+$", "", article$body) 
article[sample(nrow(article), 1), 3]

#Tokenization
article_tokenize <-(article$body) 
typeof(article_tokenize)

article_tokenize_stem<-tokenize_word_stems(article_tokenize,stopwords = stopwords::stopwords("en"))
article_corpus <- Corpus(VectorSource(article_tokenize_stem)) 
article_termdoc<- DocumentTermMatrix(article_corpus) 
article_termdoc
article_reduction<- removeSparseTerms(article_termdoc,0.99) 
article_reduction

#Naive Bayes Classification
naive_train<-article_reduction[1:6720,] 
naive_test<-article_reduction[6721:8400,] 
naive_train_lab<-article[1:6720,]$sectionId 
naive_test_lab<-article[6721:8400,]$sectionId
naive_bool=function(param){param<-ifelse(param>0,1,0)} 
naive_train<-apply(naive_train,MARGIN = 2,naive_bool) 
naive_test<-apply(naive_test,MARGIN = 2,naive_bool)
naive_classifier <- naiveBayes(naive_train, as.factor(naive_train_lab)) 
naive_predict <- predict(naive_classifier, naive_test)
article_cross<-CrossTable(naive_predict,naive_test_lab,prop.chisq = FALSE,prop.t = FALSE,dnn = c("predictions"))

