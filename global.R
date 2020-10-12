################# PROJECT 2 - TEXT MINING USING WOMENS CLOTHING E-COMMERCE DATA ##############
##******************************************************************************************##
library(tidyr)
library(tm)
library(wordcloud)
library(dplyr)
library(reshape2)
library(tidytext)
library(textdata)
library(broom)
library(ggplot2)
library(ggthemes)
library(textclean)
library(stringr)
library(topicmodels)
library(proto)
library(shiny)
library(memoise)
library(textmineR)
library(stopwords)


###******************************************************************************###
data <- readRDS("wcd.rds")


departments <- c("Trend",
              "Dresses",
              "Bottoms",
              "Intimate",
              "Jackets",
              "Tops")

tp <- data %>%
  filter(department == "Tops")

dr <- data %>%
  filter(department == "Dresses")

bt <- data %>%
  filter(department == "Bottoms")

it <- data %>%
  filter(department == "Intimate")

jt <- data %>%
  filter(department == "Jackets")

tr <- data %>%
  filter(department == "Trend")

############### A function for the Word Cloud ############### 

getTermMatrix <- memoise(function(department) {
  
  if (!(department %in% departments))
    stop("Unknown department")
  
  if (department  == "Tops") {
    text <- tp$review
  }
  
  if (department  == "Dresses") {
    text <- dr$review
  }
  
  if (department  == "Bottoms") {
    text <- bt$review
  }
  
  if (department  == "Intimate") {
    text <- it$review
  }
  
  if (department  == "Jackets") {
    text <- jt$review
  }
  
  if (department  == "Trend") {
    text <- tr$review
  }
  
  
  corpus_r = Corpus(VectorSource(text))
  corpus_r = tm_map(corpus_r, content_transformer(tolower))
  corpus_r = tm_map(corpus_r, removePunctuation)
  corpus_r = tm_map(corpus_r, removeNumbers)
  corpus_r = tm_map(corpus_r, removeWords, stopwords("english")) #ins
  corpus_r = tm_map(corpus_r, FUN = stripWhitespace) # ins
  corpus_r = tm_map(corpus_r, removeWords,
                    c(stopwords("SMART"), "also", "get","like", "made", "can", "im", "just", "i"))
  
  ## Stemming the document to avoid possible duplications
  
  corpus_r=tm_map(corpus_r, stemDocument) #ins

    ## Create a Term document Matrix

   r_Tdm <- TermDocumentMatrix(corpus_r, control = list(minWordLength = 1,
                                                         weighting =
                                                           function(x)
                                                             weightTfIdf(x, normalize =
                                                                           FALSE),
                                                         stopwords = TRUE))
  
  m = as.matrix(r_Tdm)
  
  sort(rowSums(m), decreasing = TRUE)
})


############### Word Frequency Function ############### 

getConditionedDataFrame <- function(terms) {
   word.freq <- subset(terms, terms >= 25)
  df <- data.frame(term = names(word.freq), freq = word.freq)
  return(df)
}


############### Classification/Clustering Function ############### 

hierarchical <- memoise(function(department) {
  
  if (!(department %in% departments))
    stop("Unknown department")
  
  if (department  == "Tops") {
    text <- tp$review
  }
  
  if (department  == "Dresses") {
    text <- dr$review
  }
  
  if (department  == "Bottoms") {
    text <- bt$review
  }
  
  if (department  == "Intimate") {
    text <- it$review
  }
  
  if (department  == "Jackets") {
    text <- jt$review
  }
  
  if (department  == "Trend") {
    text <- tr$review
  }
  
  
  corpus_r = Corpus(VectorSource(text))
  corpus_r = tm_map(corpus_r, content_transformer(tolower))
  corpus_r = tm_map(corpus_r, removePunctuation)
  corpus_r = tm_map(corpus_r, removeNumbers)
  corpus_r = tm_map(corpus_r, removeWords, stopwords("english")) #ins
  corpus_r = tm_map(corpus_r, FUN = stripWhitespace) # ins
  corpus_r = tm_map(corpus_r, removeWords,
                    c(stopwords("SMART"), "also", "get","like", "made", "can", "im", "just", "i"))
  
  ## Stemming the document to avoid possible duplications
  corpus_r=tm_map(corpus_r, stemDocument) #ins
  
  r_Tdm <- TermDocumentMatrix(corpus_r, control = list(minWordLength = 1,
                                                       weighting =
                                                         function(x)
                                                           weightTfIdf(x, normalize =
                                                                         FALSE),
                                                       stopwords = TRUE))
  
  tdm <- removeSparseTerms(r_Tdm, sparse = 0.90)
  m <- as.matrix(tdm)
  fit<- hclust(d = dist(m, method = "euclidean"), method = "complete")
 

})



