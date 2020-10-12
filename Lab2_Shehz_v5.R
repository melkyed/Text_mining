library(tidyverse)
library(tm)
library(scales)
library(wordcloud)
library(reshape2)
library(corrplot)
library(quanteda)
library(qdapTools)
library(qdap)
library(tidytext)
library(SnowballC)
library(qdap)
library(tidyr)
library(dplyr)
library(ggplot2)
#install.packages("qdap",INSTALL_opts = "--no-multiarch")




#### Text Mining of Women's Ecommerce Reviews Dataset 

#### Data Explorations and Cleaning ####

ecom_reviews <- read.csv("Womens Clothing E-Commerce Reviews.csv", stringsAsFactors = FALSE)
str(ecom_reviews)
summary(ecom_reviews)

#Changing column names to lower case, making it easier to reference

e_reviews <- ecom_reviews %>% 
  rename(no = X,
         item_id = Clothing.ID,
         age = Age,
         title = Title,
         review = Review.Text,
         rating = Rating,
         recommend = Recommended.IND,
         pos_feedback = Positive.Feedback.Count,
         division = Division.Name,
         department = Department.Name,
         class = Class.Name)

#Changing some columns to factors for better analysis
e_reviews$division<- as.factor(e_reviews$division)
e_reviews$department <- as.factor(e_reviews$department)
e_reviews$class <- as.factor(e_reviews$class)
str(e_reviews)
summary(e_reviews) #summary statistics looks ok, we will have to look for NUlls

#dealing with NUll values

e_reviews %>% 
  sapply(is.na) %>% 
  colSums()
#is Na is showing no nas but when scanning the dataset I did come across some empty strings 
#in some columns

e_reviews[e_reviews == ''] <- NA
e_reviews %>% 
  sapply(is.na) %>% 
  colSums()

#There we go we have missing values
#Since division, department and class have a small amount of NAs we can remove them straight away
#These variables have the same number of missing values, the funtion below might take care of all
#of them
 
r <- e_reviews %>% 
  drop_na(division)
r %>% 
  sapply(is.na) %>% 
  colSums()

#Great, the missing values referred to the same observations for division, department and class
#Reviews will be central to our analysis so let's remove all the NA's for that feature
r <- r %>% 
  drop_na(review)
r %>% 
  sapply(is.na) %>% 
  colSums()

#We can deal with the title variable later.
#We have a binary fetaure indicating whether the clothing item will be recommended or not lets explore further.

table(r$recommend)
rev_table <- as.table(table(r$recommend))
prop.table(rev_table)
plot(rev_table)

#82% of items are recommended! Good products here with an average rating of 4.1 as well
# ???where the av rating 4.1
mean(r$age) #average age of the customers giving reviews

top_feedback <- r %>%
  arrange(desc(pos_feedback))
head(select(top_feedback, item_id, pos_feedback), 25) #Items with best feedback
summary(r$pos_feedback)# The numerical values do not look like they have a set range, so hard to analyse

# Looking at the factor variables so we can see what items are being reviewed
unique(r$division)
divtab <-  as.data.frame(table(r$division))
divtab

deptab <- as.data.frame(table(r$department)) %>% 
  arrange(desc(Freq))
deptab #most popular departments 

clastab <- as.data.frame(table(r$class)) %>%
  arrange(desc(Freq))
clastab #most popular classes

corR <- cor(select(r, age, rating, recommend, pos_feedback),
    use = "pairwise.complete.obs")


#### Data Visualizations #### 

#correlation plot showing that rating and recommendations are correlated
col <- colorRampPalette(c("Blue", "Yellow", "Black"))(20)
corrplot(corR, method = "circle", type = "upper", col = col)


ggplot(data.frame(prop.table(table(r$department))), aes(x=Var1, y = Freq*100, fill = Freq)) +
  geom_bar(stat = 'identity') + 
  xlab('Department Name') +
  ylab('Percentage of Reviews') +
  geom_text(aes(label=round(Freq*100,2)), vjust=-0.40) + 
  ggtitle('Percentage of Reviews By Department')
#Tops have the highest percentage of reviews here followed by dresses

agebuckets <- r %>% 
  select(item_id, age, department) %>% 
    mutate(age_bucket = ifelse(age < 30, '18-29',
    ifelse(age < 40, '30-39',
    ifelse(age < 50, '40-49', ifelse(age < 60, '50-59', 
    ifelse(age < 70, '60-69', ifelse(age < 80, '70-79', 
    ifelse(age < 90, '80-89', '90-99')))))))) #creating buckets to analyse age

agebuckets %>% 
  filter(age < 80) %>% 
  group_by(age_bucket) %>% 
  count(department) %>% 
  ggplot(aes(department, n, fill = age_bucket)) + 
  geom_bar(stat='identity', show.legend = FALSE) +
  facet_wrap(~age_bucket, scales = 'free') + 
  xlab('Department') + 
  ylab('Number of Reviews') + 
  geom_text(aes(label = n), hjust = 1) +
  scale_y_continuous(expand = c(.1, 0)) + coord_flip() 

#distribution pretty even among departments and reviews but people in their 30s and 40s left
#the most reviews

#attempt to look at some Unigrams as well as Bigrams
#will need to tokenize and remove stopwords here for more accurate analysis
#Looking at 5 star reviews here, trying to find some positive words
table(r$rating)
rev_5 <- data.frame(text = r$review[r$rating==5],
                      stringsAsFactors = FALSE)

rev_5 %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

rev_5 %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "sienna4") +
  theme_minimal() +
  labs(title = "Top unigrams Womens clothing 5 Star Reviews")

#"love", "flattering", "perfect", "soft" all make an appearance

#5 star ratings and recommendations
rat5 <- r %>% filter(rating == 5)
table(rat5$recommend)
rat5_table <- as.table(table(rat5$recommend))
prop.table(rat5_table)
#as expected a high percentage of recommedations associated with 5 star ratings


#Next we will look at 1 star reviews to see if we find any negative words
rev_1 <- data.frame(text = r$review[r$rating==1],
                    stringsAsFactors = FALSE)

rev_1 %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

rev_1 %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "royalblue1") +
  theme_minimal() +
  labs(title = "Top unigrams Womens clothing 1 Star Reviews")
#not many outright negative words here maybe Bigrams will be more telling

rat1 <- r %>% filter(rating == 1)
table(rat1$recommend)
rat1_table <- as.table(table(rat1$recommend))
prop.table(rat1_table)
#1 star ratings have low recommendations

rev_1 %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "lightseagreen") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams Womens Clothing 1 Star Reviews")
#"poor quality" is telling as well as "cute online" some items are not as they appear online maybe
#"feels cheap", maybe some items are lacking in quality

#Looking at a Bigram for 5 Star reviews now
rev_5 %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "orange3") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams Womens Clothing 5 Star Reviews")

#"super cute", "super soft", "highly recommend", "fit perfectly" all look positive
#Looking at "fits Perfectly" and "fit perfectly" it shows that when we move forward to modelling it might be good to adopt stemming,
#lemmatization and more robust text mining techniques
#not surprisingly the prop tables show a high correlation between rating and recommendation
#similar results will be found if binary "recommend" variable analysed deeper

#Let's look at departments now
#datasets are split by department excluding "Trend"
 tops <- data.frame(text = r$review[r$department=="Tops"],
                    stringsAsFactors = FALSE)
 dresses <- data.frame(text = r$review[r$department=="Dresses"],
                     stringsAsFactors = FALSE)
 bottoms <- data.frame(text = r$review[r$department=="Bottoms"],
                     stringsAsFactors = FALSE)
 intimates <- data.frame(text = r$review[r$department=="Intimate"],
                     stringsAsFactors = FALSE)
 jackets <- data.frame(text = r$review[r$department=="Jackets"],
                     stringsAsFactors = FALSE)
 
 #recommendation proportion for tops
 tops_full <- r %>% filter(department == "Tops")
 table(tops_full$recommend)
 tops_table <- as.table(table(tops_full$recommend))
 prop.table(tops_table)
 #similar to entire dataset 
 
 #Looking at review bigrams for Tops
 tops %>% 
   unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
   separate(word, c("word1", "word2"), sep = " ") %>% 
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>% 
   unite(word,word1, word2, sep = " ") %>% 
   count(word, sort = TRUE) %>% 
   slice(1:12) %>% 
   ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "limegreen") +
   theme_minimal() +
   coord_flip() +
   labs(title = "Top Bigrams Womens Tops and Reviews")
  ## Lots of positive words here 
 
 #recommendation proportion for dresses 
 dresses_full <- r %>% filter(department == "Dresses")
 table(dresses_full$recommend)
 dress_table <- as.table(table(dresses_full$recommend))
 prop.table(dress_table)
 #similar to overall datasets 2 % less recommendations
 
 #Looking at review Bigrams for Dresses
 dresses %>% 
   unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
   separate(word, c("word1", "word2"), sep = " ") %>% 
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>% 
   unite(word,word1, word2, sep = " ") %>% 
   count(word, sort = TRUE) %>% 
   slice(1:10) %>% 
   ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "sienna") +
   theme_minimal() +
   coord_flip() +
   labs(title = "Top Bigrams Womens Dresses and Reviews")
#Postive words again prevail with mention of size
 
 #recommendation proportion for bottoms 
 bot_full <- r %>% filter(department == "Bottoms")
 table(bot_full$recommend)
 bot_table <- as.table(table(bot_full$recommend))
 prop.table(bot_table)
 #2 % higher recommendation compared to overall data
 
 #Bottoms department
 bottoms %>% 
   unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
   separate(word, c("word1", "word2"), sep = " ") %>% 
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>% 
   unite(word,word1, word2, sep = " ") %>% 
   count(word, sort = TRUE) %>% 
   slice(1:10) %>% 
   ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "skyblue2") +
   theme_minimal() +
   coord_flip() +
   labs(title = "Top Bigrams bottoms and Reviews")
 #similar words used to review
 
 #recommendation proportion of jackets 
 jackets_full <- r %>% filter(department == "Jackets")
 table(jackets_full$recommend)
 j_table <- as.table(table(jackets_full$recommend))
 prop.table(j_table)
 #similar to overall results again 
 
 #jackets department
 jackets %>% 
   unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
   separate(word, c("word1", "word2"), sep = " ") %>% 
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>% 
   unite(word,word1, word2, sep = " ") %>% 
   count(word, sort = TRUE) %>% 
   slice(1:10) %>% 
   ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "ivory4") +
   theme_minimal() +
   coord_flip() +
   labs(title = "Top Bigrams Womens Jackets and Reviews")
 #similar good bigrams and some reference to type
 
 in_full <- r %>% filter(department == "Intimate")
 table(in_full$recommend)
 in_table <- as.table(table(in_full$recommend))
 prop.table(in_table)
 #2 % higher than overall 
 # from all the prop tables it looks like dresses are affecting the overall,
 # proportion of recommendation by a slight margin
 
 intimates %>% 
   unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
   separate(word, c("word1", "word2"), sep = " ") %>% 
   filter(!word1 %in% stop_words$word) %>%
   filter(!word2 %in% stop_words$word) %>% 
   unite(word,word1, word2, sep = " ") %>% 
   count(word, sort = TRUE) %>% 
   slice(1:10) %>% 
   ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "khaki3") +
   theme_minimal() +
   coord_flip() +
   labs(title = "Top Bigrams Womens Intimates and Reviews")
 #positive review bigrams shown
 
 
 
 
 #### Data Preparation for Modelling ####
head(r, 20)
summary(r)

corpus_r <- Corpus(VectorSource(r$review)) #create a corpus
corpus_r[[5]]
corpus_r[[5]][1] #view corpus document just to make sure it's good

####Text Preprocessing
##We want a cleaned document that has all unnecessary items removed 

###Convert text to lower case

corpus_r=tm_map(corpus_r, tolower)
corpus_r[[5]][1]

## Remove Punctuation
corpus_r=tm_map(corpus_r, removePunctuation)
corpus_r[[5]][1]

## Remove Stopwords 

corpus_r=tm_map(corpus_r, removeWords, stopwords("english"))
corpus_r[[5]][1]

## Remove stopwords specific to document

corpus_r=tm_map(corpus_r, removeWords,c("also", "get","like", "made", "can", "im", "just", "i"))

##remove whitespace
corpus_r <- tm_map(corpus_r, FUN = stripWhitespace)


## Stemming the document to avoid duplications we found above

corpus_r=tm_map(corpus_r, stemDocument)
corpus_r[[5]][1] #words successfully stemmed

## Create a Term document Matrix
#tokeninze words and create a numerical matrix that is acceptable when modelling

r_Tdm <- TermDocumentMatrix(corpus_r, control = list(minWordLength = 1,
                                                     weighting =
                                                       function(x)
                                                         weightTfIdf(x, normalize =
                                                                       FALSE),
                                                     stopwords = TRUE))
inspect(r_Tdm)

rowTotals <- apply(r_Tdm , 1, sum) #Find the sum of words in each Document
rTdm<- r_Tdm[rowTotals > 0, ] #remove all docs without words

inspect(rTdm)



####Exploratory Analysis using TDM

#Frequent Terms and Association for all available data

freq.terms <- findFreqTerms(rTdm, lowfreq=5000)

term.freq <- rowSums(as.matrix(rTdm))
term.freq <- subset(term.freq, term.freq >= 10000)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "nice"?
findAssocs(rTdm, 'nice', 0.10)

# which words are associated with "order"?
findAssocs(rTdm, 'order', 0.10)

# which words are associated with "review"?
findAssocs(rTdm, 'review', 0.10)

# which words are associated with "beautiful"?
findAssocs(rTdm, 'beautiful', 0.30)

# which words are associated with "wear"?
findAssocs(rTdm, 'wear', 0.10)

# which words are associated with "return"?
findAssocs(rTdm, 'return', 0.10)

# which words are associated with "love"?
findAssocs(rTdm, 'love', 0.10)

# which words are associated with "perfect"?
findAssocs(rTdm, 'perfect', 0.10)

# which words are associated with "fit"?
findAssocs(rTdm, 'fit', 0.10)

# which words are associated with "color"?
findAssocs(rTdm, 'color', 0.10)



#Word Cloud

rTdm2 <- removeSparseTerms(rTdm, sparse = 0.98)

library(wordcloud)
m <- as.matrix(rTdm2)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)


# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, rot.per=0.35, colors = brewer.pal(8, "Dark2"))







###Lets take a look at a split on department
#Frequent Terms and Association for department Tops

split_tops <- r %>% filter(department == "Tops")


head(split_tops, 20)
summary(split_tops)

corpus_tops <- Corpus(VectorSource(split_tops$review)) #create a corpus
corpus_tops[[5]]
corpus_tops[[5]][1] #view corpus document just to make sure it's good

####Text Preprocessing
##We want a cleaned document that has all unnecessary items removed 

###Convert text to lower case

corpus_tops=tm_map(corpus_tops, tolower)
corpus_tops[[5]][1]

## Remove Punctuation
corpuops=tm_map(corpus_tops, removePunctuation)
corpus_tops[[5]][1]

## Remove Stopwords 

corpus_tops=tm_map(corpus_tops, removeWords, stopwords("english"))
corpus_tops[[5]][1]

## Remove stopwords specific to document

#corpus_tops=tm_map(corpus_tops, removeWords,c("also", "get","like", "made", "can", "im", "just", "i"))

##remove numbers and whitespace
## Stemming the document to avoid duplications we found above

corpus_tops=tm_map(corpus_tops, stemDocument)
corpus_tops[[5]][1] #words successfully stemmed

## Create a Term document Matrix for Tops

tops_Tdm <- TermDocumentMatrix(corpus_tops, control = list(minWordLength = 1,
                                                           weighting =
                                                             function(x)
                                                               weightTfIdf(x, normalize =
                                                                             FALSE),
                                                           stopwords = TRUE))
inspect(tops_Tdm)


rowTotals <- apply(tops_Tdm , 1, sum) #Find the sum of words in each Document
tops_Tdm<- tops_Tdm[rowTotals > 0, ] #remove all docs without words

inspect(tops_Tdm)


#Frequent Terms and Association

freq.terms <- findFreqTerms(tops_Tdm, lowfreq=500)

term.freq <- rowSums(as.matrix(tops_Tdm))
term.freq <- subset(term.freq, term.freq >= 4000)
tops_df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(tops_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "nice"?
findAssocs(tops_Tdm, 'nice', 0.10)

# which words are associated with "order"?
findAssocs(tops_Tdm, 'order', 0.10)

#Word Cloud for Tops

tops_Tdm2 <- removeSparseTerms(tops_Tdm, sparse = 0.98)

library(wordcloud)
m <- as.matrix(tops_Tdm2)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)


# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, rot.per=0.35, colors = brewer.pal(8, "Dark2"))






######Now lets split the data on class
#Frequent Terms and Association for class  "Sweaters" 

split_sweat <- r %>% filter(class == "Sweaters")


head(split_sweat, 20)
summary(split_sweat)

corpus_sweat <- Corpus(VectorSource(split_sweat$review)) #create a corpus
corpus_sweat[[5]]
corpus_sweat[[5]][1] #view corpus document just to make sure it's good

####Text Preprocessing
##We want a cleaned document that has all unnecessary items removed 

###Convert text to lower case

corpus_sweat=tm_map(corpus_sweat, tolower)
corpus_sweat[[5]][1]

## Remove Punctuation
corpus_sweat=tm_map(corpus_sweat, removePunctuation)
corpus_sweat[[5]][1]

## Remove Stopwords 

corpus_sweat=tm_map(corpus_sweat, removeWords, stopwords("english"))
corpus_sweat[[5]][1]

## Remove stopwords specific to document

#corpus_sweat=tm_map(corpus_sweat, removeWords,c("also", "get","like", "made", "can", "im", "just", "i"))

##remove numbers and whitespace
## Stemming the document to avoid duplications we found above

corpus_sweat=tm_map(corpus_sweat, stemDocument)
corpus_sweat[[5]][1] #words successfully stemmed

## Create a Term document Matrix

sweat_Tdm <- TermDocumentMatrix(corpus_sweat, control = list(minWordLength = 1,
                                                             weighting =
                                                               function(x)
                                                                 weightTfIdf(x, normalize =
                                                                               FALSE),
                                                             stopwords = TRUE))
inspect(sweat_Tdm)


rowTotals <- apply(sweat_Tdm , 1, sum) #Find the sum of words in each Document
sweat_Tdm<- sweat_Tdm[rowTotals > 0, ] #remove all docs without words

inspect(sweat_Tdm)



#Frequent Terms and Association

freq.terms <- findFreqTerms(sweat_Tdm, lowfreq=500)

term.freq <- rowSums(as.matrix(sweat_Tdm))
term.freq <- subset(term.freq, term.freq >= 600)
sweat_df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(sweat_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "nice"?
findAssocs(sweat_Tdm, 'nice', 0.10)

# which words are associated with "order"?
findAssocs(sweat_Tdm, 'order', 0.10)

#Word Cloud

sweat_Tdm2 <- removeSparseTerms(sweat_Tdm, sparse = 0.98)

library(wordcloud)
m <- as.matrix(sweat_Tdm2)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)


# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, rot.per=0.35, colors = brewer.pal(8, "Dark2"))






##Let's look at split on recommended 
#Frequent Terms and Association for recommended == 1

split_recom <- r %>% filter(recommend == 1)


head(split_recom, 20)
summary(split_recom)

corpus_rec <- Corpus(VectorSource(split_recom$review)) #create a corpus
corpus_rec[[5]]
corpus_rec[[5]][1] #view corpus document just to make sure it's good

####Text Preprocessing
##We want a cleaned document that has all unnecessary items removed 

###Convert text to lower case

corpus_rec=tm_map(corpus_rec, tolower)
corpus_rec[[5]][1]

## Remove Punctuation
corpus_rec=tm_map(corpus_rec, removePunctuation)
corpus_rec[[5]][1]

## Remove Stopwords 

corpus_rec=tm_map(corpus_rec, removeWords, stopwords("english"))
corpus_rec[[5]][1]

## Remove stopwords specific to document

#corpus_tops=tm_map(corpus_tops, removeWords,c("also", "get","like", "made", "can", "im", "just", "i"))

##remove numbers and whitespace
## Stemming the document to avoid duplications we found above

corpus_rec=tm_map(corpus_rec, stemDocument)
corpus_rec[[5]][1] #words successfully stemmed

## Create a Term document Matrix

rec_Tdm <- TermDocumentMatrix(corpus_rec, control = list(minWordLength = 1,
                                                         weighting =
                                                           function(x)
                                                             weightTfIdf(x, normalize =
                                                                           FALSE),
                                                         stopwords = TRUE))
inspect(rec_Tdm)


rowTotals <- apply(rec_Tdm , 1, sum) #Find the sum of words in each Document
rec_Tdm<- rec_Tdm[rowTotals > 0, ] #remove all docs without words

inspect(rec_Tdm)



####Exploratory Analysis using TDM

#Frequent Terms and Association

freq.terms <- findFreqTerms(rec_Tdm, lowfreq=5000)

term.freq <- rowSums(as.matrix(rec_Tdm))
term.freq <- subset(term.freq, term.freq >= 10000)
rec_df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(rec_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "nice"?
findAssocs(rec_Tdm, 'nice', 0.10)

# which words are associated with "order"?
findAssocs(rec_Tdm, 'order', 0.10)

#Word Cloud

rec_Tdm2 <- removeSparseTerms(rec_Tdm, sparse = 0.98)

library(wordcloud)
m <- as.matrix(rec_Tdm2)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)


# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, rot.per=0.35, colors = brewer.pal(8, "Dark2"))







##Let's look at split on NOT RECOMMENDED 
#Frequent Terms and Association for recommended == 0

split_norecom <- r %>% filter(recommend == 0)


head(split_norecom, 20)
summary(split_norecom)

corpus_norec <- Corpus(VectorSource(split_norecom$review)) #create a corpus
corpus_norec[[5]]
corpus_norec[[5]][1] #view corpus document just to make sure it's good

####Text Preprocessing
##We want a cleaned document that has all unnecessary items removed 

###Convert text to lower case

corpus_norec=tm_map(corpus_norec, tolower)
corpus_norec[[5]][1]

## Remove Punctuation
corpus_norec=tm_map(corpus_norec, removePunctuation)
corpus_norec[[5]][1]

## Remove Stopwords 

corpus_norec=tm_map(corpus_norec, removeWords, stopwords("english"))
corpus_norec[[5]][1]


##remove numbers and whitespace
## Stemming the document to avoid duplications we found above

corpus_norec=tm_map(corpus_norec, stemDocument)
corpus_norec[[5]][1] #words successfully stemmed

## Create a Term document Matrix

norec_Tdm <- TermDocumentMatrix(corpus_norec, control = list(minWordLength = 1,
                                                             weighting =
                                                               function(x)
                                                                 weightTfIdf(x, normalize =
                                                                               FALSE),
                                                             stopwords = TRUE))
inspect(norec_Tdm)


rowTotals <- apply(norec_Tdm , 1, sum) #Find the sum of words in each Document
norec_Tdm<- norec_Tdm[rowTotals > 0, ] #remove all docs without words

inspect(norec_Tdm)



####Exploratory Analysis using TDM

#Frequent Terms and Association

freq.terms <- findFreqTerms(norec_Tdm, lowfreq=500)

term.freq <- rowSums(as.matrix(norec_Tdm))
term.freq <- subset(term.freq, term.freq >= 2000)
norec_df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(norec_df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with "return"?
findAssocs(norec_Tdm, 'return', 0.10)

# which words are associated with "order"?
findAssocs(norec_Tdm, 'order', 0.10)

# which words are associated with "recommend"?
findAssocs(norec_Tdm, 'recommend', 0.10)

# which words are associated with "bad"?
findAssocs(norec_Tdm, 'bad', 0.10)

#Word Cloud

norec_Tdm2 <- removeSparseTerms(norec_Tdm, sparse = 0.98)

library(wordcloud)
m <- as.matrix(norec_Tdm2)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wordcloud(d$word, d$freq, min.freq=3)


# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)


# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20,
          random.order = F, rot.per=0.35, colors = brewer.pal(8, "Dark2"))












#Classification with target variable to predict which items will be recommended or not 
#This information will help the business decide whether to keep the clothing item in production
#or not
#lets use reviews as a variable input and try and predict if reviews affect recommendation
#businesses take reviews and recommendations very seriously, from a marketing perspective positive
#or negative,
#reviews can significantly impact sales because cosumers look for feedback before making 
#a purchase decision.

#we can create a cluster dendrogram to see what words are clustered after creating the TDM
rTdm3 <- removeSparseTerms(rTdm, sparse = 0.90)

clustr<- hclust(d = dist(rTdm3, method = "euclidean"), method = "complete")

#some items "run large", some are the "perfect length", the fits seem to be good on lots of items
#Items "look great" and are "soft and comfortable"
plot(clustr)

#loading libraries for modelling
library(irlba)
library(e1071)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(biclust)
library(igraph)
library(fpc)

#create a dictionary
dict <- findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(r$review))), lowfreq = 0)
dictCorpus <- Corpus(VectorSource(dict))

#create a document term matrix
rDtm <- DocumentTermMatrix(x = corpus_r, 
                           control = list(weighting = 
                                            function(x) weightTfIdf(x, normalize = F)))
rDtm2 <- removeSparseTerms(rDtm, sparse = 0.90)
rDtm3 <- as.data.frame(as.matrix(rDtm2))
colnames(rDtm3) <- stemCompletion(x = colnames(rDtm3),
                                      dictionary = dictCorpus, type = "prevalent")
colnames(rDtm3) <- make.names(colnames(rDtm3))
sort(colSums(rDtm3), decreasing = T)
rDtm3[1164:1170, 15:21]

#adding dtm and target variable
#we have the text data converted to the numerical matrix for modellimg and we add,
#the target binary "recommend" variable
r_rec<- cbind(recommend = r$recommend, rDtm3)

#creating a train and test set
#using train test split to to train the model then test it's accuracy having a labelled set of test data
#we will use rmse as the performance metric here, a lower rmse denotes better performance
set.seed(007)
split <- sample(1:nrow(r_rec), size = 0.7 * nrow(r_rec))
r_train <- r_rec[split, ]
r_Test <- r_rec[-split, ]
nrow(r_train)
nrow(r_Test)
nrow(r_rec)

#Modelling
#Regression Tree //dessition tree

regt1 <- rpart(recommend ~ ., r_train)
rpart.plot(regt1)
pred1 <- predict(regt1, newdata = r_Test)
pred1Rmse <- sqrt(mean((pred1 - r_Test$recommend) ^ 2))
pred1Rmse
summary(regt1)

#RandomForest Ensemble method
#built from multiple decisiion trees
#bootstrapped samples and random subsets of features used here for training
#aggregation of results used for final result
RF2=randomForest(recommend~., data=r_train)
varImpPlot(RF2, cex=.7) #what words effect classification the most
pred2 <- predict(RF2, newdata = r_Test)
pred2Rmse <- sqrt(mean((pred2 - r_Test$recommend) ^ 2))
pred2Rmse # a lower rmse here denoting better performance


# logistic regression
mglm = glm(recommend~., family = "gaussian", data = r_train)
pred3 <- predict(mglm, newdata = r_Test)
pred3Rmse <- sqrt(mean((pred3 - r_Test$recommend) ^ 2))
pred3Rmse# slightly ower rmse here 
summary(mglm)



