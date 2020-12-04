

##Final Code for Collecting Tweets

install.packages('xlsx')
install.packages('writexl')
install.packages("rtweet")
library(rtweet)
install.packages("data.table")
library(data.table)
Sys.setlocale("LC_CTYPE", "arabic" )

twitter_token <- rtweet::create_token(app='Social Tweet1',
                                      consumer_key =  , #consumer_key
                                      consumer_secret = #consumer_secret
                                       )
tweet1 <- search_tweets(
  q = ,#hashtag name or keyword
  n = 20000, #number of tweets to collect
  include_rts = FALSE
)
View(tweet1)
head(rt,n=3)
write.table(rt,"t.txt",sep="|")
#############################################################

#creating arabic words cloud

#Arabic Word Cloud
install.packages('tidyverse')
library(tidyverse)
#install.packages('textclean')
#install.packages('clean_tweets')
install.packages('tm')
install.packages('arabicStemR')
install.packages('wordcloud2')
library(tm)
library(arabicStemR)
library(wordcloud2)
Sys.setlocale("LC_CTYPE", "arabic" )
########## readig the file
arabic_text<-readLines("C:/Users/win/Desktop/GASTAT/data/2.txt", encoding = "UTF-8")
View(arabic_text) 

########### cleaning the text
arabic_text<-sub("http[^[:space:]]*", "", arabic_text) # remove url 1
arabic_text<-sub("http[[:alnum:][:punct:]]*", "", arabic_text)# remove url 2
arabic_text<-sub('@\\S+', '', arabic_text) ## Remove Mentions
arabic_text<-sub('\\b+RT', '', arabic_text) ## Remove RT
arabic_text<-str_replace_all(arabic_text,"#[a-z,A-Z]*","") #hashtags
arabic_text<-removeNumbers(arabic_text) # numbers
arabic_text<- str_replace_all(arabic_text,"[^[:alnum:][:blank:]&/\\-_]", "") #remove special characters
arabic_text<-str_replace_all(arabic_text,"[a-z,A-Z]*","") #removing english words
arabic_text<- str_replace_all(arabic_text,"[^[:alnum:][:blank:]&/\\-]", "  ") #remove special characters
arabic_text<-stripWhitespace(arabic_text) #space
view(arabic_text)
##################
write_xlsx(df_title1,"x.xlsx")
df_title1 <- data.frame(arabic_text, stringsAsFactors = F) 


########## craeting corpus
arabic_corpus<-Corpus(DataframeSource(df_title))

############
myextract<-data.frame(text=sapply(arabic_corpus,as.character),stringsAsFactors = F)
myextract$text

##############
arabic_tmd<-TermDocumentMatrix(arabic_corpus)  #document term matrix
arabic_matrix<-as.matrix(arabic_tmd)           # the occuranc of terms in each documents
arabic_freq<-sort(rowSums(arabic_matrix),decreasing = TRUE) # term frequency

##############
output_ar<-cbind(arabic_freq) # convert it to column
head(output_ar)
tail(output_ar)

##############
output_ar_df<-as.data.frame(output_ar)
output_ar_df$arabic<-row.names(output_ar_df)

############# reverse english to arabic
for(i in 1:nrow(output_ar_df)){output_ar_df$arabic_trans[i]= reverse.transliterate(output_ar_df$arabic[i])}

############
write.table(output_ar_df,"output_ar_df.txt",quote = FALSE,col.names = FALSE,
            row.names = T,sep="\t", fileEncoding = "UTF-8" )

mydata<-cbind(arabic_matrix,names(arabic_matrix))

write.table(mydata,"term.txt",quote = FALSE,col.names = FALSE,
            row.names = T,sep="\t", fileEncoding = "UTF-8" )

tap.d<-data.frame(words=names(arabic_freq),freq=arabic_freq)
wordcloud2(data=tap.d)
