library(NLP)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(stringr)
library(stringi)
library(stylo)
library(shiny)
library(xlsx)
library(mice)
library(tidyverse)
library(caret)
#new include
library(qdap)
library(sbo)
#xitsongaText1 <- read.csv("1111.csv", stringsAsFactors = FALSE)
#xitsongaText <- readLines("newtry.txt")
xitsongaText <- read.csv("3333.csv", stringsAsFactors = FALSE)
mycorpus <- Corpus(VectorSource(xitsongaText))
as.character(mycorpus[[1]])
token_delim <- " \\t\\r\\n.!?,;\"()"
#unigram
unigramXit <- NGramTokenizer(mycorpus, Weka_control(min=1,max=1, delimiters = token_delim))
#bigram
bigramXit <- NGramTokenizer(mycorpus, Weka_control(min=2,max=2, delimiters = token_delim))
#trigram
trigramXit <- NGramTokenizer(mycorpus, Weka_control(min=3,max=3, delimiters = token_delim))
#fourth gram
fourthgramXit <- NGramTokenizer(mycorpus, Weka_control(min=4,max=4, delimiters = token_delim))
#saving ngrams
write.xlsx(unigramXit, "unigram1.xlsx")
write.xlsx(bigramXit, "unigram2.xlsx")
write.xlsx(trigramXit, "unigram3.xlsx")
write.xlsx(fourthgramXit, "unigram4.xlsx")
#for unigram storing and plot
one_word <- data.frame(table(unigramXit))
sort_one <- one_word[order(one_word$Freq,decreasing=TRUE),]
write.xlsx(sort_one, "1gram.xlsx")
wordcloud(sort_one$unigramXit,sort_one$Freq,random.order=FALSE,scale = c(5,0.65),min.freq = 4,colors = brewer.pal(12,"Paired"),max.words=3300)
#for biigram storing and plot
two_word <- data.frame(table(bigramXit))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
write.xlsx(sort_two, "2gram.xlsx")
wordcloud(sort_two$bigramXit,sort_two$Freq,random.order=FALSE,scale = c(3,0.45),min.freq = 5,colors = brewer.pal(12,"Paired"),max.words=3300)
#for trigram storing and plot
three_word <- data.frame(table(trigramXit))
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
write.xlsx(sort_three, "3gram.xlsx")
wordcloud(sort_three$trigramXit,sort_three$Freq,random.order=FALSE,scale = c(2,0.25),min.freq =3 ,colors = brewer.pal(12,"Paired"),max.words=3300)
#for fourthgram storing and plot
fourth_word <- data.frame(table(fourthgramXit))
sort_fourth <- fourth_word[order(fourth_word$Freq,decreasing=TRUE),]
write.xlsx(sort_fourth, "4gram.xlsx")
wordcloud(sort_fourth$fourthgramXit,sort_fourth$Freq,random.order=FALSE,scale = c(2,0.15),min.freq = 3,colors = brewer.pal(12,"Paired"),max.words=3300)

#ploting ngrams
ggplot(data=sort_one[1:55,],aes(x=Freq, y=unigramXit))+
  geom_point(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_two[1:50,],aes(x=Freq, y=bigramXit))+
  geom_point(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_three[1:50,],aes(x=Freq, y=trigramXit))+
  geom_point(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_fourth[1:50,],aes(x=Freq, y=fourthgramXit))+
  geom_point(aes(color= factor(Freq), size = Freq))

#1010101010101010101010101111000000000000000000000110001011010s
#first we have to merge all the n-grams into single list
ngramXitList <- list(sort_one,sort_two,sort_three,sort_fourth)

#tyfitfytvfujytvfjy
xitsongaText12 <- readLines("newtry.txt")
#
p <- sbo_predictor(xitsongaText12, # preloaded example dataset
                   N = 1, # Train a 3-gram model
                   dict = target ~ 0.80, # cover 75% of training corpus
                   .preprocess = sbo::preprocess, # Preprocessing transformation 
                   EOS = ".?!:;", # End-Of-Sentence tokens
                   lambda = 0.4, # Back-off penalization in SBO algorithm
                   L = 3L, # Number of predictions for input
                   filtered = "<UNK>" # Exclude the <UNK> token from predictions
)
#
p1<- sbo_predictor(xitsongaText12, N = 1, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p2<- sbo_predictor(xitsongaText12, N = 2, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p3<- sbo_predictor(xitsongaText12, N = 3, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p4<- sbo_predictor(xitsongaText12, N = 4, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")


predict(p4, "ti")


#ploting ngrams
ggplot(data=sort_one[1:55,],aes(x=sort_one$unigramXit, y=sort_one$Freq))+
  geom_bar(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_two[1:55,],aes(x=sort_two$unigramXit, y=sort_two$Freq))+
  geom_bar(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_three[1:55,],aes(x=sort_three$unigramXit, y=sort_three$Freq))+
  geom_bar(aes(color= factor(Freq), size = Freq))

ggplot(data=sort_fourth[1:55,],aes(x=sort_one$unigramXit, y=sort_fourth$Freq))+
  geom_bar(aes(color= factor(Freq), size = Freq))

#Ti girafu to ganama
ggplot(sort_one[1:55,], aes(Freq,unigramXit))+geom_bar(stat = "identity",aes(color= factor(Freq)))
ggplot(sort_two[1:55,], aes(Freq,bigramXit))+geom_bar(stat = "identity",aes(color= factor(Freq)))
ggplot(sort_three[1:55,], aes(Freq,trigramXit))+geom_bar(stat = "identity",aes(color= factor(Freq)))
ggplot(sort_fourth[1:55,], aes(Freq,fourthgramXi))+geom_bar(stat = "identity",aes(color= factor(Freq)))
#+coord_flip()
