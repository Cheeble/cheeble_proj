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
ngramXitList <- list(sort_fourth,sort_three,sort_two,sort_one)

write.xlsx(ngramXitList, "qw.xlsx")

testS <- "ni"

getNextWord <-predict_Backoff("a ya kule",ngramXitList, isDebugMode = F)


readTextFile <- function(fileName,encoding){
  
  # Open file in binary mode
  con <- file(fileName,'rb')
  
  # I used skipnul because in twitter dataset there are few nulls
  data <- readLines(con,encoding = encoding,skipNul = T)
  
  # Close connection
  close(con)
  
  # Return the data
  return(data)
}

#' Sample text data
#'
#' This function reads text files in the binary mode
#'
#' @param data Data read by \code{\link{readTextFile}}
#' @param proportion Value between 0 to 1 which represents portion of data
#' @return a list having sampled text data
#' @seealso \code{\link{rbinom}}
#' @export
#' @importFrom stats rbinom

sampleTextData <- function(data,proportion){
  
  # Rbinom function is used to sample data
  # It will return data as per proportion argument
  return(data[as.logical(rbinom(length(data),1,proportion))])
}

#' Clean and tokenize string data
#'
#' This function applies different cleaning techniques to clean corpus data.
#'
#' @param data Data read by \code{\link{readTextFile}}
#' @return a list having sampled text data
#'
#' @details
#' This function removes non english characters, numbers, white spaces, brackets, punctuation. It also handles cases like abbreviation, contraction. It converts entire text to lower case.
#' @seealso \code{\link{tm_map}} \code{\link{iconv}} \code{\link{content_transformer}} \code{\link{removeNumbers}} \code{\link{replace_contraction}} \code{\link{replace_abbreviation}} \code{\link{bracketX}} \code{\link{removePunctuation}} \code{\link{tolower}} \code{\link{stripWhitespace}}
#' @importFrom qdap replace_contraction replace_abbreviation bracketX
#' @importFrom utils read.delim2
#' @import tm
#' @export

cleanTextData <- function(data){
  
  # Remove non english characters
  data.cleaned <- iconv(data,"latin1","ASCII",sub="'")
  
  # Create corpus of data
  corpus <- Corpus(VectorSource(list(data.cleaned)))
  
  # Remove numbers from the data
  corpus.cl <- tm_map(corpus,removeNumbers)
  
  # replace contraction with full form
  corpus.cl <- tm_map(corpus.cl,content_transformer(replace_contraction))
  
  # replace abbreviation with full form
  corpus.cl <- tm_map(corpus.cl,content_transformer(replace_abbreviation))
  
  # Remove text which is within brackets
  corpus.cl <- tm_map(corpus.cl,content_transformer(bracketX))
  
  # Remove punctuation from the data
  corpus.cl <- tm_map(corpus.cl,removePunctuation)
  
  # Convert all data to lower case
  corpus.cl <- tm_map(corpus.cl,content_transformer(tolower))
  
  # Strip whitespaces from the data
  corpus.cl <- tm_map(corpus.cl, stripWhitespace)
  
  return(corpus.cl)
}

#' Build N gram model
#'
#' This function is an abstract function used by \code{\link{generateTDM}}
#'
#' @param N size of n-gram model
#' @return function which can be used to build N-gram model
#'
#' @seealso \code{\link{NGramTokenizer}}
#' @importFrom qdap replace_contraction replace_abbreviation bracketX
#' @importFrom RWeka NGramTokenizer Weka_control
#' @export

buildNgramModel <- function(N){
  
  # Build N gram model
  return(function(x) NGramTokenizer(x, Weka_control(min = N, max = N, delimiters = " \\r\\n\\t.,;:\"()?!")))
  #return(function(x) ngramrr(x,ngmin = N,ngmax = N))
  
}

#' Generate term document frequency table from corpus
#'
#' This function builds term documement sparse matrix
#'
#' @param data It can be text corpus/data cleaned by \code{\link{cleanTextData}}
#' @param N size of n-gram model
#' @param isTrace for debugging purpose, use this if you want to track time to build model.
#' @return term document matrix for terms having N words
#'
#' @details
#' This function generates terms with N number of words specified in argument. This can be used in many tasks like information retrival, document similarity etc.
#' @seealso \code{\link{TermDocumentMatrix}} \code{\link{buildNgramModel}}
#' @importFrom tm TermDocumentMatrix
#' @export

generateTDM <- function(data,N,isTrace = F){
  
  if(isTrace){
    startTime = Sys.time()
    print(paste0("Build started: ",N,"-gram model"," @ ",startTime))
  }
  
  tdm = TermDocumentMatrix(data,control = list(tokenize = buildNgramModel(N)))
  
  tdm.df = data.frame(word=tdm$dimnames[[1]],freq=rowSums(as.matrix(tdm)),row.names = NULL)
  
  tdm.df = tdm.df[order(-tdm.df$freq),]
  
  #tdm <- textcnt(data, n = N, method = "string", recursive = TRUE)
  #tdm.df <- data.frame(word = names(tdm), freq = unclass(tdm),row.names = NULL)
  
  #tdm.df <- tdm.df[tdm.df$char.length > 2,]
  
  #tdm.df <- tdm.df[order(-tdm.df$freq),]
  
  if(isTrace){
    print(paste0("Time to build ",N,"-gram model"," :- ", Sys.time() - startTime))
  }
  
  return(tdm.df)
}

#' Predict next word using backoff method
#'
#' This function predicts next word using back-off algorithm.
#'
#' @param testline Line on which we are performing algorithm to predict next word
#' @param modelsList List having all Ngram models generated by \code{\link{generateTDM}}
#' @param isDebugMode for debugging purpose, this will print out debug statements
#' @return next predicted word
#'
#' @details
#' This function predicts next word based on previous N number of words using N-gram models generated by \code{\link{generateTDM}}.
#' @seealso \code{\link{generateTDM}} \code{\link{TermDocumentMatrix}}
#' @import dplyr
#' @export

predict_Backoff <- function(testline,modelsList,isDebugMode = F){
  
  # Max number of ngrams supported
  maxNGramIndex = length(modelsList)
  
  # Clean the test string
  line = iconv(testline,"latin1","ASCII",sub="")
  line = line %>% replace_abbreviation %>% replace_contraction %>% removeNumbers %>%  removePunctuation %>% tolower  %>% stripWhitespace
  
  if(isDebugMode)
    print(line)
  
  # Tokenize the test string
  words <- unlist(strsplit(line, split=" "));
  len <- length(words);
  
  if(isDebugMode)
    print(paste("Length of the string is: ",len))
  
  # If test string is lower than the max number of ngrams then we do not need to go through all the ngram model
  # Instead we will look into N-gram models having N less than the length of test string
  if(len < maxNGramIndex){
    nGramIndex = len + 1
    
    localModelsList = modelsList[(maxNGramIndex-len):maxNGramIndex]
    
  }else{
    nGramIndex = maxNGramIndex
    localModelsList = modelsList
  }
  
  if(isDebugMode)
    print(paste("Number of models will be used: ",length(localModelsList)))
  index = 0
  predictions = NULL
  for(model in localModelsList){
    
    # +2 offest to match number of words with nGram model
    #if(nGramIndex != maxNGramIndex)
    pattern = paste0("^",paste(words[(len - nGramIndex + 2):len],collapse = " "))
    
    if(isDebugMode)
      print(pattern)
    
    # Find the pattern in the respective n-gram model
    nextWords = model[grep(pattern,model$word)[1:3],1]
    nextWords = nextWords[!is.na(nextWords)]
    # if(length(nextWords) != 0){
    #   nextWordIndex = sample(1:length(nextWords),3)
    #   nextWord = nextWords[nextWordIndex]
    # }else{
    #   nextWord = NA
    # }
    
    
    # Print top 5 match
    if(isDebugMode)
      print(nextWords)
    
    if(isDebugMode)
      print(paste("Predicated word: ",nextWords))
    
    nGramIndex = nGramIndex - 1
    
    # If the next word is predicted then return the answer
    # Else backoff to check the word with n-1 gram models
    # if(!is.na(nextWord)){
    #
    #   # The returned word will have have queried word as it was used to match
    #   # Just remove the queried word from the predicted word for better user experience
    #   tempNextWord = unlist(strsplit(as.character(nextWord)," "))
    #
    #   if(isDebugMode)
    #     print(paste("Splitted text: ",tempNextWord))
    #
    #   nextWord = paste(tempNextWord[length(tempNextWord)])
    #
    #   break
    # }
    
    if(length(nextWords) != 0){
      print(nextWords)
      for(word in nextWords){
        index = index + 1
        tempWord = unlist(strsplit(as.character(word)," "))
        if(sum(predictions == tempWord[length(tempWord)])>0)
        {
          next
        }
        
        predictions  = c(predictions, tempWord[length(tempWord)])
        if(length(predictions) == 3){
          break
        }
      }
      if(length(predictions) == 3){
        break
      }
      
    }
    
  }
  
  #print(predictions)
  
  if(length(predictions) < 3){
    if(isDebugMode)
      print(paste("No match found in ",paste(1:maxNGramIndex,collapse = ","),"Gram models so returning the most frequent word"))
    predictions = c(predictions,modelsList[[maxNGramIndex]][1:(3-length(predictions)),1])
  }
  
  if(isDebugMode)
    print(paste("The next predicated word using",nGramIndex+1,"gram model:-", nextWords))
  return(predictions)
  
}
