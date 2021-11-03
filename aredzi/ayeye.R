# Load require functions
library(ANLP)
library(ggplot2)

# For debugging purpose
isDebugMode <-  F
isDataPresent <-  F

xitsongaText1 <- read.csv("3333.csv", stringsAsFactors = FALSE)
xitsongaText2 = Corpus(VectorSource(list(readLines("3333.csv"))))
#unigram
unigram.df = generateTDM(xitsongaText2,1,T)
saveRDS(unigram.df,file = "output/unigram.RDS")
#bigram
bigram.df = generateTDM(xitsongaText2,2,T)
saveRDS(bigram.df,file = "output/bigram.RDS")
#trigram
trigram.df = generateTDM(xitsongaText2,3,T)
saveRDS(trigram.df,file = "output/trigram.RDS")
#fourthgram
quadragram.df = generateTDM(xitsongaText2,4,T)
saveRDS(quadragram.df,file = "output/quadragram.RDS")

head(unigram.df)
head(bigram.df)
head(trigram.df)
head(quadragram.df)

