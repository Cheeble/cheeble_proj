library(ANLP)

data.xitsongaText1 <- read.csv("3333.csv", stringsAsFactors = FALSE)
 

train.size <-  0.50
testdata.size <- 0.30
validatedata.size <-0.20


data.trainme.train <-sampleTextData(data.xitsongaText1,train.size)
data.testingdata.testdata <-sampleTextData(data.xitsongaText1,testdata.size)
data.validation.validatedata <-sampleTextData(data.xitsongaText1,validatedata.size)

library(tm)
combineddata.data <- c(data.trainme.train,data.testingdata.testdata,data.validation.validatedata)

mycorpus <- combineddata.data;
writeCorpus(mycorpus,filenames = "output/combineddata1.txt")

writeCorpus(data.trainme.train,filenames = "divided/train1.txt")
writeCorpus(data.testingdata.testdata,filenames = "divided/test2.txt")
writeCorpus(data.validation.validatedata,filenames = "divided/validate3.txt")