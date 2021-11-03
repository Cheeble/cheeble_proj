library(ngram)
library(dplyr)
library(sbo)
library(ggplot2)
xitsongaText11 <- readLines("2222.txt")
xitsongaText13 <- readLines("2222test.txt")
xitsongaText12 <- readLines("2222trainme.txt")
xitsongaText14 <- readLines("2222train.txt")

p1<- sbo_predictor(xitsongaText12, N = 1, dict = target ~ 0.40, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p2<- sbo_predictor(xitsongaText12, N = 2, dict = target ~ 0.60, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p3<- sbo_predictor(xitsongaText12, N = 3, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#
p4<- sbo_predictor(xitsongaText12, N = 4, dict = target ~ 1, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")

lengths(gregexpr("\\W+", xitsongaText11))
lengths(gregexpr("\\W+", xitsongaText12))
lengths(gregexpr("\\W+", xitsongaText13))
lengths(gregexpr("\\W+", xitsongaText14))

wourdcount(xitsongaText12)
predict(p1, "")
predict(p1, "")

#set.seed(840)
#babble(p1)

#p11<- sbo_predtable(xitsongaText12, N = 1, dict = target ~ 0.80, .preprocess = sbo::preprocess, EOS = ".?!:;", lambda = 0.4, L = 3L, filtered =  "UNK")
#p111 <- sbo_predictor(p11)

#save(p11)

predict(p111, "")
#summary(p11)
#head(p11[[1]])
set.seed(840)
(evaluation1 <- eval_sbo_predictor(p1, xitsongaText13))
(evaluation2 <- eval_sbo_predictor(p2, xitsongaText13))
(evaluation3 <- eval_sbo_predictor(p3, xitsongaText13))
(evaluation4 <- eval_sbo_predictor(p4, xitsongaText13))

evaluation1 %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation2 %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation3 %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation4 %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))

evaluation1 %>% filter(true != "<EOS") %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation2 %>% filter(true != "<EOS") %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation3 %>% filter(true != "<EOS") %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))
evaluation4 %>% filter(true != "<EOS") %>% summarise(accuracy = sum(correct)/n(), uncertainty = sqrt(accuracy*(1 - accuracy)/n()))

if (require(ggplot2)) {
  evaluation %>%
    filter(correct, true != "<EOS>") %>%
    select(true) %>%
    transmute(rank = match(true, table = attr(p, "dict"))) %>%
    ggplot(aes(x = rank)) + geom_histogram(binwidth = 25)
}


#summary
(c1 <- word_coverage(p1, xitsongaText12))
(c2 <- word_coverage(p2, xitsongaText12))
(c3 <- word_coverage(p3, xitsongaText12))
(c4 <- word_coverage(p4, xitsongaText12))

summary(c1)
summary(c2)
summary(c3)
summary(c4)

plot(c1)
plot(c2)
plot(c3)
plot(c4)

#he wena
sa <- kgram_freqs(corpus = xitsongaText12, 
                 N = 4, 
                 dict = target ~ 0.80,
                 .preprocess = sbo::preprocess,
                 EOS = ".?!:;"
)
summary(sa)
