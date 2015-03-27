# exploratory analysis script for Capstone task 2

# Questions to consider
# --> Some words are more frequent than others - what are the distributions of word frequencies? 
# What are the frequencies of 2-grams and 3-grams in the dataset? 
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# How do you evaluate how many of the words come from foreign languages? 
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(rJava)
#library(RWeka)
#library(openNLP)
library(wordcloud)
library(ggplot2)
library(plyr)
library(RColorBrewer)

# read an english corpus
#eng <- VCorpus(DirSource("data/final/en_US/"))
eng <- VCorpus(DirSource("data/small/en_US/"))
#eng <- VCorpus(DirSource("data/micro/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, 
                                                          function(x) { ellen <- nchar(content(x))
                                                                        wordlist <- words(x)  # from NLP package
                                                                        c(TextDocument=meta(x,"id"),
                                                                          NChars=sum(ellen), NElements=length(content(x)),
                                                                          MedianElement=median(ellen), MaxElement=max(ellen),
                                                                          NWords2=length(wordlist), NVoc=length(unique(wordlist)) 
                                                                        )} )))
# summary statistics
Corpus.summary(eng)

# Questions
# 1) Some words are more frequent than others - what are the distributions of word frequencies? (Zipf law)

# word frequencies (per corpus)
ctrl <- list(tokenize = RWeka::WordTokenizer, # uses RWeka tokenizer
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             removeNumbers=TRUE,
             stopwords=stopwords("english"))

tab1 <- table(termFreq(eng[[1]], control = ctrl))
tab2 <- table(termFreq(eng[[2]], control = ctrl))
tab3 <- table(termFreq(eng[[3]], control = ctrl))
plot(tab1, log="x", xlab="Word Occurance", ylab="Frequency",main="Word Occurance Distribution",type="l")

dtm <- TermDocumentMatrix(eng,control=list(tokenize="words",removePunctuation=TRUE,removeNumbers=TRUE))    # uses NLP::words tokenizer
#dtm <- TermDocumentMatrix(eng,control=ctrl)    # uses RWeka tokenizer BUG
inspect(dtm[1:20,])
Docs(dtm)
nDocs(dtm)
nTerms(dtm)
head(Terms(dtm))

#install.packages("Rgraphviz") not found
#plot(dtm)

# find highest freq terms ==> not surprisingly all stopwords
findFreqTerms(dtm, max(dtm)/10)

# word frequencies and rank
v <- sort(slam::row_sums(dtm), decreasing=TRUE)  # calculate the frequency of words
sum(v)    # 788371 words
length(v) # 56482 unique words
d <- data.frame(Word=names(v), Freq=v, RelFreq=v/sum(v), Rank=rank(-v))

#show in wordcloud
par(mfrow=c(1,1));wordcloud(d$Word, d$Freq, max.words = 250, colors=brewer.pal(8,"Dark2"))

# Zipf's law (e.g., http://en.wikipedia.org/wiki/Zipf%27s_law) states that the frequency of any word is inversely proportional to its rank in the frequency table,
# or, more generally, that the pmf of the term frequencies is  of the form c k^{-β}, where k is the rank of the term (taken from the most to the least frequent one). 
# We can conveniently explore the degree to which the law holds by plotting the logarithm of the frequency against the logarithm of the rank, and inspecting the goodness of fit of a linear model.
ggplot(d, aes(x=Rank, y=RelFreq)) + geom_point() + geom_line() + 
  scale_y_log10(breaks=10^(-7:0)) + scale_x_log10(breaks=10^(0:5)) +   
  xlab("Rank") + ylab("Word Probability Mass") + ggtitle("Zipf Law: pmf of word in function of rank") +
  stat_smooth(method = "lm") 

model<- lm(log(RelFreq) ~ log(Rank), data=d)
model1<- lm(log(RelFreq) ~ log(Rank), data=d, weights=1/Rank)
model2<- lm(log(RelFreq) ~ log(Rank), data=d, weights=sqrt(1/Rank))
summary(model1)

plot(log(d$Rank),log(d$RelFreq))
lines(log(d$Rank),predict(model, d))
lines(log(d$Rank),predict(model1, d),col="blue")
lines(log(d$Rank),predict(model2, d),col="green")

ggplot(d, aes(x=Rank, y=RelFreq)) + geom_point() + geom_line() + 
  scale_y_log10(breaks=10^(-7:0)) + scale_x_log10(breaks=10^(0:5)) +   
  xlab("Word Rank") + ylab("Word Probability Mass") + ggtitle("Zipf Law: pmf of word in function of rank") +
  stat_smooth(method = "lm", aes(weight=1/Rank),colour="red") +
  stat_smooth(method = "lm",colour="blue") 

# conclusion blue line minimized pmf mse ocross all words, most of words have high ranks, red line give a uniform fit in log scale

# Now without stopwords
ctrl2 <- list(tokenize = "words", # uses NLP tokenizer
             removePunctuation = TRUE,
             removeNumbers=TRUE,
             stopwords=stopwords("english"))
dtm2 <- TermDocumentMatrix(eng,control=ctrl2)

freqnrank <- function(fdtm) {
  v <- sort(rowSums(as.matrix(fdtm[findFreqTerms(fdtm,1),])), decreasing=TRUE)
  data.frame(Word=names(v), Freq=v, RelFreq=v/sum(v), Rank=rank(-v), Corpus=Docs(fdtm)[1])
}

d <- ldply(lapply(Docs(dtm2), function(x) freqnrank(dtm2[,x])), data.frame)

# vizualize the corpora in wordclouds
par(mfrow=c(1,nDocs(dtm2)))
lapply(Docs(dtm2), function(x) with(subset(d,Corpus == x),
                                    wordcloud(Word, Freq, max.words=100, colors=brewer.pal(8,"Dark2"))))

ggplot(d, aes(x=Rank, y=RelFreq, colour=Corpus)) + geom_point() + geom_line() + 
  scale_y_log10(breaks=10^(-7:0)) + scale_x_log10(breaks=10^(0:5)) +   
  xlab("Word Rank") + ylab("Word Probability Mass") + ggtitle("Zipf Law: pmf of word in function of rank") +
  stat_smooth(method = "lm", aes(weight=1/Rank),colour="red") +
  stat_smooth(method = "lm", colour="blue") 

v <- sort(rowSums(as.matrix(dtm2)), decreasing=TRUE)  # calculate the frequency of words
sum(v)    # 546169 words
length(v) # 56382 unique words
d <- data.frame(Word=names(v), Freq=v, RelFreq=v/sum(v), Rank=rank(-v))

# Zipf's law (e.g., http://en.wikipedia.org/wiki/Zipf%27s_law) states that the frequency of any word is inversely proportional to its rank in the frequency table,
# or, more generally, that the pmf of the term frequencies is  of the form c k^{-β}, where k is the rank of the term (taken from the most to the least frequent one). 
# We can conveniently explore the degree to which the law holds by plotting the logarithm of the frequency against the logarithm of the rank, and inspecting the goodness of fit of a linear model.
ggplot(d, aes(Rank, RelFreq)) + geom_point() + geom_line() + 
  scale_y_log10() + scale_x_log10() + stat_smooth(method = "lm") + xlab("Rank") + ylab("Word Probability")

# counts of words with x occurances
plot(table(v),log="x",type="h",xlab="Word Occurance", ylab="Nr Words", main="Word occurance distribution")
str(as.data.frame(table(v)))

# Zipf's law (e.g., http://en.wikipedia.org/wiki/Zipf%27s_law) states that the frequency of any word is inversely proportional to its rank in the frequency table,
# or, more generally, that the pmf of the term frequencies is  of the form c k^{-β}, where k is the rank of the term (taken from the most to the least frequent one). 
# We can conveniently explore the degree to which the law holds by plotting the logarithm of the frequency against the logarithm of the rank, and inspecting the goodness of fit of a linear model.
ggplot(as.data.frame(table(v)), aes(as.numeric(v), Freq)) + geom_point() + geom_line() + 
  scale_y_log10() + scale_x_log10() + stat_smooth(method = "lm") + xlab("Word Occurance")

# guido's law: 
# Freq: Number of words with occurance N in copus (normalize occurance to corpus size and words to voc size )

df <- rbind(
transform(as.data.frame(table(v1,dnn="p")),Corpus="blogs"),
transform(as.data.frame(table(v2,dnn="p")),Corpus="news"),
transform(as.data.frame(table(v3,dnn="p")),Corpus="tweets"))

ggplot(df, aes(as.numeric(p), Freq, color=Corpus)) + geom_point() + geom_line() +
  scale_y_log10() + scale_x_log10() + xlab("Word Occurance")


# some more exploration
inspect(dtm2[1:20,])

findFreqTerms(dtm2, max(dtm2)/10)
findFreqTerms(dtm2[,1], max(dtm2[,1])/10)
findFreqTerms(dtm2[,2], max(dtm2[,2])/10)
findFreqTerms(dtm2[,3], max(dtm2[,3])/10)
length(findFreqTerms(dtm2, 0, 0))
length(findFreqTerms(dtm2))



# word frequencies and rank
#v1 <- sort(rowSums(as.matrix(dtm2[findFreqTerms(dtm2[,1],1),1])), decreasing=TRUE)  # calculate the relative frequency of words
#d1 <- data.frame(Word=names(v1), Freq=v1, RelFreq=v1/sum(v1), Rank=rank(-v1))
#v2 <- sort(rowSums(as.matrix(dtm2[findFreqTerms(dtm2[,2],1),2])), decreasing=TRUE)  # calculate the frequency of words
#d2 <- data.frame(Word=names(v2), Freq=v2, RelFreq=v2/sum(v2), Rank=rank(-v2))
#v3 <- sort(rowSums(as.matrix(dtm2[findFreqTerms(dtm2[,3],1),3])), decreasing=TRUE)  # calculate the frequency of words
#d3 <- data.frame(Word=names(v3), Freq=v3, RelFreq=v3/sum(v3), Rank=rank(-v3))
#d <- rbind(transform(d1,Corpus="blogs"),transform(d2,Corpus="news"),transform(d3,Corpus="tweets"))




