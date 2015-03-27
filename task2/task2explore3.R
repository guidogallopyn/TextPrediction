# exploratory analysis script for Capstone task 2

# Questions to consider
# Some words are more frequent than others - what are the distributions of word frequencies? 
# What are the frequencies of 2-grams and 3-grams in the dataset? 
# --> How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# How do you evaluate how many of the words come from foreign languages? 
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(rJava)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)



# read an english corpus
#eng <- VCorpus(DirSource("data/final/en_US/"))
eng <- VCorpus(DirSource("data/small/en_US/"))
#eng <- VCorpus(DirSource("data/micro/en_US/"))


Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)

# summary statistics
Corpus.summary(eng)

# Questions
# 2) What are the frequencies of 2-grams and 3-grams in the dataset? 

# cleaning
eng <- tm_map(eng, stripWhitespace)
eng <- tm_map(eng, content_transformer(tolower))
eng <- tm_map(eng, removePunctuation, preserve_intra_word_dashes = TRUE)
eng <- tm_map(eng, removeNumbers)
#eng <- tm_map(eng, removeWords, stopwords("english"))

# clean corpus summary statistics
Corpus.summary(eng)

tdm <- TermDocumentMatrix(eng, control=list(tokenize = RWeka::WordTokenizer))

# total unique words
nTerms(tdm)
length(findFreqTerms(tdm,1))
length(findFreqTerms(tdm,2))


# corpus coverage with vocabulary size (measured on training set) 
v <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE) # words and their frequencies sorted 
df <- data.frame(Terms=1:length(v), Coverage=cumsum(v)/sum(v))

# 50% and 90% coverage vocabulary sizes
(lin <- with(df, approx(Coverage,Terms,c(0.5, 0.9))))

ggplot(df, aes(x=Terms, y=Coverage)) + geom_line(colour="blue") + 
  scale_x_log10(breaks=10^(0:4)) + scale_y_continuous(breaks=(0:10)/10) +
  xlab("Vocabulary Size") + ylab("Coverage") + ggtitle("Corpus Coverage in function of Vocabulary Size") +
  geom_vline(xintercept = lin$y,linetype="dashed") + 
  geom_hline(yintercept = lin$x,linetype="dashed") +
  annotate("text", x = lin$y, y = .1, label = round(lin$y))
  
# measuring coverage on a test set
set.seed(123)
tmp <- Reduce(c, Map(content,eng))
train <- sample(length(tmp),length(tmp)*0.6) # split the corpus 60/40 training and test set
corpus <- c( PlainTextDocument(tmp[train], id="training"), 
             PlainTextDocument(tmp[-train],id="testing") ) # is a VCorpus

#now term frequencies on training and test sets
tdm <- TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer))

v.train <- sort(rowSums(as.matrix(tdm[,"training"])), decreasing=TRUE) # training set words and their frequencies sorted
v.train <- v.train[v.train > 0]
df <- data.frame(Terms=1:length(v.train), TrainCoverage=cumsum(v.train)/sum(v.train))

lseq <- function(from,to,length.out) round(exp(seq(log(from), log(to), length.out = length.out)))
df2 <- data.frame(Terms=lseq(1, length(v.train), 100))
for (n in 1:nrow(df2)) 
  df2$TestCoverage[n] <- sum(tdm[names(v.train[1:df2$Terms[n]]),"testing"])/sum(tdm[,"testing"])

(lin1 <- with(df, approx(TrainCoverage,Terms,c(0.5, 0.9))))
(lin2 <- with(df2, approx(TestCoverage,Terms,c(0.5, 0.9))))

ggplot(df, aes(x=Terms)) + 
  geom_line(aes(y=TrainCoverage), colour="blue") + 
  geom_line(data=df2,aes(y=TestCoverage), colour="red") +
  scale_x_log10(breaks=10^(0:4)) + scale_y_continuous(breaks=(0:10)/10) +
  xlab("Vocabulary Size") + ylab("Coverage") + ggtitle("Corpus Coverage in function of Vocabulary Size") +
  geom_hline(yintercept = lin1$x,linetype="dashed") +
  geom_vline(xintercept = lin1$y,linetype="dashed") + 
  geom_vline(xintercept = lin2$y,linetype="dashed") + 
  geom_vline(xintercept = length(v.train),linetype="dashed") +
  geom_hline(yintercept = df2$TestCoverage[nrow(df2)],linetype="dashed") +
  annotate("text", x = lin1$y, y = .1, label = round(lin1$y), color="blue") +
  annotate("text", x = lin2$y, y = .15, label = round(lin2$y), color="red") +
  annotate("text", x = 1, y = .97, label = round(df2$TestCoverage[nrow(df2)],2), color="red")
