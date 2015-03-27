# exploratory analysis script for Capstone task 0

# learning tm

library(tm)
getSources()
getReaders()

txt <- system.file("texts", "txt", package = "tm")
ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"), readerControl = list(language = "lat"))
ovid[[1]]

docs <- c("This is a text.", "This another one.")
corp1 <- VCorpus(VectorSource(docs))
corp1[[1]]

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain))
reuters[[1]]

inspect(ovid[1:2])
meta(ovid[[2]], "id")

#text proc
reuters <- tm_map(reuters, stripWhitespace)
reuters <- tm_map(reuters, content_transformer(tolower))
reuters <- tm_map(reuters, removeWords, stopwords("english"))

# filters
idx <- meta(reuters, "id") == '237' & 
       meta(reuters, "heading") == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE' 
reuters[idx]

# term-document matrices

dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])

findFreqTerms(dtm, 5)
findAssocs(dtm, "opec", 0.8)
inspect(removeSparseTerms(dtm, 0.4))

#dictionary
inspect(DocumentTermMatrix(reuters, list(dictionary = c("prices", "crude", "oil"))))
