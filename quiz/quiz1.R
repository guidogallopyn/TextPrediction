# exploratory analysis script for Capstone task 0

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

# question 1: The en_US.blogs.txt file is how many megabytes?
file.info("data/final/en_US/en_US.blogs.txt")[["size"]]/1024/1024

# question 2: The en_US.twitter.txt has how many lines of text?
eng <- VCorpus(DirSource("data/final/en_US/"))

length(content(eng[["en_US.twitter.txt"]]))
#alternatives
length(eng[["en_US.twitter.txt"]][["content"]])
length(content(eng[meta(eng, "id") == "en_US.twitter.txt"][[1]]))

# question 3: What is the length of the longest line seen in any of the three en_US data sets?
sapply(eng, function(x) c(meta(x,"id"),max(nchar(content(x)))))

# question 4; In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
nlove <- sum(grepl("love",content(eng[["en_US.twitter.txt"]])))
nhate <- sum(grepl("hate",content(eng[["en_US.twitter.txt"]])))
nlove/nhate

#question 5: The one tweet in the en_US twitter data set that matches the word "biostats" says what?
grep("biostats",content(eng[["en_US.twitter.txt"]]), value=TRUE)

#question 6: How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",content(eng[["en_US.twitter.txt"]])))
