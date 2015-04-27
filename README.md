# TextPrediction

this project contains all the sources for the Capstone TextPrediction project for the Data Sciences Specialization on Coursera.

the direcory structure is as follows

- NGramLM.R : final S3 object for NGrams in R
- task 0..3: exploration dirctorsie with various R scripts 
- task4
   + traintest.R: script to split corpora in ttaining and test sets
   + count.R: script to count Ngrams, counts stored in counts/
   + train.R: training scritpt that builds series of models with specifed parameters 
   + evaluate.R measures coverage on evaluation corpus
   + evaluate2.R; measures perplexity on evaluation corpus
   + evaluate3.R: measures prediction accuracy on evaluation corpus, measurements saore in acc/
- quiz: scripts to provide answers to quizes
- python: python LM building (not used in final submission)
- MilestoneReport: Capstone 1.Rmd is the source for the report
- shinyapp: self contained directory with all resources for the Text predictor show case app
- FinalPresentation: FinalReport.RPres is the source for the presentation, others are data files
- FinalReport; NGramLM.RData contains source for final report (un-finished, not submitted)

not included
- data: all data downloaded for this project, and splt and subsampled versions
- models: directory with all models build 
