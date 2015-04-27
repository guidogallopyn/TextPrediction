
This data product shocases the text prediction algorithm that I have built for the Coursera Data Science Capstone project.

It is a Shiny app that provides an interface to evaluate the text prediction technology. 

1. it takes as input a phrase (multiple words) in a text box input and outputs a prediction of the top-5 next best words (best on top)

2. it provides an auto-complete demonstration that simulates a mobile text App, and shows best choices got a word can be completed. This allows for an evaluation of performance give a word prefix (auto-complete) and gives a good sense of the prediction speed. 

3. it provides a glimpse under the hood, with summary statistics on the underlying language models.

4. the app allows evaluation of 3 different models with different size and performance characteristics.


### Notes/Known Errors/Etc.

The mobile app simulation keyboard and text editing functionality have a limited functionality needed to demonstrate the speed gain with auto complete and next word prediction. There is no support for capitalization, punctuation or special characters. The cursor position is assumed to be at the end of the text.


### Thanks
Thanks to Jeff Leek, Roger Peng & Brian Caffo for putting together and running the Data Specialization on Coursera. I've learned a lot.

### Author
This data product was developed by Guido Gallopyn for the Capstone project of the Data Science Specialization on Coursera from March to April 2015
