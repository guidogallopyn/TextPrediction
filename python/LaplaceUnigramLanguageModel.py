import math
from collections import defaultdict

class LaplaceUnigramLanguageModel:

  def __init__(self, corpus):
    """Initialize object in the constructor."""
    self.unigrams = defaultdict(int)
    self.f1 = defaultdict(float)
    self.total = 0
    self.train(corpus)

  def train(self, corpus):
    """ Takes a corpus and trains the language model. 
        Compute any counts or other corpus statistics.
    """  
    for sentence in corpus.corpus: # iterate over sentences in the corpus
      for token in sentence: # iterate over datums in the sentence
        self.unigrams[token] += 1
        self.total += 1
    V = len(self.unigrams) # vocabulary size    
    for ug,count in self.unigrams.iteritems():
       	self.f1[ug] = math.log10(count+1) - math.log10(self.total + V) 
       	

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using your language model. 
    """
    score = 0.0
    V = len(self.f1)     # vocabulary size
    for token in sentence:
       if token in self.f1: score += self.f1[token]
       else: score -= math.log10(self.total + V)		   # OOV 
    return score

  def printLM(self):
    print('Laplace Unigram Language Model counts\n')
    for token in self.unigrams:
      print(str(self.unigrams[token]) + '\t' + token )
      
  def save(self, filename):
    """save the LM in ARPA ngram format"""
    target = open(filename, 'w')
    target.write("\\data\\\n")
    target.write("ngram 1=" + str(len(self.f1)) + "\n\n")
    target.write("\\1-grams:\n")
    for w,p in sorted(self.f1.items()):   
        target.write(str(p) + " " + w + "\n")
    target.write("\\end\\\n")
    target.close()
        
