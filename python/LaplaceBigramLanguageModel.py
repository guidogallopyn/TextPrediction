import math
from collections import defaultdict

class LaplaceBigramLanguageModel:

  def __init__(self, corpus):
    """Initialize data structures in the constructor."""
    self.unigrams = defaultdict(int) # unigram counts
    self.bigrams = defaultdict(int)  # bigram counts
    self.f1 = defaultdict(float)     # log p for observed unigrams
    self.f2 = defaultdict(float)     # log p for observed bigrams
    self.total = 0                   # total tokens in corpus
    self.V = 0                       # vocabulary size
    self.train(corpus)

  def train(self, corpus):
    """ Takes a corpus and trains a language model. 
        Compute counts and Laplace-smoothed probabilities
    """  
    prev_token = '</s>' 
    for sentence in corpus.corpus: # iterate over sentences in the corpus
      for token in sentence: # iterate over datums in the sentence
        bigram = prev_token + ' ' + token
        self.bigrams[bigram] += 1
        self.unigrams[token] += 1
        self.total += 1
        prev_token = token
    self.V = len(self.unigrams)          # vocabulary size   
    for unigram,count in self.unigrams.items():
       	self.f1[unigram] = - math.log10(count + self.V)  
    self.f2["<UNK>"] =  - math.log10(self.total + self.V)  # OOV modeling  	  
    for bigram,bgcount in self.bigrams.items():
        prevword = bigram.split()[0] 
        ugcount = self.unigrams[prevword]
       	self.f2[bigram] =  math.log10(bgcount+1) - math.log10(ugcount+self.V)        

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using the language model.
    """
    score = 0.0
    prev_token = "<s>"            # start of sentence modeling
    for token in sentence:
      bigram = prev_token + ' ' + token
      if bigram in self.f2: wscore = self.f2[bigram]
      elif token in self.f1: wscore = self.f1[token]
      else: wscore = self.f2["<UNK>"]   # OOV modeling
      prev_token = token 
      score += wscore 
      print token + ' : ' + str(wscore)
    return score

  def printLM(self):
    print('Laplace Unigram Language Model unigram counts\n')
    for token in self.unigrams:
      print(str(self.unigrams[token]) + '\t' + token )
    print('Laplace Bigram Language Model bigram counts\n')
    for token in self.bigrams:
      print(str(self.bigrams[token]) + '\t' + token )

  def save(self, filename):
    """save the LM in ARPA ngram format"""
    target = open(filename, 'w')
    target.write("\\data\\\n")
    target.write("ngram 1=" + str(len(self.f1)) + "\n")
    target.write("ngram 2=" + str(len(self.f2)) + "\n\n")
    target.write("\\1-grams:\n")
    for w,p in sorted(self.f1.items()):   
        target.write(str(p) + " " + w + "\n")
    target.write("\n\\2-grams:\n")
    for bg,p in sorted(self.f2.items()):   
        target.write(str(p) + " " + bg + "\n")    
    target.write("\\end\\\n")
    target.close()