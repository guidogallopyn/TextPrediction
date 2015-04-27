import math
from collections import defaultdict


class UnigramLanguageModel:

  def __init__(self, corpus):
    self.unigramCounts = defaultdict(int)
    self.total = 0
    self.train(corpus)
   
  def train(self, corpus):
    """Takes a corpus, does whatever training is needed."""
    for sentence in corpus.corpus:
      for token in sentence:  
        self.unigramCounts[token] += 1
        self.total += 1
    
  def score(self, sentence):
    """Takes a list of strings, returns a score of that sentence."""
    score = 0.0 
    for token in sentence:
      if token in self.unigramCounts:
        count = self.unigramCounts[token]
        score += math.log(count)
        score -= math.log(self.total)
      else:
        score = float('-inf') # not smoothed
    return score

  def save(self, filename):
    """save the LM in ARPA ngram format"""
    target = open(filename, 'w')
    target.write("\\data\\\n")
    target.write("ngram 1=" + str(len(self.unigramCounts)) + "\n\n")
    target.write("\\1-grams:\n")
    for w,count in sorted(self.unigramCounts.items()):   
      	p = math.log10(count) - math.log10(self.total) 
        target.write(str(p) + " " + w + "\n")
    target.write("\\end\\\n")
    target.close()
    
