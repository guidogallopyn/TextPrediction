import math
from collections import defaultdict

class LaplaceNgramLanguageModel:

  def __init__(self, corpus, N):
    """Initialize data structures in the constructor."""
    self.N = N
    self.ngrams = [defaultdict(int) for x in range(N)] # ngram counts
    self.f  = [defaultdict(float) for x in range(N)]   # log p for observed nrams
    self.total = 0                   # total tokens in corpus
    self.V = 0                       # vocabulary size
    self.train(corpus)

  def train(self, corpus):
    """ Takes a corpus, counts N grams and trains a N gram language model. 
        Compute Laplace-smoothed probabilities
    """  
    for sentence in corpus.corpus: # iterate over sentences in the corpus
      ngram = ['</s>'] * (self.N-1)  # list with N-1 previous tokens
      for token in sentence: # iterate over tokens in the sentence
        ngram.append(token)
        #prefix, suffix = ngram[:-1], ngram[1:]   # N-1 prefix and suffix list of tokens
        for n in range(self.N) :
          key = ' '.join(ngram[self.N-n-1:])     # construct k for ngram counting
          self.ngrams[n][key] += 1
        self.total += 1
        ngram = ngram[1:]          # list with last N-1 tokens
    
    self.V = len(self.ngrams[0])          # vocabulary size       
    
    #probability calculations, ngram
    if self.N == 1:   #special case for unigrams
        den = math.log10(self.total + self.V) 
        for ug,count in self.ngrams[0].items():
      		self.f[0][ug] = math.log10(count + 1) - den 
      	self.f[0]["<UNK>"] =  - math.log10(self.total+self.V)  # OOV modeling 	  
    else:
    	for key,count in self.ngrams[self.N-2].items():   # prefixes
    		self.f[self.N-2][key] =  - math.log10(count+self.V)
    	for ng,count in self.ngrams[self.N-1].items():  
        	prefix = ' '.join(ng.split()[:-1])
       		self.f[self.N-1][ng] =  math.log10(count+1) + self.f[self.N-2][prefix] 
       	self.f[self.N-1]["<UNK>"] =  - math.log10(self.V)  # OOV modeling     				
      	
       	        

  def score(self, sentence):
    """ Takes a list of strings as argument and returns the log-probability of the 
        sentence using the language model.
    """
    score = 0.0
    ngram = ['</s>'] * (self.N-2)      # start of sentence modeling
    ngram.append('<s>')
    for token in sentence:
      ngram.append(token)
      key = ' '.join(ngram)
      if key in self.f[self.N-1]:      # ngram observed in training data
      	wscore = self.f[self.N-1][key]   # ngram probability
      elif self.N > 1:                 # for bigrams and up, ngram not observed
        prefix = ' '.join(ngram[:-1])
      	if prefix in self.f[self.N-2]:  # look for n-1 gram  prefix
      	  wscore = self.f[self.N-2][prefix]   
      else: # unobserved prefix 
        wscore = self.f[self.N-1]['<UNK>']   # OOV modeling
      ngram = ngram[1:] # list with last N-1 tokens
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
    for n in range(self.N) :
    	target.write("ngram " + str(n+1) + "=" + str(len(self.f[n])) + "\n")
    for n in range(self.N) :	
    	target.write("\\" + str(n+1) + "-grams:\n")
    	for ng,p in sorted(self.f[n].items()):   
        	target.write(str(p) + " " + ng + "\n")
    target.write("\\end\\\n")
    target.close()