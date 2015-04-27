import math
from collections import defaultdict

# implements a collection of smoothing altgorithms
# uses explicit OOVhandeling
# cdiscount: absolute discounting with bigrams, score =?
# kndiscount: knese-ney discouning, score 8.1
# stupid backoff with explicit OOV handeling, score 17.0
# stupid backoff baseline, score 18.0
# bigram with unigram prior smoothing, score 20.8
# bigram with good-turing smoothing, score 23.1  -> BEST
#
# baseline performance with stupid backoff with bigrams is (18.0)
def goodturing_discounts (counts, gtmax):
    """  Calculates good-turing discount factors given a dictionary with counts"""
    #   r' = (r+1) n[r+1]/n[r]
    #  f(a_z) = c(a_z) / c(a_)  if c(a_z) > gtmax with gtmax=7 for n>1, and gtmax=1 for unigrams
    #  f(a_z) = disc * (c(a_z) / c(a_)) with disc = (
    tab = defaultdict(int)
    for ng in counts: tab[counts[ng]] += 1 # count frequency of counts
    A = (gtmax+1) * tab[gtmax+1]/tab[1]
    disc = [(1 + 1/cnt) * tab[cnt+1] / tab[cnt] for cnt in counts]
    return [ (d - A)/(1-A) for d in disc ]
 
class CustomLanguageModel:

    def __init__(self, corpus, N=2, smoothing='goodturing' ):
        """Initialize data structures in the constructor."""
        self.N = N
    
        self.ngrams = [defaultdict(int) for x in range(N)] # ngram counts
        self.f   = [defaultdict(float) for x in range(N)]   # log p for observed ngrams
        self.bow = [defaultdict(float) for x in range(N)]   # weights
    
        self.predecessors = [defaultdict(int) for x in range(N)]  # count of word as successor
        self.successors = [defaultdict(int) for x in range(N)]  # count of word as predecessor
    
        self.total = 0   # corpus size
        self.V = 0 # vocabulary size
        
        self.n = [defaultdict(int) for x in range(N)]  # frequency of bigram frequency (counts)
  
        self.n1 = self.n2 = self.n3 = 0
        self.lambda1 = self.lambda2 = 0.5 
    
        self.method = smoothing
        self.train(corpus)

    def setLambda(self, lambda1):
        self.lambda1 = lambda1       # bigram weight
        self.lambda2 = 1.0 - lambda1 # unigram weight

    def setK(self, k):
        self.K = k      # k
        self.M = self.K * self.V # 
    
    def train(self, corpus):
        """ Takes a corpus and trains the language model. 
        """    
        # N gram counting    
        for sentence in corpus.corpus: # iterate over sentences in the corpus
            ngram = ['</s>'] * (self.N-1)  # list with N-1 previous tokens
            for token in sentence:   # iterate over tokens in the sentence
                ngram.append(token)
                #prefix, suffix = ngram[:-1], ngram[1:]   # N-1 prefix and suffix list of tokens
                for n in range(self.N) :
                    nspan = ngram[self.N-n-1:]    # n tokens with token the last one
                    key = ' '.join(nspan)         # construct key for ngram counting
                    if(n > 0 and not key in self.ngrams[n]): # novel continuation not for unigrams
                        self.predecessors[n][token] +=  1
                        prefix =' '.join(nspan[:-1])
                        self.successors[n][prefix] +=  1  #check!!!
                    self.ngrams[n][key] += 1  # add 1 to the count 
                self.total += 1
                ngram = ngram[1:]          # list with last N-1 tokens
    
    # problem here with </s> and <s> distorting unigram stats
    # on one hand unigram counts are needed to calc bigrams [<s> w1] so c(<s>) is needed but on the other hand in unigram probs it is not needed c(w)/V  
    
        # number of ngrams with count n
        for n in range(self.N) :
            for bg in self.ngrams[n]:
                self.n[n][self.ngrams[n][bg]] += 1 

        # explicit OOV modeling
        self.ngrams[0]['<UNK>'] = self.n[0][1]   # good turing intuition
        self.V = len(self.ngrams[0]) # vocabulary size
        self.total += self.V
        #self.predecessors['<UNK>'] = 1
        #self.successors['<UNK>'] = 1

        # parameters
        if self.method == 'cdiscount': # Ney absolute discounting
            self.d = 0.75
        elif self.method == 'kndiscount': # Kneser-Ney discounting
            pass
        elif self.method == 'stupidbackoff': # Stupid backoff
            pass
        elif self.method == 'stupidbackoffOOV': # Stupid backoff with OOV modeling
            pass
        elif self.method == 'interpolatedLM': # Interpolated laplace smoothed bigrams and uniframs
            self.setLambda(0.5)
        elif self.method == 'unigrampriorsmoothing': # unigram prior smoothed bigram
            self.setK(0.07) # paramter determined by maximizing on spelling task
        elif self.method == 'goodturing': # good turing smoothed bigram
            self.setLambda(1.0) # paramter determined by maximizing on spelling task
        elif self.method == 'goodturingOOV': # interpolated bigram-unigram LM with good turing smoothing and OOV modeling
            self.setLambda(1.0) # paramter determined by maximizing on spelling task

    """
    Smoothing methods to decrease probability mass to allow unobserved events
    see http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html
    back-off
      p(a_z) = (c(a_z) > 0) ? f(a_z) : bow(a_) p(_z)
    interpolated
      p(a_z) = g(a_z) + bow(a_) p(_z)
      or
      p(a_z) = (c(a_z) > 0) ? f(a_z) : bow(a_) p(_z) 
      f(a_z) = g(a_z) + bow(a_) p(_z)
      
    here we calculate f(a_z)  
    """

    

    def smooth_goodturing(self):
        """  Calculates good-turing discount factors given a dictionary with counts"""
        # bow(a_) = (1 - Sum_Z1 f(a_z)) / (1 - Sum_Z1 f(_z))
    
        for n in range(self.N) :
    	    disc = goodturing_discounts(self.ngrams[n])    
    	    self.bow = (1-sum)/(1-sum)
    	    # self.f = Map(lambda da_z,ca_z,ca_: da_z * ca_z / ca_, disc, self.ngrams[n] 
  
  

    """
    Smoothing methods to decrease probability mass to allow unobserved events
    see http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html
    back-off
      p(a_z) = (c(a_z) > 0) ? f(a_z) : bow(a_) p(_z)
    interpolated
      p(a_z) = g(a_z) + bow(a_) p(_z)
      or
      p(a_z) = (c(a_z) > 0) ? f(a_z) : bow(a_) p(_z) 
      f(a_z) = g(a_z) + bow(a_) p(_z)
      
    here we use f(a_z) and bow to calculate p(a_z)   
    """
   


    # mapping function for explicit OOV handeling 
    def mapOOV(self, token):
        if token in self.ngrams[0]:  # in unigrams?
            return token
        else:
            #print(token + ' --> <UNK>')
            return '<UNK>'

if __name__ == "__main__":
    from CapstoneCorpus import CapstoneCorpus
    from Tokenize import Tokenize
    
    trainPath = '../data/micro/en_US/'
    trainingCorpus = CapstoneCorpus(trainPath)
    #print str(trainingCorpus)
  
    sent = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
    tokens = Tokenize(sent)
    
    print 'Custom Language Model: '
    customLM = CustomLanguageModel(trainingCorpus,N=2)
    print "Custom LM score=" + str(customLM.score(tokens))
    
    
    

    