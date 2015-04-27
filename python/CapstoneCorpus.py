from os import listdir
from os.path import isfile, join
import string
from Tokenize import Tokenize

class CapstoneCorpus:
    corpus = [] # list of sentences

    def __init__(self, mypath=None):
        self.corpus = []
        if mypath:
            for filename in [ f for f in listdir(mypath) if isfile(join(mypath,f)) ]: 
                print "reading file :" + filename    
                self.read_capstonefile(join(mypath,filename))


    def processLine(self, line):
        return ["<s>"] + Tokenize(line) + ["</s>"]
  
    def read_capstonefile(self, filename):
        """Read in data, returns a list (sentence) of list(words) of lists(alternatives).
        The first item in each word list is the correct word."""
        f = open(filename)
        for line in f:
            sentence = self.processLine(line)      
            if sentence:
                self.corpus.append(sentence)
        f.close()    

    def __str__(self):
        str_list = []
        for sentence in self.corpus:
            str_list.append(str(sentence))
        return '\n'.join(str_list)   

