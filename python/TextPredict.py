from CapstoneCorpus import CapstoneCorpus
from Tokenize import Tokenize
from UniformLanguageModel import UniformLanguageModel
from UnigramLanguageModel import UnigramLanguageModel
from LaplaceUnigramLanguageModel import LaplaceUnigramLanguageModel
from LaplaceBigramLanguageModel import LaplaceBigramLanguageModel
from LaplaceNgramLanguageModel import LaplaceNgramLanguageModel
from CustomLanguageModel import CustomLanguageModel




    
def main():
    """Trains all of the language models and tests them on the dev data. Change devPath if you
     wish to do things like test on the training data.
    """
  
    trainPath = '../data/micro/en_US/'
    trainingCorpus = CapstoneCorpus(trainPath)
    #print str(trainingCorpus)
  
    sent = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
    tokens = Tokenize(sent)
  
    print 'Uniform Language Model: '
    uniformLM = UniformLanguageModel(trainingCorpus)
    print "VocSize= " + str(len(uniformLM.words))
    print sent
    print tokens
    print "uniform score=" + str(uniformLM.score(tokens))
  
    print 'Unigram Language Model: '
    unigramLM = UnigramLanguageModel(trainingCorpus)
    print "VocSize= " + str(len(unigramLM.unigramCounts))
    print "unigram score=" + str(unigramLM.score(tokens))
  
    print 'Laplace Unigram Language Model: ' 
    laplaceUnigramLM = LaplaceUnigramLanguageModel(trainingCorpus)
    laplaceUnigramLM.save("smallUnigram.LM")
    print "VocSize= " + str(len(laplaceUnigramLM.f1))
    print "unigram score=" + str(laplaceUnigramLM.score(tokens))
  
    print 'Laplace Bigram Language Model: '
    laplaceBigramLM = LaplaceBigramLanguageModel(trainingCorpus)
    laplaceBigramLM.save("smallBigram.LM")
    print "bigram score=" + str(laplaceBigramLM.score(tokens))
  
    print 'Laplace Ngram Language Model: N=2'
    laplaceN2gramLM = LaplaceNgramLanguageModel(trainingCorpus,2)
    laplaceN2gramLM.save("smallN2gram.LM")
    print "N=2gram score=" + str(laplaceN2gramLM.score(tokens))

    print 'Laplace Ngram Language Model: N=3'
    laplaceN3gramLM = LaplaceNgramLanguageModel(trainingCorpus,3)
    laplaceN3gramLM.save("smallN3gram.LM")
    print "N=3gram score=" + str(laplaceN2gramLM.score(tokens))

    print 'Custom Language Model: '
    customLM = CustomLanguageModel(trainingCorpus,N=2)
    print "Custom LM score=" + str(customLM.score(tokens))

if __name__ == "__main__":
    main()
