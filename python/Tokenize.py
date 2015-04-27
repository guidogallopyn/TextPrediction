import string

def _removeNonAscii(s): return "".join(i for i in s if ord(i)<128)

def Tokenize(line):
    line = line.strip()
    line = _removeNonAscii(line)
    line = line.lower() 
    line = line.translate(string.maketrans(string.punctuation, " " * len(string.punctuation))) #translate punctuation to whitespace
    line = line.translate(string.maketrans("",""), string.digits)  # delete digits
    if line == '': return None
    return line.split()
