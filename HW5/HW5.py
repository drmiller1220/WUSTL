from bs4 import BeautifulSoup
from urllib import urlopen
import urllib2 
import json
import numpy
import re
import os
import csv
from collections import Counter
from nltk import word_tokenize
from nltk import bigrams
from nltk import trigrams
from nltk import ngrams
from nltk.stem.lancaster import LancasterStemmer
from nltk.stem import PorterStemmer
from nltk.stem.snowball import EnglishStemmer
from nltk.util import ngrams
from nltk.corpus import stopwords
stop = set(stopwords.words('english'))
import sys
reload(sys)  # Reload does the trick!
sys.setdefaultencoding('UTF8')

os.chdir("C:/Users/drmiller1220/Documents/GitHub/WUSTL")

# Part 2

pt = PorterStemmer()
stop_pt = map(pt.stem, stop)

mach_texts_path = "Mach/MachText"
mach_texts = os.listdir(mach_texts_path)

mach_list = list()
for i in range(0, len(mach_texts), 1):
	text_name = mach_texts[i]
	text_text = open(mach_texts_path + "/" + mach_texts[i]).read().splitlines()[0]
	text_text = re.sub('\W', ' ', text_text)
	text_text = text_text.lower()
	text_text = word_tokenize(text_text)
	# applying porter stemmer
	text_text = map(pt.stem, text_text)
	# removing stop words
	text_text = [k for k in text_text if k not in stop_pt]
	mach_list.append(dict({"Name": text_name, "Text":text_text}))
	
unigram_dict = {}
for k in range(0, len(mach_list), 1):
	text_text = mach_list[k]["Text"]
	for j in range(0, len(text_text), 1):
		if text_text[j] in unigram_dict.keys():
			unigram_dict[text_text[j]] += 1
		else:
			unigram_dict[text_text[j]] = 1

common_unigrams = dict(Counter(unigram_dict).most_common(500))

for x in range(0, len(mach_list), 1):
	mach = mach_list[x]
	mach_text = mach["Text"]
	unigram_dict = dict.fromkeys(common_unigrams.keys(), 0)
	unigram_dict["Name"] = mach["Name"]
	for k in range(0, len(mach_text), 1):
		if mach_text[k] in common_unigrams.keys():
			unigram_dict[mach_text[k]] += 1
	if x==0:
		with open('HW5/unigram_dm.csv', 'wb') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writeheader()
			w.writerow(unigram_dict)
	else:
		with open('HW5/unigram_dm.csv', 'ab') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writerow(unigram_dict)	
		
import sys
reload(sys)  # Reload does the trick!
sys.setdefaultencoding('ASCII')