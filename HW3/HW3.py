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

with open('nyt_ac.json') as data_file:
	nyt_stories = json.load(data_file)
	
# data are stored as a list at the top level; each item is a dict with two keys, one for the 'body'
# and one for the metadata ('meta'); the element 'body' is also a dict with two keys, one for 'title'
# of the article and the other for 'body_text'; the element 'meta' has many items, the only relevant one
# of which is 'dsk' for the desk the story comes from

# writing all of the stories to their own files in a new directory
for i in range(0, len(nyt_stories), 1):
	story_title = nyt_stories[i]["body"]["title"]
	story_title = re.sub("\W", "_", story_title)
	story_text = nyt_stories[i]["body"]["body_text"]
	with open("HW3/NYT_Story_Texts/" + str(story_title) + ".txt", "w") as text_file:
		text_file.write(story_text)
		
# creating a document term matrix with the 1000 most used terms
		
pt = PorterStemmer()
stop_pt = map(pt.stem, stop)


story_list = list()
for i in range(0, len(nyt_stories), 1):		
	story_title = nyt_stories[i]["body"]["title"]
	story_title = re.sub("\W", "_", story_title)
	story_text = nyt_stories[i]["body"]["body_text"]	
	story_desk = nyt_stories[i]["meta"]["dsk"]
	story_text = story_text.lower()
	story_text = re.sub('\W', ' ', story_text)
	story_text = word_tokenize(story_text)
	story_text = map(pt.stem, story_text)
	story_text = [k for k in story_text if k not in stop_pt]
	story_list.append(dict({"Title": story_title, "Desk": story_desk, "Text": story_text}))
	
unigram_dict = {}
for k in range(0, len(story_list), 1):
	story_text = story_list[k]["Text"]
	for j in range(0, len(story_text), 1):
		if story_text[j] in unigram_dict.keys():
			unigram_dict[story_text[j]] += 1
		else:
			unigram_dict[story_text[j]] = 1

common_unigrams = dict(Counter(unigram_dict).most_common(1000))

for x in range(0, len(story_list), 1):
	story = story_list[x]
	story_text = story["Text"]
	unigram_dict = dict.fromkeys(common_unigrams.keys(), 0)
	unigram_dict["Story Title"] = story["Title"]
	unigram_dict["Story Desk"] = story["Desk"]
	for k in range(0, len(story_text), 1):
		if story_text[k] in common_unigrams.keys():
			unigram_dict[story_text[k]] += 1
	if x==0:
		with open('HW3/unigram_dm.csv', 'wb') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writeheader()
			w.writerow(unigram_dict)
	else:
		with open('HW3/unigram_dm.csv', 'ab') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writerow(unigram_dict)	
		
import sys
reload(sys)  # Reload does the trick!
sys.setdefaultencoding('ASCII')

##############################
### Dictionary Classification Methods

positive_words = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read().splitlines()
positive_words_port = map(pt.stem, positive_words)

negative_words = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read().splitlines()
negative_words_port = map(pt.stem, negative_words)

story_emo_list = list()
for x in range(0, len(story_list), 1):
	story = story_list[x]
	story_text = story["Text"]
	story_title = story["Title"]
	story_desk = story["Desk"]
	story_text_positive_words_port = [k for k in story_text if k in positive_words_port]
	num_positive_words_port = len(story_text_positive_words_port)
	story_text_negative_words_port = [k for k in story_text if k in negative_words_port]
	num_negative_words_port = len(story_text_negative_words_port)
	positivity = (num_positive_words_port - num_negative_words_port) / float(len(story_text))
	if x >186:
		after_election =1
	else:
		after_election = 0
	story_emo_list.append(dict({"Title": story_title, "Desk": story_desk, "Text": story_text, "Positive Words": num_positive_words_port, "Negative Words":num_negative_words_port, "Positivity":positivity, "After Election":after_election}))
	if x==0:
		with open('HW3/nyt_emo.csv', 'wb') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Title", "Desk", "Text", "Positive Words", "Negative Words", "Positivity", "After Election"))
			my_writer.writeheader()
			my_writer.writerow({"Title": story_title, "Desk": story_desk, "Text": story_text, "Positive Words": num_positive_words_port, "Negative Words":num_negative_words_port, "Positivity":positivity, "After Election":after_election})
	else:
		with open('HW3/nyt_emo.csv', 'ab') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Title", "Desk", "Text", "Positive Words", "Negative Words", "Positivity", "After Election"))
			my_writer.writerow({"Title": story_title, "Desk": story_desk, "Text": story_text, "Positive Words": num_positive_words_port, "Negative Words":num_negative_words_port, "Positivity":positivity, "After Election":after_election})
