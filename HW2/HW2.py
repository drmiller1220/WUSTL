from bs4 import BeautifulSoup
from urllib import urlopen
import csv
import re, os
import sys
import urllib2 
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
stop.update(["shelby", "sessions", "richard", "jeff", "email", "press", "room", "member", "senate"])

# part 1

os.chdir("C:/Users/drmiller1220/Documents/GitHub/WUSTL/HW2")

sessions_statements = "GrimmerSenatePressReleases-master/GrimmerSenatePressReleases-master/raw/Sessions"
sessions_statements = os.listdir(sessions_statements)
shelby_statements = "GrimmerSenatePressReleases-master/GrimmerSenatePressReleases-master/raw/Shelby"
shelby_statements = os.listdir(shelby_statements)


for i in range(0, len(sessions_statements), 1):
	statement_name = sessions_statements[i]
	statement_month = statement_name[0:2]
	statement_day = statement_name[2:5]
	statement_year = statement_name[5:9]
	statement_author = re.split('\d', statement_name[9:len(statement_name)])[0]
	statement_text = open("GrimmerSenatePressReleases-master/GrimmerSenatePressReleases-master/raw/Sessions/" + statement_name).read().splitlines()
	if i==0:
		with open('press_releases.csv', 'wb') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Statement Month", "Statement Day", "Statement Year", "Statement Author", "Statement Text"))
			my_writer.writeheader()
			my_writer.writerow({"Statement Month":statement_month, "Statement Day":statement_day, "Statement Year":statement_year, "Statement Author":statement_author, "Statement Text":statement_text})
	else:
		with open('press_releases.csv', 'ab') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Statement Month", "Statement Day", "Statement Year", "Statement Author", "Statement Text"))
			my_writer.writerow({"Statement Month":statement_month, "Statement Day":statement_day, "Statement Year":statement_year, "Statement Author":statement_author, "Statement Text":statement_text})
			
			
for i in range(0, len(shelby_statements), 1):
	statement_name = shelby_statements[i]
	statement_month = statement_name[0:2]
	statement_day = statement_name[2:5]
	statement_year = statement_name[5:9]
	statement_author = re.split('\d', statement_name[9:len(statement_name)])[0]
	statement_text = open("GrimmerSenatePressReleases-master/GrimmerSenatePressReleases-master/raw/Shelby/" + statement_name).read().splitlines()
	with open('press_releases.csv', 'ab') as f:
		my_writer = csv.DictWriter(f, fieldnames=("Statement Month", "Statement Day", "Statement Year", "Statement Author", "Statement Text"))
		my_writer.writerow({"Statement Month":statement_month, "Statement Day":statement_day, "Statement Year":statement_year, "Statement Author":statement_author, "Statement Text":statement_text})
			
# part 2
			
press_releases = csv.DictReader(open('press_releases.csv'))

result = {}
for row in press_releases:
    for column, value in row.iteritems():
        result.setdefault(column, []).append(value)
		
press_releases =  result		

pt = PorterStemmer()
stop_pt = map(pt.stem, stop)

press_releases["Trigrams"] = ["Null"] * len(press_releases["Statement Text"])

for i in range(0, len(press_releases['Statement Text']), 1):
	press_release_text = press_releases['Statement Text'][i]
	press_release_text = re.sub('\W', ' ', press_release_text)
	press_release_text = press_release_text.lower()
	press_release_text = word_tokenize(press_release_text)
	# applying porter stemmer
	press_release_text = map(pt.stem, press_release_text)
	# removing stop words
	press_release_text = [k for k in press_release_text if k not in stop_pt]
	press_releases['Statement Text'][i] = press_release_text
	press_release_text_trigrams = ngrams(press_release_text, 3)
	press_release_text_trigrams = [k for k in press_release_text_trigrams]
	press_releases['Trigrams'][i] = press_release_text_trigrams
	
unigram_dict = {}
for j in range(0, len(press_releases['Statement Text']), 1):
	press_release = press_releases['Statement Text'][j]
	for k in range(0, len(press_release), 1):
		if press_release[k] in unigram_dict.keys():
			unigram_dict[press_release[k]] += 1
		else:
			unigram_dict[press_release[k]] = 1
			
with open('unigram.csv', 'wb') as f: 
    w = csv.DictWriter(f, unigram_dict.keys())
    w.writeheader()
    w.writerow(unigram_dict)
			
trigram_dict = {}
for j in range(0, len(press_releases['Trigrams']), 1):
	press_release = press_releases['Trigrams'][j]
	for k in range(0, len(press_release), 1):
		if press_release[k] in trigram_dict.keys():
			trigram_dict[press_release[k]] += 1
		else:
			trigram_dict[press_release[k]] = 1
			
with open('trigram.csv', 'wb') as f: 
    w = csv.DictWriter(f, trigram_dict.keys())
    w.writeheader()
    w.writerow(trigram_dict)

###
	
unigram_dict = csv.DictReader(open('unigram.csv'))

result = {}
for row in unigram_dict:
    for column, value in row.iteritems():
        result.setdefault(column, []).append(value)
		
unigram_dict =  result

trigram_dict = csv.DictReader(open('trigram.csv'))

result = {}
for row in trigram_dict:
    for column, value in row.iteritems():
        result.setdefault(column, []).append(value)
		
trigram_dict =  result

###

common_unigrams = dict(Counter(unigram_dict).most_common(1000))

common_trigrams = dict(Counter(trigram_dict).most_common(500))

for m in range(0, len(common_trigrams), 1):
	original_key = common_trigrams.keys()[m]
	#if len(original_key.split(","))>1:
	tri = original_key.split(",")[0] + original_key.split(",")[1] + original_key.split(",")[2]
	tri = re.sub("[\"()]|u'", '', tri)
	tri = re.sub("'\s", ".", tri)
	tri = re.sub("'", "", tri)
	common_trigrams[tri] = common_trigrams.pop(original_key)
#	else:
#		continue

#######################################

press_releases = csv.DictReader(open('press_releases.csv'))

result = {}
for row in press_releases:
    for column, value in row.iteritems():
        result.setdefault(column, []).append(value)
		
press_releases =  result		

pt = PorterStemmer()
stop_pt = map(pt.stem, stop)

press_releases["Trigrams"] = ["Null"] * len(press_releases["Statement Text"])

for i in range(0, len(press_releases['Statement Text']), 1):
	press_release_text = press_releases['Statement Text'][i]
	press_release_text = re.sub('\W', ' ', press_release_text)
	press_release_text = press_release_text.lower()
	press_release_text = word_tokenize(press_release_text)
	# applying porter stemmer
	press_release_text = map(pt.stem, press_release_text)
	# removing stop words
	press_release_text = [k for k in press_release_text if k not in stop_pt]
	press_releases['Statement Text'][i] = press_release_text
	press_release_text_trigrams = ngrams(press_release_text, 3)
	press_release_text_trigrams = [k for k in press_release_text_trigrams]
	press_releases['Trigrams'][i] = press_release_text_trigrams
	
trigrams_list = []	
for z in range(0, len(common_trigrams.keys()), 1):
	trigram_set = eval(common_trigrams.keys()[z])
	trigrams_list.append(trigram_set)
	
for x in range(0, len(press_releases['Statement Text']), 1):
	press_release_text = press_releases['Statement Text'][x]
	unigram_dict = dict.fromkeys(common_unigrams.keys(), 0)
	unigram_dict["Statement Author"] = press_releases['Statement Author'][x]
	for k in range(0, len(press_release_text), 1):
		if press_release_text[k] in common_unigrams.keys():
			unigram_dict[press_release_text[k]] += 1
	press_release_text_trigrams = press_releases['Trigrams'][x]
	trigram_dict = dict.fromkeys(trigrams_list, 0)
	trigram_dict["Statement Author"] = press_releases['Statement Author'][x]
	for k in range(0, len(press_release_text_trigrams), 1):
		if press_release_text_trigrams[k] in trigrams_list:
			trigram_dict[press_release_text_trigrams[k]] += 1
	if x==0:
		with open('unigram_dm.csv', 'wb') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writeheader()
			w.writerow(unigram_dict)
		with open('trigram_dm.csv', 'wb') as f: 
			w = csv.DictWriter(f, trigram_dict.keys())
			w.writeheader()
			w.writerow(trigram_dict)
	else:
		with open('unigram_dm.csv', 'ab') as f: 
			w = csv.DictWriter(f, unigram_dict.keys())
			w.writerow(unigram_dict)
		with open('trigram_dm.csv', 'ab') as f: 
			w = csv.DictWriter(f, trigram_dict.keys())
			w.writerow(trigram_dict)
	