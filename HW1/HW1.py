from bs4 import BeautifulSoup
from urllib import urlopen
import csv
import re, os
import urllib2 
from nltk import word_tokenize
from nltk import bigrams
from nltk import trigrams
from nltk import ngrams
from nltk.stem.lancaster import LancasterStemmer
from nltk.stem import PorterStemmer
from nltk.stem.snowball import EnglishStemmer

# part 1

debate_html = open("C:\Users\drmiller1220\Documents\GitHub\WUSTL\HW1\Debate1.html")

soup = BeautifulSoup(debate_html, 'html.parser')

speaker_statements = soup.find_all("p")

# setting the speaker to begin with NONE to handle intro text
current_speaker = "NONE"

# for loop identifies whether there is a speaker in the statement; if there is not, refer to
# the previous statement and use that speaker
for i in range(0, len(speaker_statements), 1):
	if "OBAMA" in speaker_statements[i].get_text().split(' ', 1)[0]:
		current_speaker = "OBAMA"
		speaker_statements[i] = speaker_statements[i].get_text()
	elif "ROMNEY" in speaker_statements[i].get_text().split(' ', 1)[0]:
		current_speaker = "ROMNEY"
		speaker_statements[i] = speaker_statements[i].get_text()
	elif "LEHRER" in speaker_statements[i].get_text().split(' ', 1)[0]:
		current_speaker = "LEHRER"
		speaker_statements[i] = speaker_statements[i].get_text()
	else:
		speaker_statements[i] = current_speaker + ": " + speaker_statements[i].get_text()
		
statements = [speaker_statements[0]]
current_speaker = "NONE"

# for loop adjoins all consecutive statements with a common speaker

for i in range(1, len(speaker_statements), 1):
	if speaker_statements[i].split(":", 1)[0]==current_speaker:
		statements[len(statements)-1] = statements[len(statements)-1] + speaker_statements[i].split(":", 1)[1]
	else:
		current_speaker = speaker_statements[i].split(":", 1)[0]
		statements.append(speaker_statements[i])

# removing notes for audience behavior
for i in range(0, len(statements), 1):
	statements[i] = re.sub("(\([^()]*\))", "", statements[i])
		
#part 2

from nltk.corpus import stopwords
stop = set(stopwords.words('english'))


positive_words = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/positive.txt").read().splitlines()

negative_words = urllib2.urlopen("http://www.unc.edu/~ncaren/haphazard/negative.txt").read().splitlines()

st = LancasterStemmer()
pt = PorterStemmer()
sb = EnglishStemmer()

positive_words_lan = map(st.stem, positive_words)
positive_words_port = map(pt.stem, positive_words)
positive_words_snow = map(sb.stem, positive_words)

negative_words_lan = map(st.stem, negative_words)
negative_words_port = map(pt.stem, negative_words)
negative_words_snow = map(sb.stem, negative_words)

statements = statements[1:len(statements)]
# used to remove miscellaneous text at the end of the file
statements[166] = statements[166].split("COPYRIGHT")[0]


# for loop tallies numbers of positive and negative words for each statement and writes the counts
# to a csv
for i in range(0, len(statements), 1):
	statement_number = i
	speaker = statements[i].split(":", 1)[0]
	statements[i] = re.sub('\W', ' ', statements[i])
	statements[i] = statements[i].lower()
	statements[i] = word_tokenize(statements[i])
	nonstop_words = [k for k in statements[i] if k not in stop]
	num_nonstop_words = len(nonstop_words)
	statement_positive_words = [k for k in statements[i] if k in positive_words]
	num_positive_words = len(statement_positive_words)
	statement_negative_words = [k for k in statements[i] if k in negative_words]
	num_negative_words = len(statement_negative_words)
	statement_lan = map(st.stem, nonstop_words)
	statement_port = map(pt.stem, nonstop_words)
	statement_snow = map(sb.stem, nonstop_words)
	statement_positive_words_lan = [k for k in statement_lan if k in positive_words_lan]
	num_positive_words_lan = len(statement_positive_words_lan)
	statement_negative_words_lan = [k for k in statement_lan if k in negative_words_lan]
	num_negative_words_lan = len(statement_negative_words_lan)
	statement_positive_words_port = [k for k in statement_port if k in positive_words_port]
	num_positive_words_port = len(statement_positive_words_port)
	statement_negative_words_port = [k for k in statement_port if k in negative_words_port]
	num_negative_words_port = len(statement_negative_words_port)
	statement_positive_words_snow = [k for k in statement_snow if k in positive_words_snow]
	num_positive_words_snow = len(statement_positive_words_snow)
	statement_negative_words_snow = [k for k in statement_snow if k in negative_words_snow]
	num_negative_words_snow = len(statement_negative_words_snow)
	if i==0:
		with open('debatewords.csv', 'wb') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Statement Number", "Speaker", "Number of non-stop words", "Number of positive words", "Number of negative words", "Number of Lancaster stemmed positive words", "Number of Lancaster stemmed negative words", "Number of Porter stemmed positive words", "Number of Porter stemmed negative words", "Number of Snowball stemmed positive words", "Number of Snowball stemmed negative words"))
			my_writer.writeheader()
			my_writer.writerow({"Statement Number":statement_number, "Speaker":speaker, "Number of non-stop words":num_nonstop_words, "Number of positive words":num_positive_words, "Number of negative words":num_negative_words, "Number of Lancaster stemmed positive words":num_positive_words_lan, "Number of Lancaster stemmed negative words":num_negative_words_lan, "Number of Porter stemmed positive words":num_positive_words_port, "Number of Porter stemmed negative words":num_positive_words_port, "Number of Snowball stemmed positive words":num_positive_words_snow, "Number of Snowball stemmed negative words":num_negative_words_snow})
	else:
		with open('debatewords.csv', 'ab') as f:
			my_writer = csv.DictWriter(f, fieldnames=("Statement Number", "Speaker", "Number of non-stop words", "Number of positive words", "Number of negative words", "Number of Lancaster stemmed positive words", "Number of Lancaster stemmed negative words", "Number of Porter stemmed positive words", "Number of Porter stemmed negative words", "Number of Snowball stemmed positive words", "Number of Snowball stemmed negative words"))
			my_writer.writerow({"Statement Number":statement_number, "Speaker":speaker, "Number of non-stop words":num_nonstop_words, "Number of positive words":num_positive_words, "Number of negative words":num_negative_words, "Number of Lancaster stemmed positive words":num_positive_words_lan, "Number of Lancaster stemmed negative words":num_negative_words_lan, "Number of Porter stemmed positive words":num_positive_words_port, "Number of Porter stemmed negative words":num_negative_words_port, "Number of Snowball stemmed positive words":num_positive_words_snow, "Number of Snowball stemmed negative words":num_negative_words_snow})