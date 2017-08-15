from bs4 import BeautifulSoup
from urllib import urlopen
import re, os

debate_html = open("C:\Users\drmiller1220\Documents\GitHub\WUSTL\Debate1.html")

soup = BeautifulSoup(debate_html, 'html.parser')

speaker_statements = soup.find_all("p")

current_speaker = None

for i in range(0, len(speaker_statements), 1):
	if "OBAMA" in speaker_statements[i].get_text().split(' ', 1)[0]