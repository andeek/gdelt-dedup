#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import bs4
import urllib.request
import re


GDELT_FILE_REGEX = re.compile(r"^[12].*\.zip")
BASEURL = 'http://data.gdeltproject.org/events/'

html_page = None
with urllib.request.urlopen(BASEURL + 'index.html') as response:
   html_page = response.read()
soup = bs4.BeautifulSoup(html_page, features="html5lib")
for link in soup.findAll('a', attrs={'href': re.compile(GDELT_FILE_REGEX)}):
    print(BASEURL + link.get('href'))
