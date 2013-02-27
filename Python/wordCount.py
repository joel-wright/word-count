#!/usr/bin/env python

import fileinput
import re
import string

wordMap = {}

# Create a regular expression to match any non-word character or digit
pattern = re.compile('[\W\d]+')

# Iterate through the file a line at a time
for line in fileinput.input():
   words = pattern.split(line.lower())
   for word in words:
      if word:
         if word in wordMap:
            wordMap[word] += 1
         else:
            wordMap[word] = 1

# Print the first 10 results
for w in sorted(wordMap, key=wordMap.get, reverse=True)[:10]:
   print("%s: %s" % (w, wordMap[w]))

