# -*- coding: utf-8 -*-
#!/usr/bin/python
# linecount.py
# 2009-1-20
# author:
#   Jason Lee
#
import sys
import os

def statics(exts):
  file_count = 0
  line_count = 0
  char_count = 0
  for root,dirs,files in os.walk(os.getcwd()):
    for filename in files:
      filename = (root + '/'+ filename).lower()
      try:
        ext = filename[filename.rindex('.'):]
      except ValueError:
        continue
      try:
        if(exts.index(ext) >= 0):
          file_count += 1
          line_count += len(tuple(open(filename).xreadlines()))
          char_count += len(tuple(open(filename).read()))
      except:
        pass
  return file_count, line_count, char_count

print 'python files: %d, lines: %d, characters: %d'% statics(['.py'])
print 'text files: %d, lines: %d, characters: %d'% statics(['.txt'])
print 'rest files: %d, lines: %d, characters: %d'% statics(['.rst'])
  