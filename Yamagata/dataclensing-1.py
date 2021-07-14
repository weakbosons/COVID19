#!/usr/local/bin/python3


#居住地リストを作成

import csv,sys
import re

##csvファイルのパスを引数からとる
value = sys.argv
FILE = value[1]

fin = open( FILE, 'rt')
for line in fin:
    ll = line.strip()
    m = re.search(r'(?P<date>\S+)\s+(?P<gen>\S+)\s+(?P<sex>\S+)\s+(?P<hab>\S+).',ll)
    print(
    '"'+m.group('date')+'",'
    +'"'+m.group('hab')+'",'
    +'"'+m.group('gen')+'",'
    +'"'+m.group('sex')+'"'
    )
