#!/usr/local/bin/python3


#居住地リストを作成

import csv,sys


##csvファイルのパスを引数からとる
value = sys.argv
FILE = value[1]

##csvファイルを開く
with open( FILE, 'rt') as fin:
    cin = csv.DictReader(fin,fieldnames =['発表日','居住地','年代','性別'])
    infected = [row for row in cin]

##居住地の一覧リストを作成
list = []
for line in infected:
    if line['居住地'] in list:
        pass
    else:
        list.append(line['居住地'])

list.sort()


#居住地リストを表示
for aa in list:
    print(aa, end="\n")
