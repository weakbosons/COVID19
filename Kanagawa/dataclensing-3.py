#!/usr/local/bin/python3
#感染者リストを正規化する


import csv,sys,re


##保健所管轄リスト
area =['横浜','川崎','横須賀','鎌倉','茅ヶ崎','厚木','小田原','相模原','藤沢','平塚']

#感染者リストを初期化
infected = []

##標準入力からデータを読み込む
cin = csv.DictReader(sys.stdin,fieldnames =['発表日','居住地','年代','性別'])
infected = [row for row in cin]

#＃見出し行を出力
print('"発表日", "居住地", "年代","性別"', end="\n")

#元リストの見出し行を削除
del infected[0]
#print(infected)
#quit()

#正規化
for line in infected:
    #発表日−＞そのまま
    print('"'+line['発表日']+'"',end=",")
    #居住地->正規化
    source = line['居住地']
    tg = ""
    for hc in area:
        m = re.search(hc ,source)
        if m:
            tg=m.group()
    print('"'+tg+'"',end=",")
    #年代−＞"代"をとる
    source = line['年代']
    tg = ""
    m = re.search(r'([1-9]0)',source)
    if m:
            tg=m.group()
    print('"'+tg+'"',end=",")
    #性別−＞そのまま
    print('"'+line['性別']+'"',end="\n")
