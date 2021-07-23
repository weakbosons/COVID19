#!/bin/bash


THIS=/Users/momma/COVID19/Kanagawa
echo "+++Get,Clense and write patient2.csv"
$THIS/curl.sh| $THIS/iconv.sh| $THIS/dataclensing-3.py > $THIS/patient2.csv


