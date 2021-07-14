#!/bin/bash

echo "+++Get,Clense and write patient2.csv"
./curl.sh|./iconv.sh|./dataclensing-3.py >patient2.csv


