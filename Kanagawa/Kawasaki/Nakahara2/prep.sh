#!/bin/bash

echo "+++Get,Clense and write patient2.csv"
cat ../patient2.csv|egrep "中原" > patient2.csv
