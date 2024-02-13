#!/bin/sh
curl -L -k -o FL_insurance_sample.csv.zip http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip
ls
unzip FL_insurance_sample.csv.zip
ls -l FL_insurance_sample.csv
rm -rf __MACOSX
rm -f FL_insurance_sample.csv.zip
ls -al --block-size=MB FL_insurance_sample.csv
head -5 FL_insurance_sample.csv

# Count lines before conversion
lines_before=$(wc -l < FL_insurance_sample.csv)
echo "Lines before conversion: $lines_before"


# Convert file format from DOS to UNIX
dos2unix FL_insurance_sample.csv

# Count lines after conversion
lines_after=$(wc -l < FL_insurance_sample.csv)
echo "Lines after conversion: $lines_after"
