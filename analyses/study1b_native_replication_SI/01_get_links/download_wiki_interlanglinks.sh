#!/bin/bash

langs='ar da de en es fa fi fr he hi hr id it ja ko ms nl no pl pt ro sv tl tr zh sr'
in_path="https://dumps.wikimedia.org/"
out_path="/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_studies/16_wiki_native/data/01_raw_links/"
for i in $langs
do
	echo "=====$i======="
	full_in_path="$in_path${i}wiki/latest/${i}wiki-latest-langlinks.sql.gz"
	full_out_path="$out_path${i}wiki-latest-langlinks.sql.gz"
	echo "$full_in_path"
	echo "$full_out_path"

	curl -0 -v $full_in_path --output "${full_out_path}" --max-time 20000
done
	echo All done
