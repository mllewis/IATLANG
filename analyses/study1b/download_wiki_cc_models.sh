#!/bin/bash
# chmod +x download_wiki_cc_models.sh
# sh download_wiki_cc_models.sh
langs='ar bn bg zh nl fr hr de da el en gu hi ig id it ja kn ko ml mr ne pa pl pt ro ru es tl ta te th tr ur vi yo fa zu'
langs='ms no sk sv'
in_path="https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc."
out_path="/Volumes/wilbur_the_great/wiki_cc_models/wiki_cc."

for i in $langs
do
	echo "=====$i======="
	full_in_path="$in_path$i.300.vec.gz"
	full_out_path="$out_path$i.vec"
	curl -0 -v $full_in_path --output "${full_out_path}" --max-time 20000
done
	echo All done
