#!/bin/bash

#langs='ar bn bg zh nl fr de el en gu hi ig id it ja kn ko ml mr ne pa pl pt ro ru es tl ta te th tr ur vi yo fa'
#langs='fa de nl pt zh es id da fi ja ko ml no po ru ur hi sv ro ms ta'
langs='he'
in_path="https://s3-us-west-1.amazonaws.com/fasttext-vectors/wiki."
out_path="/Volumes/wilbur/wiki_models/wiki."

for i in $langs
do
	echo "=====$i======="
	full_in_path="$in_path$i.vec"
	full_out_path="$out_path$i.vec"
	curl -0 -v $full_in_path --output "${full_out_path}" --max-time 20000
done
	echo All done
