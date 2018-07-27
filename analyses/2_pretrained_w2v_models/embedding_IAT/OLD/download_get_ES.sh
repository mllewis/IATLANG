#!/bin/bash

#langs='ar bn bg zh nl fr de el en gu hi ig id it ja kn ko ml mr ne pa pl pt ro ru es tl ta te th tr ur vi yo fa'
langs='ar'

path_name1="https://s3-us-west-1.amazonaws.com/fasttext-vectors/wiki."

for i in $langs
do
	echo "=====$i======="
	full_path="$path_name1$i.vec"
	echo $full_path
	output_path="/Volumes/My\ Passport\ for\ Mac/molly/wiki_models/wiki.$i.vec"
	curl -0 -v $full_path --output $output_path --max-time 20000
done
	echo All done
