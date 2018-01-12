#!/bin/bash

#countries='ar bn bg zh nl fr de el en gu hi ig id it ja kn ko ml mr ne pa pl pt ro ru es tl ta te th tr ur vi yo fa'
countries='ar'

path_name1="https://s3-us-west-1.amazonaws.com/fasttext-vectors/wiki."

for i in $countries
do
	echo "=====$i======="
	full_path="$path_name1$i.vec"
	output_path="temp/$i-tempmodel.txt"
	curl -0 -v $full_path --output $output_path --max-time 20000
	#Rscript get_IAT_ES.R
done
	echo All done
