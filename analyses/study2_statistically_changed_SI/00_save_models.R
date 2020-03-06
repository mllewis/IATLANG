library(data.table)

f = fread('http://sapir.psych.wisc.edu/~lupyan/es_native_wiki_corpus_dim200_minCount1_ep15_feminized2.vec', skip = 1)
fwrite(f, "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_feminized2.vec" )


m = fread('http://sapir.psych.wisc.edu/~lupyan/es_native_wiki_corpus_dim200_minCount1_ep15_masculinized2.vec', skip = 1)
fwrite(m, "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_masculinized2.vec" )


orig = fread("http://sapir.psych.wisc.edu/~lupyan/es_native_wiki_corpus_dim200_minCount1_ep15_orig2.vec", skip = 1)
fwrite(orig, "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_orig2.vec" )


ie = fread('http://sapir.psych.wisc.edu/~lupyan/es_native_wiki_corpus_dim200_minCount1_ep15_neutered_ie.vec', skip = 1)
fwrite(ie, "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_neuteredie.vec" )


x = fread("http://sapir.psych.wisc.edu/~lupyan/es_native_wiki_corpus_dim200_minCount1_ep15_neutered_x.vec", skip = 1)
fwrite(x, "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_neuteredx.vec" )
