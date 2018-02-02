## Cogsci 2018 paper directory

The primary analyses are in the manuscript file, `gender_cogsci_2018.Rmd`. Pre-processing and data for the three studies in the paper can be found in the `analysis/` directory. The contents of this directory are described in more detail below. Note that in order to reproduce the entire pipeline files from three sources must be downloaded that are not contained in this repository because they are too large ([raw Project Implicit IAT data](https://osf.io/b5q8g/); [country-by-langauge data](https://github.com/opendatajson/factbook.json); [pre-trained Wikipedia models](https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md)). All of these data sources are freely available.

* Study 1/
	* data/
		* The raw project implicit data is from [Gender-Career IAT.public.2005-2016.sav](https://osf.io/b5q8g/), not included here. It has been converted into a feather file. 
		* `project_implicit_country_codes`: mapping from country codes to country names, taken from [Gender-Career_IAT_public_2005-2016_codebook.xlsx](https://osf.io/b5q8g/)
		* `EPI.csv`: [English Proficiency Index (EPI)](https://www.ef.edu/epi/)
		* `WPS index`: [Women, Peace and Security Index (WPS)]( https://giwps.georgetown.edu/)

* Study 2a/
	* `caliskan_2017_replication.R`: Code for reproducing effect sizes from Caliskan et al. (2017) paper with the Wikipedia corpus
	* `IAT_utils.R`: Helper functions for calculating IAT semantic effect sizes.
	* data/
		* `caliskan_es.csv`: Effect sizes from Caliskan et al. (2017) paper
		* `wiki.en.vec`: Wikipedia-trained English model, not included here (https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md)
		* `caliskan_wiki_es.csv`: Outputed effect sizes calculated from Wikipedia trained English corpus (by `caliskan_2017_replication.R` script).

* Study 2b/
	* `1_tidy_hand_translations.R`: Code for tidying hand translations (inputs `Clean - IATLANG STUDY 1 TRANSLATIONS.csv`; outputs `data/tidy_hand_translations.csv`).
	* `2_get_IAT_scores_from_wiki_models.R`: Script to calculate ES from non-English models (inputs `data/tidy_hand_translations.csv` and [Wikipedia-trained embedding models](https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md); outputs three sets of files in `data/wiki_language_embeddings_career/raw/`,  `data/wiki_language_embeddings_career/calculated/` and `data/career_effect_sizes_hand_translations.csv`).
	* `3_get_language_percentage.R`: Get proportion of language spoken in each country. (inputs from https://github.com/opendatajson/factbook.json, not included here; outputs `languages_with_percent.csv` and `percent_country_language_clean.csv`).
	* data/
		* `Clean - IATLANG STUDY 1 TRANSLATIONS.csv`: raw hand translations
		* `tidy_hand_translations.csv`: tidy hand translations
		* `career_effect_sizes_hand_translations.csv`: processed cross-linguistic gender effect sizes
		* `language_names_to_wiki_codes.csv`: Key of Wikipedia embedding model language codes to names (coded by hand)
		* `percent_country_language_clean.csv`: All language by country percentages from CIA factbook
		* `languages_with_percent.csv`: Tidy with only languages with at least 20% speakers.
		* `wiki_language_embeddings_career/raw/`: This is an unprocessed subset of the Wikipedia trained fast text models, with only the target words included (there are more words than target words because some translations have more than one word). Each language has its own file.
		* `wiki_language_embeddings_career/calculated/`: This is a processed subset of the Wikipedia trained fast text models, taking the centroid of tranlsations with more than one entry (N rows = N target words). Each language has its own file.

* Study 3/
	* data/
		* `gender_grammar`: Coding of languages from grammatical gender type. Data taken from WALS where available, and also the [Wikipedia gender grammar page](https://en.wikipedia.org/wiki/List_of_languages_by_type_of_grammatical_genders).
		