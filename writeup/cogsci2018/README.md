## Cogsci 2018 paper directory

Mansucript: `gender_cogsci_2018.Rmd`
Analyses and data for three studies in paper: `analysis/`

* Study 1/
	* data/
		* The raw project implicit data is from this file Gender-Career IAT.public.2005-2016.sav (https://osf.io/b5q8g/), converted into a feather file. It is not included here because it is too large but can be freely downloaded form the osf link.
		* `project_implicit_country_codes`: mapping from country codes to country names, taken from Gender-Career_IAT_public_2005-2016_codebook.xlsx (https://osf.io/b5q8g/)
		* `EPI.csv`: [English Proficiency Index (EPI)]  (https://www.ef.edu/epi/)
		* `WPS index`: [Women, Peace and Security Index (WPS)]( https://giwps.georgetown.edu/)

* Study 2a/
	* `caliskan_2017_replication.R`: Code for reproducing effect sizes from Caliskan et al. (2017) paper with the Wikipedia corpus
	* `IAT_utils.R`: Helper functions for calculating IAT semantic effect sizes.
	* data/
		* `caliskan_es.csv`: Effect sizes from Caliskan et al. (2017) paper
		* `wiki.en.vec`: Wikipedia-trained English model can be found at (https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md)
		* `caliskan_wiki_es.csv`: Outputed ffect sizes calculated from Wikipedia trained English corpus (by `caliskan_2017_replication.R` script).

* Study 2b/
	* `1_tidy_hand_translations.R`: Code for getting tidying hand translations (inputs `Clean - IATLANG STUDY 1 TRANSLATIONS.csv`; outputs `data/tidy_hand_translations.csv`).
	* `2_get_IAT_scores_from_wiki_models.R`: Script to calculate ES from non-English models (inputs `data/tidy_hand_translations.csv` and [Wikipedia-trained embedding models](https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md); outputs three sets of files in `data/wiki_language_embeddings_career/raw/`,  `data/wiki_language_embeddings_career/calculated/` and `data/career_effect_sizes_hand_translations.csv`).
	* `3_get_language_percentage.R`: Get proportion of language spoken in each country. (input from https://github.com/opendatajson/factbook.json, not included here; outputs `languages_with_percent.csv` and `percent_country_language_clean.csv`).
	* data/
		* `Clean - IATLANG STUDY 1 TRANSLATIONS.csv`: raw hand translations
		* `tidy_hand_translations.csv`: tidy hand translations
		* `career_effect_sizes_hand_translations.csv`: processed cross-linguistic gender effect sizes
		* `language_names_to_wiki_codes.csv`: Key of wikipedia embedding model language codes to names (coded by hand)
		* `percent_country_language_clean.csv`: All language by country percentages from CIA factbook
		* `languages_with_percent.csv`: Tidy with only languages with at least 20% speakers.
		* `wiki_language_embeddings_career/raw/`: This is an unprocessed subset of the Wikipedia trained fast text models, with only the target words included (there are more words than target words because some translations have more than one word).
		* `wiki_language_embeddings_career/calculated/`: This is a processed subset of the Wikipedia trained fast text models, taking the centroid of tranlsations with more than one entry (N rows = N target words).

* Study 3/
	* data/
		* `gender_grammar`: Coding of languages from grammatical gender type. Data taken from WALS where available, and also [Wikipedia gender grammar page](https://en.wikipedia.org/wiki/List_of_languages_by_type_of_grammatical_genders).
		