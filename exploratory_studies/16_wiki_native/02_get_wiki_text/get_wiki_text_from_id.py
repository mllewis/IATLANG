from wiki_dump_reader import Cleaner, iterate
import pandas as pd
import csv
import sys

def get_native_wiki_text(langlinks, wiki_xml_file, lang):
    output_filename = lang+'_nativelang_article_text.csv'
    article_ids_to_exclude = pd.read_csv(langlinks,header=None,usecols=[0])
    article_ids_to_exclude.columns = ['article_id'] #renames column 
    article_ids_to_exclude_dict = dict((y,x) for x,y in enumerate(article_ids_to_exclude['article_id']))
    cleaner = Cleaner()
    with open(output_filename, 'w', newline='') as output_file:
        wiki_writer = csv.writer(output_file, delimiter=',',
                                quotechar='"', quoting=csv.QUOTE_MINIMAL)
        for article_id, title, text in iterate(wiki_xml_file):
            if article_ids_to_exclude_dict.get(int(article_id)) == None:
                text = cleaner.clean_text(text)
                cleaned_text, _ = cleaner.build_links(text)
                wiki_writer.writerow([article_id,title,cleaned_text])