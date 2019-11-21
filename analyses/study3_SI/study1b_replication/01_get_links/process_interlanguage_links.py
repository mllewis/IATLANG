# coding: utf-8

# Author:
# -----------
# Copyright (C) 2017 Bill Thompson (biltho@mpi.nl) 
# 
#
# Description:
# -----------
# Parses a Wikimedia sql dump of the interlanguage links table (e.g. avwiki-latest-langlinks.sql.gz) for a given wiki.
# Dumps hosted at e.g. "https://dumps.wikimedia.org/{0}wiki/latest/{0}wiki-latest-langlinks.sql.gz".format(language_iso)
#
#
# Usage: 
# -----------
# python main.py -f avwiki-latest-langlinks.sql.gz
# 
#
# Returns:
# -----------
# Writes out a csv with (page_id, target_language, paget_title) columns
#

import argparse
import gzip
import pandas as pd

from time import strftime
TIMESTAMP = strftime("%Y-%m-%d__%H-%M-%S")

import logging
logging.basicConfig(format='%(levelname)s: %(message)s', level=logging.INFO)


def parse(filename):
    logging.info("Pre-processing > Attempting parse on: {0}".format(filename))

    pages, target_languages, titles = [], [], []

    logging.info("Pre-processing > Unzipping: {0}".format(filename))
    with gzip.open(filename, 'r') as f:    
        logging.info("Pre-processing > Unzip Success")
                     
        logging.info("Parser > Looping over file")
        for line in f:
            
            if line.startswith('INSERT INTO `langlinks` VALUES'):
                line = line.decode("utf-8", "ignore") ## added by MLL 12/14/19
                entries = line[31:].split('),(')
                
                for entry in entries:
                    fields = entry.strip('(').strip(')').split(',')
                    try:
                        page, target_language, title = fields
                    except:
                        page, target_language, title = fields[0], fields[1], fields[2:] 
                    
                    pages.append(page)
                    target_languages.append(target_language)
                    titles.append(title)


    logging.info("Parser > Parse Complete")

    new_filename = '-'.join([filename.strip('.sql.gz'), 'parsed.csv'])

    logging.info("Post-processing > Writing dataframe out to: {0}".format(new_filename))

    results = pd.DataFrame(dict(page = pages, target_language = target_languages, title = titles))

    results.to_csv(new_filename, index = False, encoding = 'utf-8')

    logging.info("JOB COMPLETE.")

