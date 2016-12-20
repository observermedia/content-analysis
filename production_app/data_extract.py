#!/usr/bin/env python2

# -*- coding: utf-8 -*-
"""
Input: raw .xml data
Output: cleaned json data (ready for input into topic model pipe)

@author: iclark
"""
import os
from xml.dom import minidom
import json
import pandas as pd
import sys

def main():
    '''
    Main Script
    '''
    path = str(sys.argv[1])
    os.chdir(path)
    #init empty dict
    data = {'link':[], 'title':[], 'author':[]  ,'pubdate':[],'content':[], 'label':[], 'sublabel':[]}
    
    #loop through all files
    for xmldoc in os.listdir(path):
        #try/except block to handel errors in XML encoding
        try:
            xmldoc = minidom.parse(xmldoc)
            nb_docs = len(xmldoc.getElementsByTagName('item'))
        except:
            continue
    #for a single xml file, extract all content and fill in dict
        for i in range(0, nb_docs/10):
            try:
                link = xmldoc.getElementsByTagName('item')[i].getElementsByTagName('link')[0].firstChild.nodeValue
                title = xmldoc.getElementsByTagName('wp:post_name')[i].firstChild.nodeValue
                author = xmldoc.getElementsByTagName('item')[i].getElementsByTagName('category')[2].firstChild.nodeValue
                pubdate = xmldoc.getElementsByTagName('pubDate')[i].firstChild.nodeValue
                content = xmldoc.getElementsByTagName('content:encoded')[i].firstChild.nodeValue
                label = xmldoc.getElementsByTagName('item')[i].getElementsByTagName('category')[0].firstChild.nodeValue
                sublabel = xmldoc.getElementsByTagName('item')[i].getElementsByTagName('category')[1].firstChild.nodeValue
            except: 
                continue 
    
            data['link'].append(link)
            data['title'].append(title)
            data['author'].append(author) #WRONG. Too much variability in author xml format.. cant capture author as is.. gets droped in R script
            data['pubdate'].append(pubdate)
            data['content'].append(content)
            data['label'].append(label)
            data['sublabel'].append(sublabel)
            print(xmldoc + " processed")


    os.chdir('..')
    dataframe = pd.DataFrame.from_dict(data)
    dataframe.reset_index().to_json('clean_data_links.json',orient = 'records')
    print("Data extraction complete... Now topic modeling.")

if __name__ == '__main__':
    main()
