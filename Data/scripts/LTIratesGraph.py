# -*- coding: utf-8 -*-
"""
Created on Mon Jun 22 12:46:16 2015

@author: Matt
"""
import csv
import os
os.chdir("../csv/")


with open('long_term_interest_rates_ECB.csv', 'rb') as csvfile:
    datareader = csv.reader(csvfile, delimiter=',')
    
