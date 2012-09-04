R Scripts for Error Analysis 
=============================

A set of scripts that are used for analyzing output files of a machine learning system for NLP of review sites


What does it to?
--------------

It allows importing the output file in a given format for each of the test cases for analysis and visualization of the results:
* Percision and recall of each category/label
* Confusion Matrix between the categories/labels
* Diff analysis between different runs of the system 

How to make it work?
--------------------

Download R or RStudio from their open source sites

Use the sample files that have the following format
V1 - Score - how certain is the classification (in the sample file we have only 1.0 and null)
V2 - Match - is the classification correct (can be also calculated by comapring V3 and V4 below)
V3 - Correct label
V4 - Classified label as guessed by the system


