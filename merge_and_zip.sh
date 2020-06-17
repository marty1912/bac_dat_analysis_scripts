#!/bin/bash

rm *.csv ; 
# delete files smaller than 2 byte.
find data/ -size -2c -type f -delete;

Rscript merge.r && 
    echo "merge finished" &&  
    Rscript calc_accuracy.r && 
    Rscript clean_data.r && 
    Rscript calc_rev_dist.r && 
    zip -r data_merged.zip data && 
    for i in *.csv ; do zip -u data_merged.zip $i ; done

