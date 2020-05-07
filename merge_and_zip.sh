#!/bin/bash

rm *.csv ; find data/ -empty -type f -delete ; Rscript merge.r && Rscript calc_accuracy.r && Rscript clean_data.r && Rscript calc_rev_dist.r && ./datatozip.sh

