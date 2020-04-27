#!/bin/bash

rm *.csv && Rscript merge.r && Rscript calc_rev_dist.r && Rscript calc_accuracy.r && ./datatozip.sh

