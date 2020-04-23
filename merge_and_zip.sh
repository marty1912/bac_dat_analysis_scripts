#!/bin/bash

rm *.csv && Rscript merge.r && Rscript calc_rev_dist.r && ./datatozip.sh

