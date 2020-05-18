#!/bin/bash

zip -r data_merged.zip data && for i in *.csv ; do zip -u data_merged.zip $i ; done
