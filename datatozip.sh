#!/bin/bash

zip -r data.zip data && for i in *.csv ; do zip -u data.zip $i ; done
