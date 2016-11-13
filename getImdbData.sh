#!/usr/bin/env bash

mkdir data
cd data
wget ftp://ftp.fu-berlin.de/pub/misc/movies/database/plot.list.gz
gunzip plot.list.gz
wget ftp://ftp.fu-berlin.de/pub/misc/movies/database/ratings.list.gz
gunzip ratings.list.gz
