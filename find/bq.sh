#!/bin/bash

outf=seed.bq

# Extract URLs of Erlang projects to serve as seed.

#TODO this BigQuery
#  SELECT repo_name FROM [bigquery-public-data:github_repos.languages] WHERE language.name = 'Erlang'

exit 0
cat results-*.csv | tail -n +2 | sed 's%^%github.com/%' | tr '[:upper:]' '[:lower:]' >$outf
