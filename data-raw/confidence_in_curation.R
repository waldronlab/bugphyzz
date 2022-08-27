## Prepare confidece in curation to add it to extdata
library(readr)

url <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vTRiwYhaVaBhGkHQrjRMHJglc9cqxjA90GLsIxCQa91VwZQ65qRQn2wZgwpw3uAOBSMlwFgOPae8PYq/pub?gid=0&single=true&output=tsv'
cc <- read_tsv(url, show_col_types = FALSE)
write_tsv(cc, 'inst/extdata/confidence_in_curation.tsv')

