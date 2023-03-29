library(bugphyzz)
library(purrr)
bp <- importBugphyzz()
sigs <- getBugphyzzSignatures(
  bp, tax.id.type = 'Taxon_name', tax.level = 'order', frequency = 'always'
)
lapply(sigs, head)

