library(bugphyzz)
##
bp <- importBugphyzz(keyword = 'all', version = 'devel', cache = FALSE)
sigs <- getBugphyzzSignatures(
  bp, tax.id.type = 'Taxon_name', tax.level = 'order', frequency = 'always'
)
lapply(sigs, head)

