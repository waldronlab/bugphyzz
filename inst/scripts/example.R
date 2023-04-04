library(bugphyzz)
bp <- importBugphyzz(version = 'devel', force_download = FALSE)
num <- importBugphyzzNumeric(version = 'devel', force_download = FALSE)

sigs <- getBugphyzzSignatures(
  bp, tax.id.type = 'Taxon_name', tax.level = 'order', frequency = 'always'
)

gt <- bp[which(bp$Attribute_group == 'growth temperature'), ]
gt_sigs <- getBugphyzzSignatures(gt, tax.id.type = 'NCBI_ID', tax.level = 'species')
lapply(gt_sigs, function(x) head(x))

bp_num <- importBugphyzzNumeric()

names(bp_num)

gt <- bp_num[['growth temperature']]

## Get taxa that grows optimally between 0 and 25 Celsius degrees
sub_gt <- gt[which(gt$Attribute_value_min >= 0 & gt$Attribute_value_max <= 25),]

## Create a signature at the genus level
sigs <- getBugphyzzSignatures(sub_gt, tax.id.type = 'Taxon_name', tax.level = 'genus')
head(sigs[[1]])


