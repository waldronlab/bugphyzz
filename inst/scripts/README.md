# Description about the files in the extdata directory

All of the files in the extdata directory, with the exeception of
the `validation_summary.tsv` file, were manually created/edited.

These files are necessary for 1) the bugphyzz package to work properly and
2) a convenient way for curators to update the files.

What follows is the description of each file.

## attribute_sources.tsv

Contains the original sources of bugphyzz annotations.

Columns:

| Column | Description |
| --- | --- |
| Attribute_source | The abbreviated name of the source. This is the one used in the output of `importBugphyzz`.|
| Confidence_in_curation | One of three: high, medium, low.|
| Evidence \* | The type of evidence supporting the annotations in the original source. |
| full_source | The full name of the source. It could be a citation, a link to webpage or project, etc. |

**\* Evidence**
Options:
- exp. Inferred from experiment.
- igc. Inferred from genomic context.
- nas. Non-traceable author statement.
- tas. Traceable author statement. 
- tax. IBD = inferred from biological aspect of descendant.
- asr. anscestral state reconstruction.

## attributes.tsv

It contains the description of the attribute values included in bugphyzz.

Columns:

| Column name | Description |
| --- | --- |
| attribute | The name of the attribute. |
| validity | The type of the attribute, either 'logical' for categorical values or 'numeric' for ranges. |
| ontology | Mapping to an ontology term (if possible). |
| attribute_group | The group(s) where an attribute name could be found. |
| description | The meaning of the attribute described in the 'attribute' column. |
| ordinal_position | Position of the state on an ordered scale (1 = lowest), used for linearly weighted kappa in the cross-validation; NA for states without an order. Set for aerophilicity (aerobic, facultatively anaerobic, anaerobic), gram stain (negative, variable, positive), biosafety level and COGEM pathogenicity rating (bugphyzzExports ADR 0006). |

## spreadsheet_custmolinks.tsv

Links for datasets in google spreadsheets that are not in tidy format and
need to be converted to tidy format.

Columns:

| Column name | Description |
| --- | --- |
| physiology | Name of the physiology or attribute group. |
| ontology | If applicably, mapping to an ontology term. |
| link | Link to the csv export. |
| functionname | The name of the function (unexported) in bugphyzz. |
| source_link |  Link to the source spreadsheet on Google Docs. |

## spreadsheet_links.tsv

Links for datasets in spreadsheets that are already in tidy format.

Columns:

| Column name | Description |
| --- | --- |
| physiology | Name of the dataset. Equivalent to attribute group. |
| ontolgy | Mapping to an ontology term if applicable. |
| sig_type | The type of the signature. Either range for numeric or logical for categorical and binary values. |
| link | Link to the csv export. |
| source_link | Link to the source spreadsheet. |

## curation_template.tsv

Contains the formal description of the data model in the spreadsheets imported
by the physiologies funtion.

Columns:

| Column name | Description |
| --- | --- |
| column_name | Name of the column in the spreadsheet. |
| requiredness | Whether the column needs to be present in all attributes (required) or not (optional). |
| required_column_order | Order of the column if required. |
| attribute_types | Types of the attributes. Not R classes. Import function behavior will change based on this values. |
| valid_values | A regular expression with the values that are accepted. Except for the column 'Attriubte' which is a function name ('.attributes'). |
| value_test | If 'string' the test of validity is based on the regular expresion. If '.attributes', it's based on the function .attriubtes (no exported). |
| column_class | One of the R classes for atomic vectors. |

## thresholds.tsv

Tresholds for converting numeric/range data to categorical.

Columns:

| Column name | Description |
| ----------- | ----------- |
| Attribute_group | Physiology. Name of the spreadsheet. |
| Attribute | The actual attribute used. |
| lower | lower threshold. |
| uppwer | upper threshold. |
| unit | units used for numeric data. |


## validation_attributes.tsv and validation_states.tsv

From waldronlab/taxPProValidation (`create_attribute_summary.R`, #12,
waldronlab/taxPProValidation#13). `importBugphyzz()` keeps or drops all
imputed annotations of an attribute by `score_lcb` in
`validation_attributes.tsv` (bugphyzzExports ADR 0006): one row per rank and
physiology, with Cohen's kappa (`metric` "kappa", or "weighted kappa" for
attributes with an `ordinal_position` in attributes.tsv) or R2 ("r2"),
held-out tips weighted to the NSTI distribution of the tips to be imputed;
`score_mean`, `score_sd` and `score_lcb` (lower bound of a two-sided 95%
t-interval over folds), the unweighted score, the number of held-out tips,
the effective sample size of the weights as a fraction, the largest weight
and the median NSTI of held-out and target tips. `validation_states.tsv`
gives the precision of each predicted state of discrete attributes, with the
same weights, and the number of predictions.

Copied from the attribute-kappa branch of waldronlab/taxPProValidation on
November 9, 2026 (discrete scores from the v1.0.7 folds, numeric re-run on
November 9).

## validation_summary.tsv

This file was obtained from the waldronlab/taxPProValidation repository
hosted on GitHub. It contains the results of a 10-fold cross-validation of the
ancestral state reconstruction (ASR) methods used to get annotations for the
package. Folds are assigned per species, so strains are never predicted from
their own species (waldronlab/taxPProValidation#1).

Detailed information (code and explanation) can be found on the repository.

This is version 1.0.6, from waldronlab/taxPProValidation#7 (commit 16aed99),
downloaded on November 8, 2026 with:

```bash
wget https://raw.githubusercontent.com/waldronlab/taxPProValidation/16aed99/validation_summary.tsv
```

Version 1.0.5 came from waldronlab/taxPProValidation#5 (commit 8996e05,
downloaded November 5, 2026).
Version 1.0.4 came from commit cc45761 (downloaded May 31, 2024).

| Column name | Description |
| ----------- | ----------- |
| method | ASR method (phytools-ltp or castor-ltp) |
| rank | Taxonomic rank (all, genus, species, strain). "all" was used for the output of `importBugphyzz`.|
| physiology | The name of the attribute group. |
| attribute | The name of the attribute or attribute value.|
| mcc_mean | The mean of Mathew's correlation coefficient for discrete only. |
| mcc_sd | The standard deviation of Matthew's correlation coefficient for discrete only.|
| r2_mean | The mean of the R-squared for numeric only.|
| r2_sd | The standard deviation of the R-squared for numeric only.|
| ltp_bp | Interesction between taxa in bugphyzz (per attribute/attribute value) and the ltp tree.|
| bp | Taxa in bugphyzz (per attribute/attribute value). |
| ltp_bp_phys | Intersection between taxa in bugphyz (per attribute group) and the ltp tree.|
| bp_phys | Total taxa in the bugphyzz (per attribute group).|
| ltp | Number of taxa in the LTP tree. |
| nsti_mean | The mean of the NSTI values. This was calculated for all physiologies, but it's only relevant for numeric ones.|
| nsti_sd | The standard deviation of the NSTI values. This was calculated for all physiologies, but it's only relevant for numeric ones.|
| ltp_bp_per | Intersection of LTP tree and bugphyzz (per attribute/attribute value) in numbers.|
| ltp_bp_phys_per | Intersection of LTP tree and bugphyzz (per physiology/attribute group) in percentage.|

