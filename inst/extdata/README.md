# Files description

The files provided here were generated manually.

## [attribute_sources.tsv](./attribute_sources.tsv)

Contains the original sources of bugphyzz annotations.

Columns:

| Column | Description |
| --- | --- |
| Attribute_source | The abbreviated name of the source. |
| Confidence_in_curation\*| How trustable is a source. |
| Evidence \*\* | The type of evidence supporting the annotations in the original source. Options: exp, igc, nas, tas. |
| full_source | The full name of the source. It could be a citation, a link to webpage or project, etc. |

**\* Confidence_in_curation**
- High.
- Medium.
- Low.

**\*\* Evidence**
- exp. Inferred from experiment.
- igc. Inferred from genomic context.
- nas. Non-traceable author statement.
- tas. Traceable author statement. 

## [attributes.tsv](./attributes.tsv)

Contains the description of the attribute values included in bugphyzz.

Columns:

| Column name | Description |
| --- | --- |
| attribute | The name of the attribute. |
| validity | The type of the attribute, either 'logical' for categorical values or 'numeric' for ranges. |
| ontology | Mapping to an ontology term (if possible). |
| attribute_group | The group(s) where an attribute name could be found. |
| description | The meaning of the attribute described in the 'attribute' column. |

## [custmolinks.tsv](./customlinks.tsv)

Links for datasets in spreadsheets that are not in tidy format and need to be
converted to tidy format. These datasets are not imported through the 
physiologies function, so they need their own fucntion.

Columns:

| Column name | Description |
| --- | --- |
| physiology | Name of the physiology or attribute group. |
| ontology | If applicably, mapping to an ontology term. |
| link | Link to the csv export. |
| functionname | The name of the function (unexported) in bugphyzz. |
| source_link |  Link to the source spreadsheet on Google Docs. |


## [links](./links.tsv)

Links for datasets in spreadsheets that are already in tidy format.
These datsets are imported with the physiologies function (no exported).

| Column name | Description |
| --- | --- |
| physiology | Name of the dataset. Equivalent to attribute group. |
| ontolgy | Mapping to an ontology term if applicable. |
| sig_type | The type of the signature. Either range for numeric or logical for categorical and binary values. |
| link | Link to the csv export. |
| source_link | Link to the source spreadsheet. |

