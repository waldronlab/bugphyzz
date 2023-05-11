# Files description

## [attribute_sources.tsv](./attribute_sources.tsv)

Contains the original sources of bugphyzz annotations.

Columns:

| Column | Description |
| --- | --- |
| Attribute_source | The abbreviated name of the source. |
| Confidence_in_curation\*| How trustable is a source. |
| Evidence \*\* | The type of evidence supporting the annotations. Options: igc, nas, tas, asr, inh, exp. |
| full_source | The full name of the source. It could be a citation, a link to webpage or project, etc. |

**\* Confidence_in_curation**
- High.
- Medium.
- Low.

**\*\* Evidence**
- igc. Inferred from genomic context.
- exp. Experimental (wet lab).
- nas.
- tas.

## [attributes.tsv](./attributes.tsv)

Contains the description of the attribute values included in bugphyzz.

Columns:

| Column name | Description |
| --- | --- |
| attribute | The name of the attribute. |
| validity | The type of the attribute, either 'logical' for categorical values or 'numeric' for ranges. |
| ontology | Mapping to an ontology term (if possible). |
| description | The meaning of the attribute described in the 'attribute' column. |
