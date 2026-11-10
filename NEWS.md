# bugphyzz 1.7.16
+ `attributes.tsv`: three antimicrobial resistance values the export writes
  as the FALSE side of a paired attribute (sensitive to beta-lactam,
  erythromycin A, kanamycin A) are in the vocabulary, and the
  butyrate-producing attribute's group also carries the name the export gives it,
  `butyrate-producing bacteria` (part of #281). Both made the shipped
  data fail `checkCuration()` in the tests (bugphyzzExports#99).

# bugphyzz 1.7.15
+ Imputed annotations are kept or dropped per attribute, as in
  bugphyzzExports ADR 0006: all states of an attribute together, by the lower
  95% bound of its cross-validated Cohen's kappa (linearly weighted for
  ordered states) or R2, with held-out taxa weighted to the phylogenetic
  distance of the taxa to be imputed. The scores are in the new
  `inst/extdata/validation_attributes.tsv` (waldronlab/taxPProValidation#12);
  per-state precision is in `validation_states.tsv`. With the shipped table,
  aerophilicity keeps its aerobic and facultatively anaerobic imputations,
  gram stain its "variable" ones, and COGEM pathogenicity rating loses its
  imputations (#292).
+ `importBugphyzz()` arguments: `kappaThreshold` (default 0.7) replaces
  `mccThreshold`, and `r2Threshold` (0.5) now applies per attribute;
  `asrScoreThreshold` (0.5) filters imputed annotations by `Score`. Setting
  any of them below its default reads the export's dropped imputed
  annotations as well, from GitHub versions or from Zenodo deposits that
  contain them, so `includeDroppedAsr` is removed. `mccThreshold` and
  `includeDroppedAsr` were never in a Bioconductor release.
+ `excludeRarely` is deprecated: `excludeRarely = FALSE` is
  `asrScoreThreshold = 0`. Curated annotations with Frequency "rarely" or
  "never" are always excluded; `physiologies()` returns all curated data.
  `v` now sets `kappaThreshold` and `r2Threshold`.
+ `attributes.tsv` gains `ordinal_position`, the order of the states of
  aerophilicity, gram stain, biosafety level and COGEM pathogenicity rating.
+ Downloads are cached per version: switching `version` without
  `forceDownload = TRUE` used to return the tables of the version downloaded
  first.

# bugphyzz 1.7.14
+ ASR annotations are filtered against validation v1.0.7
  (waldronlab/taxPProValidation#11): the cross-validation now calls a
  predicted state positive at the score the export ships (0.5 or more)
  instead of above 0.8, so the MCC describes the data users receive. Same
  predictions as v1.0.6. The export and `importBugphyzz()` filter on the
  rank "all" rows, where no class changes side of MCC 0.7, so the released
  annotations do not change; anaerobic rises to 0.90 and aerobic to 0.68.
  Shape "filamentous" passes at genus rank, which the filters do not use.

# bugphyzz 1.7.13
+ ASR annotations are filtered against validation v1.0.6
  (waldronlab/taxPProValidation#7): the cross-validation now drops the rows
  the bugphyzzExports export drops (the 2020 Bergey's scrape and the sheet
  overrides) and was re-run on the sources of bugphyzzExports v1.1.0. At rank
  "all", COGEM pathogenicity rating 3 passes MCC 0.7; coding genes, genome
  size and G+C content improve. host-associated (only TRUE values since The
  Microbe Directory import) and biofilm formation (too little of the tree)
  are not validated, so the export drops their imputed annotations.

# bugphyzz 1.7.12
+ `importBugphyzz()` gains `includeDroppedAsr`: with a `devel` or commit
  version it adds back the imputed annotations the export removed at the
  default thresholds, so `mccThreshold` and `r2Threshold` can be set lower
  to evaluate a threshold against known ground truth.

# bugphyzz 1.7.11
+ The Microbe Directory now comes from bugphyzzExports `sources/`
  (waldronlab/bugphyzzExports#87), imported from its published table with
  its own errors corrected, instead of rows copied into the curation sheets.
  Seven sheets held only those rows and are no longer read: animal pathogen,
  antimicrobial sensitivity, biofilm formation, COGEM pathogenicity rating,
  extreme environment, optimal ph and plant pathogenicity.

# bugphyzz 1.7.10
+ ASR annotations are filtered against validation v1.0.5
  (waldronlab/taxPProValidation#5), re-run with folds assigned per species
  on the 2026-11 sources. `gc content` predictions are now kept (R2 0.92).
  Predictions of "aerobic", the "spirillum" shape and COGEM rating 3 are
  dropped: their MCC fell below 0.7 once strains were no longer predicted
  from their own species. Genome size, coding genes and growth temperature
  validate better than before. The shape value "tail" has too few species
  for 10 folds and no longer has a validation row, so its predictions are
  dropped at any threshold.

# bugphyzz 1.7.9
+ `attributes.tsv` gains `resistance to beta-lactam` and `resistance to
  carbapenems`, the names bugphyzzExports gives these antimicrobial
  resistance terms when it pairs resistant and sensitive values
  (waldronlab/bugphyzzExports#83).

# bugphyzz 1.7.8
+ `makeSignatures()` names discrete signatures `bugphyzz:` like the range
  ones; they were `bugphyz:` (#256). Code that matched the old prefix in the
  GMT files needs updating; bugphyzzExports v1.1.0 onward carries the new
  names.
+ `importBugphyzz()` reads only the three `bugphyzz_*.csv` tables from a
  Zenodo deposit, extracted into a fresh directory. The archive now also
  holds `sources/`, whose `.csv.gz` files matched the old pattern.
+ `attributes.tsv`: the group typo `extreme environemnt`, the plural
  `mutation rates` and a trailing semicolon are fixed, so those values match
  their physiology; the `Butyrate-Producing Bacteria` row keeps the sheet's
  spelling and loses the unused mixed-case group (#281). Rows without an
  `attribute_group` are unchanged.
+ Tests that download data skip when offline, and the `physiologies()`
  example is no longer run on check (#266, in part).
+ `rlang` was listed in both Imports and Suggests; now Imports only.

# bugphyzz 1.7.7
+ New synonym table `inst/extdata/attribute_synonyms.tsv` maps source terms
  to the controlled vocabulary in `attributes.tsv`; `physiologies()` applies
  it to sheet, BacDive and `sources/` rows alike. The export drops any value
  not in the vocabulary, which until now discarded every BacDive shape
  ("rod-shaped", "coccus-shaped", ...), the 2020 Bergey's "vibriod cell",
  BacDive's "facultative aerobe", "biosafety level 1+" and others (#229,
  #264). Shape rows with a valid value go from 25,556 to 41,268, habitat
  from 4,906 to 5,703; aerophilicity, disease association, biosafety level
  and hemolysis gain a few dozen each. "aerotolerant" and "microaerotolerant"
  become "anaerobic". Shape terms with no vocabulary equivalent (flask-,
  ring-, star-, spore-shaped, "other") are dropped by the table on purpose.
+ A test checks that every discrete value from every sheet and source is in
  the vocabulary after mapping, so a new unmapped term fails here instead of
  disappearing in the export.

# bugphyzz 1.7.6
+ New source `NCBI_RefSeq` in `attribute_sources.tsv`: genome size, G+C
  content and protein-coding gene count of one RefSeq assembly per
  bacterial or archaeal species (77,673 species), built by
  `sources/ncbi_assemblies` in bugphyzzExports (issue waldronlab/bugphyzzExports#74,
  added in waldronlab/bugphyzzExports#81).
+ `.getBacDive()` returns the NCBI id columns as character, as the other
  importers do; `read.csv` had made them integer, which failed a test added
  in 1.7.5.

# bugphyzz 1.7.5
+ README now documents the whole pipeline: curation sheets, programmatic
  sources, export, cross-validation, Zenodo release, and the related
  repositories.
+ BacDive data now come from the BacDive API (free since February 2026)
  through `sources/bacdive` in bugphyzzExports instead of a 2023 Google
  Sheet snapshot: every strain in BacDive, each with the NCBI taxid BacDive
  assigns (the sheet had one for 6% of strains), plus G+C content, cell
  length and cell width, which the sheet never carried.

# bugphyzz 1.7.4
+ `physiologies()` now also reads programmatic sources kept under `sources/`
  in waldronlab/bugphyzzExports (manifest plus one TSV per physiology, cached
  with BiocFileCache): a 2026 extraction of Bergey's Manual genus abstracts
  (`Bergeys_2026`, new entry in `attribute_sources.tsv`) and the full Madin
  et al. 2020 species traits. `sources = FALSE` turns this off;
  `options(bugphyzz.sourcesBaseUrl = ...)` points it at a local checkout.
+ New source `Bergeys_2026`: genus traits (aerophilicity, Gram stain, shape,
  motility, spore formation, growth temperature optimum, G+C content)
  extracted sentence by sentence from the abstracts of all 2,249 Bergey's
  Manual genus chapters. It reports every state the abstract gives, with a
  Frequency, where the 2020 scrape (`Bergey's Manual`) kept one. Both are
  present for now; see waldronlab/bugphyzz#274.
+ Madin et al. 2020 is now imported in full (Gram stain, spore formation,
  motility, shape, genome size, coding genes, growth temperature, optimal pH,
  G+C content, aerophilicity) at species level, about 70,000 rows; only
  aerophilicity was used before.
+ New attribute `gc content` (range, mol%, signature bins low < 40, medium
  40 to 60, high > 60) and new shape value `vibrio`. `showPhys()` lists
  `gc content`; it has no curation sheet and comes only from the
  programmatic sources.
+ Bug fix: range attributes (length, width) had `Attribute_value_max` set
  equal to `Attribute_value_min` in `physiologies()`, so every range collapsed
  to its lower bound.
+ `physiologies()` warns and uses the sheets only when the programmatic
  sources cannot be fetched.
+ Data exports on the `devel` branch of bugphyzzExports
  (`importBugphyzz(version = "devel")`) now apply the validation thresholds
  and drop `rarely` and `never` states at export time, so the files are about
  a tenth of their previous size. The Zenodo release is unchanged until the
  next deposit.

# bugphyzz 1.7.3
+ `importBugphyzz()` now filters imputed (ASR) annotations with separate
  thresholds for discrete and numeric attributes: `mccThreshold` (default 0.7)
  and `r2Threshold` (default 0.5), the values used in the bugphyzz manuscript.
  The old single `v` argument (default 0.8, documented as 0.5) is deprecated.
  With the new defaults, imputed genome size, coding genes and growth
  temperature are included again.
+ `excludeRarely = TRUE` now also drops annotations with Frequency "never", as
  the documentation already said.
+ Imputed annotations with no cross-validation score are now dropped (they were
  kept before). Validation scores are matched case-insensitively, so attributes
  such as "COGEM pathogenicity rating" are no longer lost.

# bugphyzz 1.7.2
+ Levi Waldron is now the maintainer.

# bugphyzz 0.99.3
+ Update table with validations results in extdata to version 1.0.2

# bugphyzz 0.99.0
+ First version of the package for submission in Bioconductor.
