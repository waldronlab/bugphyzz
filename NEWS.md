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
