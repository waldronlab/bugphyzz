fname <- system.file("extdata/template.tsv", package ="bugphyzz")
readr::read_tsv(fname, col_types = "ccccc")

oxygen = bugphyzz::physiologies("aerophilicity")[[1]]

x = .template(oxygen)



