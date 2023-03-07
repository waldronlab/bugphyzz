
importBugphyzz <- function(keyword = 'all', version = 'devel', cache = TRUE) {

  if (version == 'devel') {
    file <- 'https://raw.githubusercontent.com/waldronlab/bugphyzzExports/main/full_dump.csv'
  }

  bugphyzz <- vroom::vroom(
    file, delim = ',', skip = 1, show_col_types = FALSE,
    col_types = vroom::cols(
      NCBI_ID = readr::col_character(),
      Parent_NCBI_ID =  readr::col_character()
    )
  )
  return(bugphyzz)
}

getBugphyzzSignatures <- function(
    df, tax.id.type = c('ncbi', 'taxname'), tax.level = 'mixed', min.size = 5,
    evidence = c('asr', 'inh', 'exp', 'tas', 'nas', 'igc'),
    frequency = c('unknown', 'always', 'usually', 'sometimes')

    # exact.tax.level = TRUE, min.size = 5
) {

}
