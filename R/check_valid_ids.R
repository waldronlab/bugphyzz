.check_valid_ncbi_ids <- function(dat) {
  ncbi_ids <- dat[['NCBI_ID']]
  ncbi_ids <- ncbi_ids[!is.na(ncbi_ids)]
  ncbi_ids <- unique(as.character(ncbi_ids))
  ncbi_ids <- ncbi_ids[!ncbi_ids == 'unknown']

  ## I use the taxid2name just because it's faster. It could be
  ## another function of taxizedb
  taxizedb_output <- taxizedb::taxid2name(ncbi_ids, db = 'ncbi')
  names(taxizedb_output) <- ncbi_ids
  invalid_ncbi_ids <- names(taxizedb_output[is.na(taxizedb_output)])

  if (!length(invalid_ncbi_ids)) {
    message('All NCBI ids are valid.')
    return(NULL)
  }

  taxize_output <- vector('list', length(invalid_ncbi_ids))
  names(taxize_output) <- invalid_ncbi_ids
  for (i in seq_along(taxize_output)) {
    taxize_output[[i]] <- tryCatch(
      {
        x <- taxize::classification(invalid_ncbi_ids[i], db = 'ncbi')[[1]]
        df_out <- tibble::tibble(
          suggested_id = tail(x$id, 1),
          suggested_name = tail(x$name, 1),
          comment = 'outdated'
        )
      },
      error = function(e) {
        data.frame(
          suggested_id = NA,
          suggested_name = NA,
          comment = 'not found'
        )
      }
    )
  }

  invalid_data <- dplyr::bind_rows(taxize_output, .id = 'invalid_id')

  invalid_pos <- vector('list', length(invalid_ncbi_ids))
  for (i in seq_along(invalid_pos)) {
    invalid_positions <- which(dat$NCBI_ID == invalid_ncbi_ids[i])
    current_name <- dat$Taxon_name[invalid_positions]
    invalid_pos[[i]] <- tibble::tibble(
      invalid_id = invalid_ncbi_ids[i],
      invalid_pos = invalid_positions,
      current_name = current_name
    )
  }

  invalid_positions_df <- dplyr::bind_rows(invalid_pos)

  dplyr::left_join(invalid_positions_df, invalid_data, by = 'invalid_id') %>%
    dplyr::relocate(
      invalid_pos, comment, invalid_id, suggested_id,
      current_name, suggested_name
    )
}
