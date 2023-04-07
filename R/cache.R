.getCache <- function() {
    cache <- tools::R_user_dir("bugphyzz", which = "cache")
    BiocFileCache::BiocFileCache(cache, ask = FALSE)
}

.getResource <- function(rname, url, force = FALSE, verbose = TRUE) {
    cache <- .getCache()
    res <- BiocFileCache::bfcquery(cache, rname, "rname", exact = TRUE)
    rid <- res$rid
    if (length(rid) > 0 && force) {
      time <- res$create_time
      if (verbose) {
        message('Replacing previous version downloaded on ', time, '.')
      }
      BiocFileCache::bfcremove(cache, rid)
      rid <- names(BiocFileCache::bfcadd(cache, rname, url))
    }
    if (!length(rid)) {
      if (verbose) {
        message( "Downloading, ", rname, ".")
      }
      rid <- names(BiocFileCache::bfcadd(cache, rname, url))
    }
    time <- BiocFileCache::bfcinfo(cache, rid)$create_time
    message('Using data downloaded on ', time, '.')
    BiocFileCache::bfcrpath(cache, rids = rid)
}

.removeCache <- function() {
  cache <- .getCache()
  BiocFileCache::removebfc(cache)
}
