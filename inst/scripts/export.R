library(bugphyzz)

x <- physiologies()

expectValidHeadersAll(x)

xuniform <- lapply(x, function(x1) x1[, validHeaders()])
all <- do.call(rbind, xuniform)

(fname <- paste0("bugphyzz_", Sys.Date(), ".csv.bz2"))
unlink(fname)
con <- bzfile(fname, "w")
write.csv(all, file=con)
close(con)
