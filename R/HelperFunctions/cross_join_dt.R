#' as downloaded from 
#' http://stackoverflow.com/questions/25888706/r-data-table-cross-join-not-working
cross_join_dt = function(X, Y){
  stopifnot(is.data.table(X),is.data.table(Y))
  k = NULL
  X = X[, c(k=1, .SD)]
  setkey(X, k)
  Y = Y[, c(k=1, .SD)]
  setkey(Y, NULL)
  X[Y, allow.cartesian=TRUE][, k := NULL][]
}