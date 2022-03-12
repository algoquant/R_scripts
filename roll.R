# Functions for rolling aggregations

# define functional for rolling aggregations over endpoints
roll_agg <- function(xtes, endpoints, FUN, ...) {
  nrows <- length(endpoints)
  # startpoints are single-period lag of endpoints
  startpoints <- endpoints[c(1, 1:(nrows-1))] + 1
  # perform aggregations over length of endpoints
  agg_regations <- lapply(2:nrows, 
                          function(indeks) FUN(.subset_xts(xtes, 
                                                           startpoints[indeks]:endpoints[indeks]), ...))  # end lapply
  # rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # coerce agg_regations into xts series
    xts(agg_regations, order.by=index(xtes[endpoints]))
  agg_regations
}  # end roll_agg

