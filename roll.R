# Functions for rolling aggregations

# define functional for rolling aggregations over end_points
roll_agg <- function(x_ts, end_points, FUN, ...) {
  len_gth <- length(end_points)
  # start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
  # perform aggregations over length of end_points
  agg_regations <- lapply(2:len_gth, 
                          function(in_dex) FUN(.subset_xts(x_ts, 
                                                           start_points[in_dex]:end_points[in_dex]), ...))  # end lapply
  # rbind list into single xts or matrix
  agg_regations <- rutils::do_call_rbind(agg_regations)
  if (!is.xts(agg_regations))
    agg_regations <-  # coerce agg_regations into xts series
    xts(agg_regations, order.by=index(x_ts[end_points]))
  agg_regations
}  # end roll_agg

