#' Title
#'
#' @param y a
#' @param param_df b
#' @param howmanysd c
#'
#' @return d
#' @rdname validate_
#' @export
#'
#' @examples

validate_nan = function(y){
  correct_df = y[["correct_df"]]
  error_df = y[["error_df"]]

  new_error_df = correct_df[is.nan(correct_df$value), c("TIMESTAMP", "RECORD", "name", "value")]
  new_error_df$problem = "NaN values"

  error_df = rbind(error_df, new_error_df)
  correct_df = correct_df[!is.nan(correct_df$value), ]

  list(correct_df = correct_df,
       error_df = error_df)
}

#' @name validate_
#' @export
validate_min = function(y, param_df){
  correct_df = y[["correct_df"]]
  error_df = y[["error_df"]]

  new_error_df = correct_df[correct_df$value < param_df$min_value, c("TIMESTAMP", "RECORD", "name", "value")]
  new_error_df$problem = "Values below the expected minimum"

  error_df = rbind(error_df, new_error_df)
  correct_df = correct_df[correct_df$value >= param_df$min_value, ]

  list(correct_df = correct_df,
       error_df = error_df)
}

#' @name validate_
#' @export
validate_max = function(y, param_df){
  correct_df = y[["correct_df"]]
  error_df = y[["error_df"]]

  new_error_df = correct_df[correct_df$value > param_df$max_value, c("TIMESTAMP", "RECORD", "name", "value")]
  new_error_df$problem = "Values above the expected maximum"

  error_df = rbind(error_df, new_error_df)
  correct_df = correct_df[correct_df$value <= param_df$max_value, ]

  list(correct_df = correct_df,
       error_df = error_df)
}

#' @name validate_
#' @export
validate_longterm = function(y, param_df, howmanysd = 3){
  if (!is.na(param_df$longterm_sd)){
    longterm_sds = howmanysd * param_df$longterm_sd
    longterm_range = param_df$longterm_avg + c(-longterm_sds, longterm_sds)

    correct_df = y[["correct_df"]]
    error_df = y[["error_df"]]

    new_error_df = correct_df[!dplyr::between(correct_df$value, longterm_range[1], longterm_range[2]), c("TIMESTAMP", "RECORD", "name", "value")]
    new_error_df$problem = "Values outside of the long-term range"

    error_df = rbind(error_df, new_error_df)
    correct_df = correct_df[dplyr::between(correct_df$value, longterm_range[1], longterm_range[2]), ]

    list(correct_df = correct_df,
         error_df = error_df)
  } else {
    y
  }
}
