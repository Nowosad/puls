#' Prepares Input Data
#'
#' This function is used iternally in the `validator()` function.
#'
#' @param x to a `.dat` file
#' @param start_date The start date of the validation period
#' @param end_date The end state of the validation period
#'
#' @return A reformatted data frame with a TIMESTAMP, RECORD, name, value, parameter, and site
#' @export
#'
#' @examples
#' dat_filepath = system.file("dat/CR3000_Rain.dat", package = "puls")
#' cleaner(dat_filepath, "BR")
cleaner = function(x, site_code = NULL,
                   start_date = as.Date(-Inf, origin = "1970-01-01"),
                   end_date = as.Date(Inf, origin = "1970-01-01")){

  if (is.null(site_code)){
    folder_name = basename(dirname(x))
    site_code = switch (folder_name,
                     "Kusowo" = "KU",
                     "Brody" = "BR",
                     "Rzecin" = "RZ"
    )
  }

  if (tools::file_ext(x) == "dat"){
    result = read_dat(x)
    result = dplyr::filter(result, dplyr::between(as.Date(TIMESTAMP),
                                   as.Date(start_date),
                                   as.Date(end_date)))
    param_nrow = nrow(result)
    param_colnames = colnames(result)[(!colnames(result) %in% c("TIMESTAMP", "RECORD"))]
    param_names = unify_param_names(param_colnames)
    # result = tidyr::pivot_longer(result, -c("TIMESTAMP", "RECORD"))
    result = tidyr::gather(result, name, value, -TIMESTAMP, -RECORD)
    # cleaner_names = unify_param_names(result$name)
    result$parameter = rep(param_names, each = param_nrow)
    result = dplyr::mutate(result, site = site_code)
  } #else {
    #x %>%
    #  read_ghg()
  #}

  return(result)

}

unify_param_names0 = function(x){
  y = numeric(length = length(x))
  for (i in seq_along(x)){
    spec = stringr::str_subset(x[i], "^Spec")
    if (length(spec) == 1){
      y[i] = stringr::str_to_upper(spec)
    } else {
      non_spec = stringr::str_subset(x[i], "Spec", negate = TRUE)
      non_spec = stringr::str_extract(non_spec, "^[a-zA-Z]*[\\_]?[a-zA-Z]*")
      non_spec = stringr::str_replace(non_spec, "[\\_]$", "")
      y[i] = stringr::str_to_upper(non_spec)
    }
  }
  return(y)
}

unify_param_names = function(x){
  # spec = stringr::str_which(x[i], "^Spec")
  non_spec = stringr::str_which(x, "Spec", negate = TRUE)
  non_spec_vec = stringr::str_extract(x[non_spec], "^[a-zA-Z]*[\\_]?[a-zA-Z]*")
  non_spec_vec = stringr::str_replace(non_spec_vec, "[\\_]$", "")

  x[non_spec] = non_spec_vec
  x = stringr::str_to_upper(x)
  return(x)
}
