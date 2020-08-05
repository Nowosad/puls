#' Prepares Input Data
#'
#' This function is used iternally in the `validator()` function.
#'
#' @param Path to a `.dat` file
#'
#' @return A reformatted data frame with a TIMESTAMP, RECORD, name, value, parameter, and site
#' @export
#'
#' @examples
#' dat_filepath = system.file("dat/CR3000_Rain.dat", package = "puls")
#' cleaner(dat_filepath, "BR")
cleaner = function(x, site_code = NULL){

  if (is.null(site_code)){
    folder_name = basename(dirname(x))
    site_code = switch (folder_name,
                     "Kusowo" = "KU",
                     "Brody" = "BR",
                     "Rzecin" = "RZ"
    )
  }

  if (tools::file_ext(x) == "dat"){
    result = x %>%
      read_dat() %>%
      tidyr::pivot_longer(-c("TIMESTAMP", "RECORD")) %>%
      dplyr::mutate(parameter = unify_param_names(name)) %>%
      dplyr::mutate(site = site_code)
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
