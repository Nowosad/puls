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

  x %>%
    read_dat() %>%
    tidyr::pivot_longer(-c("TIMESTAMP", "RECORD")) %>%
    dplyr::mutate(parameter = stringr::str_extract(name, "^[a-zA-Z]*[\\_]?[a-zA-Z]*")) %>%
    dplyr::mutate(parameter = stringr::str_replace(parameter, "[\\_]$", "")) %>%
    dplyr::mutate(site = site_code)
}
