#' Read a dat file
#'
#' Reads a dat file and creates a data frame from it.
#'
#' @param file the file path to the input file
#'
#' @return a data.frame with data extracted from the input dat file
#' @export
#'
#' @examples
#' dat_filepath = system.file("dat/CR3000_Rain.dat", package = "puls")
#' my_dat = read_dat(dat_filepath)
#' head(my_dat)
read_dat = function(file){
  col_names = readr::read_lines(file, skip = 1, n_max = 1)
  col_names = strsplit(col_names, split = ",")[[1]]
  col_names = gsub('"', "", col_names)
  readr::read_csv(
    file,
    skip = 4,
    col_names = col_names
  )
}



