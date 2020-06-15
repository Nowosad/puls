#' Read a ghg file
#'
#' Reads a ghg file and creates a data frame from it.
#'
#' @param file the file path to the input file
#'
#' @return a data.frame with data extracted from the input ghg file
#' @export
#'
#' @examples
#' ghg_filepath = system.file("ghg/2020-02-19T220000_AIU-0874.ghg", package = "puls")
#' my_ghg = read_ghg(ghg_filepath)
#' head(my_ghg)
read_ghg = function(file){
  ghg_tempdir = tempdir()
  utils::unzip(file, exdir = ghg_tempdir)
  ghg_basename = tools::file_path_sans_ext(basename(file))
  # read.table(
  #   paste0(ghg_tempdir, "/", ghg_basename, ".data"),
  #   skip = 7,
  #   header = TRUE,
  #   sep = "\t"
  # )
  readr::read_tsv(
    paste0(ghg_tempdir, "/", ghg_basename, ".data"),
    skip = 7,
    col_names = TRUE#,
    # sep = "\t"
  )
}



