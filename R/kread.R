#' kread - A utility function to read data files based on their extensions
#'
#' @description
#' This function reads data files based on their file extensions and returns the
#' corresponding data frame or tibble. It supports multiple file formats such as
#' CSV, TSV, Excel (XLS, XLSX), and ODS.
#'
#' @param path [character] specifying the file path to be read. The file
#' must have an extension to determine the appropriate reader function.
#' @param ... Additional arguments passed to the underlying reader function.
#'
#' @details
#' The function determines the file type by extracting the file extension from
#' the provided `path`. It then uses the appropriate reader function from
#' packages such as `readr`, `readxl`, or `readODS` to read the file. If the
#' file extension is not recognized or the path does not reference a file, an
#' error is raised.
#'
#' Supported file extensions and their corresponding reader functions:
#' - `csv`: `readr::read_delim`
#' - `tsv`: `readr::read_delim`
#' - `xls`: `readxl::read_excel`
#' - `xlsx`: `readxl::read_excel`
#' - `ods`: `readODS::read_ods`
#'
#' @return A data frame or tibble containing the data from the file.
#'
#' @examples
#' \dontrun{
#' # Reading a CSV file
#' data <- kread("data.csv")
#'
#' # Reading an Excel file
#' data <- kread("data.xlsx", sheet = 1)
#'
#' # Reading an ODS file
#' data <- kread("data.ods")
#' }
#' 
#' @export
#' 
kread <- function(path, ...) {
  ext <- tools::file_ext(path)
  if (ext == "") stop("Path not referencing a file")
  switch (
    ext,
    csv = readr::read_delim,
    tsv = readr::read_delim,
    xls = readxl::read_excel,
    xlsx = readxl::read_excel,
    ods = readODS::read_ods
  )(path, ...)
}