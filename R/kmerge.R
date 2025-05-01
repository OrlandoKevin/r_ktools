#' @title Generic kmerge Function
#'
#' @description A generic function to merge objects using method dispatch.
#'
#' @param x The object to be merged. The type of this object determines the
#' method used.
#' @param y An optional second object to merge with `x`. The type of this
#' object also determines the method used.
#' @param ... Additional arguments passed to specific methods.
#' @template cfg
#'
#' @return The merged object.
#'
#' @export
#'
kmerge <- function(x, ...) {
  UseMethod("kmerge", x)
}

#' @title kmerge Method for Data Frames
#'
#' @description Merges two data frames using the specified package.
#'
#' @inheritParams kmerge
#' @param package The package to use for merging. Options are "base" or "dplyr".
#'
#' @return A merged data frame.
#'
#' @export
#' @rdname kmerge
#'
kmerge.data.frame <- function(x, y, package = "base", ..., cfg = loadConfig()) {
  FUN <- switch(
    package,
    base = merge,
    dplyr = dplyr::full_join
  )
  FUN(x, y, ...)
}

#' @title kmerge Method for Lists
#'
#' @description Merges list items or multiple lists based on the provided
#' arguments.
#'
#' @param x A list to be merged.
#' @param y An optional second list to merge with `x`. If `NULL`, merges items
#' within `x`.
#' @param ... Additional arguments passed to the merging functions.
#'
#' @return A merged object, either from list items or multiple lists.
#'
#' @export
#' @rdname kmerge
#'
kmerge.list <- function(x, y = NULL, ..., cfg = loadConfig()) {
  if (is.null(y)) {
    merged <- kmergeListItems(x, ..., cfg = cfg)
  } else {
    merged <- kmergeMultipleLists(x, y, ..., cfg = cfg)
  }
  return(merged)
}
