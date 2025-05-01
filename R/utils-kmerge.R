#' @title Merge List Items
#'
#' @description Merges all items within a list into a single object.
#'
#' @inheritParams kmerge
#'
#' @return A single merged object from the list items.
#'
#' @export
#' @rdname utils-kmerge
#'
kmergeListItems <- function(x, ..., cfg = loadConfig()) {
  if(length(x) == 1) return(x[[1]])
  Reduce(function(x, y) {merge(x, y, ...)}, x)
}

#' @title Merge Multiple Lists
#'
#' @description Placeholder function for merging multiple lists. Not implemented
#' yet.
#'
#' @inheritParams kmerge
#'
#' @return Throws an error indicating the function is not implemented.
#'
#' @export
#' @rdname utils-kmerge
#'
kmergeMultipleLists <- function(x, y, ..., cfg = loadConfig()) {
  stop("Not implemented yet")
}