#' Scrape page
#'
#' Convert patient info page into data frame
#'
#' @param url URL to the page to scrape
#' @return A data frame
#' @export
scrape_patient_forum_page <- function(url) {
  x <- xml2::read_html(url)
  x
}

#' Scrape pages
#'
#' Convert patient info pages into list of data frames
#'
#' @param urls URLs to the page to scrape
#' @return A list of data frames
#' @export
scrape_patient_forum_pages <- function(urls) {
  data <- vector("list", length(urls))
  for ( i in seq_along(urls)) {
    data[[i]] <- scrape_patient_forum_page(urls[i])
  }
  data
}
