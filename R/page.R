#' Scrape page
#'
#' Convert patient info page into data frame
#'
#' @param url URL to the page to scrape
#' @return A data frame
#' @export
scrape_patient_forum_page <- function(url) {
  x <- xml2::read_html(url)
  if (is_multi_page(x)) {
    pn <- get_page_numbers(x)
    urls <- paste0(url, "?page=", pn)
    #lapply(urls, convert_page_to_data_frae=me)
    x <- x
  } else {
    x <- x
  }
  x
}

`%P%` <- function(lhs, rhs) paste0(lhs, rhs)

read_web_page <- function(x) xml2::read_html(x)

n_uq <- function(x) length(unique(x))

is_multi_page <- function(x) {
  !is.na(determine_page_count(x))
}

determine_page_count <- function(x) {
  x %>%
    rvest::html_node(".reply__control.reply-pagination") %>%
    rvest::html_text()
}

get_page_numbers <- function(x) {
  p <- x %>%
    rvest::html_node(".reply__control.reply-pagination") %>%
    rvest::html_text()
  m <- gregexpr("\\d+(?=/)", p, perl = TRUE)
  regmatches(p, m)[[1]]
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
    data[[i]] <- tryCatch(
      scrape_patient_forum_page(urls[i]),
      error = function(e) NULL
    )
    if (is.null(data[[i]])) {
      warning("Returned NULL, breaking loop...", call. = FALSE)
      break
    }
  }
  data
}
