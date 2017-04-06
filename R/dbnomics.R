#' Get serie
#'
#' @param slug
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_serie(slug = "bis-pp-ls-q-au")
#'
get_serie <- function(slug) {
  base_url <- "https://api.db.nomics.world/api/v1/json/"
  get_request <- httr::GET(url = paste0(base_url, "series/", slug)) %>%
    httr::content() %>%
    magrittr::extract2("data") %>%
    magrittr::extract2("values")

  tibble::tibble(
    period = purrr::map_chr(.x = get_request, .f = "period"),
    values = purrr::map_chr(.x = get_request, .f = "value")
  )
}


