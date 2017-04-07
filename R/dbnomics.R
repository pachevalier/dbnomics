#' Get serie
#'
#' @param slug a string
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_serie(slug = "bis-pp-ls-q-au")
#'
get_serie <- function(slug) {

  get_request <- get_dbnomics(endpoint = "series", slug = slug) %>%
  httr::content() %>%
  magrittr::extract2("data") %>%
  magrittr::extract2("values")

  tibble::tibble(
    period = purrr::map_chr(.x = get_request, .f = "period"),
    values = purrr::map_chr(.x = get_request, .f = "value")
  )

}

#' Get dbnomics
#'
#' @param endpoint a string either "providers", "datasets" or "series"
#' @param slug a string name of the slug
#' @param suffix a suffix. For instance "values"
#'
#' @return a get request
#' @export
#'
#' @examples
#'
#' get_dbnomics(endpoint = "series", slug = "bis-pp-ls-q-au")
#'
#'
#' # https://api.db.nomics.world/api/v1/json/datasets/insee-cna-2010-dep-apu/values
#' get_dbnomics(
#' endpoint = "datasets",
#' slug = "insee-cna-2010-dep-apu",
#' suffix = "values")
#'
#' \dontrun{
#' library("magrittr")
#' get_dbnomics(
#' endpoint = "datasets",
#' slug = "insee-cna-2010-dep-apu",
#' suffix = "values") %>%
#' httr::content() %>%
#' magrittr::extract2("data")
#' }
#'

get_dbnomics <- function(endpoint, slug, suffix) {

  base_url <- "https://api.db.nomics.world/api/v1/json/"

  url <-ifelse(
    test = missing(suffix) == TRUE,
    yes = paste0(base_url, endpoint, "/", slug),
    no = paste0(base_url, endpoint, "/", slug, "/", suffix)
    )

  get_request <- httr::GET(url = url)

  message(httr::status_code(get_request))

  return(get_request)
}

