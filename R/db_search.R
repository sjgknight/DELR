
#' query_table_process
#'
#' @param query_table
#'
#' @return query_table
#' @export query_table_process
#' @importFrom purrr map_chr
#' @importFrom stringr str_flatten
#' @import magrittr
#' @import dplyr
#' @examples
query_table_process <- function(query_table){
  query_table %>%
    rowwise() %>%
    mutate(across(starts_with("terms_fixed"),
                  ~purrr::map_chr(.x, ~shQuote((.))) %>%
                    unique() %>%
                    stringr::str_flatten(collapse = " OR ",na.rm = T)
    )) %>%
    ungroup() %>%
    mutate(
      across(starts_with("terms_stem"),
               ~query_stemmer(.x) %>%
               unique() %>%
               stringr::str_flatten(collapse = " OR ",na.rm = T)
    ))
}

#' query_stemmer
#'
#' @param list_col
#'
#' @return
#' @export
#' @importFrom purrr map
#' @importFrom SnowballC wordstem
#' @importFrom stringr str_flatten
#' @import dplyr
#' @import magrittr
#' @examples
query_stemmer <- function(list_col) {
  list_col %>%
    purrr::map(function(.x) {
      if (length(.x) > 0) {
        .x %>%
          SnowballC::wordStem() %>%
          unique() %>%
          paste0("*") %>%
          stringr::str_flatten(collapse = " OR ",na.rm = T)
      } else {
        NULL
      }
    })
}
#textstem::stem_words() or SnowballC::wordStem or tm::stemDocument
#tm::stemCompletion may also be useful

query_quoter <- function(terms){
  purrr::map_chr(terms, ~shQuote((.))) %>%
    unique() %>%
    stringr::str_flatten(collapse = " OR ",na.rm = T)
}
