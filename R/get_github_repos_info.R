#' Get github repository information
#' @description
#' Takes a list of github repositories, returns a tibble of their names, descriptions, URL, and last commit date
#'
#' @details
#' Largely useful just to tabulate the information and get the commit date as a way of finding the most recently committed (/weeding out inactive repos)
#'
#'
#' @param repo_urls a vector of repository strings of form "owner/repo"
#'
#' @return
#' @export
#'
#' @examples get_github_repos_info(c("sjgknight/rureporting", "sjgknight/learnr"))
#' @importFrom gh gh
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom glue glue
get_github_repos_info <- function(repo_urls) {
  repo_data_list <- lapply(repo_urls, function(url) {
    # Extract the repository owner and name from the GitHub URL
    #url <- zrclients[1]
    url_parts <- strsplit(url, "/")
    owner <- url_parts[[1]][1]
    repo <- url_parts[[1]][2]

    # Get the repository information
    repo_info <- gh::gh(endpoint = glue::glue("GET /repos/{owner}/{repo}"))

    # Create a tibble with the desired information
    tibble::tibble(
      Name = repo_info$name,
      Description = repo_info$description,
      URL = repo_info$html_url,
      Last_Commit_Date = as.Date(repo_info$updated_at)
    )
  })

  repo_data <- dplyr::bind_rows(repo_data_list)
  return(repo_data)
}
