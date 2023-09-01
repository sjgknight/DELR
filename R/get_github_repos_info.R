#' Title
#'
#' @param repo_urls
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom gh gh
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
    tibble(
      Name = repo_info$name,
      Description = repo_info$description,
      URL = repo_info$html_url,
      Last_Commit_Date = as.Date(repo_info$updated_at)
    )
  })

  repo_data <- bind_rows(repo_data_list)
  return(repo_data)
}
