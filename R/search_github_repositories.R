#' search_github_repositories
#'
#' @param query Search query
#' @param sort Sort
#' @param order Order result
#'
#' @return Parsed response
#' @export
#'
#' @examples search_github_repositories(query = "sta-518")
search_github_repositories <-
  function(query, sort = "stars", order = "desc") {
    # urls
    github_api_url <- "https://api.github.com"
    search_endpoint <- "/search/repositories"
    
    # build query params
    query_params <-
      paste(paste0("?q=", query),
            paste0("sort=", sort),
            paste0("order=", order),
            sep = "&")
    
    print(paste("Url is", github_api_url, search_endpoint, query_params))
    
    # make http request to Github Api
    response <-
      httr::GET(url = paste0(github_api_url, search_endpoint, query_params))
    
    response_status_code <- httr::status_code(response)
    
    # Stop if unexpected status code
    if (response_status_code != 200) {
      stop(paste0("Unexpected status", response_status_code, "encountered"))
    }
    
    # Parse the JSON response
    response_parsed <- httr::content(response, as = "parsed")
    
    # Print stats
    print(paste(
      "Search for",
      query,
      "found",
      response_parsed$total_count,
      "results"
    ))
    
    return(response_parsed)
}
