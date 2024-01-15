create_gh_pull_request <- function(
    source_branch = gert::git_branch(),
    target_branch = "main",
    repo_url = get_remote_repo_url(),
    pr_template = NULL
) {

  # gh_pr_query_string_example <- "https://github.com/octo-org/octo-repo/compare/main...my-branch?quick_pull=1"
  # remote_list <- gert::git_remote_list(repo = ".")
  # origin_url <- remote_list[remote_list$name %in% "origin"]$url[1]
  # # origin_url <- remote_list |> dplyr::filter(name %in% "origin") |> dplyr::pull(url)

  #############################################################################|
  ## PR query string docs: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/using-query-parameters-to-create-a-pull-request
  ## PR query string: "https://github.com/octo-org/octo-repo/compare/main...my-branch?quick_pull=1"
  ## PR template query string: "https://github.com/octo-org/octo-repo/compare/main...my-branch?quick_pull=1&template=issue_template.md"
  #############################################################################|

  gh_pr_query_string <- glue::glue("{repo_url}/compare/{target_branch}...{source_branch}?quick_pull=1")

  if (!is.null(pr_template)) {
    gh_pr_query_string <- paste0(gh_pr_query_string, "template=", pr_template)
  }

  browseURL(gh_pr_query_string)

  return(gh_pr_query_string)
}

get_remote_repo_url <- function() {
  remote_list <- gert::git_remote_list(repo = ".")
  origin_repo <- remote_list[remote_list$name %in% "origin"]$url[1]
  origin_url <- sub(":(?!//)", "/", x = origin_repo, perl = TRUE) |>
    gsub("^git@", "https://", x = _, perl = TRUE) |>
    gsub("\\.git$", "", x = _, perl = TRUE)

  return(origin_url)
}
