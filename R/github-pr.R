#' Create GitHub Pull Request
#'
#' @param source_branch The name of the branch containing changes to be merged
#'   into the `target_branch`. The current branch is used by default.
#' @param target_branch The name of the receiving branch, which is typically the
#'   "main" (default) branch.
#' @param repo_url The GitHub repository URL where the Pull Request will be
#'   created. By default, `gitworkr::get_remote_repo_url()` is used to parse the
#'   URL of the remote "origin".
#' @param pr_template The name of a pull request template file located in the
#'   subdirectory ".github/PULL_REQUEST_TEMPLATE/" or
#'   "docs/PULL_REQUEST_TEMPLATE/" of the repo specified in `repo_url`.
#'
#' @return
#' @export
#'
#' @examples
create_gh_pull_request <- function(
    source_branch = gert::git_branch(),
    target_branch = "main",
    repo_url = gitworkr::get_remote_repo_url(),
    pr_template = NULL
) {

  #############################################################################|
  ## PR query string docs: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/using-query-parameters-to-create-a-pull-request
  ## PR query string: "https://github.com/octo-org/octo-repo/compare/main...my-branch?quick_pull=1"
  ## PR template query string: "https://github.com/octo-org/octo-repo/compare/main...my-branch?quick_pull=1&template=issue_template.md"
  #############################################################################|

  gh_pr_query_string <- glue::glue("{repo_url}/compare/{target_branch}...{source_branch}?quick_pull=1")

  if (!is.null(pr_template)) {
    pr_template_file <- basename(pr_template)
    gh_pr_query_string <- paste0(gh_pr_query_string, "&template=", pr_template_file)
  }

  ## open new pull request in the browser
  browseURL(gh_pr_query_string)

  return(gh_pr_query_string)
}

#' Remote Repository URL
#'
#' @return
#' @export
#'
#' @examples
get_remote_repo_url <- function() {
  remote_list <- gert::git_remote_list(repo = ".")
  origin_repo <- remote_list[remote_list$name %in% "origin"]$url[1]
  origin_url <- sub(":(?!//)", "/", x = origin_repo, perl = TRUE) |>
    gsub("^git@", "https://", x = _, perl = TRUE) |>
    gsub("\\.git$", "", x = _, perl = TRUE)

  return(origin_url)
}
