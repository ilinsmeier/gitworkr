#'  Create new repository from GitHub template repo
#'
#' @param repo_owner
#' @param repo_name
#' @param repo_descr
#' @param proj_dir
#' @param tmplt_owner
#' @param tmplt_repo
#'
#' @return
#' @export
#'
#' @examples
gen_repo_from_template <- function(repo_owner, repo_name, repo_descr, proj_dir,
                                   tmplt_owner, tmplt_repo) {

  ## create repo from template using github api
  gh::gh("POST /repos/{tmplt_owner}/{tmplt_repo}/generate",
         .accept = "application/vnd.github+json",
         template_owner = tmplt_owner,
         template_repo  = tmplt_repo,
         owner = repo_owner,
         name  = repo_name,
         description = repo_descr,
         include_all_branches = FALSE,
         private = TRUE
  )

  ## clone new github repo & open as RStudio project in a new session
  usethis::create_from_github(
    repo_spec = glue::glue("https://github.com/{repo_owner}/{repo_name}.git"),
    destdir = proj_dir,
    fork = FALSE,
    rstudio = TRUE,
    open = TRUE,
    protocol = "https"
  )

}
