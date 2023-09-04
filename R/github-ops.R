#' Create new repository from GitHub template repo
#'
#' Create a new GitHub repository from a template repo, clone the new repo to
#' your local machine, create an RStudio project for the cloned repository, and
#' open the RStudio project in a separate R session.
#'
#' @param repo_owner  github username (string)
#' @param repo_name   name of the new the github repository being created
#'   (string).
#' @param repo_descr  a description for the new github repository (string).
#' @param proj_dir    target path to clone repository and initialize RStudio
#'   project (string).
#' @param tmplt_owner username associated with github template repository
#'   (string).
#' @param tmplt_repo  name of github template repository (string).
#'
#' @return list containing github API response object `gh_response` as well as
#'   the local path to the new RStudio project
#' @export
#'
#' @examples
#' \dontrun{
#'   ## create a new repo from a github template repository
#'   gen_repo_from_template(
#'     repo_owner = "ilinsmeier",
#'     repo_name = "new-github-repo-from-template",
#'     repo_descr = "This repository was created from a github template repo.",
#'     proj_dir = "C:/Users/ilinsmeier/projects",
#'     tmplt_owner = "ilinsmeier",
#'     tmplt_repo = "bg-labs-r-project-template"
#'   )
#' }
#'
#' @importFrom gh gh
#' @importFrom usethis create_from_github
#' @importFrom glue glue
#' @importFrom fs path_abs path_dir path_file path_filter
#' @importFrom rlang check_required
gen_repo_from_template <- function(repo_owner,
                                   repo_name,
                                   repo_descr = "",
                                   proj_dir,
                                   tmplt_owner,
                                   tmplt_repo
                                   ) {
  ## verify that required input args have been specified
  rlang::check_required(repo_owner)
  rlang::check_required(repo_name)
  rlang::check_required(proj_dir)
  rlang::check_required(tmplt_owner)
  rlang::check_required(tmplt_repo)

  ## check input path
  error_msg <- paste0("target path `proj_dir = ", proj_dir, "` does not exist!")
  if (!dir.exists(proj_dir)) stop(error_msg, call. = FALSE)
  # proj_dir <- usethis:::user_path_prep(proj_dir)
  # name <- fs::path_file(fs::path_abs(proj_dir))
  # usethis:::challenge_nested_project(fs::path_dir(proj_dir), name)
  # usethis:::challenge_home_directory(proj_dir)

  ## create repo from template using github api
  gh_response <- gh::gh("POST /repos/{template_owner}/{template_repo}/generate",
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
  rproj_path <- usethis::create_from_github(
    repo_spec = glue::glue("https://github.com/{repo_owner}/{repo_name}.git"),
    destdir = proj_dir,
    fork = FALSE,
    rstudio = TRUE,
    open = FALSE,
    protocol = "https"
  )

  ## verify initialization of RStudio project folder
  stopifnot(dir.exists(rproj_path))

  ## rename RStudio project file to match new repository name
  cur_rproj_file <- fs::path_filter(list.files(path = rproj_path, full.names = TRUE), regexp = "(?i)^.*\\.Rproj$", perl = TRUE)
  new_rproj_file <- file.path(dirname(cur_rproj_file), glue::glue("{repo_name}.Rproj"))
  file.rename(cur_rproj_file, new_rproj_file)

  ## open RStudio project in new R session
  usethis::proj_activate(rproj_path)

  ## return github API response object & local path to created RStudio project
  list(
    gh_response = gh_response,
    rproj_path  = rproj_path
  )
}
