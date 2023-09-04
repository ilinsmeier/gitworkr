test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("error if missing required input args for `gen_repo_from_template()`", {

  ## missing `repo_owner`
  expect_error(gen_repo_from_template(
    repo_name = "new-github-repo-from-template",
    proj_dir = "C:/Users/ilinsmeier/projects",
    tmplt_owner = "ilinsmeier",
    tmplt_repo = "bg-labs-r-project-template"))

  ## missing `repo_name`
  expect_error(gen_repo_from_template(
    repo_owner = "ilinsmeier",
    proj_dir = "C:/Users/ilinsmeier/projects",
    tmplt_owner = "ilinsmeier",
    tmplt_repo = "bg-labs-r-project-template"))

  ## missing `proj_dir`
  expect_error(gen_repo_from_template(
    repo_owner = "ilinsmeier",
    repo_name = "new-github-repo-from-template",
    tmplt_owner = "ilinsmeier",
    tmplt_repo = "bg-labs-r-project-template"))

  ## missing `tmplt_owner`
  expect_error(gen_repo_from_template(
    repo_owner = "ilinsmeier",
    repo_name = "new-github-repo-from-template",
    proj_dir = "C:/Users/ilinsmeier/projects",
    tmplt_repo = "bg-labs-r-project-template"))

  ## missing `tmplt_repo`
  expect_error(gen_repo_from_template(
    repo_owner = "ilinsmeier",
    repo_name = "new-github-repo-from-template",
    proj_dir = "C:/Users/ilinsmeier/projects",
    tmplt_owner = "ilinsmeier"))
})

test_that("invalid input args produce error for `gen_repo_from_template()`", {

  ## invalid `proj_dir`: path does not exist
  expect_error(gen_repo_from_template(
    repo_owner = "ilinsmeier",
    repo_name = "new-github-repo-from-template",
    repo_descr = "This repository was created from a github template repo.",
    proj_dir = "C:/Users/ilinsmeier/projects",
    tmplt_owner = "ilinsmeier",
    tmplt_repo = "bg-labs-r-project-template"
  ))
})
