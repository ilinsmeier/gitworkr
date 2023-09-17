PR_branch_name <- "test-PR-branch"
review_file_comment <- "## file for review"
PR_review_files <- c(
  "README.md",
  ".github/workflows/deploy_docs.yaml"
)
remote_repo <- gert::git_remote_list() |> print()

## Create a branch
gert::git_branch_create("test-feature", checkout = TRUE)
initial_branch <- gert::git_branch() |> print()
gert::git_branch_create(PR_branch_name, checkout = TRUE)
stopifnot(identical(gert::git_branch(), PR_branch_name))


PR_review_files_txt <- purrr::map(PR_review_files, readLines)
PR_review_files_update <- purrr::map(PR_review_files_txt, ~ c(review_file_comment, "", .x))
purrr::walk2(PR_review_files_update, PR_review_files, ~ writeLines(.x, .y))

gert::git_commit_all(message = "setup review file branch")

gert::git_push(remote_repo$name)

gert::git_status() |> print()

usethis::pr_init(PR_branch_name)
usethis::pr_push()



