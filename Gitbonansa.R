library(usethis)

library(usethis)
use_git_config(
  user.name = "TageVestlie", 
  user.email = "tve008@gmail.com"
)
use_git()
use_github()
create_from_github("TageVestlie/302")
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
create_github_token()
gitcreds::gitcreds_set()
