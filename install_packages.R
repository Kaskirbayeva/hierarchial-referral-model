############################################################
# install_packages.R - install everything the pipeline needs
############################################################

options(repos = c(CRAN = "https://cloud.r-project.org"))

cran <- c("tidyverse", "lubridate", "ranger", "pROC",
          "patchwork", "scales", "sf", "shiny", "remotes")
missing <- cran[!cran %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

# Kazakhstan administrative boundaries (maps, step 13)
if (!requireNamespace("geokz", quietly = TRUE)) remotes::install_github("arodionoff/geokz")

# CatBoost (not on CRAN): binary from the GitHub releases
if (!requireNamespace("catboost", quietly = TRUE)) {
  cb_version <- "1.2.8"
  os <- switch(Sys.info()[["sysname"]], Windows = "windows-x86_64", Darwin = "darwin-universal2", "linux-x86_64")
  remotes::install_url(
    sprintf("https://github.com/catboost/catboost/releases/download/v%s/catboost-R-%s-%s.tgz", cb_version, os, cb_version),
    INSTALL_opts = c("--no-multiarch", "--no-test-load")
  )
}

cat("Installed:", paste(c(cran, "geokz", "catboost")[sapply(c(cran, "geokz", "catboost"), requireNamespace, quietly = TRUE)], collapse = ", "), "\n")
