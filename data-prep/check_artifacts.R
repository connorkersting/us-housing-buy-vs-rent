# check_artifacts.R -- prints the actual schema of every .rds artifact so the
# adapters in app/R/01_data.R can be patched against reality instead of
# guesses. Base R only. Run from the repo root:
#   Rscript data-prep/check_artifacts.R
# then paste the full output back to Claude.
# Generated with AI assistance (Claude); see AI_LOG.md.

art_dir <- "app/data"
files <- list.files(art_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(files) == 0) {
  stop("No .rds files found in ", art_dir,
       ". Run data-prep/build_artifacts.R first, or check the path.")
}

for (f in files) {
  x <- readRDS(f)
  cat("\n========", basename(f), "========\n")
  cat("class:", paste(class(x), collapse = " / "), "\n")
  if (is.data.frame(x)) {
    cat("dim:", nrow(x), "rows x", ncol(x), "cols\n")
    cat("columns:\n")
    for (nm in names(x)) {
      cat(sprintf("  %-28s %s\n", nm, paste(class(x[[nm]]), collapse = "/")))
    }
    cat("head(3):\n")
    print(utils::head(as.data.frame(x), 3))
  } else {
    utils::str(x, max.level = 1)
  }
}
cat("\nDone. Paste everything above back into the chat.\n")
