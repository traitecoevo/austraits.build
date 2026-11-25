# Run traits.build::dataset_test() on many datasets at once, one per core.
#
#   Rscript .github/scripts/dataset_test_parallel.R              # every dataset
#   Rscript .github/scripts/dataset_test_parallel.R Wright_2002  # just these
#
# Run it from the repository root. It prints the failures of each dataset that
# has any, then a one-line summary, and exits non-zero if any dataset has a
# failure or an error, so CI turns red. Calling `dataset_test(dir("data"))`
# directly does not do that: it reports failures and still exits 0.
#
# Set TEST_CORES to override the number of cores (default: all of them).
# Datasets run in forked processes, so this works on Linux and macOS only.

suppressPackageStartupMessages(library(traits.build))

if (file.exists("R/custom_R_code.R")) source("R/custom_R_code.R")

ids <- commandArgs(trailingOnly = TRUE)
if (length(ids) == 0) ids <- dir("data")

# A pull request that deletes a dataset lists it as changed; there is nothing
# left to test
gone <- ids[!dir.exists(file.path("data", ids))]
if (length(gone) > 0) {
  message("Not in data/, skipped: ", paste(gone, collapse = ", "))
}
ids <- setdiff(ids, gone)
if (length(ids) == 0) {
  message("No datasets to test.")
  quit(status = 0)
}

# Start the biggest datasets first. Test time grows with size, and AVH_2026
# alone takes over two minutes: started last, it would leave the other cores
# idle while it runs.
size <- vapply(ids, function(id) {
  sum(file.size(list.files(file.path("data", id), recursive = TRUE, full.names = TRUE)))
}, numeric(1))
ids <- ids[order(-size)]

cores <- as.integer(Sys.getenv("TEST_CORES", parallel::detectCores()))
message(sprintf("Testing %d datasets on %d cores", length(ids), cores))

test_one <- function(id) {
  report <- tempfile(fileext = ".txt")
  counts <- testthat::ListReporter$new()
  # Writes each failure's message, and no progress lines: a progress reporter
  # prints one line per expectation, over 130,000 lines for a full run
  summary <- testthat::SummaryReporter$new(
    show_praise = FALSE, omit_dots = TRUE, max_reports = Inf, file = report
  )
  secs <- system.time(
    dataset_test(id, reporter = testthat::MultiReporter$new(list(counts, summary)))
  )[["elapsed"]]
  res <- as.data.frame(counts$get_results())
  list(
    id = id,
    expectations = sum(res$nb),
    failed = sum(res$failed),
    error = any(res$error),
    warning = sum(res$warning),
    secs = secs,
    report = readLines(report)
  )
}

start <- Sys.time()
results <- parallel::mclapply(
  ids,
  function(id) {
    tryCatch(test_one(id), error = function(e) {
      list(id = id, expectations = 0, failed = 0, error = TRUE, warning = 0,
           secs = NA_real_, report = paste("dataset_test() itself failed:", conditionMessage(e)))
    })
  },
  mc.cores = cores,
  mc.preschedule = FALSE
)
elapsed <- as.numeric(Sys.time() - start, units = "secs")

# A worker killed outright (out of memory, say) returns a try-error, not a list
results <- Map(function(r, id) {
  if (is.list(r)) return(r)
  list(id = id, expectations = 0, failed = 0, error = TRUE, warning = 0,
       secs = NA_real_, report = paste("The test process died:", as.character(r)))
}, results, ids)

tab <- data.frame(
  id = vapply(results, `[[`, "", "id"),
  expectations = vapply(results, `[[`, 0, "expectations"),
  failed = vapply(results, `[[`, 0, "failed"),
  error = vapply(results, `[[`, FALSE, "error"),
  warning = vapply(results, `[[`, 0, "warning"),
  secs = vapply(results, `[[`, 0, "secs")
)

bad <- tab$failed > 0 | tab$error | tab$warning > 0
in_ci <- nzchar(Sys.getenv("GITHUB_ACTIONS"))
for (i in order(tab$id)[bad[order(tab$id)]]) {
  r <- results[[i]]
  cat(sprintf("\n── %s: %d failed, %d warnings%s ──\n",
              r$id, r$failed, r$warning, if (r$error) ", stopped by an error" else ""))
  # Drop the reporter's per-context header lines ("dataset_test: 12")
  cat(grep("^dataset_test: ", r$report, value = TRUE, invert = TRUE), sep = "\n")
  if (in_ci && (r$failed > 0 || r$error)) {
    cat(sprintf("::error title=dataset_test::%s has %d failures%s\n",
                r$id, r$failed, if (r$error) " and stopped with an error" else ""))
  }
}

cat("\nSlowest datasets:\n")
slow <- head(tab[order(-tab$secs), c("id", "secs", "expectations")], 5)
print(slow, row.names = FALSE)

n_fail <- sum(tab$failed)
cat(sprintf(
  "\n[ FAIL %d | WARN %d | PASS %d ] %d datasets in %.0f s; %d with failures or errors\n",
  n_fail, sum(tab$warning), sum(tab$expectations) - n_fail - sum(tab$warning),
  nrow(tab), elapsed, sum(tab$failed > 0 | tab$error)
))

if (any(tab$failed > 0 | tab$error)) quit(status = 1)
