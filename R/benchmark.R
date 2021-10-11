#' Check performance on current system using package bench
#'
#' @return A dataframe of results
#' @importFrom rlang .data .env
#' @export

run_benchmark <- function( ) {
  f_build <- function(x, n_max=2000) {

    definitions <- read_yaml("config/definitions.yml")
    unit_conversions <- make_unit_conversion_functions("config/unit_conversions.csv")
    
    config <- subset_config(sprintf("data/%s/metadata.yml", x), definitions, unit_conversions)
    data <- load_study(sprintf("data/%s/data.csv", x), config)
    
    data
  }
  
  dataset_ids <- c("ANBG_2019", "Baker_2019", "Bloomfield_2018", "Catford_2014", "Cheal_2017", "Maslin_2012", "Tomlinson_2019", "Westoby_2014")

  message("Running benchmarks via `run_benchmarks`")
  bench::press(dataset_id = dataset_ids,
               {
                 bench::mark(
                   check = FALSE,
                   # We're not expecting different results to be equivalent
                   iterations = 1,
                   data = f_build(.env$dataset_id)
                 )
               })
}


