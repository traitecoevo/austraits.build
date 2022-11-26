#' Check performance on current system using package bench
#'
#' @return A dataframe of results
#' @export

run_benchmark <- function( ) {
  f_build <- function(x, n_max=2000) {

    schema <- get_schema()
    resource_metadata <- get_schema("config/metadata.yml", "metadata")
    definitions <- get_schema("config/traits.yml", "traits")
    unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
    
    config <- dataset_configure(sprintf("data/%s/metadata.yml", x), definitions, unit_conversions)
    data <- dataset_process(sprintf("data/%s/data.csv", x), config, schema, resource_metadata)
    
    data
  }
  
  dataset_id <- NULL
  dataset_ids <- c("ANBG_2019", "Baker_2019", "Bloomfield_2018", "Catford_2014", "Cheal_2017", "Maslin_2012", "Tomlinson_2019", "Westoby_2014")

  message("Running benchmarks via `run_benchmark`")
  bench::press(dataset_id = dataset_ids,
               {
                 bench::mark(
                   check = FALSE,
                   # We're not expecting different results to be equivalent
                   iterations = 1,
                   data = f_build(dataset_id)
                 )
               })
}


