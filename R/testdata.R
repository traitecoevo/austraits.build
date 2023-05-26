#' Test whether specified dataset_id has the correct setup
#'
#' Run tests to ensure that specified dataset_id has the correct setup
#'
#' @param dataset_ids vector of dataset_id for sources to be tested
#' @param path_config path to folder containing configuration files
#' @param path_data path to folder containing data files
#' @param reporter testthat reporter to use to summarise output
#'
#' @importFrom rlang .data .env
#' @export

dataset_test <-
  function(dataset_ids,
           path_config = "config",
           path_data = "data",
           reporter = testthat::default_reporter()) {
    
    requireNamespace("testthat", quietly = TRUE)
    
    # clean up when done
    Sys.setenv('TESTTHAT_MAX_FAILS' = Inf)
    
    testthat::with_reporter(
      reporter,
      dataset_test_worker(
        test_dataset_ids = dataset_ids,
        path_config = path_config,
        path_data = path_data
      ),
      start_end_reporter = TRUE
    )
  }


#' Test whether specified dataset_id has the correct setup
#'
#' Run tests to ensure that specified dataset_id has the correct setup
#'
#' @param test_dataset_ids vector of dataset_id for sources to be tested
#' @inheritParams dataset_test
#' @param schema data schema
#' @param definitions trait defininitons
#' @importFrom testthat local_edition compare expect expect_true expect_named test_that context expect_silent expect_type
#' @importFrom rlang .data
#' @importFrom stats na.omit
dataset_test_worker <-
  function(test_dataset_ids,
           path_config = "config",
           path_data = "data",
           schema = get_schema(),
           definitions =
             get_schema(file.path(path_config, "traits.yml"), I("traits"))
           ) {
    
    # We're using 2nd edition of test that, which has "context" field
    # https://cran.r-project.org/web/packages/testthat/vignettes/third-edition.html
    local_edition(2)
    
    ## New expect_that helper functions; test that a number is in a range,
    ## or that a range contains a number.
    
    expect_isin <-
      function(object,
               expected,
               ...,
               info = NULL,
               label = NULL,
               expected.label = NULL,
               na.rm = TRUE) {
        if (na.rm)
          object <- object[!is.na(object)]
        i <- object %in% expected
        
        comp <- compare(all(i), TRUE, ...)
        expect(comp$equal,
               sprintf(
                 "%s - should not contain: %s",
                 info,
                 paste(object[!i], collapse = ", ")
               ))
        
        invisible(object)
      }
    
    # Expectation: one set contains the other
    expect_contains <- function(object, expected, ..., info = NULL) {
      i <- expected %in% object
      
      comp <- compare(all(i), TRUE, ...)
      expect(comp$equal,
             sprintf(
               "%s - does not contain: %s",
               info,
               paste(expected[!i], collapse = ", ")
             ))
      
      invisible(object)
    }
    
    # Expectation: one set contains the other
    expect_allowed <- function(object, allowed, ..., info = NULL) {
      i <- object %in% allowed
      
      comp <- compare(all(i), TRUE, ...)
      expect(comp$equal,
             sprintf(
               "%s - includes invalid terms: %s",
               info,
               paste(object[!i], collapse = ", ")
             ))
      
      invisible(object)
    }
    
    expect_not_NA <- function (object,
                               info = NULL,
                               label = NULL) {
      i <- !is.na(object)
      comp <- compare(all(i), TRUE)
      expect(comp$equal,
             sprintf("%s - contains NAs: %s", info, label))
      invisible(object)
    }
    
    expect_length_zero <- function (object,
                                    info = NULL,
                                    label = NULL) {
      comp <- compare(length(object), 0)
      expect(comp$equal,
             sprintf("%s: %s", info, label))
      invisible(object)
    }
    
    expect_unique <- function (object,
                               info = NULL,
                               label = NULL) {
      x <- table(unlist(object))
      i <- x == 1
      comp <- compare(all(i), TRUE)
      expect(comp$equal,
             sprintf("%s - not unique: %s", info, paste(names(x)[!i], collapse = ", ")))
      invisible(object)
    }
    
    expect_allowed_text <- function(object,
                                    info = NULL,
                                    label = NULL) {
      if (length(object) > 0) {
        disallowed <-
          object %>% lapply(check_disallowed_chars) %>% simplify2array()
        
        check <- disallowed %>% lapply(any) %>% unlist()
        
        txt <- "\n"
        for (i in which(check)) {
          txt <- sprintf("%s\t- ln %s: %s\n",
                         txt,
                         i,
                         colour_characters(object[[i]], which(disallowed[[i]])))
        }
                
        expect(
          identical(as.vector(all(!check)), TRUE),
          sprintf("%s -- disallowed characters detected: %s", info, txt)
        )
      }
      invisible(object)
    }
    
    colour_characters <- function(x, i = NULL) {
      chars <- x %>% charToRaw() %>% lapply(rawToChar) %>% unlist()
      
      # Wrapper around characters to print as colour
      # obtained from crayon::red(x)
      if (!is.null(i))
        chars[i] <- sprintf("\033[31m%s\033[39m", chars[i])
      
      paste0(chars, collapse = "")
    }
    
    check_disallowed_chars <- function(x) {
      i <- charToRaw(x)
      # allow all ascii text
      is_ascii <- i < 0x7F
      
      # allow some utf8 characters, those with accents over letters for foreign names
      # list of codes is here: http://www.utf8-chartable.de/
      # note c3 is needed because this is prefix for allowed UTF8 chars
      exceptions <- c("ÁÅÀÂÄÆÃĀâíåæäãàáíÇčóöøéèłńl°êÜüùúû±µµ“”‘’-–—≈˜×")
      
      is_allowed <- i %in% charToRaw(exceptions)
      ! (is_ascii | is_allowed)
    }
    
    # Better than expect_silent as contains `info` and allows for complete failures
    expect_no_error <-
      function (object,
                regexp = NULL,
                ...,
                info = NULL,
                label = NULL)
      {
        error <- tryCatch({
          object
          NULL
        }, error = function(e) {
          e
        })
        expect(is.null(error),
               sprintf(
                 "%s threw an error: %s",
                 label,
                 paste(error$message, collapse = ",")
               ),
               info = info)
        invisible(NULL)
      }
    
    expect_list_elements_contains_names <- function(object, expected, info) {
      for (i in seq_along(object))
        expect_contains(names(object[[i]]), expected, info = paste(info, i))
      
      invisible(NULL)
    }
    
    expect_list_elements_allowed_names <- function(object, allowed, info) {
      for (i in seq_along(object))
        expect_allowed(names(object[[i]]), allowed,  info = paste(info, i))
      
      invisible(NULL)
    }
    
    test_dataframe_valid <- function(data, info) {
      expect_not_NA(colnames(data), info = info)
      expect_allowed_text(colnames(data), info = info)
      expect_unique(colnames(data), info = info)
      expect_true(is.data.frame(data), info = info)
    }
    
    test_dataframe_named <- function(data, expected_colnames, info) {
      test_dataframe_valid(data, info)
      expect_named(data, expected_colnames, info = info)
    }
    
    test_dataframe_names_contain <-
      function(data, expected_colnames, info) {
        test_dataframe_valid(data, info)
        expect_contains(names(data), expected_colnames, info = info)
      }
    
    test_list <- function(data, info) {
      expect_true(class(data) == "list", info = info)
    }
    
    test_list_names_valid <- function(data, info) {
      test_list(data, info)
      expect_not_NA(names(data), info = info)
      expect_allowed_text(names(data), info = info)
      expect_unique(names(data), info = info)
    }
    
    test_list_named_exact <- function(data, expected_names, info) {
      test_list_names_valid(data, info)
      expect_named(data, expected_names, info = info)
    }
    
    test_list_named_allowed <- function(data, allowed_names, info) {
      test_list_names_valid(data, info)
      expect_named(data)
      expect_allowed(names(data), allowed_names, info = info)
    }
    
    test_list_named_contains <- function(data, expected_names, info) {
      test_list_names_valid(data, info)
      expect_named(data)
      expect_contains(names(data), expected_names, info = info)
    }
    
    # Now run tests for each dataset
    
    for (dataset_id in test_dataset_ids) {
      s <- file.path(path_data, dataset_id)
      
      test_that(dataset_id, {
        context(sprintf("%s", dataset_id))
        
        # Exists
        files <- file.path(s, c("data.csv", "metadata.yml"))
        for (f in files) {
          expect_true(file.exists(f), info = f)
        }
        
        # check for other files
        vals <- c("data.csv", "metadata.yml", "raw")
        expect_isin(dir(s), vals, info = paste(f, " disallowed files"))
        
        
        # data.csv
        f <- files[1]
        expect_silent(data <-
                        read_csv(
                          f,
                          col_types = cols(),
                          guess_max = 1e5,
                          progress = FALSE
                        ))
        # check no issues flagged when parsing file
        expect_no_error(
          readr::stop_for_problems(data),
          info = sprintf(
            "problems present when reading data, run `read_csv(%s)` to investigate",
            f
          )
        )
        
        test_dataframe_valid(data, info = f)
      
        # Metadata
        f <- files[2]
        expect_allowed_text(readLines(f), info = f)
        expect_silent(metadata <- yaml::read_yaml(f))
        test_list_named_exact(metadata,
                              schema$metadata$elements %>% names(),
                              info = f)
        
        # custom R code
        txt <- metadata[["dataset"]][["custom_R_code"]]
        #expect_false(grepl("#", txt), label=paste0(files[3], "-custom_R_code cannot contain comments, except on last line"))
        expect_no_error(process_custom_code(txt)(data),
                        label = paste0(files[3], "-custom_R_code"))
        
        # Apply custom manipulations
        data <- process_custom_code(txt)(data)
        
        # source
        test_list(metadata[["source"]], info = f)
        test_list_names_valid(metadata[["source"]], info = f)
        
        v <- names(metadata[["source"]])
        i <- grepl("primary", v) | grepl("secondary", v) | grepl("original", v)
        
        expect_contains(v, "primary", info = f)
        
        expect_true(
          sum(grepl("primary", v)) <= 1,
          info = paste(
            f,
            "sources can have max 1 type labelled 'primary': ",
            paste(v, collapse = ", ")
          )
        )
        
        expect_true(all(i),
                    info = paste(
                      f,
                      "sources must be either primary or secondary:",
                      paste(v[!i], collapse = ", ")
                    ))
        
        vals <- c("key", "bibtype", "author", "title", "year")
        
        for (bib in names(metadata[["source"]])) {
          expect_contains(names(metadata[["source"]][[bib]]), vals, info = f)
        }
        
        keys <- unlist(lapply(metadata[["source"]], "[[", "key"))
        
        expect_unique(keys, info = paste(
          f,
          "sources must have unique keys:",
          paste(keys, collapse = ", ")
        ))
        
        # people
        test_list(metadata[["contributors"]], info = f)
        
        test_list_named_allowed(metadata[["contributors"]],
          schema$metadata$elements$contributors$elements %>% names(),
          info = f
        )

        # data_collectors
        if(!is.na(metadata[["contributors"]][["data_collectors"]][1])){
          test_list(metadata[["contributors"]][["data_collectors"]], info=f)
        
          vars <- schema$metadata$elements$contributors$elements$data_collectors$elements %>% names()
          for(i in seq_along(metadata[["contributors"]][["data_collectors"]])){
            test_list_named_allowed(
              metadata[["contributors"]][["data_collectors"]][[i]], 
              vars, info = paste(f, "data_collector", i)
            )
            expect_contains(metadata[["contributors"]][["data_collectors"]][[i]] %>% names(), vars[1:4])
          }
        }

        # austraits_curators
        expect_true(!is.null(metadata[["contributors"]][["austraits_curators"]]))
        expect_type(metadata[["contributors"]][["austraits_curators"]], "character")
  
        # assistants
        if(!is.null(metadata[["contributors"]][["assistants"]][1]))
          expect_type(metadata[["contributors"]][["assistants"]], "character")

        # dataset

        test_list_named_allowed(metadata[["dataset"]],
                                schema$metadata$elements$dataset$values %>% names(),
                                info = paste0(f,"-dataset"))
        
        expect_type(metadata[["dataset"]][["data_is_long_format"]], "logical")
        expect_type(metadata[["dataset"]], "list")
        
        # locations
        if (length(unlist(metadata[["locations"]])) > 1) {
          test_list(metadata[["locations"]], info = f)
          
          expect_silent(
            locations <-
              metadata$locations %>%
              process_format_locations(dataset_id, schema) %>%
              process_add_all_columns(names(schema[["austraits"]][["elements"]][["locations"]][["elements"]]))
          )
          
          test_dataframe_names_contain(
            locations,
            c("dataset_id", "location_name", "location_property", "value"),
            info = paste0(f, " - locations")
          )
          
          for (v in names(metadata$locations)) {
            test_list(metadata[["locations"]][[v]], info = f)
            expect_contains(
              names(metadata[["locations"]][[v]]),
              c("latitude (deg)", "longitude (deg)"),
              info = paste0(f, " - site: ", v)
            )
          }
        }
        
        # contexts
        expect_silent(
          contexts <-
            metadata$contexts %>%
            process_format_contexts(dataset_id)
        )

        ## check context details load
        if(nrow(contexts > 0)) {
          
          test_dataframe_names_contain(
            contexts,
            schema$metadata$elements$contexts$elements %>% names(),
            info = paste0(f, "-contexts")
          )
        }
        
        # Traits        
        expect_list_elements_contains_names(metadata[["traits"]],
                                    schema$metadata$elements$traits$elements[1:3] %>% names(),
                                    info = paste0(f, "-traits"))
        expect_list_elements_allowed_names(metadata[["traits"]],
                                    c(schema$metadata$elements$traits$elements %>% names(), unique(contexts$var_in)),
                                    info = paste0(f, "-traits"))
        expect_silent(
          traits <- austraits.build::util_list_to_df2(metadata[["traits"]])
          )
        expect_true(is.data.frame(traits))
        
        expect_isin(traits$trait_name,
                    definitions$elements %>% names(),
                    info = paste0(f, "-traits"))
        
        # Now that traits loaded, check details of context match
        if (nrow(contexts > 0)) {

          # Check they are in context dataset
          expect_contains(
            c(names(data), names(traits)),
            unique(contexts$var_in),
            info = files[2]
          )


          for (j in unique(contexts[["var_in"]])) {
            contextsub <- 
              contexts %>% filter(var_in == j)
            
            unique2 <- function(x) {unique(x[!is.na(x)])}
            # Context values align either with a column of data or a column of traits table
            if(is.null(data[[j]])) {
              v <- traits[[j]] %>% unique2()
            } else {
              v <- data[[j]] %>% unique2()
            }

            if (all(!is.na(contextsub[["find"]]))) {
              i <- v %in% contextsub[["find"]]
            } else {
              i <- v %in% contextsub[["value"]]
            }

            expect_true(all(i),
              info = paste0(
                f,
                "- context names from data file not present in metadata contexts: ",
                v[!i]
              )
            )
          }
        }

        # Check value types in metadata and any columns of data
        
        # XXXX To do -- also check for entity type, basis of value and any other columns
        
        i <- (traits$value_type %in% names(data))
        
        value_type_fixed <- traits$value_type[!i] %>% unique()
        value_type_cols <- traits$value_type[i] %>% unique()

        expect_isin(
          value_type_fixed,
          schema$value_type$values %>% names,
          info = paste0(f, "-value types")
        )
        
        if(length(value_type_cols) > 0){
          for(v in value_type_cols)
            expect_isin(
              data[[v]] %>% unique(),
              schema$value_type$values %>% names,
              info = paste(f, v, "- value types columns")
            )
        }
        
        # Substitutions
        if (!is.na(metadata[["substitutions"]][1])) {
          expect_list_elements_contains_names(
            metadata[["substitutions"]],
            schema$metadata$elements$substitutions$values %>% names(),
            "metadata - substitution #"
          )
          trait_names <-
            sapply(metadata[["substitutions"]], "[[", "trait_name")
          expect_isin(unique(trait_names),
                      definitions$elements %>% names(),
                      info = paste0(f, "-substitutions-trait_name"))
          expect_isin(
            unique(trait_names),
            unique(traits$trait_name),
            info = paste0(f, "-substitutions-trait_name")
          )
          
          # check for allowable values of categorical variables
          expect_no_error(x <-
                            metadata[["substitutions"]] %>% util_list_to_df2() %>% split(.$trait_name))
          
          for (trait in names(x)) {
            if (!is.null(definitions$elements[[trait]]) &&
                definitions$elements[[trait]]$type == "categorical") {
              to_check <- x[[trait]]$replace %>% unique()
              allowable <-
                c(definitions$elements[[trait]]$allowed_values_levels %>% names(),
                  NA)
              failing <- to_check[!(
                is.na(to_check) |
                  to_check %in% allowable |
                  to_check %>% sapply(util_check_all_values_in, allowable)
              )]
              expect_length_zero(
                failing,
                info = sprintf(
                  "%s - substitutions for `%s` have invalid replacement values",
                  f,
                  trait
                ),
                label = failing %>% paste(collapse = ", ")
              )
            }
          }
        }
        
        ## Check config files contain all relevant columns
        if (metadata[["dataset"]][["data_is_long_format"]]) {
          # Variable match
          #expect_isin(names(metadata[["dataset"]]), c("taxon_name",  "trait_name", "value","location_name", "individual_id", "context_name", "collection_date"), info=paste0(f, " - variable_match"))
          
          # For vertical datasets, expect all values of "trait column" found in traits
          var_out <- names(metadata[["dataset"]])
          var_in <- unlist(metadata[["dataset"]])
          i <- match("trait_name", var_out)
          values <- unique(data[[var_in[i]]])
          expect_contains(traits[["var_in"]], values, info = files[2])
        } else {
          # Variable match
          #expect_isin(names(metadata[["dataset"]]), c("taxon_name", "location_name", "individual_id", "context_name", "collection_date"), info=paste0(f, " - variable_match"))
          
          # For wide datasets, expect variables in traits are header in the data
          values <- names(data)
          expect_isin(traits[["var_in"]], values, info = files[2])
        }
        
        
        ## TODO
        
        ## Make sure specified columns exist
        
        
        ## TODO
        
        ## For numeric trait data, check it looks reasonable & converts properly
        
        ## check location_names are in locations dataset
        
        if (length(unlist(metadata[["locations"]])) > 1) {
          expect_true(
            !is.null(metadata[["dataset"]][["location_name"]]),
            info = paste0(files[2], " - variable_match -> location_name is missing")
          )
          
          expect_contains(
            names(data),
            metadata[["dataset"]][["location_name"]],
            info = paste0(files[2], " - column ", metadata[["dataset"]][["location_name"]], "not found in data")
          )
          
          v <-
            (data[[metadata[["dataset"]][["location_name"]]]] %>% unique %>% na.omit)
          i <- v %in% names(metadata$locations)
          expect_true(all(i),
                      info = paste0(f,  "- site names from data file not present in metadata: ", v[!i]))
          
          i <- names(metadata$locations) %in% v
          expect_true(all(i),
                      info = paste0(
                        f ,
                        "-site names from metadata not present in data file: ",
                        names(metadata$locations)[!i]
                      ))
        }
        
    
      })
    }

    # keep this
    context("end")
  }
