library(messydatapackage)
library(testthat)

test_chained_functions <- function(csv_file) {
test_that("check file values for general preprocessing", {
  # naming helper
  tname <- function(n) {
    paste0(home,
      "/data/known_value/",
      csv,
      ".",
      n,
      ".test"
    )
  }

  # create file path to csv file examples
  csv_path <-
    paste0(home, "/data/csv_examples/", csv_file)
  csv <- stringr::str_remove(csv_file, ".csv")
  df1 <- readr::read_csv(file=csv_path, col_names=TRUE, col_types = readr::cols())
  general_pre_proc <- messydatapackage::general_data_preprocess(df1)
  # SuppressWarnings used as expect_known_value is a depreciated function
  suppressWarnings(testthat::expect_known_value(
    general_pre_proc, tname("gen_proc")))
  cs_es_table <- messydatapackage::create_cs_es_table(general_pre_proc)
  suppressWarnings(testthat::expect_known_value(
    cs_es_table, tname("cs_es_table")))

  es_ab_table <- messydatapackage::create_es_ab_table(general_pre_proc)
  suppressWarnings(testthat::expect_known_value(
    es_ab_table, tname("es_ab_table")))

  task3_preprocessing_level1 <- messydatapackage::task3_preprocessing_level1(general_pre_proc)
  suppressWarnings(testthat::expect_known_value(
    task3_preprocessing_level1, tname("task3_level1")))

  task3_preprocessing_level2 <- messydatapackage::task3_preprocessing_level2(general_pre_proc)
  suppressWarnings(testthat::expect_known_value(
    task3_preprocessing_level2, tname("task3_level2")))

  chisq1 <- chisq_function(cs_es_table)
  suppressWarnings(testthat::expect_known_value(
      chisq1, tname("chisq_level1")))

  chisq2 <- chisq_function(es_ab_table)
  suppressWarnings(testthat::expect_known_value(
    chisq2, tname("chisq_level2")))

  ggplot_prep_function <- ggplot_prep_function(cs_es_table)
  suppressWarnings(testthat::expect_known_value(
    ggplot_prep_function, tname("ggplot")))

  y <- "keen_move"
  x <- "current_work + city_size + gender + age + education + hf_com + ab_com + curr_sal + exp_sal"
  dataset <- task3_preprocessing_level2
  glm_function_return <- glm_function(y, x, dataset)
  suppressWarnings(testthat::expect_known_value(
    glm_function_return, tname("glm")))

})
}

# create the ALS file path
home <- setwd(Sys.getenv("HOME"))

csv_file_path <- file.path(home, "data/csv_examples")

csv_files_list <- list.files(path = csv_file_path, pattern = "*.csv$", full.names = FALSE)

# apply 1 list vector to the function
purrr::map(csv_files_list, test_chained_functions)
