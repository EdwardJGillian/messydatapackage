#' Create cs_es_table
#'
#' Task 1 - distribution of the expected net income (se)
#' in relation to the current net income (cs)
#'
#' @param df1 - main df1
#'
#' @return - cs_es_table
#' @export
#'
create_cs_es_table <- function(df1) {


  ############### create matrix for calculation of statistics ############################


  # create table with frequency counts for exp_sal and curr_sal per category of level
  #
  cs_es_table <- df1 %>%
    dplyr::count(sal_level, exp_sal, curr_sal) %>%
    as.data.frame()

  # convert cs_es_table to long format and summarise exp_sal and curr_sal frequencies
  cs_es_table <- cs_es_table %>%
    tidyr::pivot_wider(names_from = c(curr_sal, exp_sal), values_from = n) %>%
    dplyr::select(-ends_with("NA"))

  new_sal_names2 <- c("sal_level",
                      "Current Salary",
                      "Expected Salary")

  # rename columns with old_names and new_names vectors
  cs_es_table <- cs_es_table %>% dplyr::rename_all(~new_sal_names2)


  # recode values in the first column - need to specify library dplyr with recode function
  cs_es_table <- cs_es_table %>% dplyr::mutate(sal_level=dplyr::recode(sal_level, "1" = "< 1000 EUR",
                                                                "2" = "1001-1500 EUR",
                                                                "3" = "2001-3000 EUR",
                                                                "4" = "3001-4000 EUR",
                                                                "5" = "4001-5000 EUR",
                                                                "6" = "5001-6000 EUR",
                                                                "7" = "> 6000 EUR",
                                                                "8" = "no answer"))

  cs_es_table <- cs_es_table %>%
    dplyr::filter(sal_level != "no answer")

  # move column heading to rowname
  cs_es_table <- cs_es_table %>%
    tibble::column_to_rownames(var = "sal_level") %>%
    as.data.frame()

  return(cs_es_table)
}
