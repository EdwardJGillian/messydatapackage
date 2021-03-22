#' Create es_ab_table
#'
#' Task 2 - correlations desire to earn more (se)
#' between possibility to commute longer (ab)
#'
#' @param df1 - dataset for processing
#'
#' @return es_ab_table
#' @export
#'
create_es_ab_table <- function(df1) {
  ############### create matrix for calculation of statistics ############################

  # select the columns to check for correlations
  # this counts all the exp_sal by sal_level and all the ab_com by com_level
  # this is similar to the result in task 1
  es_ab_table <- df1 %>%
    dplyr::count(sal_level, exp_sal, com_level, ab_com) %>%
    as.data.frame()

  ######## pivot salary columns wider - remove ab_com and NA
  es_ab_table <- es_ab_table %>%
    tidyr::pivot_wider(names_from = c(sal_level, exp_sal), values_from = n) %>%
    dplyr::select(-ab_com, -ends_with("NA"))

  #  need to combine and summarise rows
  es_ab_table <- es_ab_table %>%
    dplyr::group_by(com_level) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup()

  new_sal_names3 <- c("com_level",
                      "< 1000 EUR",
                      "1001-1500 EUR",
                      "2001-3000 EUR",
                      "3001-4000 EUR",
                      "4001-5000 EUR",
                      "5001-6000 EUR",
                      "> 6000 EUR",
                      "no answer")


  # rename columns with old_names and new_names vectors
  es_ab_table <- es_ab_table %>% dplyr::rename_all(~new_sal_names3)

  # recode values in the first column - need to specify library dplyr with recode function

  es_ab_table <- es_ab_table %>% dplyr::mutate(com_level=dplyr::recode(com_level, "A" = "< 5 km",
                                                                "B" = "5 - 10 km",
                                                                "C" = "11 - 20 km",
                                                                "D" = "21 - 50 km",
                                                                "E" = "51 - 100 km",
                                                                "F" = "> 100 km"))

  # replace NA values with zeros and remove "no answer column"
  # es_ab_table_1 <- es_ab_table %>% dplyr::mutate_all(dplyr::funs(tidyr::replace_na(., 0))) %>%
  #  dplyr::select(-c("no answer"))

  es_ab_table <- es_ab_table %>% replace(is.na(.), 0) %>%
    dplyr::select(-c("no answer"))

  # move column heading to rowname
  es_ab_table <- es_ab_table %>%
    tibble::column_to_rownames(var = "com_level") %>%
    as.data.frame()
  return(es_ab_table)
}
