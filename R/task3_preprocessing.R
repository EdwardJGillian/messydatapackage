#' Task 3 preprocessing level 1
#'
#' Preprocessing Data Level 1
#'
#' @param df1 - input data frame
#'
#' @return df_level1
#' @export
#'
task3_preprocessing_level1 <- function(df1) {


  # convert character variables to factors
  df_level1 <-
    df1 %>%
    dplyr::mutate(across(c(current_work,
                    before_work,
                    keen_move,
                    city_size,
                    gender,
                    age,
                    education),
                  factor))

  return(df_level1)
}


#' Preprocessing Data Level 2
#'
#' put in code to change all NAs to zeros in df1 for logistic regression
#'
#' convert all NAs in columns curr_sal, exp_sal, hf_com, ab_com to 0
#' to allow for logistic regression based on factors
#'
#' @param df1 - input data frame
#'
#' @return df_level2
#' @export
#'
task3_preprocessing_level2 <- function(df1) {
  df_level2 <- df1 %>%
    dplyr::mutate_at(dplyr::vars(curr_sal,
                   exp_sal,
                   hf_com,
                   ab_com), ~tidyr::replace_na(., 0))


  # convert character variables to factors
  df_level2 <-
    df_level2 %>%
    dplyr::mutate(across(c(current_work,
                    before_work,
                    keen_move,
                    city_size,
                    gender,
                    age,
                    education,
                    com_level,
                    sal_level),
                  factor))

  # convert number variables to factors
  df_level2 <-
    df_level2 %>%
    dplyr::mutate(across(c(hf_com,
                    ab_com,
                    curr_sal,
                    exp_sal),
                  factor))
  return(df_level2)
}
