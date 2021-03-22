
#' general data transformation
#'
#' @param df1 - input dataframe
#'
#' @return df1 - transformed dataframe
#' @export
#'
general_data_preprocess <- function(df1) {
  # set as general option that Strings are not factors
  options(stringsAsFactors = FALSE)

  new_names = c("np_id","np_look_work", "np_current_work",
                "hf_A", "ac_A",
                "hf_B", "ac_B",
                "hf_C", "ac_C",
                "hf_D", "ac_D",
                "hf_E", "ac_E",
                "hf_F", "ac_F",
                "cs_1", "se_1",
                "cs_2", "se_2",
                "cs_3", "se_3",
                "cs_4", "se_4",
                "cs_5", "se_5",
                "cs_6", "se_6",
                "cs_7", "se_7",
                "cs_8", "se_8",
                "np_before_work",
                "np_keen_move",
                "np_city_size",
                "np_gender",
                "np_age",
                "np_education")

  # rename columns with old_names and new_names vectors
  df1 <- df1 %>% dplyr::rename_all(~new_names)

  # filter rows with yes for Column look_job
  # code needs dplyr::filter to ensure base filter not used
  df1 <- dplyr::filter(df1, np_look_work == "yes")

  # match all strings ending with job and replace with 1
  # .*
  df1 <- df1 %>%
    dplyr::mutate_all(~stringr::str_replace_all(., '.*job$', '1'))

  # reorder columns
  df1 <- df1 %>% dplyr::select(np_id, ends_with('work'), np_before_work:np_education, starts_with('hf_'),
                        starts_with('ac_'), starts_with('cs_'), starts_with('se_'))

  # change the blanks to NA
  df1 %>% dplyr::na_if("")

  # pivot_longer  - transforms the columns in wide format starting with 'hf' and 'ac' to long format in separate columns
  # names_to parameters:
  # .value = contains metadata on the cell values that correspond to the original columns
  # these values are pivoted in long format and added in a new columns "hf" and "ac"
  # column "group" has the original column endings (e.g. the numbers 1-6) pivoted to long format
  # names_pattern = regex argument specifying character "_" where column names are to be broken up

  # create key column com_level for 6 levels of commuting and move values to long format
  df1 <- df1 %>%
    tidyr::pivot_longer(cols = c(starts_with("hf"), starts_with("ac")),
                        names_to = c(".value", "com_level"),
                        names_pattern = "(.*)_(.*)"
    )

  # create key column sal_level for 8 levels of salary and move values to long format
  df1 <- df1 %>%
    tidyr::pivot_longer(cols = c(starts_with("cs"), starts_with("se")),
                        names_to = c(".value", "sal_level"),
                        names_pattern = "(.*)_(.*)"
                        # values_drop_na = TRUE
    )


  # omit only those rows where columns hf, ac  are all NA
  df1 <- df1[!(is.na(df1$hf)) | !(is.na(df1$ac)),]
  # omit only those rows where columns cs, se are all NA
  df1 <- df1[!(is.na(df1$cs)) | !(is.na(df1$se)),]

  # set up character vectors for old and new column names
  col.from <- c("hf", "ac", "cs", "se")
  col.to <- c("hf_com", "ab_com", "curr_sal", "exp_sal")

  df1 <- df1 %>% dplyr::rename_with(~ col.to[which(col.from == .x)], .cols = col.from)

  # remove all np from the start of column names
  df1 <- df1 %>%
    dplyr::rename_all(~stringr::str_replace(.,"^np_",""))

  return(df1)
}
