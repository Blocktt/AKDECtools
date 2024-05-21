#' Categorize Assessment Units (AUs)
#'
#' The categorize_AU function assigns each AU/characteristic combination to an
#' IR category and then assigns an overall designated use category to the AU
#' for every applicable use.
#'
#' @param MagDurFreq_Results Final results from MagDurFreq_combine function.
#' @param simplify_standards True/False input to condense final category for constituents
#' with multiple standards per use.
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#' library(AKDECtools)
#'
#' df_MagDurFreq_Results <- read_csv(system.file("extdata/AK_Final_MagDurFreq_Output.csv"
#'                                        , package = "AKDECtools")
#'                                        , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' categorize_AU(df_MagDurFreq_Results)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return Categorized AUs
#' @export
#'
categorize_AU <- function(MagDurFreq_Results, simplify_standards){

  calc_individual <- input_analysis %>%
    filter(Exceed != 'Requires manual analysis') %>%
    filter(Exceed != 'AU not lake waters') %>%
    filter(Exceed != 'Natural conditions less than or equal to 50 NTU') %>%
    dplyr::mutate(Individual_Category = case_when(is.na(Data_Sufficient) ~ NA,
                                                  Data_Sufficient == "No" ~ '3',
                                                  Exceed == 'Yes' ~ '5',
                                                  Exceed == 'No' ~ '2',
                                                  Exceed == 'Insufficient hardness' ~ '3',
                                                  Exceed == 'Insufficient dependent data' ~ '3',
                                                  T ~ NA))

  if(simplify_standards == T){

    mid_step <- calc_individual %>%
      dplyr::group_by(AUID_ATTNS, Use, `Use Description`, TADA.CharacteristicName) %>%
      dplyr::mutate(n = n(),
                    is_2 = sum(ifelse(Individual_Category == '2', 1, 0)),
                    is_3 = sum(ifelse(Individual_Category == '3', 1, 0)),
                    is_5 = sum(ifelse(Individual_Category == '5', 1, 0)),
                    #If n > 1, choose worse category
                    new_Individual_Category = case_when(n > 1 & is_5 == 1 ~
                                                          '5',
                                                        n > 1 & is_5 == 0 & is_2 > 0 ~
                                                          '2',
                                                        T ~ Individual_Category)) %>%
      dplyr::filter(Individual_Category == new_Individual_Category) %>%
      dplyr::select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
      dplyr::rename(Individual_Category = new_Individual_Category)


  } else {
    mid_step <- calc_individual
  }


  calc_overall <- mid_step %>%
    dplyr::group_by(AUID_ATTNS, Use) %>%
    dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
                  cat_2_present = length(Individual_Category[Individual_Category=='2']),
                  Use_Category = case_when(cat_5_present > 0 ~ '5',
                                           cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                           cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                           T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS, Use)

  return(calc_overall)


} # end categorize_AU
