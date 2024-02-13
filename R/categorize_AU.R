#' Categorize Assessment Units (AUs)
#'
#' The categorize_AU function assigns each AU/characteristic combination to an
#' IR category and then assigns an overall category to the AU.
#'
#' @param MagDurFreq_Results Final results from MagDurFreq_combine function.
#'
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
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
categorize_AU <- function(MagDurFreq_Results){

  calc_individual <- MagDurFreq_Results %>%
    dplyr::mutate(Individual_Category = case_when(Data_Sufficient == "(?i)No" ~ '3',
                                           Exceed == 'Yes' ~ '5',
                                           Exceed == 'No' ~ '2',
                                           Exceed == 'Insufficient hardness' ~ '3',
                                           Exceed == 'Insufficient dependent data' ~ '3',
                                           T ~ NA))

  calc_overall <- calc_individual %>%
    dplyr::group_by(AUID_ATTNS, Use) %>%
    dplyr:: mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
           cat_2_present = length(Individual_Category[Individual_Category=='2']),
           Overall_Category = case_when(cat_5_present > 0 ~ '5',
                                        cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                        cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                        T~NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS, Use)

  return(calc_overall)

} # end categorize_AU
