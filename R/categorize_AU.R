#' Categorize Assessment Units (AUs) by Use
#'
#' The categorize_AU function assigns each AU to an IR category.
#'
#' @param input_categorized_uses Final results from categorize_AU_uses function.
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
#' uses_categorized <- categorize_AU_uses(df_MagDurFreq_Results, simplify_standards = F)
#'
#' categorize_AU(uses_categorized)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return Categorized AUs for each use
#' @export
#'


categorize_AU <- function(input_categorized_uses){

  calc_overall <- input_categorized_uses %>%
    dplyr::group_by(AUID_ATTNS) %>%
    dplyr::mutate(cat_5_present = length(Use_Category[Use_Category=='5']),
                  cat_2_present = length(Use_Category[Use_Category=='2']),
                  Overall_Category = dplyr::case_when(cat_5_present > 0 ~ '5',
                                               cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                               cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                               T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS)

  return(calc_overall)

}
