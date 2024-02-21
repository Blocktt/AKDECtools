#' Freshwater ammonia warning
#'
#' This function checks if there is sufficient freshwater ammonia data for
#' water quality analyses. Returns message.
#'
#' @details
#' Required fields for input arguments
#'
#' * input_sufficiency:
#'
#' @param input_sufficiency Data sufficiency table generated using the data_processsing.R script
#'
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#' library(AKDECtools)
#'
#' df_Data_Sufficiency <- read_csv(system.file("extdata/AK_Data_Sufficiency.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' freshwaterAmmoniaWarning(df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return Returns message
#' @export
#'
freshwaterAmmoniaWarning <- function(input_sufficiency){

  #Find freshwater ammonia
  ammonia <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName == 'AMMONIA') %>%
    dplyr::filter(`Waterbody Type` == 'Freshwater') %>%
    dplyr::filter(Data_Sufficient == "Yes")

  if(nrow(ammonia) > 0) {
    return(paste0("There is sufficient freshwater ammonia in at least one AU."
                  , " Please add fish data to analysis. "
                  , "Alaska Water Quality Criteria Manual for Toxic and Other"
                  , " Deleterious Organic and Inorganic Substances Appendices"
                  , " C-E (Sept 2022)"))
  } else {
    return(paste0("There is NO sufficient freshwater ammonia."
           , " No changes requried for analysis. "
           , "Continue to MagDurFreq_pH function."))
  }
} # end
