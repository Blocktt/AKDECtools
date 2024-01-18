#' Filter WQ data.
#'
#' A simple function to filter specific data from a larger WQ dataset by either AU_ID
#' , constituent, or both
#'
#' @import magrittr
#'
#' @param data A WQ dataset in long format. Must contain either AU_ID or TADA.CharacteristicName
#' @param AU_ID An AK DEC Assessment Unit (AU) Identifier
#' @param constituent TADA.CharacteristicName as generated after running data_processing.R
#'
#' @examples
#' # Examples 1-4, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#'
#' df_ExampSamps <- read_csv(system.file("extdata/AK_Example_Samples.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1 - single AU and single constituent
#'
#' test_pull <- simplePull(data = df_ExampSamps, AU_ID = c('AK_R_1010504_005')
#' , constituent = c('PH'))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2 - multiple AUs and single constituent
#'
#' test_pull <- simplePull(data = df_ExampSamps, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
#' , constituent = c('TEMPERATURE, WATER'))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 3 - single AU and multiple constituents
#'
#' test_pull <- simplePull(data = df_ExampSamps, AU_ID = c('AK_R_1010504_005')
#' , constituent = c('PH', 'TEMPERATURE, WATER'))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 4 - multiple AUs and multiple constituents
#'
#' test_pull <- simplePull(data = df_ExampSamps, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
#' , constituent = c('PH', 'TEMPERATURE, WATER'))
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A filtered dataset.
#' @export
#'
simplePull <- function(data, AU_ID, constituent){ #AU & constituent are optional inputs
  if(missing(AU_ID)) {
    filt <- data %>% dplyr::filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% dplyr::filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% dplyr::filter(AUID_ATTNS %in% AU_ID) %>%
      dplyr::filter(TADA.CharacteristicName %in% constituent)
  }
  return(filt)
}
