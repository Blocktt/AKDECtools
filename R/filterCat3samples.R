#' Filter out IR Category 3 samples
#'
#' The filterCat3samples function filters a data frame of water quality samples
#' based on the data sufficiency output produced by data_processing.R. Only
#' AU/characteristic combinations with Data_Sufficient == 'Yes'are kept.
#'
#' @param data_samples Water quality data in long format. Output from data_processing.R.
#' @param data_sufficiency Data sufficiency by AU/characteristic. Output from data_processing.R.
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#'
#' df_ExampSamps <- read_csv(system.file("extdata/AK_Example_Samples.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' df_Data_Sufficiency <- read_csv(system.file("extdata/AK_Data_Sufficiency.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' filterCat3samples(data_samples = df_ExampSamps
#' , data_sufficiency = df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#' @return Filtered dataset not including Category 3 AU/characteristic combinations.
#' @export
#'
filterCat3samples <- function(data_samples, data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    dplyr::select(AUID_ATTNS, TADA.CharacteristicName) %>%
    unique()

  samples <- data_samples %>%
    dplyr::right_join(suff_sites, by = dplyr::join_by('AUID_ATTNS'
                                               , 'TADA.CharacteristicName')) %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue))

  return(samples)
} # end filterCat3samples
