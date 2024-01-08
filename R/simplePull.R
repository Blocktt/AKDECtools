#Simple pull
#' A simple function to pull specific data from a larger WQ dataset by either AU_ID
#' , constituent, or both
#'
#' @import magrittr
#'
#' @param data A WQ dataset in long format. Must contain either AU_ID or TADA.CharacteristicName
#' @param AU_ID An AK DEC Assessment Unit (AU) Identifier
#' @param constituent TADA.CharacteristicName as generated after running data_processing.R
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
