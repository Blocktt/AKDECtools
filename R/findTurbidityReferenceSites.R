#' Identify the monitoring locations within AUs with sufficient turbidity
#'
#' @param input_samples_filtered Water quality samples limited to those AUs with
#' sufficient data for a given characteristic. Use filterCat3samples function to filter
#' before running this function.
#'
#' @return Table with a column of AUs with sufficient turbidity, a column of
#' the monitoring location sites within that AU, and a column totalling the
#' number of monitoring sites within that AU.
#' @export
#'
#' @examples
#' #' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#' library(AKDECtools)
#' library(dplyr)
#'
#' df_ExampSamps <- read_csv(system.file("extdata/AK_Example_Samples.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#'  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' list_of_needed_sites <- findTurbidityReferenceSites(df_ExampSamps)
#'
findTurbidityReferenceSites <- function(input_samples_filtered) {

  #Find all AUs with sufficient turbidity
  AUsufficient <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
    dplyr::select(AUID_ATTNS, MonitoringLocationIdentifier) %>%
    unique() %>%
    dplyr::arrange(AUID_ATTNS) %>%
    dplyr::group_by(AUID_ATTNS) %>%
    dplyr::reframe(AUID_ATTNS = AUID_ATTNS,
                   MonitoringLocationIdentifier = list(unique(MonitoringLocationIdentifier)),
                   n_MonitoringLocations = n()) %>%
    unique()

  return(AUsufficient)
}
