#' WQ time series plotting.
#'
#' The timeSeries function produces time series plots for WQ data depending on
#' an assessment unit identifier (AU_ID). Each WQ characteristic is plotted separately.
#' The y-axis can be Log10 scaled using a TRUE/FALSE argument.
#'
#' @details
#' Required fields for input arguments
#'
#' * data: TADA.CharacteristicName, AUID_ATTNS, ActivityStartDate,
#' TADA.ResultMeasureValue, MonitoringLocationIdentifier, TADA.ResultMeasure.MeasureUnitCode
#' * WQS_table: TADA.Constituent
#' * AU_ID: NA
#' * y_axis_log: TRUE/FALSE
#'
#' @import magrittr
#'
#' @param data A WQ dataset in long format. Must contain either AU_ID or TADA.CharacteristicName
#' @param WQS_table Water quality standards crosswalk table
#' @param AU_ID An AK DEC Assessment Unit (AU) Identifier
#' @param y_axis_log A TRUE/FALSE argument that specifies whether the Y-axis should be Log10 scaled
#'
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#' library(AKDECtools)
#'
#' df_ExampSamps <- read_csv(system.file("extdata/AK_Example_Samples.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' df_WQS_Crosswalk <- read_csv(system.file("extdata/AK_WQS_Crosswalk.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' timeSeries(data = df_ExampSamps, WQS_table = df_WQS_Crosswalk
#' , AU_ID = c('AK_R_1010504_005'), y_axis_log = F)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A time series plot
#' @export
#'
timeSeries <- function(data, WQS_table, AU_ID, y_axis_log) {

  relevant_constituents <- WQS_table %>%
    dplyr::select(TADA.Constituent) %>%
    unique() %>%
    dplyr::pull()

  relevant_data <- data %>%
    dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
    dplyr::filter(AUID_ATTNS == AU_ID)

  constituents <- relevant_data %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()

  results <- list()
  counter <- 0
  #Loop through constituents
  for(j in constituents) {

    counter<- counter+1

    filt <- relevant_data %>%
      dplyr::filter(TADA.CharacteristicName == j)

    if(y_axis_log == F) {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_point(data = filt,
                            ggplot2::aes(x = ActivityStartDate,
                                y = TADA.ResultMeasureValue,
                                fill = MonitoringLocationIdentifier),
                            color = 'black',
                            shape = 21,
                            size = 2,
                            alpha = 0.8) +
        ggplot2::xlab('Time') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top")

      results[[counter]] <- plt
    } else {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_point(data = filt,
                            ggplot2::aes(x = ActivityStartDate,
                                y = TADA.ResultMeasureValue,
                                fill = MonitoringLocationIdentifier),
                            color = 'black',
                            shape = 21,
                            size = 2,
                            alpha = 0.8) +
        ggplot2::xlab('Time') +
        ggplot2::scale_y_log10() +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top")

      results[[counter]] <- plt
    }
  }
  return(results)
} # end if/else

