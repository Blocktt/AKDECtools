#' WQ boxplot.
#'
#' The boxPlot function produces boxplots for WQ data depending on
#' an assessment unit identifier (AU_ID). Each WQ characteristic is plotted separately.
#' Individual values are plotted as points, colored by monitoring station.
#' The y-axis can be Log10 scaled using a TRUE/FALSE argument.
#'
#' @details
#' Required fields for input arguments
#'
#' * data: TADA.CharacteristicName, AUID_ATTNS, TADA.ResultMeasureValue,
#' MonitoringLocationIdentifier, TADA.ResultMeasure.MeasureUnitCode
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
#' # Example 1 - single AU and single constituent
#'
#' boxPlot(data = df_ExampSamps, WQS_table = df_WQS_Crosswalk
#' , AU_ID = c('AK_R_1010504_005'), y_axis_log = F)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A boxplot
#' @export
#'
boxPlot <- function(data, WQS_table, AU_ID, y_axis_log) {

  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts

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
        ggplot2::geom_boxplot(data = filt,
                              ggplot2::aes(x = AUID_ATTNS,
                                  y = TADA.ResultMeasureValue),
                              color = 'gray30') +
        ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                              y = TADA.ResultMeasureValue,
                                              fill = MonitoringLocationIdentifier),
                             color = 'black',
                             shape = 21,
                             size = 2,
                             width = 0.2,
                             alpha = 0.8) +
        ggplot2::xlab('AU ID') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw(base_size = 22) +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))

      results[[counter]] <- plt
    } else {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = filt,
                              ggplot2::aes(x = AUID_ATTNS,
                                  y = TADA.ResultMeasureValue),
                              color = 'gray30') +
        ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                              y = TADA.ResultMeasureValue,
                                              fill = MonitoringLocationIdentifier),
                             color = 'black',
                             shape = 21,
                             size = 2,
                             width = 0.2,
                             alpha = 0.8) +
        ggplot2::xlab('AU ID') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::scale_y_log10() +
        ggplot2::theme_bw(base_size = 22) +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))

      results[[counter]] <- plt
    }
  }
  return(results)
} # end if/else
