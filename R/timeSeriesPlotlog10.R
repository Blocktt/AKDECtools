#' WQ time series plotting (log10 scale).
#'
#' This function produces time series plots (log10 scale) for WQ data
#' depending on AU_ID, WQ constituent, or both.
#'
#' @import magrittr
#'
#' @param data A WQ dataset in long format. Must contain either AU_ID or TADA.CharacteristicName
#' @param AU_ID An AK DEC Assessment Unit (AU) Identifier
#' @param constituent TADA.CharacteristicName as generated after running data_processing.R
#'
#' @return A log10-scaled plot
#' @export
#'
timeSerieslog10 <- function(data, AU_ID, constituent) {

  if(missing(AU_ID)) {
    filt <- data %>% dplyr::filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% dplyr::filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% dplyr::filter(AUID_ATTNS %in% AU_ID) %>%
      dplyr::filter(TADA.CharacteristicName %in% constituent)
  }

  val_name <- filt %>% dplyr::select(TADA.CharacteristicName) %>% unique() %>% nrow()


  if(length(AU_ID) == 1 & length(constituent) == 1) {
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt, ggplot2::aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      ggplot2::scale_y_log10() +
      ggplot2::xlab('Time') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::ggtitle(paste0(AU_ID)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position="none")

  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt, ggplot2::aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      ggplot2::facet_wrap(~AUID_ATTNS) +
      ggplot2::scale_y_log10() +
      ggplot2::xlab('Time') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'),
            legend.position="none")

  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt, ggplot2::aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      ggplot2::facet_wrap(~TADA.CharacteristicName, scale = "free_y") +
      ggplot2::scale_y_log10() +
      ggplot2::xlab('Time') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::ggtitle(paste0(AU_ID)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'),
            legend.position="none")

  } else {
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt, ggplot2::aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      ggplot2::facet_grid(cols = ggplot2::vars(AUID_ATTNS),
                 rows = ggplot2::vars(TADA.CharacteristicName),
                 scales = "free_y") +
      ggplot2::scale_y_log10() +
      ggplot2::xlab('Time') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'))
  }

  return(plt)
} # end if/else
