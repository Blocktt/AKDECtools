#' WQ boxplot.
#'
#' This function produces boxplots for WQ datadepending on AU_ID, WQ constituent
#' , or both.
#'
#' @import magrittr
#'
#' @param data A WQ dataset in long format. Must contain either AU_ID or TADA.CharacteristicName
#' @param AU_ID An AK DEC Assessment Unit (AU) Identifier
#' @param constituent TADA.CharacteristicName as generated after running data_processing.R
#'
#' @return A boxplot
#' @export
#'
boxPlot <- function(data, AU_ID, constituent) {

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
      ggplot2::geom_boxplot(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none")
  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_classic() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'),
            legend.position = "none")
  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      ggplot2::facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'),
            legend.position = "none")
  } else {
    plt<-ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      ggplot2::facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      ggplot2::xlab('AU ID') +
      ggplot2::ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = 'gray95'),
            axis.text.x = ggplot2::element_text(angle = 45, hjust=1),
            legend.position = "none")
  }

  return(plt)
} # end if/else
