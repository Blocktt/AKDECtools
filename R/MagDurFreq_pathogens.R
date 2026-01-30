#' Assess magnitude, duration, and frequency (pathogens)
#'
#' The MagDurFreq_pathogens function applies Alaska's surface water quality
#' standards (SWQS) to pathogen water quality data. Ideally, data are
#' filtered before running this function to only include data sufficient
#' for water quality assessments. Water quality exceedances are determined using the
#' magnitude, duration, and frequency components of Alaska's SWQS.
#'
#' @details
#' Required fields for input arguments
#'
#' * wqs_crosswalk: Constituent, Directionality, Frequency, Duration, Details, Use,
#'  Waterbody Type, Magnitude_Text, Magnitude_Numeric
#' * input_samples_filtered: AUID_ATTNS, ActivityStartDate, AU_Type,
#'  ActivityStartTime.Time, TADA.ResultMeasureValue
#' * input_sufficiency: AUID_ATTNS, TADA.CharacteristicName, Use, Waterbody Type,
#'  Fraction, Type
#'
#' @param wqs_crosswalk Water quality standards crosswalk table
#' @param input_samples_filtered Water quality samples limited to those AUs with
#' sufficient data for a given characteristic. Use filterCat3samples function to filter
#' before running this function.
#' @param input_sufficiency Data sufficiency table generated using the data_processsing.R script
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
#' df_Data_Sufficiency <- read_csv(system.file("extdata/AK_Data_Sufficiency.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#' # Filter samples to only those with sufficient data to make MagDurFreq
#' ## run more quickly
#'
#' input_samples_filtered <- filterCat3samples(data_samples = df_ExampSamps
#' , data_sufficiency = df_Data_Sufficiency)
#'
#' MagDurFreq_pathogens(df_WQS_Crosswalk, input_samples_filtered
#' , df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'

MagDurFreq_pathogens <- function(input_samples_filtered, wqs_crosswalk, input_sufficiency) {

  pathogen_criteria <- wqs_crosswalk %>%
    dplyr::filter(`Constituent Group` == "Bacteria") %>%
    dplyr::select(!Magnitude_Text)

  pathogen_data <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% unique(pathogen_criteria$TADA.Constituent)) %>%
    dplyr::mutate(year = year(ActivityStartDate),
                  month = month(ActivityStartDate),
                  w_year = ifelse(month < 10, year, year + 1))

  result_list <- list()
  counter <- 0

  for (auid in unique(pathogen_data$AUID_ATTNS)) {
    print(auid)

    df <- pathogen_data %>%
      dplyr::filter(AUID_ATTNS == auid) %>%
      dplyr::group_by(TADA.CharacteristicName, ActivityStartDate) %>%
      dplyr::mutate(TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue)) %>%
      unique()

    if (nrow(df) == 0) next

    my_AU_Type <- unique(df$AU_Type)

    # Incorporate AU_Type to Waterbody Type mapping logic
    if (my_AU_Type %in% c("Beach", "Marine")) {
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake") {
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    }

    constituents <- unique(df$TADA.CharacteristicName)

    for (constituent in constituents) {
      filt_df <- df %>% dplyr::filter(TADA.CharacteristicName == constituent)

      relevant_criteria <- pathogen_criteria %>%
        dplyr::filter(TADA.Constituent == constituent,
               `Waterbody Type` %in% my_WtrBdy_Type)

      if (nrow(relevant_criteria) < 2) next

      unique_uses <- relevant_criteria %>% dplyr::distinct(Use, `Use Description`)

      for (u in 1:nrow(unique_uses)) {
        crit_set <- relevant_criteria %>%
          dplyr::filter(Use == unique_uses$Use[u]) %>%
          dplyr::filter(`Use Description` == unique_uses$`Use Description`[u])

        ###Criterion 1: Geomean###
        crit1 <- crit_set %>%
          dplyr::filter(stringr::str_detect(tolower(Details), "geometric mean"),
                        Duration == "30-day period") %>%
          dplyr::slice(1)

        geo_exceed_years <- filt_df %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::group_by(w_year) %>%
          dplyr::group_modify(~ {
            .x %>%
              dplyr::mutate(
                #map over ActivityStartDate and return BOTH value Exceed and geom_val
                res = purrr::map(ActivityStartDate, function(d) {
                  in_win <- .x$ActivityStartDate >= (d - lubridate::days(30)) & .x$ActivityStartDate <= d

                  n_unique_dates <- dplyr::n_distinct(as.Date(.x$ActivityStartDate[in_win]))
                  if (n_unique_dates < 5) {
                    return(list(Exceed = FALSE, GeomVal = NA_real_))
                  }

                  geom_val <- psych::geometric.mean(.x$TADA.ResultMeasureValue[in_win])
                  exceed <- !is.na(geom_val) && geom_val >= crit1$Magnitude_Numeric
                  list(Exceed = exceed, GeomVal = geom_val)
                }),
                # unpack list-cols into two new columns
                Exceed   = map_lgl(res, "Exceed"),
                GeomVal  = map_dbl(res, "GeomVal")
              ) %>%
              dplyr::select(-res)
          }) %>%
          dplyr::ungroup() %>%
          dplyr::filter(Exceed) %>%
          dplyr::distinct(w_year) %>%
          dplyr::pull(w_year)

        ###Criterion 2: 10% Exceedance###
        crit2 <- crit_set %>%
          dplyr::filter(Frequency == "10% of samples",
                 Duration == "Water year average") %>%
          dplyr::slice(1)

        pct_exceed_years <- filt_df %>%
          dplyr::group_by(w_year) %>%
          dplyr::summarise(
            total = n(),
            exceed = sum(TADA.ResultMeasureValue >= crit2$Magnitude_Numeric, na.rm = TRUE),
            freq = exceed / total
          ) %>%
          dplyr::filter(total >=5, freq >= 0.1) %>%
          dplyr::pull(w_year)

        ###Impairment Rule###
        all_exceed_years <- lubridate::union(geo_exceed_years, pct_exceed_years)
        unique_years_exceeded <- length(unique(all_exceed_years))
        impaired <- ifelse(unique_years_exceeded >= 2, "Yes", "No")



        ###Format Output###
        for (crit_row in list(crit1, crit2)) {
          if (nrow(crit_row) == 0) next
          counter <- counter + 1

          result_list[[counter]] <- crit_row %>%
            dplyr::mutate(AUID_ATTNS = auid,
                   Exceed_Num = unique_years_exceeded,
                   Exceed_Freq = NA,
                   Exceed = impaired)
        }
      }
    }
  }

  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>%
    dplyr::distinct()

  #combine with relevant WQS table, removing the constituents that are calculated in other functions
  #these constituents come back in the hardness, pH, and turbidity specific functions
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('ESCHERICHIA COLI',
                                                 'FECAL COLIFORM',
                                                 'ENTEROCOCCUS'))

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #DEC added Use Description
                                           'Fraction', 'Type', 'Constituent Group'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(c(Exceed_Num, Exceed_Freq, Exceed), .after = last_col())

  return(data_suff_WQS)
}

