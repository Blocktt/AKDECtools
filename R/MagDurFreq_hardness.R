#' Assess magnitude, duration, and frequency (hardness dependent)
#'
#' The MagDurFreq_hardness function applies Alaska's surface water quality
#' standards (SWQS) to hardness-dependent water quality data. Ideally, data are
#' filtered before running this function to only include data sufficient
#' for water quality assessments. Water quality exceedances are determined using the
#' magnitude, duration, and frequency components of Alaska's SWQS.
#'
#' @param wqs_crosswalk Water quality standards crosswalk table
#' @param input_samples All water quality samples (not limited)
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
#' MagDurFreq_hardness(df_WQS_Crosswalk, df_ExampSamps, input_samples_filtered
#' , df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'
MagDurFreq_hardness<- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  #Not used in code, but as a resource for creating/updating methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                     'Nickel', 'Silver', 'Zinc')) %>%
    dplyr::filter(Use == 'Aquatic Life') %>%
    dplyr::filter(is.na(Magnitude_Numeric)) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()

  #Find just the required samples
  input_samples_filtered_relevant <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                 'NICKEL', 'SILVER', 'ZINC'))

  #Return message if insufficient samples
  if(nrow(input_samples_filtered_relevant) == 0) {
    return(print("Insufficient site samples for hardness dependent variables. No analysis performed."))
  }

  #Find unique AU IDs to cycle through
  Unique_AUIDs <- unique(input_samples_filtered_relevant$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU

    # filter data
    df_subset <- input_samples_filtered_relevant %>%
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = lubridate::year(ActivityStartDate),
                    month = lubridate::month(ActivityStartDate),
                    w_year = ifelse(month < 10, year, year+1))

    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)

    # use AU_Type to choose Waterbody Type in WQS table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement

    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)

    # trim data WQS table to only relevant information
    my_data_magfreqdur <- wqs_crosswalk %>%
      dplyr::filter(TADA.Constituent %in% my_constituents) %>%
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text)) %>%
      dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                       'Nickel', 'Silver', 'Zinc')) %>%
      dplyr::filter(Use == 'Aquatic Life')

    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }

    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]

      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)

      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
         filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == T &
         stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #1 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes - NOT pH DEPENDENT
        #Chronic

        #Pull matching hardness samples from all samples
        hardness <- input_samples %>%
          dplyr::filter(AUID_ATTNS == i) %>%
          dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
          dplyr::rename(Hardness = TADA.ResultMeasureValue,
                        Hardness.Date = ActivityStartDate) %>%
          dplyr::select(Hardness.Date, ActivityStartTime.Time, AUID_ATTNS, Hardness) %>%
          dplyr::group_by(Hardness.Date) %>%
          dplyr::reframe(Hardness.Date = Hardness.Date,
                         Hardness = mean(Hardness)) %>%
          unique()


        #Mark result as insufficient if no hardness available
        if(nrow(hardness) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- "Insufficient hardness"
        } else {
          #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
          if(filter_by$Constituent == 'Chromium (III)'){
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+0.6848)*0.860)

          } else if(filter_by$Constituent == 'Copper') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8545*(log(Hardness))-1.702)*0.960)

          } else if(filter_by$Constituent == 'Lead') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-4.705)*(1.46203-log(Hardness)*0.145712))

          } else if(filter_by$Constituent == 'Nickel') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+0.0584)*0.997)

          } else if(filter_by$Constituent == 'Zinc') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.986)

          } else if(filter_by$Constituent == 'Cadmium') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.7409*(log(Hardness))-4.719)*(1.101672-log(Hardness)*0.041838))

          }
          results <- joined %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::arrange(ActivityStartDate) %>%
            dplyr::mutate(roll_4day_mean = purrr::map_dbl(ActivityStartDate,
                                                   ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(4), .x)])),
                          bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0))

          bad_tot <- results %>%
            dplyr::ungroup() %>%
            dplyr::select(year, ActivityStartDate, bad_samp) %>%
            unique() %>%
            #If more than 2 exceedances in 3 years assign value of 1, else 0
            #Left bound is date - 3 years, right bound is date
            dplyr::mutate(Exceedances = ifelse(purrr::map_dbl(ActivityStartDate,
                                                       ~sum(bad_samp[dplyr::between(ActivityStartDate, .x - lubridate::years(3), .x)]))>=2, 1, 0),
                          #Give every samples a count of 1
                          r_count = 1.0,
                          #Total up number of samples in last 3 years
                          num_samples_3yrs = purrr::map_dbl(ActivityStartDate,
                                                     ~sum(r_count[dplyr::between(ActivityStartDate, .x - lubridate::years(3), .x)])),
                          #Calculate exceedance frequency
                          Exceed_Freq = Exceedances/num_samples_3yrs,
                          #Determine if exceedance criteria met
                          tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0))

          bad_sum <- sum(bad_tot$tot_exceed)

          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        }
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #2 ----
        #Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes
        #Acute
        #Could not do 1 hour average as some samples do not have a time recorded
        #Assumed no samples were taken within 1 hour of each other at the same location

        #Pull matching hardness samples
        hardness <- input_samples %>%
          dplyr::filter(AUID_ATTNS == i) %>%
          dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
          dplyr::rename(Hardness = TADA.ResultMeasureValue,
                        Hardness.Date = ActivityStartDate) %>%
          dplyr::select(Hardness.Date, ActivityStartTime.Time, AUID_ATTNS, Hardness)%>%
          dplyr::group_by(Hardness.Date) %>%
          dplyr::reframe(Hardness.Date = Hardness.Date,
                         Hardness = mean(Hardness)) %>%
          unique()
        #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic

        #Mark result as insufficient if no hardness available
        if(nrow(hardness) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- "Insufficient hardness"
        } else {
          if(filter_by$Constituent == 'Chromium (III)'){
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+3.7256)*0.316)

          } else if(filter_by$Constituent == 'Copper') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.9422*(log(Hardness))-1.700)*0.960)

          } else if(filter_by$Constituent == 'Lead') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-1.460)*(1.46203-log(Hardness)*0.145712))

          } else if(filter_by$Constituent == 'Nickel') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+2.255)*0.998)

          } else if(filter_by$Constituent == 'Zinc') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.978)

          } else if(filter_by$Constituent == 'Cadmium') {
            #Combine matching hardness & calculate magnitude - just need one hardness value for IR cycle
            match_dates <- filt %>%
              dplyr::left_join(hardness, by = dplyr::join_by(closest(ActivityStartDate >= Hardness.Date))) %>%
              dplyr::filter(!is.na(Hardness.Date))
            #Equation from Toxics Manual Appendix A
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.0166*(log(Hardness))-3.924)*(1.136672-log(Hardness)*0.041838))

          }

          max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()

          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0))

          bad_tot <- results %>%
            dplyr::ungroup() %>%
            dplyr::select(bad_samp) %>%
            unique()

          bad_sum <- sum(bad_tot$bad_samp)

          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        } #End of hardness check
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      } #End of methods if/else

      result_list[[counter]] <- filter_by
    } #End of MagDurFreq loop

  } #End of AU loop

  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>%
    dplyr::distinct()

  #combine with relevant data standards table
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                 'NICKEL', 'SILVER', 'ZINC')) %>%
    dplyr::filter(Use == 'Aquatic Life')

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())

  return(data_suff_WQS)
} #End of hardness dependent function
