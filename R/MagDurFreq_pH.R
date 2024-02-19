#' Assess magnitude, duration, and frequency (pH dependent)
#'
#' The MagDurFreq_pH function applies Alaska's surface water quality
#' standards (SWQS) to pH-dependent water quality data. Ideally, data are
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
#' MagDurFreq_pH(df_WQS_Crosswalk, df_ExampSamps, input_samples_filtered
#' , df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'
MagDurFreq_pH <- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Ammonia') |
                    (Constituent %in% c('Pentachloro-phenol') & `Waterbody Type` == 'Freshwater')) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()

  input_samples_filtered_relevant <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL'))

  #Return message if no samples available
  if(nrow(input_samples_filtered_relevant) == 0) {
    #If no samples available - just return sufficiency with empty Exceed column
    relevant_suff <- input_sufficiency %>%
      dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL')) %>%
      dplyr::mutate(Exceed = NA)

    return(relevant_suff)
  }

  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(input_samples_filtered_relevant$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU

    # dplyr::filter data
    df_subset <- input_samples_filtered %>% #CHANGE to input_samples_filtered_relevant
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = lubridate::year(ActivityStartDate),
                    month = lubridate::month(ActivityStartDate),
                    w_year = ifelse(month < 10, year, year+1))

    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)

    # use AU_Type to choose Waterbody Type in data sufficiency table
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
      dplyr::filter(Constituent %in% c('Ammonia', 'Pentachloro-phenol'))

    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }

    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]

      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)

      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
         filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T){
        #Method #1 ----
        #Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Acute
        #Could not do 1 hour average as some samples do not have a time recoreded
        #Assumed no samples were taken within 1 hour of each other at the same location

        #Pull matching pH
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()

        if(filter_by$Constituent == 'Pentachloro-phenol') {
          #Combine matching pH & calculate magnitude
          #Join with pH - needs to be at least one pH value present during IR cycle
          match_dates <- filt %>%
            dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))

          #Equation from WQS sheet 'Details'
          joined <- match_dates %>%
            dplyr::mutate(magnitude = exp(1.005*pH-4.869))

        } else {#Acute freshwater ammonia

          match_dates <- filt %>%
            dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))

          #Equation from Toxics Manual Appendix C - no salmonids
          joined <- match_dates %>%
            dplyr::mutate(magnitude = ((0.411/(1+10^(7.204-pH)))+(58.4/(1+10^(pH-7.204)))))

        }

        if(nrow(joined) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {
          max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()

          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0))

          bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
          bad_sum <- sum(bad_tot$bad_samp)

          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        }

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '30-day average' & is.na(filter_by$Magnitude_Numeric) == T){
        #Method #2 ----
        #Chronic, freshwater ammonia
        #Maximum, 2 or more exceedances and >5% exceedance frequency in 3 year period for 30-day averages

        #Pull matching pH
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()

        #Pull matching temperature
        temp <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
          dplyr::rename(temp = TADA.ResultMeasureValue,
                        temp.Date = ActivityStartDate) %>%
          dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
          dplyr::group_by(temp.Date) %>%
          dplyr::reframe(temp.Date = temp.Date,
                         temp = mean(temp)) %>%
          unique()

        #Get nearest pH sample to ammonia sample date
        match_dates1 <- filt %>%
          dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
          dplyr::filter(!is.na(pH.Date))

        #Get only same-day matches for temp and ammonia
        match_dates <- match_dates1 %>%
          dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date))

        if(nrow(match_dates) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {
          #Calculate the min() part of the equation from Toxics Manual Appendix D
          match_dates_w_min <- match_dates %>%
            dplyr::mutate(Min_Value = ifelse(1.45*10^(0.028*(25-temp)) > 2.85, 2.85, 1.45*10^(0.028*(25-temp))))

          #Calculate magnitude, rolling 30 day average, and if something is a "bad sample"
          joined <- match_dates_w_min %>%
            dplyr::arrange(ActivityStartDate) %>%
            #Equation from Toxics Manual Appendix D
            dplyr::mutate(magnitude = ((0.0577/(1+10^(7.688-pH)))+(2.487/(1+10^(pH-7.688))))*Min_Value,
                          #Rolling 30 day average
                          average_30_day = zoo::rollapplyr(TADA.ResultMeasureValue,
                                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate),
                                                           mean),
                          bad_samp = ifelse(average_30_day >= magnitude, 1, 0))

          bad_tot <- joined %>%
            dplyr::ungroup() %>%
            dplyr::select(w_year, ActivityStartDate, bad_samp) %>%
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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == T) {
        #Method #3 ----
        #Maximum, 2 or more exceedances or >5% exceedance frequency in 3 years, 96-hour average (4 day)
        #Chronic Pentachloro-phenol
        #Marine chronic Ammonia
        #Pull matching pH
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()

        if(filter_by$Constituent == 'Pentachloro-phenol') {
          #Combine matching pH & calculate magnitude - just need one pH value for IR cycle
          match_dates <- filt %>%
            dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))

          #Equation from WQS sheet 'Details'
          joined <- match_dates %>%
            dplyr::mutate(magnitude = exp(1.005*pH-5.134))

          if(nrow(joined) == 0) {
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- 'Insufficient dependent data'
          } else {

            max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()

            results <- joined %>%
              dplyr::filter(w_year >= max_year - 3) %>%
              dplyr::group_by(ActivityStartDate) %>%
              dplyr::mutate(roll_4day_mean = purrr::map_dbl(ActivityStartDate,
                                                     ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(4), .x)])),
                            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0))

            bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
            bad_sum <- sum(bad_tot$bad_samp)

            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
          }

        } else {#Chronic marine ammonia
          #Pull matching temperature
          temp <- input_samples %>%
            dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
            dplyr::rename(temp = TADA.ResultMeasureValue,
                          temp.Date = ActivityStartDate) %>%
            dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
            dplyr::group_by(temp.Date) %>%
            dplyr::reframe(temp.Date = temp.Date,
                           temp = mean(temp)) %>%
            unique()


          #Pull matching salinity
          salinity <- input_samples %>%
            dplyr::filter(TADA.CharacteristicName == "SALINITY") %>%
            dplyr::rename(salinity = TADA.ResultMeasureValue,
                          salinity.Date = ActivityStartDate) %>%
            dplyr::select(salinity.Date, ActivityStartTime.Time, AUID_ATTNS, salinity) %>%
            dplyr::group_by(salinity.Date) %>%
            dplyr::reframe(salinity.Date = salinity.Date,
                           salinity = mean(salinity)) %>%
            unique()

          #Join ammonia with pH - needs to be at least one pH value present during IR cycle
          match_dates1 <- filt %>%
            dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
            dplyr::filter(!is.na(pH.Date))

          #Join ammonia with salinity - needs to be at least one salinity value present during IR cycle
          match_dates2 <- match_dates1 %>%
            dplyr::left_join(salinity, by = dplyr::join_by(closest(ActivityStartDate >= salinity.Date))) %>%
            dplyr::filter(!is.na(salinity.Date))

          #Get only same-day matches for temp and ammonia
          match_dates <- match_dates2 %>%
            dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date))

          #Table from Toxics Manual Appendix G
          cross_table <- readr::read_csv("Data/data_analysis/chronic_marine_ammonia.csv") %>%
            dplyr::mutate(Salinity = DescTools::RoundTo(Salinity, 10000000),
                          Salinity = as.character(Salinity),
                          Temperature = as.character(Temperature),
                          pH = as.character(pH))

          #Convert numerics to character in order to get join to work
          joined <- match_dates %>%
            dplyr::mutate(salinity_round = DescTools::RoundTo(salinity, 10000000),
                          temp_round = DescTools::RoundTo(temp, 5),
                          pH_round = DescTools::RoundTo(pH, 0.2),
                          salinity_round = as.character(salinity_round),
                          temp_round = as.character(temp_round),
                          pH_round = as.character(pH_round)) %>%
            dplyr::left_join(cross_table, by = dplyr::join_by(salinity_round == Salinity,
                                                              temp_round == Temperature,
                                                              pH_round == pH)) %>%
            dplyr::filter(!is.na(Magnitude)) #Get rid of values with no Magnitude match

          if(nrow(joined) == 0) {
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- 'Insufficient dependent data'
          } else {

            max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()

            results <- joined %>%
              dplyr::filter(w_year >= max_year - 3) %>%
              dplyr::group_by(ActivityStartDate) %>%
              dplyr::mutate(roll_4day_mean = purrr::map_dbl(ActivityStartDate,
                                                     ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(4), .x)])),
                            bad_samp = ifelse(roll_4day_mean >= Magnitude, 1, 0))

            bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
            bad_sum <- sum(bad_tot$bad_samp)

            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
          }

        }

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '30-day average' & is.na(filter_by$Magnitude_Numeric) == T) {
        #Method #4 ----
        #Acute marine ammonia

        #Pull matching pH
        pH <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "PH") %>%
          dplyr::rename(pH = TADA.ResultMeasureValue,
                        pH.Date = ActivityStartDate) %>%
          dplyr::select(pH.Date, ActivityStartTime.Time, AUID_ATTNS, pH) %>%
          dplyr::group_by(pH.Date) %>%
          dplyr::reframe(pH.Date = pH.Date,
                         pH = mean(pH)) %>%
          unique()
        #Pull matching temperature
        temp <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
          dplyr::rename(temp = TADA.ResultMeasureValue,
                        temp.Date = ActivityStartDate) %>%
          dplyr::select(temp.Date, ActivityStartTime.Time, AUID_ATTNS, temp) %>%
          dplyr::group_by(temp.Date) %>%
          dplyr::reframe(temp.Date = temp.Date,
                         temp = mean(temp)) %>%
          unique()


        #Pull matching salinity
        salinity <- input_samples %>%
          dplyr::filter(TADA.CharacteristicName == "SALINITY") %>%
          dplyr::rename(salinity = TADA.ResultMeasureValue,
                        salinity.Date = ActivityStartDate) %>%
          dplyr::select(salinity.Date, ActivityStartTime.Time, AUID_ATTNS, salinity) %>%
          dplyr::group_by(salinity.Date) %>%
          dplyr::reframe(salinity.Date = salinity.Date,
                         salinity = mean(salinity)) %>%
          unique()

        #Match ammonia with pH - only need one pH sample for IR cycle
        match_dates1 <- filt %>%
          dplyr::left_join(pH, by = dplyr::join_by(closest(ActivityStartDate >= pH.Date))) %>%
          dplyr::filter(!is.na(pH.Date))

        #Match ammonia with salinity - only need one salinity sample for IR cycle
        match_dates2 <- match_dates1 %>%
          dplyr::left_join(salinity, by = dplyr::join_by(closest(ActivityStartDate >= salinity.Date))) %>%
          dplyr::filter(!is.na(salinity.Date))

        #Get only same-day matches for temp and ammonia
        match_dates <- match_dates2 %>%
          dplyr::inner_join(temp, by = dplyr::join_by(ActivityStartDate == temp.Date))

        #Table from Toxics Manual Appendix F
        cross_table <- readr::read_csv("Data/data_analysis/acute_marine_ammonia.csv") %>%
          dplyr::mutate(Salinity = DescTools::RoundTo(Salinity, 10000000),
                        Salinity = as.character(Salinity),
                        Temperature = as.character(Temperature),
                        pH = as.character(pH))

        #Convert numerics to character in order to get join to work
        joined <- match_dates %>%
          dplyr::mutate(salinity_round = DescTools::RoundTo(salinity, 10000000),
                        temp_round = DescTools::RoundTo(temp, 5),
                        pH_round = DescTools::RoundTo(pH, 0.2),
                        salinity_round = as.character(salinity_round),
                        temp_round = as.character(temp_round),
                        pH_round = as.character(pH_round)) %>%
          dplyr::left_join(cross_table, by = dplyr::join_by(salinity_round == Salinity,
                                                            temp_round == Temperature,
                                                            pH_round == pH)) %>%
          dplyr::filter(!is.na(Magnitude)) #Get rid of values with no Magnitude match

        if(nrow(joined) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Insufficient dependent data'
        } else {

          max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()

          results <- joined %>%
            dplyr::filter(w_year >= max_year - 3) %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::mutate(roll_30day_mean = purrr::map_dbl(ActivityStartDate,
                                                    ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(30), .x)])),
                          bad_samp = ifelse(roll_30day_mean >= Magnitude, 1, 0))

          bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique() %>% stats::na.omit()
          bad_sum <- sum(bad_tot$bad_samp)

          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        }
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
    dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA', 'PENTACHLOROPHENOL'))

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())

  return(data_suff_WQS)
} #End of pH dependent function
