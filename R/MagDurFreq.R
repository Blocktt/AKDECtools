#' Assess magnitude, duration, and frequency
#'
#' The MagDurFreq function applies Alaska's surface water quality standards (SWQS) to water quality data.
#' Ideally, data are filtered before running this function to only include data sufficient
#' for water quality assessments. Water quality exceedances are determined using the
#' magnitude, duration, and frequency components of Alaska's SWQS.
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
#' MagDurFreq(df_WQS_Crosswalk, input_samples_filtered, df_Data_Sufficiency)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'
MagDurFreq <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency) {

  ##Magnitude, Frequency, Duration - unique combinations
  #This is not used in the code, but instead used as reference for making the methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent != 'Ammonia') %>%
    dplyr::filter(!(Constituent == 'Pentachloro-phenol' & `Waterbody Type` == 'Freshwater')) %>%
    dplyr::filter(Constituent != 'Turbidity') %>%
    dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                       'Nickel', 'Silver', 'Zinc') & Use == 'Aquatic Life')) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()

  # write_csv(unique_methods, 'Output/data_analysis/wqs_unique_methods.csv')

  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU

    # Filter data for just AU and make water year
    df_subset <- input_samples_filtered %>%
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = lubridate::year(ActivityStartDate),
             month = lubridate::month(ActivityStartDate),
             w_year = ifelse(month < 10, year, year+1))

    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)

    # use AU_Type to choose Waterbody Type in data standards table
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
    #remove information for instances found in the special case functions
    my_data_magfreqdur <- wqs_crosswalk %>%
      dplyr::filter(TADA.Constituent %in% my_constituents) %>%
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text))  %>%
      dplyr::filter(Constituent != 'Ammonia') %>% #Filter out special cases
      dplyr::filter(Constituent != 'Pentachloro-phenol') %>%
      dplyr::filter(Constituent != 'Turbidity') %>%
      dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                         'Nickel', 'Silver', 'Zinc') & Use == 'Aquatic Life'))

    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }

    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      #Pull relevant methods
      filter_by <- my_data_magfreqdur[j,]

      #Pull just that constituent data
      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)


      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
         filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #1 ----
        #Maximum, not to exceed, 30-day geometric mean
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue,
                                                       seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate),
                                                       psych::geometric.mean),
                        Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)

        filter_by$AUID_ATTNS <- i

        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))

        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #2 ----
        #Maximum, Not to exceed, water year average, geometric mean
        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_1yr = psych::geometric.mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(geo_mean_1yr >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_1yr)

        filter_by$AUID_ATTNS <- i

        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))

        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')

      }else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10% of samples' &
               filter_by$Duration == 'Water year average') {
        #Method #3 ----
        #Maximum, 10% of samples, Water year average

        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(wyear_row = dplyr::n(),
                        bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/wyear_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::select(w_year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T){
        #Method #4 ----
        #Not to exceed, 30 day geometric mean

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue,
                                                       seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate),
                                                       psych::geometric.mean),
                        Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)

        filter_by$AUID_ATTNS <- i

        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))

        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method #5 ----
        #Maximum, not to exceed, geometric mean for water year

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_year = psych::geometric.mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(geo_mean_year >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_year) %>%
          dplyr::ungroup()

        filter_by$AUID_ATTNS <- i

        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))

        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Daily average') {
        #Method #6 ----
        #Maximum, not to exceed, daily arithmetic mean

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!daily_mean) %>%
          dplyr::ungroup()

        filter_by$AUID_ATTNS <- i

        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))

        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average'){
        #Method #7 ----
        #Maximum, 10% of samples, daily arithmetic mean

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = dplyr::n(),
                        bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>%
          dplyr::ungroup() %>%
          dplyr::select(year, bad_year) %>%
          unique()

        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '1m Depth') == T){
        #Method #8 ----
        #Minimum, 10%, daily arithmetic mean, 1m Depth

        results <- filt %>%
          dplyr::filter(TADA.ActivityDepthHeightMeasure.MeasureValue <= 1) %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = dplyr::n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Estuaries and tidal tributaries') == T){
        #Method #9 ----
        #Minimum, 10%, daily arithmetic mean

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = dplyr::n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average'){
        #Method #10 ----
        #Minimum, 10%, daily arithmetic mean

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = dplyr::n(),
                        bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily minimum'){
        #Method #11 ----
        #Minimum, 10%, daily minimum

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_min = min(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                        bad_samp = ifelse(daily_min <= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily maximum'){
        #Method #12 ----
        #Maximum, 10%, daily Maximum

        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_max = max(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                        bad_samp = ifelse(daily_max >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/day_row>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Not to exceed'){
        #Method #13 ----
        #Maximum, 10%, not to exceed

        results <- filt %>%
          dplyr::mutate(num_samples = dplyr::n(),
                        bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                        sum = sum(bad_samp),
                        bad_year = ifelse(sum/num_samples>=0.1, 1, 0))

        bad_tot <- results %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Not to exceed'){
        #Method #14 ----
        #Maximum, not to exceed, not to exceed

        results <- filt %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == 'Arithmetic mean' &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Frequency - harmonic mean of most recent 3 years') == T){
        #Method #15 ----
        #Maximum, not to exceed, arithmetic Mean of last 3 years

        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::mutate(mean_samps = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(mean_samps >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                filter_by$Duration == '30-day arithmetic average'){
        #Method #16 ----
        #Maximum, not to exceed, 30 day arithmetic mean
        results <- filt %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(mean_30_day = zoo::rollapplyr(TADA.ResultMeasureValue,
                                                      seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate),
                                                      mean),
                        bad_samp = ifelse(mean_30_day >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour max'){
        #Method #17 ----
        #Maximum, 1 instance in previous 3 years, 24 hour max

        max_year <- results %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(max_samps = max(TADA.ResultMeasureValue),
                        bad_samp = ifelse(max_samps >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                (filter_by$Duration == '24-hour average' | filter_by$Duration == '24-hour arithmetic average') & is.na(filter_by$Details) == T){
        #Method #18 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 24 hour average

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0))

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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Magnitude is minimum') == T){
        #Method #19 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude is min

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = purrr::map_dbl(ActivityStartDate,
                                                 ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(4), .x)])),
                        bad_samp = ifelse(roll_4day_mean < filter_by$Magnitude_Numeric, 1, 0))

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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method #20 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude listed

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = purrr::map_dbl(ActivityStartDate,
                                                 ~mean(TADA.ResultMeasureValue[dplyr::between(ActivityStartDate, .x - lubridate::days(4), .x)])),
                        bad_samp = ifelse(roll_4day_mean >= filter_by$Magnitude_Numeric, 1, 0))

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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #21 ----
        #Maximum, 1 in most recent 3 years, 1 hour average
        #Could not do 1 hour average as some samples do not have a time recoreded
        #Assumed no samples were taken within 1 hour of each other at the same location
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour arithmetic average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method #22 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average


        results <- filt %>%
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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '>=2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method #23 ----
        #Maximum, >=2 exceedances and >5% exceedance frequency in 3 year period, 1 hour average, magnitude listed

        #1-hour averages: use the daily value
        #(it will be rare that multiple samples are taken in a day, and if they are they would be considered duplicates)
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))

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

      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour average'){
        #Method #24 ----
        #Maximum, 1 in most recent 3 years, 24 hour average

        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_mean >= magnitude, 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      }

      result_list[[counter]] <- filter_by
    }
  } # end of for loop


  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>%
    distinct()

  df_AU_data_WQS %>% dplyr::select(Exceed) %>% dplyr::group_by(Exceed) %>% dplyr::mutate(n = dplyr::n()) %>% unique()

  #combine with relevant WQS table, removing the constituents that are calculated in other functions
  #these constituents come back in the hardness, pH, and turbidity specific functions
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(!(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                                   'NICKEL', 'SILVER', 'ZINC') & Use == 'Aquatic Life')) %>%
    dplyr::filter(TADA.CharacteristicName != 'AMMONIA') %>%
    dplyr::filter(TADA.CharacteristicName != 'PENTACHLORO-PHENOL') %>%
    dplyr::filter(TADA.CharacteristicName != 'TURBIDITY')

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())

  return(data_suff_WQS)

}
