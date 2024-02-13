#' Assess magnitude, duration, and frequency (turbidity dependent)
#'
#' The MagDurFreq_turbidity function applies Alaska's surface water quality
#' standards (SWQS) to turbidity-dependent water quality data. Ideally, data are
#' filtered before running this function to only include data sufficient
#' for water quality assessments. Water quality exceedances are determined using the
#' magnitude, duration, and frequency components of Alaska's SWQS.
#'
#' @param wqs_crosswalk Water quality standards crosswalk table
#' @param input_samples_filtered Water quality samples limited to those AUs with
#' sufficient data for a given characteristic. Use filterCat3samples function to filter
#' before running this function.
#' @param input_sufficiency Data sufficiency table generated using the data_processsing.R script
#' @param reference_sites List of paired reference sites for each AU in 'input_samples_filtered'
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
#' input_samples_filtered <- filterCat3samples(data_samples = df_ExampSamps
#' , data_sufficiency = df_Data_Sufficiency)
#'
#' # create reference table
#' reference_sites <- tibble(AUID_ATTNS = au_sites
#' , ReferenceSites = sites)
#'
#' # run MagDurFreq_hardness
#' MagDurFreq_turbidity(df_WQS_Crosswalk, input_samples_filtered, df_Data_Sufficiency
#' , reference_sites)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'
MagDurFreq_turbidity <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency, reference_sites) {

  ##Magnitude, Frequency, Duration - unique combinations
  #This is not used in the code, but instead used as reference for making the methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent == 'Turbidity') %>%

    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()

  #Calculate mean turbidity at reference sites
  pull_reference <- input_samples_filtered %>%
    dplyr::filter(MonitoringLocationIdentifier %in% reference_sites$ReferenceSites) %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::mutate(mean_reference = mean(TADA.ResultMeasureValue)) %>%
    dplyr::select(MonitoringLocationIdentifier, mean_reference) %>%
    unique()

  reference_sites_mean <- reference_sites %>%
    dplyr::left_join(pull_reference, by = c('ReferenceSites' = 'MonitoringLocationIdentifier'))

  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(reference_sites_mean$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU

    # Filter data for just AU and make water year
    df_subset <- input_samples_filtered %>%
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
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
      dplyr::filter(Constituent == 'Turbidity')


    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }

    #Pull reference value for AU
    au_reference_conditions <- reference_sites_mean %>%
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::select(mean_reference) %>%
      dplyr::pull()

    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      #Pull relevant methods
      filter_by <- my_data_magfreqdur[j,]

      #Pull just that constituent data
      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)

      #All turbidity analysis is for maximum value, not to exceed in a 24-hour average (daily)
      if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                             'May not exceed 25 NTU above natural conditions') == T){
        #Method #1 ----
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (au_reference_conditions+25), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if((stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     'For lake waters, may not exceed 5 NTU above natural conditions') == T |
                 stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     'For lakes, turbidity may not exceed 5 NTU above natural turbidity.') == T |
                 stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     '5 NTU above natural conditions, for all lake waters') == T) &
                my_AU_Type == "Lake"){
        #Method #2 ----
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (au_reference_conditions+5), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if((stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     'For lake waters, may not exceed 5 NTU above natural conditions') == T |
                 stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     'For lakes, turbidity may not exceed 5 NTU above natural turbidity.') == T |
                 stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     '5 NTU above natural conditions, for all lake waters') == T) &
                my_AU_Type != "Lake"){
        #Method #3 ----

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'AU not lake waters'

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    '5 NTU above natural conditions, when natural turbidity is 50 NTU or less.') == T &
                au_reference_conditions <= 50){
        #Method #4 ----
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (au_reference_conditions+5), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    '5 NTU above natural conditions, when natural turbidity is 50 NTU or less.') == T &
                au_reference_conditions > 50){
        #Method #5 ----

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Natural conditions greater than 50 NTU'

      }  else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                     'No more than 10% increase when natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
                 au_reference_conditions > 50){
        #Method #6 ----

        max_over <- ifelse(au_reference_conditions*0.1 >= 15, 15, au_reference_conditions*0.1)

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'No more than 10% increase when natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
                au_reference_conditions <= 50){
        #Method #7 ----

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'When natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
                au_reference_conditions > 50){
        #Method #8 ----

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (au_reference_conditions+15), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'When natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
                au_reference_conditions <= 50){
        #Method #9 ----

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'May not exceed 10 NTU above natural conditions when natural turbidity is 50 NTU or less') == T &
                au_reference_conditions <= 50){
        #Method #10 ----

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (au_reference_conditions+10), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'May not exceed 10 NTU above natural conditions when natural turbidity is 50 NTU or less') == T &
                au_reference_conditions > 50){
        #Method #11 ----

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- "Natural conditions greater than 50 NTU"

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'May not exceed 20% increase in turbidity when natural turbidity is greater than 50 NTU, not to exceed a maximum increase of 15 NTU') == T &
                au_reference_conditions > 50){
        #Method #12 ----
        max_over <- ifelse(au_reference_conditions*0.2 >= 15, 15, au_reference_conditions*0.1)

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'May not exceed 20% increase in turbidity when natural turbidity is greater than 50 NTU, not to exceed a maximum increase of 15 NTU') == T &
                au_reference_conditions <= 50){
        #Method #13 ----
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'No more than 10% increase when natural condition is more than 50 NTU, not to exceed a maximum increase of 25 NTU.') == T &
                au_reference_conditions > 50){
        #Method #14 ----
        max_over <- ifelse(au_reference_conditions*0.1 >= 25, 25, au_reference_conditions*0.1)

        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

      } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
                                    'No more than 10% increase when natural condition is more than 50 NTU, not to exceed a maximum increase of 25 NTU.') == T &
                au_reference_conditions <= 50){
        #Method #15 ----
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"

      } else if(is.na(filter_by$Details) == T){
        #Method #16 ----
        #Marine turbidity - if 24-hour average turbidity is larger than specified magnitude
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                        bad_samp = ifelse(daily_avg >= filter_by$Magnitude_Numeric, 1, 0))

        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)

        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')

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
    distinct()

  #combine with relevant data standards table
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName == 'TURBIDITY')

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                           'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())

  return(data_suff_WQS)
} #End of turbidity function
