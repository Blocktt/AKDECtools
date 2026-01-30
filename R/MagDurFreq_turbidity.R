#' Assess magnitude, duration, and frequency (turbidity)
#'
#' The MagDurFreq_turbidity function applies Alaska's surface water quality
#' standards (SWQS) to turbidity water quality data. Ideally, data are
#' filtered before running this function to only include data sufficient
#' for water quality assessments. Water quality exceedances are determined using the
#' magnitude, duration, and frequency components of Alaska's SWQS. Note, that
#' turbidity SWQS depend on a reference site for each assessment unit
#' (e.g., "May not exceed 25 NTU above natural conditions.")
#'
#' @details
#' Required fields for input arguments
#'
#' * wqs_crosswalk: Constituent, Directionality, Frequency, Duration, Details,
#'  Use, Waterbody Type, Magnitude_Text, Magnitude_Numeric
#' * input_samples_filtered: AUID_ATTNS, ActivityStartDate, AU_Type,
#'  ActivityStartTime.Time, TADA.ResultMeasureValue
#' * input_sufficiency: AUID_ATTNS, TADA.CharacteristicName,
#'  Use, Waterbody Type, Fraction, Type
#' * reference_sites: AUID_ATTNS, ReferenceSites
#'
#' @param wqs_crosswalk Water quality standards crosswalk table
#' @param input_samples_filtered Water quality samples limited to those AUs with
#' sufficient data for a given characteristic. Use filterCat3samples function to filter
#' before running this function.
#' @param input_sufficiency Data sufficiency table generated using the data_processsing.R script
#' @param turbidity_sites List of turbidity sites grouped by method (DoD or Avg), AU_ID, and identified as either a "Test" or "Reference" role
#'
#' @examples
#' # Example, data from Excel
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
#' df_WQS_Crosswalk <- read_csv(system.file("extdata/AK_WQS_Crosswalk.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' df_Data_Sufficiency <- read_csv(system.file("extdata/AK_Data_Sufficiency.csv"
#'                                        , package = "AKDECtools")
#'                             , guess_max = 10^6)
#'
#' df_Turbidity_Sites <- read_csv(system.file("extdata/turbidity_sites.csv"
#'                                        , package = "AKDECtools")
#'                                        , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' # set seed
#' set.seed(42)
#'
#' Filter samples to only those with sufficient data to make MagDurFreq
#' input_samples_filtered <- filterCat3samples(data_samples = df_ExampSamps
#' , data_sufficiency = df_Data_Sufficiency)
#'
#' # run MagDurFreq_hardness
#' MagDurFreq_turbidity(df_WQS_Crosswalk, input_samples_filtered
#' , df_Data_Sufficiency, df_Turbidity_Sites)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A dataset with MagDurFreq results
#' @export
#'
MagDurFreq_turbidity <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency, turbidity_sites) {

  wqs_crosswalk_filt <- wqs_crosswalk %>%
    dplyr::filter(Constituent == 'Turbidity') %>%
    #Pull percentage increase and make it a numeric
    dplyr::mutate(Magnitude_Numeric = ifelse(stringr::str_detect(Magnitude_Text, "%"),
                                      readr::parse_number(Magnitude_Text),
                                      Magnitude_Numeric),
           Units = ifelse(stringr::str_detect(Magnitude_Text, "%"),
                          "Percent",
                          Units))

  lake_aus <- input_samples_filtered %>%
    dplyr::filter(TADA.MonitoringLocationTypeName == 'LAKE') %>%
    dplyr::select(AUID_ATTNS) %>%
    unique() %>%
    dplyr::pull()

  #Prepare results list
  result_list <- list()
  counter <- 0

  #Loop over each standards row
  for(i in 1:nrow(wqs_crosswalk_filt)) {

    wqs_row <- wqs_crosswalk_filt[i,]
    water_type <- wqs_row$`Waterbody Type`
    threshold <- wqs_row$Magnitude_Numeric
    threshold_unit <- wqs_row$Units

    #Loop over each unique Group in turbidity_sites
    groups <- turbidity_sites %>%
      #filter for group AUs that have the same water type as the standard
      dplyr::filter(AUID %in% unique(dplyr::filter(input_sufficiency, `Waterbody Type` == water_type))$AUID_ATTNS) %>%
      dplyr::select(Group) %>%
      unique() %>%
      dplyr::pull()


    for (g in groups) {
      counter <- counter + 1
      group_meta <- turbidity_sites %>% dplyr::filter(Group == g)
      method <- unique(group_meta$Method)


      #Skip if there are no lake AUs present in the group and the WQS is lake-specific
      if(grepl("lake", wqs_row$Details, ignore.case = T) == T &
         any(!group_meta$AUID %in% lake_aus)) {
        next
      }

      #QA check
      if (length(method) != 1) stop(paste("Group", g, "has conflicting methods"))

      #Pull test site(s)
      test_sites <- group_meta %>%
        dplyr::filter(Role == "Test") %>% dplyr::pull(SiteID)

      #Pull reference site(s)
      ref_sites <- group_meta %>%
        dplyr::filter(Role == "Reference") %>% dplyr::pull(SiteID)

      #Pull test site(s) samples
      test_df <- input_samples_filtered %>%
        dplyr::filter(tolower(MonitoringLocationIdentifier) %in% tolower(test_sites)) %>%
        #remove Min, Max, Med, Daily min, & daily max
        dplyr::filter(is.na(StatisticalBaseCode) | StatisticalBaseCode == 'Mean') %>%
        dplyr::group_by(ActivityStartDate) %>%
        dplyr::summarise(daily_avg = mean(TADA.ResultMeasureValue, na.rm = TRUE), .groups = "drop")

      #Pull reference site(s) samples
      ref_df <- input_samples_filtered %>%
        dplyr::filter(tolower(MonitoringLocationIdentifier) %in% tolower(ref_sites)) %>%
        #remove Min, Max, Med, Daily min, & daily max
        dplyr::filter(is.na(StatisticalBaseCode) | StatisticalBaseCode == 'Mean') %>%
        dplyr::group_by(ActivityStartDate) %>%
        dplyr::summarise(daily_avg = mean(TADA.ResultMeasureValue, na.rm = TRUE), .groups = "drop")


      #If there's no samples - return insufficient data
      if(nrow(test_df) == 0 | nrow(ref_df) == 0){

        wqs_row$AUID_ATTNS <- group_meta %>% dplyr::filter(Role == 'Test') %>%
          dplyr::select(AUID) %>% unique() %>% dplyr::pull()
        wqs_row$Group <- g
        wqs_row$Method <- method
        wqs_row$Threshold <- NA
        wqs_row$Pctl90 <- NA
        wqs_row$LCL90 <- NA
        wqs_row$Exceed <- "Insufficient Data"
        wqs_row$Exceed_Num <- NA
        wqs_row$Exceed_Freq <- NA
        wqs_row$Notes <- paste0(method, ' - Group', g)

        next
      }

      #Calculate dynamic threshold if percentage-based
      ref_mean <- mean(ref_df$daily_avg, na.rm = TRUE)

      #Skip if the reference sites is less than 50 NTU and the WQS is for
      #natural condition is over 50 NTU
      if(ref_mean <= 50 &
         (grepl("natural condition is more than 50 NTU", wqs_row$Details, ignore.case = T) == T |
          grepl("natural turbidity is greater than 50 NTU", wqs_row$Details, ignore.case = T) == T)
      ){next}

      #Skip if the reference sites is more than 50 NTU and the WQS is for
      #natural condition is under 50 NTU
      if(ref_mean >= 50 &
         grepl("natural turbidity is 50 NTU or less", wqs_row$Details, ignore.case = T) == T)
      {next}


      #Calculate Distribution of Differences
      if (method == "DoD") {

        #Log-transform the daily average turbidity values for test and reference sites
        test_log <- log(test_df$daily_avg)
        ref_log <- log(ref_df$daily_avg)

        #Calculate the mean difference between test and reference log-transformed turbidity
        diff_mean <- mean(test_log) - mean(ref_log)

        #Calculate the standard deviation of the difference using variance and sample size
        #Variance = st.dev^2 so can cimplify this eq
        diff_sd <- sqrt(var(test_log)/length(test_log) + var(ref_log)/length(ref_log))

        #Get the z-score for the 90th percentile (used to calculate the 90th percentile of the distribution)
        z90 <- qnorm(0.9)

        #Compute the 90th percentile of the difference distribution (back-transformed from log scale)
        pctl90 <- exp(diff_mean + z90 * diff_sd)

        #Get z-score for lower confidence limit (LCL), alpha = 0.1 one-sided
        z_alpha <- qnorm(0.9 - 0.1)

        #Estimate standard error for the percentile using binomial proportion error formula
        se_pct <- sqrt((0.9*0.1)/length(test_log) + (0.9*0.1)/length(ref_log))

        #Calculate the adjusted percentile corresponding to the LCL
        lcl_pctile <- 0.9 - z_alpha * se_pct

        #Convert the LCL from log units to NTU by applying inverse log transformation
        lcl_val <- exp(qnorm(lcl_pctile, mean = diff_mean, sd = diff_sd))

        #If the LCL is greater than the threshold, it's considered an exceedance (impaired)
        result <- ifelse(lcl_val > threshold, "Yes", "No")

        wqs_row$AUID_ATTNS <- group_meta %>%
          dplyr::filter(Role == 'Test') %>%
          dplyr::select(AUID) %>%
          unique() %>%
          dplyr::pull()

        wqs_row$Group <- g
        wqs_row$Method <- method
        wqs_row$Threshold <- threshold
        wqs_row$Pctl90 <- pctl90
        wqs_row$LCL90 <- lcl_val
        wqs_row$Exceed <- result
        wqs_row$Exceed_Num <- NA
        wqs_row$Exceed_Freq <- NA
        wqs_row$Notes <- paste0(method, ' - Group', g)


      } else if (method == "Avg") {

        date_col <- "ActivityStartDate"

        #Pair test/ref by day (only days with both values)
        daily_pairs <- test_df %>%
          dplyr::select(!!date_col, test_daily_avg = daily_avg) %>%
          dplyr::inner_join(ref_df %>% dplyr::select(!!date_col, ref_daily_avg = daily_avg),
                            by = date_col) %>%
          dplyr::filter(!is.na(test_daily_avg), !is.na(ref_daily_avg))

        #If no paired days, mark as insufficient and move on
        if (nrow(daily_pairs) == 0) {
          wqs_row$AUID_ATTNS <- group_meta %>%
            dplyr::filter(Role == "Test") %>%
            dplyr::select(AUID) %>%
            unique() %>%
            dplyr::pull()

          wqs_row$Group <- g
          wqs_row$Method <- method
          wqs_row$Threshold <- threshold
          wqs_row$Pctl90 <- NA
          wqs_row$LCL90 <- NA

          wqs_row$Exceed <- NA
          wqs_row$Exceed_Num <- NA
          wqs_row$Exceed_Freq <- NA
          wqs_row$Notes <- paste0(method, " - Group", g, " (no paired test/ref days)")
        } else {

          #Day-specific threshold + exceedance check
          if (threshold_unit == "Percent") {
            #daily threshold is ref * (1 + pct)
            daily_pairs <- daily_pairs %>%
              dplyr::mutate(allowed_increase = ref_daily_avg * (threshold / 100),
                            threshold_day = ref_daily_avg + allowed_increase,
                            exceed_day = test_daily_avg > threshold_day)
          } else {
            #Fixed NTU criteria: ref_daily_avg + threshold (e.g., +5 NTU)
            daily_pairs <- daily_pairs %>%
              dplyr::mutate(threshold_day = ref_daily_avg + threshold,
                            exceed_day = test_daily_avg > threshold_day)
          }

          exceed_num <- sum(daily_pairs$exceed_day, na.rm = TRUE)
          n_days <- nrow(daily_pairs)
          exceed_freq <- exceed_num / n_days

          #Decide overall exceedance.
          result <- ifelse(exceed_num > 0, "Yes", "No")

          wqs_row$AUID_ATTNS <- group_meta %>%
            dplyr::filter(Role == "Test") %>%
            dplyr::select(AUID) %>%
            unique() %>%
            dplyr::pull()

          wqs_row$Group <- g
          wqs_row$Method <- method

          #For Avg, "Threshold" is not a single constant anymore; keep the criteria
          wqs_row$Threshold <- NA
          wqs_row$Pctl90 <- NA
          wqs_row$LCL90 <- NA
          wqs_row$Exceed <- result
          wqs_row$Exceed_Num <- exceed_num
          wqs_row$Exceed_Freq <- exceed_freq
          wqs_row$Notes <- paste0(method, " - Group", g)

        }

      }
      result_list[[counter]] <- wqs_row
    }
  }

  # Combine all group results
  df_loop_results <- dplyr::bind_rows(result_list)
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>%
    dplyr::distinct()

  #combine with relevant WQS table, removing the constituents that are calculated in other functions
  #these constituents come back in the hardness, pH, and turbidity specific functions
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName == 'TURBIDITY')

  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName',
                                           'Use', 'Use Description', 'Waterbody Type', #DEC added Use Description
                                           'Fraction', 'Type', 'Constituent Group'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(c(Exceed_Num, Exceed_Freq, Exceed), .after = last_col()) %>%
    dplyr::select(!c(Magnitude_Text, Group, Method, Threshold, Pctl90, LCL90))

  return(data_suff_WQS)
} #End of turbidity function
