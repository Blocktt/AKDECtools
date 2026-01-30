#' Categorize Assessment Units (AUs) by Use
#'
#' The categorize_AU function assigns each AU/characteristic combination to an
#' IR category and then assigns an overall designated use category to the AU
#' for every applicable use.
#'
#' @param MagDurFreq_Results Final results from MagDurFreq_combine function.
#' @param assessments Assessment data from ATTAINS expert query found at https://owapps.epa.gov/expertquery/attains/
#' @param simplify_standards True/False input to condense final category for constituents
#' with multiple standards per use.
#' @examples
#' # Example, data from Excel
#'\dontrun{
#' # Packages
#' library(readr) # readr is a tidyverse package to read CSVs
#' library(AKDECtools)
#'
#' df_MagDurFreq_Results <- read_csv(system.file("extdata/AK_Final_MagDurFreq_Output.csv"
#'                                        , package = "AKDECtools")
#'                                        , guess_max = 10^6)
#'
#' df_Assessments <- read_csv(system.file("extdata/AK_Assessments.csv"
#'                                        , package = "AKDECtools")
#'                                        , guess_max = 10^6)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#'
#' categorize_AU_uses(df_MagDurFreq_Results, df_Assessments, simplify_standards = F)
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return Categorized AUs for each use
#' @export
#'
  categorize_AU_uses <- function(MagDurFreq_Results, assessments, simplify_standards){

    #Go to ATTAINS expert query. Choose "Assessment". filter for "AK". Download. https://owapps.epa.gov/expertquery/attains/
    assessments <- assessments %>%
      dplyr::select(assessmentUnitId,
             parameterName,
             useName,
             parameterStatus,
             parameterAttainment) %>%
      dplyr::mutate(parameterName = ifelse(parameterName == 'DISSOLVED OXYGEN', "DISSOLVED OXYGEN (DO)", parameterName)) %>%
      dplyr::mutate(parameterName = ifelse(parameterName == 'ESCHERICHIA COLI (E. COLI)', "ESCHERICHIA COLI", parameterName)) %>%
      dplyr::mutate(parameterName = ifelse(parameterName == 'TOTAL DISSOLVED SOLIDS (TDS)', "TOTAL DISSOLVED SOLIDS", parameterName))

    #Don't want to overwrite previous ATTAINs 2s and 5s with new 3s
    merge_uses <- MagDurFreq_Results %>%
      dplyr::mutate(ATTAINS_waterbody = dplyr::case_when(`Waterbody Type` == 'Freshwater' ~
                                             'FRESH WATER',
                                           `Waterbody Type` == 'Freshwater streams and rivers' ~
                                             'FRESH WATER',
                                           T ~ 'MARINE WATER'),
             ATTAINS_USE_merge = paste0(ATTAINS_waterbody, ' / ',  Use, ' / ', `Use Description`),
             ATTAINS_USE_merge = gsub(" / NA", "", ATTAINS_USE_merge)) %>%
      dplyr::left_join(assessments, by = c('AUID_ATTNS' = 'assessmentUnitId',
                                    'TADA.CharacteristicName' = 'parameterName',
                                    'ATTAINS_USE_merge' = 'useName')) %>%
      dplyr::mutate(PARAM_ATTAINMENT_CODE_new = dplyr::case_when(parameterAttainment == "Not meeting criteria" ~
                                                     '5',
                                                   parameterAttainment == "Meeting criteria" ~
                                                     '2',
                                                   parameterAttainment == "Not enough information" ~
                                                     '3',
                                                   T ~
                                                     NA))

    calc_individual <- merge_uses %>%
      dplyr::mutate(Exceed = ifelse(is.na(Exceed) == T, 'N/A', Exceed)) %>%
      dplyr::filter(Exceed != 'Requires manual analysis') %>%
      dplyr::filter(Exceed != 'AU not lake waters') %>%
      dplyr::filter(Exceed != 'Natural conditions less than or equal to 50 NTU') %>%
      dplyr::mutate(Individual_Category = dplyr::case_when(is.na(Data_Sufficient) ~ NA,
                                                    (PARAM_ATTAINMENT_CODE_new != '3' &
                                                       Data_Sufficient == 'No') ~
                                                      PARAM_ATTAINMENT_CODE_new,
                                                    Data_Sufficient == "No" ~ '3',
                                                    Exceed == 'Yes' ~ '5',
                                                    Exceed == 'No' ~ '2',
                                                    Exceed == 'Insufficient hardness' ~ '3',
                                                    Exceed == 'Insufficient dependent data' ~ '3',
                                                    Exceed == 'Admin cat 3' ~ '3',
                                                    T ~ NA))

    if(simplify_standards == T){

      mid_step <- calc_individual %>%
        dplyr::group_by(AUID_ATTNS, Use, `Use Description`, TADA.CharacteristicName) %>%
        dplyr::mutate(n = dplyr::n(),
                      is_2 = sum(ifelse(Individual_Category == '2', 1, 0)),
                      is_3 = sum(ifelse(Individual_Category == '3', 1, 0)),
                      is_5 = sum(ifelse(Individual_Category == '5', 1, 0)),
                      #If n > 1, choose worse category
                      new_Individual_Category = dplyr::case_when(n > 1 & is_5 == 1 ~
                                                            '5',
                                                          n > 1 & is_5 == 0 & is_2 > 0 ~
                                                            '2',
                                                          T ~ Individual_Category)) %>%
        dplyr::filter(Individual_Category == new_Individual_Category) %>%
        dplyr::select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
        dplyr::rename(Individual_Category = new_Individual_Category)


    } else {
      mid_step <- calc_individual
    }


    calc_overall <- mid_step %>%
      dplyr::group_by(AUID_ATTNS, Use, `Use Description`) %>%
      dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
                    cat_2_present = length(Individual_Category[Individual_Category=='2']),
                    Use_Category = dplyr::case_when(cat_5_present > 0 ~ '5',
                                             cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                             cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                             T~ NA)) %>%
      dplyr::select(!c(cat_5_present, cat_2_present)) %>%
      dplyr::arrange(AUID_ATTNS, Use)

    return(calc_overall)

  }
