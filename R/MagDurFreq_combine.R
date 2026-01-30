#' Combine MagDurFreq results
#'
#' The MagDurFreq_combine function combines output data from the MagDurFreq
#' , MagDurFreq_hardness, MagDurFreq_pH, MagDurFreq_turbidity, and MagDurFreq_pathogens functions
#' into a single data frame.
#'
#' @param standard_output Output from MagDurFreq function.
#' @param hardness_output Output from MagDurFreq_hardness function.
#' @param pH_output Output from MagDurFreq_pH function.
#' @param turbidity_output Output from MagDurFreq_turbidity function.
#' @param pathogens_output Output from MagDurFreq_pathogens function.
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
#' df_Turbidity_Sites <- read_csv(system.file("extdata/turbidity_sites.csv"
#'                                        , package = "AKDECtools")
#'                                        , guess_max = 10^6)
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 1
#' # Filter samples to only those with sufficient data to make MagDurFreq
#' ## run more quickly
#'
#' input_samples_filtered <- filterCat3samples(data_samples = df_ExampSamps
#' , data_sufficiency = df_Data_Sufficiency)
#'
#' # run MagDurFreq
#' standard_output <- MagDurFreq(df_WQS_Crosswalk, input_samples_filtered,
#'  df_Data_Sufficiency)
#'
#' # run MagDurFreq_pH
#' pH_output <- MagDurFreq_pH(df_WQS_Crosswalk, df_ExampSamps, input_samples_filtered
#' , df_Data_Sufficiency)
#'
#' # run MagDurFreq_hardness
#' hardness_output <- MagDurFreq_hardness(df_WQS_Crosswalk, df_ExampSamps
#' , input_samples_filtered, df_Data_Sufficiency)
#'
#' # run MagDurFreq_turbidity
#' turbidity_output <- MagDurFreq_turbidity(df_WQS_Crosswalk, input_samples_filtered
#' , df_Data_Sufficiency, df_Tubidity_Sites)
#'
#'# run MagDurFreq_pathogens
#' pathogens_output <-  MagDurFreq_pathogens(df_WQS_Crosswalk, input_samples_filtered
#' , df_Data_Sufficiency)
#'
#' # combine data
#' MagDurFreq_combine(standard_output, pH_output, hardness_output, turbidity_output, pathogens_output)
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A combined dataset with MagDurFreq results
#' @export
#'
MagDurFreq_combine <- function(standard_output, hardness_output, pH_output, turbidity_output, pathogens_output) {
  output <- standard_output %>%
    rbind(hardness_output) %>%
    rbind(pH_output) %>%
    rbind(pathogens_output) %>%
    rbind(turbidity_output) %>% #BINOMIAL TEST
    dplyr::mutate(`Constituent Group` = ifelse(
      is.na(`Constituent Group`),
      dplyr::first(stats::na.omit(`Constituent Group`)),
      `Constituent Group`),
      Binomial_Value = dplyr::case_when(`Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples < 10 ~
                                          1,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 10 & n_Samples <= 18 ~
                                          2,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 19 & n_Samples <= 22 ~
                                          3,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 23 & n_Samples <= 35 ~
                                          4,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 36 & n_Samples <= 49 ~
                                          5,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 50 & n_Samples <= 63 ~
                                          6,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 64 & n_Samples <= 78 ~
                                          7,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 79 & n_Samples <= 92 ~
                                          8,
                                        `Constituent Group` == 'Toxics' & Use == 'GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE' &
                                          Type == 'Chronic' & n_Samples >= 93 ~
                                          9,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples < 10 ~
                                          1,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 10 & n_Samples <= 11 ~
                                          2,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 12 & n_Samples <= 18 ~
                                          4,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 19 & n_Samples <= 25 ~
                                          5,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 26 & n_Samples <= 32 ~
                                          6,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 33 & n_Samples <= 40 ~
                                          7,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 41 & n_Samples <= 47 ~
                                          8,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 48 & n_Samples <= 55 ~
                                          9,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 56 & n_Samples <= 63 ~
                                          10,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 64 & n_Samples <= 71 ~
                                          11,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 72 & n_Samples <= 79 ~
                                          12,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 80 & n_Samples <= 88 ~
                                          13,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 89 & n_Samples <= 96 ~
                                          14,
                                        !`Constituent Group` %in% c('Toxics', 'Turbidity', 'Petroleum Hydrocarbons', 'Bacteria') &
                                          n_Samples >= 97 ~
                                          15,
                                        T ~ NA),
      Exceed_Binomial = dplyr::case_when(Exceed == 'Yes' & Exceed_Num >= Binomial_Value ~
                                           'Yes',
                                         Exceed == 'Yes' & Exceed_Num < Binomial_Value ~
                                           'No',
                                         T ~ Exceed))



  return(output)
}
