#' Combine MagDurFreq results
#'
#' The MagDurFreq_combine function combines output data from the MagDurFreq
#' , MagDurFreq_hardness, MagDurFreq_pH, and MagDurFreq_turbidity functions
#' into a single data frame.
#'
#' @param standard_output Output from MagDurFreq function.
#' @param hardness_output Output from MagDurFreq_hardness function.
#' @param pH_output Output from MagDurFreq_pH function.
#' @param turbidity_output Output from MagDurFreq_turbidity function.
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
#' df_reference_sites <- tibble(AUID_ATTNS = au_sites, ReferenceSites = sites)
#'
#' turbidity_output <- MagDurFreq_turbidity(df_WQS_Crosswalk, input_samples_filtered
#' , df_Data_Sufficiency, df_reference_sites)
#'
#' # combine data
#' MagDurFreq_combine(standard_output, pH_output, hardness_output, turbidity_output)
#'
#'
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' }
#'
#' @return A combined dataset with MagDurFreq results
#' @export
#'
MagDurFreq_combine <- function(standard_output, hardness_output, pH_output
                               , turbidity_output) {
  output <- standard_output %>%
    rbind(hardness_output) %>%
    rbind(pH_output) %>%
    rbind(turbidity_output)

  return(output)
} # end MagDurFreq_combine
