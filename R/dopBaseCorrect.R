#' Baseline correct
#'
#' Subtract the average of a baseline period from each waveform.
#' "dop" variable is returned with dop$epoch$base which is the baseline-corrected data.
#'
#' @param dop list DOPOSSCI list of settings + data etc.
#' @param baseline numeric Lower and Upper limits of baseline period in seconds
#'
#' @return dop
#' @export
#'
#' @examples
dopBaseCorrect <- function(dop, baseline){
  
  if (missing(baseline)){
    baseline <- dop$set$baseline
  } else {
    dop$def$baseline <- baseline
  }
  
  dop$filt$baseline <- dop$epoch$latency >= baseline[1] & dop$epoch$latency <= baseline[2]
  
  tmp.baselines <- colMeans(dop$epoch$data[dop$filt$baseline,])
  # subtract the baselines from each column
  dop$epoch$base <- sweep(dop$epoch$data,2,tmp.baselines)
  dop
}
