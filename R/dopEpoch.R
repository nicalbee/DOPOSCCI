#' Epoch continuous fTCD data
#' 
#' Examines the event channel to create a matrix of sample by epoch sized matrix
#'
#' @param dop list DOPOSSCI list of settings + data etc.
#'
#' @return dop
#' @export
#'
#' @examples
dopEpoch <- function(dop){
  dop$epoch <- list('definition' = 'Dividing the data into trials/epochs:')
  # save a copy of the event times - these can be dropped with downsampling - if we downsample
  dop$epoch$event_secs <- dop$data$use$time[dop$data$use$event == 1]
  dop$epoch$n <- length(dop$epoch$event_secs)
  
  # create a list of pairs of numbers for each epoch, lower and upper limits
  # 'lapply' function with '+' adds the epoch array (lower and upper elements) to
  # each of the event markers (in seconds: event_secs variable)
  # note: divideing the seconds by the sample secs concerts the event seconds to samples
  dop$epoch$epochs <- lapply(dop$epoch$event_secs, '+', dop$set$epoch/dop$set$sample_secs)
  # now it's easy to trim off the unnecessary start and end values
  
  dop$epoch$latency <- seq(dop$set$epoch[1],dop$set$epoch[2],dop$set$sample_secs)
  dop$epoch$data <- matrix(nrow = length(dop$epoch$latency), ncol = dop$epoch$n)
  
  for (i in 1 : dop$epoch$n){
    dop$epoch$data[, i] <- dop$data$use$velocity[dop$epoch$epochs[[i]][1]:dop$epoch$epochs[[i]][2]+1]
    # plus 1 on the end here to account for the zero, could just as easily be -1 from the other side
  }
  dop
}
