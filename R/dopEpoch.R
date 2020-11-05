#' Epoch continuous fTCD data
#' 
#' Examines the event channel to create a matrix of sample by epoch sized matrix
#'
#' @param df data.frame 
#' @param data_channel character Character array of data channel/s
#' @param event_channel character Variable/column name of the event_channel
#' @param epoch numeric Number 1 x 2 array with lower and upper epoch limits
#' @param sample_Hertz numeric Sampling frequency of the recording in Hertz (i.e., 100 times per second)
#' @param rm.first logical True or False to remove the first event maker
#' @param rm.last logical True or False to remove the last event maker
#' @param expected_n numeric Number of expected epochs
#'
#' @return ep List of epoche information + epoched data
#' @export
#'
#' @examples

# Note: This could be simpler - not exporting a structure, just the data
dopEpoch <- function(df, data_channel, event_channel, epoch = c(-15,25), sample_Hertz = 100,
                     expected_n, rm.first = F, rm.last = F){
  ep <- list('definition' = 'Dividing the data into trials/epochs:')
  # save a copy of the event times - these can be dropped with downsampling - if we downsample
  ep$event_samples <- which(df[, event_channel] == 1)
  
  ep$sample_secs <- (1/sample_Hertz)
  
  # ------------------------------------------------------------------------------------
  # check if a regular event should be removed
  ep$n <- length(ep$event_samples)
  if (rm.first){
    ep$event_samples <- ep$event_samples[-1]
  }
  ep$n <- length(ep$event_samples)
  if (rm.last){
    ep$event_samples <- ep$event_samples[-ep$n]
  }
  ep$n <- length(ep$event_samples)
  
  # ------------------------------------------------------------------------------------
  # create a list of pairs of numbers for each epoch, lower and upper limits
  # 'lapply' function with '+' adds the epoch array (lower and upper elements) to
  # each of the event markers (in seconds: event_secs variable)
  # note: divideing the seconds by the sample secs concerts the event seconds to samples
  ep$epochs <- lapply(ep$event_samples, '+', epoch/ep$sample_secs)
  # now it's easy to trim off the unnecessary start and end values
  
  ep$latency <- seq(epoch[1],epoch[2],ep$sample_secs)
  
  # define an empty matrix for the data
  ep$epoch_samples <- length(ep$latency)
  
  ep$wide$colnames <- c('latency',rep('name',ep$n*length(data_channel)))
  ep$data$wide <- matrix(nrow = ep$epoch_samples, ncol = ep$n*length(data_channel))
  # note: plus 1 on the end here to account for the zero, could just as easily be -1 from the other side
  
  ep$long$rows <- (ep$epoch_samples)*ep$n*length(data_channel)
  ep$data$long <- data.frame(latency = rep(ep$latency,ep$long$rows/ep$epoch_samples),
                             data = rep(NA,ep$long$rows), epoch = rep(NA,ep$long$rows),
                             channel = rep(NA,ep$long$rows))
  
  for (i in 1 : ep$n){
    for (j in 1 : length(data_channel)){
      tmp.col <- i+(ep$n*(j-1))
    ep$wide$colnames[1+tmp.col] <- paste0('epoch',i,data_channel[j])
    ep$data$wide[, tmp.col] <- df[ep$epochs[[i]][1] : (ep$epochs[[i]][2]), data_channel[j]]
    
    ep$data$long[((1+(i-1)*ep$epoch_samples) : (i*ep$epoch_samples)), c(2:dim(ep$data$long)[2])] <-
      data.frame(df[ep$epochs[[i]][1]:(ep$epochs[[i]][2]), data_channel[j]],
            rep(i,ep$epoch_samples), rep(data_channel[j],ep$epoch_samples))
    }
  }
  ep$data$wide <- as.data.frame(cbind(ep$latency,ep$data$wide))
  colnames(ep$data$wide) <- ep$wide$colnames
  ep
}
