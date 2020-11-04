#' Correct for the Heart Cycle/Heart Best in fTCD signal
#'
#' The raw fTCD signal include heart beats which are best smoothed over for most fTCD processing
#'
#' The idea comes from Deppe et al. (1997)
#'
#' References
#' Deppe, M., Knecht, S., Henningsen, H., & Ringelstein, E. B. (1997). AVERAGE: A Windows® program for automated analysis of event related cerebral blood flow. Journal of Neuroscience Methods, 75(2), 147–154. https://doi.org/10.1016/S0165-0270(97)00067-8
#'
#' @param dop list
#' @param heart_cycle_window numeric
#' @param channel_name character
#'
#' @return dop list
#' @export
#'
#' @examples
dopHeartCycle <- function(dop, heart_cycle_window = .26, channel_name = 'velocity'){
  
  tmp.data <- as.numeric(dop$data$use[,channel_name])
  
  tmp.range <- round(heart_cycle_window/(1/dop$set$sample_Hertz))
  
  # update: look for peaks by range
  tmp.count <- 0
  tmp.sample <- c() # systolic
  for (i in 2 : (length(tmp.data)-1-tmp.range)){
    if (tmp.count == 0){ # first peak, searching forward, not backward
        if (all(tmp.data[i] >= tmp.data[(i+1):(i+2+tmp.range)])){ # greater than next range
          tmp.count <- tmp.count + 1
          tmp.sample[tmp.count] <- i
        }
    } else if ((i-tmp.range) > 0){
      if ((all(tmp.data[i] >= tmp.data[(i-tmp.range):(i-1)])) & # greater than previous range
          (all(tmp.data[i] >= tmp.data[(i+1):(i+2+tmp.range)])) & # greater than next range
          ((i-max(tmp.sample) > tmp.range))){ # ensure that it's outside the acceptable range - find first peak
        tmp.count <- tmp.count + 1
        tmp.sample[tmp.count] <- i
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # check with a plot - uncomment to test
  # plot.range <- 1 : 500
  # plot(plot.range,tmp.data[plot.range], type = 'l')
  # plot.samples <- tmp.sample[which(tmp.sample > min(plot.range) & tmp.sample < max(plot.range))]
  # for (i in 1 : length(plot.samples)){
  #   lines(rep(plot.samples[i],2),c(0,100), type = 'l', col = 'red')
  # }
  
  # does a pretty reasonable job
  # but extreme values/dropouts will affect the average
  # though I guess we want this to be the case
  
  # ----------------------------------------------------------------------------
  # correction: calculate average between peaks
  # step by step = Deppe
  
  # could do this by creating epochs
  # tmp.epochs <- matrix(c(tmp.sample[1:(tmp.count-1)],tmp.sample[2:tmp.count]), nrow = (tmp.count-1), ncol = 2, byrow = F)
  tmp.epochs <- data.frame(lower = tmp.sample[1:(tmp.count-1)], upper = tmp.sample[2:tmp.count]-1) # minus 1 to avoid overlap
  tmp.epochs <- rbind(c(1,tmp.epochs[1,1]-1), # first one
                      tmp.epochs,
                      c(tmp.epochs[tmp.count-1,2]+1,length(tmp.data))) # last one
  colnames(tmp.epochs) <- c('lower','upper')
  # mean between each of these:
  out.data <- c()
  for (i in 1 : dim(tmp.epochs)[1]){
    tmp.range <- tmp.epochs$lower[i]:tmp.epochs$upper[i]
    tmp.epochs$mean[i] <- mean(tmp.data[tmp.range])
    out.data[tmp.range] <- tmp.epochs$mean[i]
  }
  # ----------------------------------------------------------------------------
  # check with a plot - uncomment to test
  # plot.range <- 1 : 500
  # plot(plot.range,tmp.data[plot.range], type = 'l')
  # lines(plot.range,out.data[plot.range], type = 'l', col = 'red')
  
  # ----------------------------------------------------------------------------
  # output
  dop$data$use$velocity <- out.data # replace this in current case
  dop
}
