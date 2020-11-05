#' Baseline correct
#'
#' Subtract the average of a baseline period from each waveform.
#' "dop" variable is returned with dop$epoch$base which is the baseline-corrected data.
#'
#' @param df data.frame Long or wide epoched data (see dopEpoch)
#' @param baseline numeric Lower and Upper limits of baseline period in seconds
#'
#' @return df
#' @export
#'
#' @examples
dopBaseCorrect <- function(df, baseline){
  
  
  
  if ("epoch" %in% colnames(df)){ # long
    tmp.chs <- unique(df$channel)
    for (i in 1 : length(tmp.chs)){
      filt.ch <- df$channel == tmp.chs[i]
      tmp.df <- df[, !(names(df) %in% 'channel')]
      df.wide <- reshape(tmp.df[filt.ch,], direction = "wide", idvar = "latency", timevar = c("epoch"))
      
      filt.baseline <- df.wide$latency >= baseline[1] & df.wide$latency <= baseline[2]
      
      tmp.cols <- 2:dim(df.wide)[2]
      tmp.baselines <- colMeans(df.wide[filt.baseline, tmp.cols])
      
      df.wide <- cbind(df.wide[,'latency'],sweep(df.wide[, tmp.cols],2,tmp.baselines))
      
      df.long <- reshape(df.wide, direction = "long", timevar = "epoch",
                         v.names = c("data"), varying = names(df.wide)[tmp.cols],
                         times = names(df.wide)[tmp.cols])
      
      df.long <- df.long[, !(names(df.long) %in% 'id')]
      
      rownames(df.long) <- NULL
      
      # rename the latency column back to latency...
      tmp.colnames <- colnames(df.long)
      tmp.colnames[1] = "latency"
      colnames(df.long) = tmp.colnames
      
      # remove the 'data' bit from the epoch variable:
      df.long[,'epoch'] <- gsub('data.','',df.long[,'epoch'])
      
      # put the baseline corrected data back into the long data.frame
      df[filt.ch,] <- df.long
    }
  } else { #wide
    filt.baseline <- df$latency >= baseline[1] & df$latency <= baseline[2]
    tmp.baselines <- colMeans(df[filt.baseline, 2: dim(df)[2]])
    # subtract the baselines from each column
    df[ , 2:dim(df)[2]] <- sweep(df[ , 2:dim(df)[2]],2,tmp.baselines)
  }
  df
}
