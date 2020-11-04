#' Import functional transcranial Doppler ultrasound (fTCD) data: Tari system
#'
#' @param file_name character File name or path/file name to fTCD file
#'
#' @return df dataframe Data frame on imported data
#' @export
#'
#' @examples
dopReadTari <- function(file_name){

  # ----------------------------------------------------------------------------
  # Important information + list/structure variables:
  # previously used 'im.xxx' to define important information about I'm switching
  # to lists and 'in' is a reserved function/label
  # im <- list('definition' = 'Import settings/information:')
  # tmp <- list('definition' = 'Temporary variables:')
  # use <- list('definition' = 'Working variables - currently in use:')

  # ----------------------------------------------------------------------------
  # import the data

  # im.data <- read.table(im.fullfile)
  # read.table doesn't work as there are blocks of information (variable name: data)
  # at the top and varying intervals in the file, and then the column data.
  # Need to try another strategy - readLines will be slower but it'll get us started.
  im.lines <- readLines(file_name)

  # find the first and last lines in the information blocks
  im.block_start_lines <- grep('Interval',im.lines)
  im.block_end_lines <- grep('BottomValue',im.lines)

  # loop through and pull out the information
  for (i in 1 : length(im.block_start_lines)){
    # ----------------------------------------------------------------------------
    # get the information between the start and end of each block
    tmp.block_range <- c(im.block_start_lines[i],im.block_end_lines[i])
    tmp.block <- im.lines[tmp.block_range[1]:tmp.block_range[2]]
    # tmp.block looks something like:
    # [1] "Interval=\t0.01 s"
    # [2] "ExcelDateTime=\t4.3788633901988404e+004\t19/11/2019 15:12:49.131798"
    # [3] "TimeFormat=\tStartOfBlock"
    # [4] "DateFormat=\t"
    # [5] "ChannelTitle=\tUltrasound"
    # [6] "Range=\t10.000 V"
    # [7] "UnitName=\tcm/s"
    # [8] "TopValue=\t1499.27"
    # [9] "BottomValue=\t-1660.81"

    # I think we could add this as columns to the data, for safe keeping
    # probably won't use it but might as well hang onto it

    # let's grab the data to add to it
    tmp.data_range <- im.block_end_lines[i]+1
    if (i < length(im.block_start_lines)){
      tmp.data_range[2] <- im.block_start_lines[i+1]-1
    } else {
      tmp.data_range[2] <- length(im.lines)
    }
    tmp.data <- im.lines[tmp.data_range[1]:tmp.data_range[2]]

    # ----------------------------------------------------------------------------
    # the events are marked with #*
    # these are also tab delimited but not in every line, so the read.delim
    # function drops them from the table !! No it doesn't!! They become another row - so need to remove them
    # make a copy and add them after as their own column
    tmp.events <- grep('\t#\\*',tmp.data) # row numbers
    tmp.data2cols <- gsub('\t#\\*[a-z]?','',tmp.data) # [a-z] gets the letters on the end
    # would be ideal to keep this information...
    tmp.event_data <- sub('.*\t\\#','',tmp.data[tmp.events])
    # pop the data into a table
    tmp.data_table <- read.delim(text = tmp.data2cols, header = F, sep = '\t')
    tmp.data_table2 <- read.delim(text = tmp.data, header = F, sep = '\t')
    # add the column names
    colnames(tmp.data_table) <- c('time','velocity')

    tmp.data_table$event <- rep(0,dim(tmp.data_table)[1]) # create an empty array of zeros
    tmp.data_table$event[tmp.events] <- 1 # set the event marker values to 1

    # create an empty array of spaces - NAs probably better but harder to reader
    tmp.data_table$event_str <- rep(NA,dim(tmp.data_table)[1])
    tmp.data_table$event_str[tmp.events] <- tmp.event_data # set the event marker values to 1

    # ----------------------------------------------------------------------------
    # Now get the block information:
    # this doesn't work as the ExcelDateTime row has two values in it - 2 tabs
    # tmp.block_table <- read.table(text = tmp.block, header = F, sep = '\t')
    #
    # could do it with a loop, line by line, adding as column to the data block
    # let's pull them out first:
    for (j in 1 : length(tmp.block)){
      # tmp.var_name <- strsplit(tmp.block[j],"=")
      tmp.row_separated <- strsplit(tmp.block[j],"\t")

      tmp.var_name <- tmp.row_separated[[1]][1]
      # remove the = sign at the end
      tmp.var_name <- substr(tmp.var_name, 1, nchar(tmp.var_name)-1)
      # add it to the tmp.data_table
      eval(parse(text = paste0(
        'tmp.data_table$',tmp.var_name, ' = rep("',tmp.row_separated[[1]][2],
        '", length = ',dim(tmp.data_table[1]),')')))
      if (length(tmp.row_separated[[1]]) > 2){
        eval(parse(text = paste0(
          'tmp.data_table$',tmp.var_name, '2 = rep("',tmp.row_separated[[1]][3],
          '", length = ',dim(tmp.data_table[1]),')')))
      }

    }
    # ----------------------------------------------------------------------------
    # Now concatenate the data
    if (i == 1){ #(!exists("use.data")){ #
      df <- tmp.data_table
    } else {
      # bind the rows - essentially stacking the data
      df <- rbind(df, tmp.data_table)
    }
  }
  df
}
