#' Import functional transcranial Doppler ultrasound (fTCD) data
#'
#' dopRead acts as a switch to pass a file name to an fTCD-system specific import function
#'
#'
#'
#' @param file_name character File name or path/file name to fTCD file
#' @param fTCD_system character The fTCD recording system/hardware. Options: "DWL", "Tari"
#'
#' @return df = dataframe of imported data
#' @export
#'
#' @examples
#'

# 2020-11-03 NAB not sure how to test this without data files which "aren't portable"
# df <- dopRead('myfile.exp','DWL')
# df <- dopRead('myfile.txt','Tari')
dopRead <- function(file_name,fTCD_system){

print(paste0("Attempting to import: ",file_name))

  switch(fTCD_system,
         DWL={
           # case 'DWL' here...
           print("Importing DWL '.exp' file")
           Ls <- c("D","W","L") #LETTERS[1:3]
           fac <- sample(Ls, 10, replace = TRUE)
           (d <- data.frame(x = 1, y = 1:10, fac = fac))
           ## The "same" with automatic column names:
           df <- data.frame(1, 1:10, sample(Ls, 10, replace = TRUE))
         },
         Tari={
           # case 'Tari' here...
           print("Importing Tari '.txt' file")
           # Ls <- c("T","A","R","I") #LETTERS[1:3]
           # fac <- sample(Ls, 10, replace = TRUE)
           # (d <- data.frame(x = 1, y = 1:10, fac = fac))
           # ## The "same" with automatic column names:
           # df <- data.frame(1, 1:10, sample(Ls, 10, replace = TRUE))
           df <- dopReadTari(file_name)
         },
         {
           print('System not found. Please contact author.')
           Ls <- c("E","M","P","T","Y") #LETTERS[1:3]
           fac <- sample(Ls, 10, replace = TRUE)
           (d <- data.frame(x = 1, y = 1:10, fac = fac))
           ## The "same" with automatic column names:
           df <- data.frame(1, 1:10, sample(Ls, 10, replace = TRUE))
         }
  )
}
