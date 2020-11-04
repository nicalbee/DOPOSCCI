#' Trim the ends of the fTCD data
#'
#' @param dop list DOPOSCCI list/stucture holding settings
#'
#' @return dop list Updated DOPOSCCI list
#' @export
#'
#' @examples
dopTrim <- function(dop){
  dop$filt <- list('definition' = 'collection of filters/screening used at different processing stages:')
  # dop$filt$trim <- vector(mode = 'logical', length = dim(use$data)[1])
  dop$filt$trim <- dop$data$use$time >= dop$epoch$epochs[[1]][1] &  dop$data$use$time <= dop$epoch$epochs[[dop$epoch$n]][2]

  dop$data$use <- dop$data$use[dop$filt$trim, ]
  dop
}
