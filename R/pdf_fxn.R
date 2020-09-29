#' PNG figure funtion

#' @param wd location to save figure
#' @param file plot name
#' @param w figure width
#' @param h figure height in inches
#' @param pt point size
#' @authorwritten by Chantel Wetzel
pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}