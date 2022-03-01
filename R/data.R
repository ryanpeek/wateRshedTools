#' @importFrom tibble tibble
NULL

#' California Water Year Types.
#'
#' Water Year Types from the CDEC website for San Joaquin and Sacramento Indices. See \url{http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST}
#'
#' @format A data frame with seven variables:
#' \describe{
#'   \item{WY}{the water year}
#'   \item{sv_Oct_Mar}{the index for Oct through Mar for sacramento valley}
#'   \item{sv_Apr_Jul}{the index for Apr through Jul for sacramento valley}
#'   \item{sv_WYsum}{the sum of Oct_Mar + Apr_Jul for sacramento valley}
#'   \item{sv_Index}{The indexed water year value for sacramento valley}
#'   \item{sv_WYtype}{the water year type (Wet, Above Normal, Below Normal, Dry, Critically Dry for sacramento valley)}
#'   \item{sj_Oct_Mar}{the index for Oct through Mar for san joaquin}
#'   \item{sj_Apr_Jul}{the index for Apr through Jul for san joaquin}
#'   \item{sj_WYsum}{the sum of Oct_Mar + Apr_Jul for san joaquin}
#'   \item{sj_Index}{The indexed water year value for san joaquin}
#'   \item{sj_WYtype}{the water year type (Wet, Above Normal, Below Normal, Dry, Critically Dry for san joaquin)}
#' }
#' @source \url{http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST}
"ca_wytypes"


#' River Runoff from 8-station index.
#'
#' The 8-station index for CA river runoff.
#'
#' @format A data frame with 115 rows (updated annually) and 7 variables:
#' \describe{
#'   \item{WY}{the water year}
#'   \item{Dec}{The 8 station index for Dec of a given water year}
#'   \item{Jan}{The 8 station index for Jan of a given water year}
#'   \item{Feb}{The 8 station index for Feb of a given water year}
#'   \item{Mar}{The 8 station index for Mar of a given water year}
#'   \item{Apr}{The 8 station index for Apr of a given water year}
#'   \item{May}{The 8 station index for May of a given water year}
#' }
#' @source \url{https://cdec.water.ca.gov/reportapp/javareports?name=PLOT_ESI.pdf}
"riv8_runoff"
