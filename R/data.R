#' Dry deposition parameters
#'
#' Dry deposition parameters as used by the Wesely 1989 parameterisation.
#'
#' @references https://doi.org/10.1016/0004-6981(89)90153-4
#'
#' @format A list of 5 data.frames with variables given for 13 land cover types. Each
#' 	data.frame represents a different part of the growing season and contains columns:
#' \describe{
#'   \item{lu}{land use index}
#'   \item{r.min}{minimal stomatal resistance}
#'   \item{r.cut.0}{base cuticle resistance}
#'   \item{r.canp}{lower canopy resistance}
#'   \item{r.soil.SO2}{soil resistance for SO2}
#'   \item{r.soil.O3}{soil resistance for O3}
#'   \item{r.surf.SO2}{exposed surface resistance for SO2}
#'   \item{r.surf.O3}{exposed surface resistance for O3}
#'   \item{season}{season index; same as list index}
#' }
#' Seasons are: 
#' \describe{ 
#' 	\item{1}{midsummer with lush vegetation}
#'  \item{2}{autumn with unharvested cropland}
#'  \item{3}{late autumn after frost, no snow}
#'  \item{4}{winter, snow on ground and subfreezing}
#'  \item{5}{transitional spring with partially green short annuals}
#' }
#' Land cover types are:
#' \describe{
#'	\item{1}{Urban land}
#'	\item{2}{agricultural land}
#'	\item{3}{range land}
#'	\item{4}{deciduous forest}
#'	\item{5}{coniferous forest}
#'	\item{6}{mixed forest including wetland}
#'	\item{7}{water, both salt and fresh}
#'	\item{8}{barren land, mostly desert}
#'	\item{9}{non-forested wetland}
#'	\item{10}{mixed agricultural and range land}
#'	\item{11}{rocky open areas with low-growing shrubs}
#'	\item{12}{snow; added in FLEXPART Stohl et al. 2005}
#'	\item{13}{rainforest; added in FLEXPART Stohl et al. 2005}
#' }
"dry.depo.para"

#' Frequency of dispersion category
#'
#' Frequency of wind-speed, wind-direction, stability categories for the MeteoSwiss site
#' Reckenholz and the year 2015.
#' @format data.frame containing fields:
#' \describe{
#'   \item{freq}{frequency of dispersion category}
#'   \item{WS}{Wind speed, central value in units m s-1.}
#'   \item{WD}{Wind direction, central value.}
#'   \item{stability}{Pasquill stability category}
#'   \item{h.m}{mixing layer height; all values NA as not measured; can be set for supplying data.frame to 'average.gauss.plume.from.freq'}
#' }
"reh.freq"

#' Time series of observed meteorology and stability category
#'
#' Time series of meteorological observations and stability categories for the MeteoSwiss site
#' Reckenholz with hourly resolution and for the year 2015.
#' @format data.frame containing fields:
#' \describe{
#'   \item{dtm}{date/time chron object}
#'   \item{WS}{Wind speed in units m s-1.}
#'   \item{WD}{Wind direction, central value.}
#'   \item{stability}{Pasquill stability category}
#'   \item{TT}{2m ambient temperature in degree C}
#'   \item{RH}{2m relative humidity in \%}
#' }
"reh.ts"
