#' Get HUC Basins
#'
#' This function uses a spatial polygon/sf object to search and download the watersheds for a given area.
#' Based on USGS tools and using NHD Data.
#'
#' This function requires an internet connection, as well as the \code{httr} and \code{sf} packages.
#'
#' @examples
#'
#' # get data
#' yose <- data.frame(site=c("YOSEMITE","LOON") ,lon = c(-119.76865,-120.31862), lat = c(37.95748, 38.99134))
#' yose_sf <- sf::st_as_sf(yose, coords=c("lon", "lat"), crs=4326, remove=FALSE)
#' # now run function:
#'
#'
#' @param sites an sf formatted object
#' @param filePath a location to save the file, if specified
#'
#' @export

get_basins <- function(sites, filePath = NA){
  library(httr)
  postURL <- "https://cida.usgs.gov/nwc/geoserver/NWC/ows"
  # postURL <- "http://cida-test.er.usgs.gov/nwc/geoserver/NWC/ows"
  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="https://owi.usgs.gov/NWC" typeName="feature:epa_basins" srsName="EPSG:4326">')


  if(length(sites) > 1){
    siteText <- ""
    for(i in sites){
      siteText <- paste0(siteText,'<ogc:PropertyIsEqualTo  matchCase="true">',
                         '<ogc:PropertyName>site_no</ogc:PropertyName>',
                         '<ogc:Literal>',i,'</ogc:Literal>',
                         '</ogc:PropertyIsEqualTo>')
    }

    filterXML <- paste0(filterXML,'<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                        '<ogc:Or>',siteText,'</ogc:Or>',
                        '</ogc:Filter>')

  } else {
    filterXML <- paste0(filterXML,
                        '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                        '<ogc:PropertyIsEqualTo matchCase="true">',
                        '<ogc:PropertyName>site_no</ogc:PropertyName>',
                        '<ogc:Literal>',sites,'</ogc:Literal>',
                        '</ogc:PropertyIsEqualTo>',
                        '</ogc:Filter>')
  }

  filterXML <- paste0(filterXML,'</wfs:Query>',
                      '</wfs:GetFeature>')

  destination = file.path(tempdir(), 'basins_shape.zip')

  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))

  if(is.na(filePath)){
    filePath <- tempdir()
  } else {
    print(paste("Saving to", filePath))
  }

  unzip(paste0(destination,"/basins_shape.zip"), exdir = filePath)
  basins <- sf::st_read(filePath, layer='epa_basins')
  #return(filePath)
  return(basins)
}
