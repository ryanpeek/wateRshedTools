# get zipped shp
#' Read a zipped shapefile
#'
#' This function takes a \emph{path} to a given shapefile
#' from online or local directory, and
#' returns a \{sf} based data.frame to the R environment.
#'
#' @param shp_url A character path to a zipped shapefile, locally or online.
#' @return Returns an sf data.frame.
#' @examples
#' # set up the parameters to use in function:
#' library(sf)
#' library(utils)
#' library(mapview)
#'
#' # get a shapefile of all USGS gages in North America
#' shp_url <- "https://water.usgs.gov/GIS/dsdl/gagesII_9322_point_shapefile.zip"
#'
#' gages2 <- get_shp_zip(durl)
#'
#' # now plot
#' mapview::mapview(gages2)


#' @export
get_shp_zip <- function(shp_url){
    dest_dir <- tempdir() # make temp dir
    temp_shp <- tempfile(tmpdir = dest_dir) # make tempfile for shp
    download.file(shp_url,temp_shp) # download the zipped file
    # now unzip the file depending on extension
    if( grepl('.tgz$|.tar.gz$', shp_url) ){
      utils::untar(temp_shp, exdir = dest_dir)
    } else if(grepl('.zip$', shp_url)){
      utils::unzip(temp_shp, exdir = dest_dir)
    } else{
      stop('not *.zip or *.tgz or *.tar.gz!')
    }
    # now get shp name with full temp path
    shp_file <- list.files(dest_dir, pattern = ".shp$", full.names=TRUE)
    # read it in!
    sf::st_read(shp_file)
    # when we close the R session, the temp dir/files are removed!
  }



