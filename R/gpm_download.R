#' Download IMERG v05B Final, Early and Late Run
#'
#' @author Cesar Aybar
#' @description Download IMERG v05B Final, Early and Late Run.
#' @param path path (character). The default corresponds to the working directory, getwd(). Missing values will be ignored.
#' @param user pmm user (character). See https://pmm.nasa.gov/data-access/downloads/gpm
#' @param password pmm password (character). See https://pmm.nasa.gov/data-access/downloads/gpm
#' @param product IMERG product (character). Currently are supported: 'finalrun', 'late' and 'early'.
#' @param dates sequences of days.
#' @param band The available bands are: \cr 
#' band "HQobservationTime" = 1. \cr 
#' band "HQprecipSource" = 2. \cr 
#' band "HQprecipitation" = 3. \cr 
#' band "IRkalmanFilterWeight" = 4. \cr 
#' band "IRprecipitation" = 5. \cr 
#' band "precipitationCal" = 6. \cr 
#' band "precipitationUncal" = 7. \cr 
#' band "probabilityLiquidPrecipitation" = 8. \cr 
#' band "randomError" = 9. \cr 
#' @param lonMin Minimum latitude of bounding box
#' @param lonMax Maximum latitude of bounding box
#' @param latMin Minimum longitude of bounding box
#' @param latMax Maximum longitude of bounding box
#' @param removeHDF5 Remove HD5 files after the conversion to GEOTIFF
#' @param quiet logical; suppress info of the download.
#' @param n Specify maximum number of connections (axel parameter)
#' @param s Specify maximum speed (bytes per second)
#' @details
#' You need to have installed "Axel download accelerator
#' application" and GDAL. Obs: If you are using Windows,
#' the executables of "GDAL" and "AXEL" must be set as
#' system variables. For install axel try the following: \cr
#'
#' Ubuntu/Debian users: apt-get install axel \cr
#' Windows users: gpm_getaxel() \cr
#'
#' The use of the "PPS" ftp to download GPM and TRMM data is free, but you first need register
#' your email address in http://registration.pps.eosdis.nasa.gov/registration/.
#' You can found more information about axel here:\cr
#' https://github.com/axel-download-accelerator/axel/.
#' @importFrom raster raster res extent flip crop writeRaster projection 'res<-' 'projection<-' as.matrix
#' @importFrom lubridate year day month
#' @importFrom gdalUtils get_subdatasets
#' @importFrom stringr str_subset str_remove_all str_subset str_replace_all str_remove
#' @importFrom RCurl getURL
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' gpm_download(path = '.',
#'             user = 'you_user@gmail.com',
#'             password = 'you_user@gmail.com',
#'             dates = c('2018-01-01'),
#'             product = 'finalrun')
#' }
#' @export

gpm_download <- function(path,
                         user,
                         password,
                         dates,
                         product,
                         band=6,
                         lonMin = -86,
                         lonMax = -66,
                         latMin = -19.25,
                         latMax = 1.25,
                         removeHDF5 = TRUE,
                         quiet = TRUE,
                         n = 1,
                         s = 2600000
) {
  
  # 1._ Create a list with the files to download
  message('Searching the IMERG-HDF5 files ... please wait')
  userpwd <- sprintf("%s:%s",user,password)
  axel_exist()
  gdaltranslate_exist()
  if (product == 'early') {
    url <- 'ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/early/'
    url_m <- sprintf('%s/%s%02d/',
                     url,
                     year(dates),
                     month(dates))
    url_m <- unique(url_m)


    # Scrapping the ftp
    imerg_bymonth <- url_m %>%
      map(~ getURL(url = .x,
                   ftp.use.epsv = FALSE,
                   ftplistonly = TRUE,
                   crlf = TRUE,
                   userpwd = userpwd)) %>%
      unlist()

    # Getting only the HHR files
    download_files <- imerg_bymonth %>%
      map(~ strsplit(.x,'\r*\n')[[1]]) %>%
      map(~ str_subset(.x,'HHR')) %>%
      map(~ sprintf('%s/%s',url_m,.x)) %>%
      '[['(1)

    # From character to a list
    datesf2 <- str_remove_all(dates,'-')
    nums <- 1:length(datesf2)

    imerg_files <- nums %>%
      map(~ str_subset(download_files,datesf2[.x])) %>%
      map(~ sort(.x)) %>%
      'names<-'(datesf2) %>%
      map(~ str_replace_all(.x,'ftp://', paste0('ftp://',userpwd,'@')))

  }  else if (product == 'late'){

    url <- 'ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/late/'
    url_m <- sprintf('%s/%s%02d/',
                     url,
                     year(dates),
                     month(dates))
    url_m <- unique(url_m)


    # Scrapping the ftp
    imerg_bymonth <- url_m %>%
      map(~ getURL(url = .x,
                   ftp.use.epsv = FALSE,
                   ftplistonly = TRUE,
                   crlf = TRUE,
                   userpwd = userpwd)) %>%
      unlist()


    # Getting only the HHR files
    download_files <- imerg_bymonth %>%
      map(~ strsplit(.x,'\r*\n')[[1]]) %>%
      map(~ str_subset(.x,'HHR')) %>%
      map(~ sprintf('%s/%s',url_m,.x)) %>%
      '[['(1)

    # From character to a list
    datesf2 <- str_remove_all(dates,'-')
    nums <- 1:length(datesf2)
    imerg_files <- nums %>%
      map(~ str_subset(download_files,datesf2[.x])) %>%
      map(~ sort(.x)) %>%
      'names<-'(datesf2) %>%
      map(~ str_replace_all(.x,'ftp://', paste0('ftp://',userpwd,'@')))

  }  else if (product == 'finalrun'){

    url <- 'ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata'
    url_d <- sprintf('%s/%s/%02d/%02d/imerg/',
                     url,
                     year(dates),
                     month(dates),
                     day(dates))

    # Scrapping the ftp
    imerg_byday <- url_d %>%
      map(~ getURL(url = .x,
                   ftp.use.epsv = FALSE,
                   ftplistonly = TRUE,
                   crlf = TRUE,
                   userpwd = userpwd))

    # Getting only the HHR files
    download_files <- imerg_byday %>%
      map(~ strsplit(.x,'\r*\n')[[1]]) %>%
      map(~ str_subset(.x,'HHR'))

    # From character to a list
    datesf2 <- str_remove_all(dates,'-')
    nums <- 1:length(datesf2)

    download_files <- nums %>%
      map(~sprintf('%s/%s',url_d[.x],download_files[[.x]])) #url + file name

    imerg_files <- nums %>%
      map(~ str_subset(download_files[[.x]], datesf2[.x])) %>%
      map(~ sort(.x)) %>%
      'names<-'(datesf2) %>%
      map(~ str_replace_all(.x,'ftp://', paste0('ftp://',userpwd,'@')))

  } else {
    stop('The "product" argument only can be: "finalrun", "late" or "early".')
  }
  
  # 2. Downloading one by one the IMERG files
  datesf2 <- names(imerg_files)
  for (x in 1:length(datesf2)) {
    fullpath <- sprintf('%s%s/',path,datesf2[x])
    dir.create(fullpath)
    for (z in 1:length(imerg_files[[x]])) {
      imerg_file <- imerg_files[[x]][z]
      fullname <- sprintf('%s%s',fullpath,basename(imerg_file))
      message(sprintf('Downloading: %s',basename(imerg_file)))
      axel(imerg_file,fullname,n,s,quiet)

      file_tiff <- paste0(str_remove(fullname,'\\.RT-H5'),'.tif')
      gdal_translate(fullname,band,file_tiff,removeHDF5)

      baseraster <- raster(extent(-180,180,-90,90))
      res(baseraster) <- 0.1
      projection(baseraster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

      file_r <- raster(file_tiff)
      file_matrix <- as.matrix(file_r)
      file_t <- file_matrix %>% t
      mtrx = matrix(file_t, nrow=1800, ncol=3600)
      baseraster[] <- mtrx
      flp <- flip(baseraster,direction = "y")
      prec <- crop(flp,extent(lonMin,lonMax,latMin,latMax))
      writeRaster(prec/2,file_tiff, overwrite=T)
    }
  }
}
