#' Download and convert GPM Product data in GeoTIFF version 2.0
#' @author Cesar Aybar Camacho 
#' @description This is the preliminary version for massive downloading data GPM.  
#' @details You need to have installed "Axel download accelerator application" and gdal.
#'          Obs: If you are using Windows OS executables it should be declared as environment variables.
#'          Download Linux axel: apt-get install axel
#'          Download windows axel: https://st0rage.org/~n2j3/?page_id=225717166
#' @param fileLocation: file path where to save.
#' @param user: enter your user.
#' @param password: enter your password.
#' @param typeProduct:  default is "finalrun" 
#' @param dates: defines the discharge range
#' - "Early" period that has now data (2015-03-31- present).
#' - "Late": Period currently has data (31.03.2015).
#' - "Finalrun" which features data Period (12/03/2014 to 2015-09).
#' @param band: 
#'              band "HQobservationTime" = 1
#'              band "HQprecipSource" = 2
#'              band "HQprecipitation" = 3 
#'              band "IRkalmanFilterWeight" = 4  
#'              band "IRprecipitation" = 5
#'              band "precipitationCal" = 6  #' for default
#'              band "precipitationUncal" = 7
#'              band "probabilityLiquidPrecipitation" = 8
#'              band "randomError" = 9  
#' @param BBlonMin Minimum latitude of bounding box
#' @param BBlonMax Maximum latitude of bounding box
#' @param BBlatMin Minimum longitude of bounding box
#' @param BBlatMax Maximum longitude of bounding box
#' @details This code is based on the script  "ModisDownload.R" created by Babak Naimi and "TRMM.R" Claudia Vitolo: 
#' https://gist.github.com/cvitolo/9b5322be6fb940cc718a
#' Use of the PPS FTP to download GPM and TRMM data is free, but you are required to first register your email address 
#' http://registration.pps.eosdis.nasa.gov/registration/
#' more information about GPM see ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/IMERG_ATBD_V4.5.pdf
#' more information about Axel see http://www.linuxjournal.com/content/speed-your-downloads-axel
#' @examples 
#' #downloadGPM(fileLocation = "C:/Users/Papita/Desktop/prueb/lat/",user = "xxx",password = "xxx",typeProduct = "late",dates = c('2015.05.01','2015.05.02'))
#'
#rm(list = ls())
downloadGPM <- function(fileLocation = "/home/UserX/folder1/",
                        user="xxx",
                        password="xxx",
                        dates=c('2014.05.01','2014.05.02'),
                        typeProduct = "finalrun",
                        maxspeeddownload= 504800, # 504,800 Bytes/second (500KB/s)
                        numconnections='3',
                        band=6,
                        BBlonMin = -86,
                        BBlonMax = -66,
                        BBlatMin = -19.25,
                        BBlatMax = 1.25
){
  
  if (!require(RCurl)) stop("Package RCurl is not installed")
  if (!require(raster)) stop("Package raster is not installed")
  if (!require(rgdal)) stop("Package rgdal is not installed")
  if (!require(gdalUtils)) stop("Package gdalUtils is not installed")
  
  if(!inherits(dates,"character")) stop("your date is not a character!")
  if (inherits(dates,"character")) dates <- as.Date(dates,format='%Y.%m.%d')
  dates <- na.omit(as.Date(dates))
  if (length(dates) == 0) stop("dates is not appropriately selected!")
  dates <- sort(dates) 
  timeseries<-seq(from=dates[1],to=dates[2],by='day')
  #--------
  
  .gpmdownloadI<- function(day=1){ 
    if (trim(typeProduct)=='finalrun'){ 
      myURL <-paste0("ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata","/",format(timeseries[day], "%Y"),"/",format(timeseries[day], "%m"),
                     "/",format(timeseries[day], "%d"),"/imerg/")
      filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, 
                          crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
      filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
      selectedfilePaths <- filePaths[grep(filePaths,pattern="*.HDF5")] 
      selectedfilePaths <- sort(selectedfilePaths)
      selectedfilePaths<-selectedfilePaths[-49]}
    
    
    if (trim(typeProduct)=='late'){ 
      myURL <-paste("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/late","/",format(timeseries[day], "%Y"),format(timeseries[day], "%m"),"/",sep="")
      filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
      filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
      if(day<10)  day2<-paste0("0",day)
      selectedfilePaths <- filePaths[grep(filePaths,pattern=paste("*",format(timeseries[day], "%Y"),format(timeseries[day], "%m"),day2,"-",sep=""))]}
    
    if (trim(typeProduct)=='early'){ 
      myURL <-paste("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/early","/",format(timeseries[day], "%Y"),format(timeseries[day], "%m"),"/",sep="")
      filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
      filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
      if(day<10)  day2<-paste0("0",day)
      selectedfilePaths <- filePaths[grep(filePaths,pattern=paste("*",format(timeseries[day], "%Y"),format(timeseries[day], "%m"),day2,"-",sep=""))]}
    
    setwd(trim(fileLocation))
    dir.create(paste(format(timeseries[day], "%Y"),"-",format(timeseries[day], "%m")
                     ,"-",format(timeseries[day], "%d"),sep=""))
    
    setwd(paste(fileLocation,format(timeseries[day], "%Y"),
                "-",format(timeseries[day], "%m"),"-",format(timeseries[day], "%d"),sep=""))
    
    down<- function(Url,bs)  system(paste0("axel -n ",numconnections,
                                           " -s ",maxspeeddownload," ",Url," -o ",paste(fileLocation,format(timeseries[day], "%Y"),
                                                                                        "-",format(timeseries[day], "%m"),"-",format(timeseries[day], "%d"),sep=""),"/",bs))
    #--------
    
    .gpmdownloadII<-function(timeS=1){
      selectedfilePaths2<- selectedfilePaths[timeS]
      down(Url = paste0("ftp://",user,":",password,"@",substr(selectedfilePaths2,7,150)),bs = basename(selectedfilePaths2))
      y<- function(a,b) system(paste0('gdal_translate ',get_subdatasets(a)[band]," ",b,"-",band,".tif"))  
      file_path<-paste0(paste(fileLocation,"/",format(timeseries[day], "%Y"),
                              "-",format(timeseries[day], "%m"),"-",
                              format(timeseries[day], "%d"),sep=""),"/",basename(list.files())[length(basename(list.files()))])
      y(file_path,basename(file_path))
      baseraster <- raster(extent(-180,180,-90,90))
      res(baseraster) <- 0.1
      projection(baseraster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      rl<-list.files(pattern = ".tif")[length(list.files(pattern = ".tif"))]
      rl1<-t(as.matrix(raster(rl)))
      mtrx = matrix(rl1, nrow=1800, ncol=3600)
      baseraster[]<-mtrx  
      flp<-flip(baseraster,direction = "y")
      prec<-crop(flp,extent(BBlonMin,BBlonMax,BBlatMin,BBlatMax) )
      writeRaster(prec/2,rl,overwrite=T)
      file.remove(file_path)
    }
    mapply(.gpmdownloadII,1:48)
  }
  mapply(.gpmdownloadI,1:length(timeseries))  
}
