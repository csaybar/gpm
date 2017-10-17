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
# library(lubridate)
# library(tidyverse)

downloadGPM <- function(fileLocation = "/home/UserX/folder1/",
                        user="xxx",
                        password="xxx",
                        dates=c('2014.05.01','2014.05.02'),
                        typeProduct = "finalrun",
                        maxspeeddownload= 2004800, # 504,800 Bytes/second (500KB/s)
                        numconnections='5',
                        init="1200",
                        band=6,
                        BBlonMin = -86,
                        BBlonMax = -66,
                        BBlatMin = -19.25,
                        BBlatMax = 1.25
){
  if (!require(tidyverse)) stop("Package tidyverse is not installed")
  if (!require(lubridate)) stop("Package lubridate is not installed")
  if (!require(tidyverse)) stop("Package tidyverse is not installed")
  if (!require(RCurl)) stop("Package RCurl is not installed")
  if (!require(raster)) stop("Package raster is not installed")
  if (!require(rgdal)) stop("Package rgdal is not installed")
  if (!require(gdalUtils)) stop("Package gdalUtils is not installed")
  if(!inherits(dates,"Date")) stop("your date is not a Date!")
  timeseries<-dates
  #--------
  for(d in 1:length(timeseries)){
    
    year2=c(year(timeseries[d]),year(timeseries[d]+1))
    month2=c(month(timeseries[d]),month(timeseries[d]+1))
    day2=c(day(timeseries[d]),day(timeseries[d]+1))
    
    if (trim(typeProduct)=='finalrun'){
      myURL <- lapply(1:2, function(i) sprintf("ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata/%s/%02d/%02d/imerg/",
                                               year2[i],month2[i],day2[i]))
      filenames<-try(lapply(1:2,function(i) getURL(myURL[[i]], ftp.use.epsv = FALSE, ftplistonly=TRUE,
                                                   crlf=TRUE,userpwd=sprintf("%s:%s",user,password))))
      if(class(filenames)=="try-error") stop("Problem in the IMERG-finalrun download")
      filePaths <-  lapply(1:2, function(i) paste(myURL[[i]], strsplit(filenames[[i]], "\r*\n")[[1]], sep=""))
      filePaths <- sort(do.call("c",filePaths))
      position_init=grep(sprintf("*%s%02d%02d-S%s",year2[1],month2[1],day2[1],init),filePaths)
      selectedfilePaths <- filePaths[position_init:(position_init+47)]
    }
    
    if (trim(typeProduct)=='late'){
      if(month2[1]==month2[2]){
        myURL <- sprintf("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/late/%s%02d/",year2[1],month2[1])
        filenames<-getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd=sprintf("%s:%s",user,password))
        filePaths <-  paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
        position_init <- grep(sprintf("*%s%02d%02d-S%s",year2[1],month2[1],day2[1],init),filePaths)
        selectedfilePaths <- filePaths[position_init:(position_init+47)]  
      }
      else{
        myURL <- lapply(1:2, function(i) sprintf("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/late/%s%02d/",year2[i],month2[i]))
        filenames<-lapply(1:2,function(i) getURL(myURL[[i]], ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd=sprintf("%s:%s",user,password)))
        filePaths <-  lapply(1:2, function(i) paste(myURL[[i]], strsplit(filenames[[i]], "\r*\n")[[1]], sep=""))
        filePaths <- sort(do.call("c",filePaths))
        position_init=grep(sprintf("*%s%02d%02d-S%s",year2[1],month2[1],day2[1],init),filePaths)
        selectedfilePaths <- filePaths[position_init:(position_init+47)]
      }
    }
    if (trim(typeProduct)=='early'){
      if(month2[1]==month2[2]){
        myURL <- sprintf("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/early/%s%02d/",year2[1],month2[1])
        filenames<-getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd=sprintf("%s:%s",user,password))
        filePaths <-  paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
        position_init <- grep(sprintf("*%s%02d%02d-S%s",year2[1],month2[1],day2[1],init),filePaths)
        selectedfilePaths <- filePaths[position_init:(position_init+47)]  
      }
      else{
        myURL <- lapply(1:2, function(i) sprintf("ftp://jsimpson.pps.eosdis.nasa.gov/NRTPUB/imerg/early/%s%02d/",year2[i],month2[i]))
        filenames<-lapply(1:2,function(i) getURL(myURL[[i]], ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd=sprintf("%s:%s",user,password)))
        filePaths <-  lapply(1:2, function(i) paste(myURL[[i]], strsplit(filenames[[i]], "\r*\n")[[1]], sep=""))
        filePaths <- sort(do.call("c",filePaths))
        position_init=grep(sprintf("*%s%02d%02d-S%s",year2[1],month2[1],day2[1],init),filePaths)
        selectedfilePaths <- filePaths[position_init:(position_init+47)]
      }
    }
    
    bs = basename(selectedfilePaths)
    setwd(trim(fileLocation))
    dir.create(sprintf("%s-%02d-%02d",year2[1],month2[1],day2[1]))
    setwd(sprintf("%s/%s-%02d-%02d",trim(fileLocation),year2[1],month2[1],day2[1]))
    Url = sprintf("ftp://%s:%s@%s",user,password,gsub("ftp://","",selectedfilePaths))
    bs = basename(selectedfilePaths)
    
    down<- function(i)  system(sprintf("axel -n %s -s %s %s -o %s/%s-%02d-%02d/%s",numconnections,
                                       maxspeeddownload,Url[i],trim(fileLocation),year2[1],month2[1],day2[1],bs[i]))
    y<- function(a,b) system(paste0('gdal_translate ',get_subdatasets(a)[band]," ",b,"-",band,".tif"))  
    #--------
    
    for(i in 1:48){
      repeat{
        down(i)
        file_path<-sprintf("%s/%s-%02d-%02d/%s",trim(fileLocation),year2[1],month2[1],day2[1],bs[i])
        a=try(y(file_path,basename(file_path)))
        if(!class(a)=="try-error") break
      }
      baseraster <- raster(extent(-180,180,-90,90))
      res(baseraster) <- 0.1
      projection(baseraster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      rl<-list.files(pattern = ".tif")[length(list.files(pattern = ".tif"))]
      rl1<-t(as.matrix(raster(rl)))
      mtrx = matrix(rl1, nrow=1800, ncol=3600)
      baseraster[]<-mtrx  
      flp<-flip(baseraster,direction = "y")
      prec<-crop(flp,extent(BBlonMin,BBlonMax,BBlatMin,BBlatMax))
      writeRaster(prec/2,rl,overwrite=T)
      file.remove(file_path)
    }
  }
}

#Example
# downloadGPM(fileLocation = "/home/.../",
#             user = "x@gmail.com",
#             password = "x@gmail.com",
#             typeProduct = "early",
#             dates = seq(as.Date('2016-07-31'),as.Date('2016-10-02'),"day"))