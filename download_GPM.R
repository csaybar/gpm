#' Download and convert GPM Product data in GeoTIFF
#' @author Cesar Aybar Camacho 
#' @description This is the preliminary version for massive downloading data GPM.  
#' @details You need to have installed "Axel download accelerator application" and gdal.
#'          Obs: If you are using Windows OS executables it should be declared as environment variables.
#'          Download Linux axel: apt-get install axel
#'          Download windows axel: https://st0rage.org/~n2j3/?page_id=225717166
#' @param fileLocation: file path where to save.
#' @param url: ftp where data is stored
#'        soported: 
#'              -ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/" 
#'              -"ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata/" (for default)
#'              -http://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHH.03/ (comming soon)
#' @param user: enter your user.
#' @param password: enter your password.
#' @param formatDownL: Indicate the data format download (soported "HDF5" or "geoTIFF")
#' @param formatSave: Indicate the data format  save "HDF5" or "geoTIIF".
#' @param latency:  default is "early" (only valid in ftp://jsimpson.pps.eosdis.nasa.gov)
#' @param year: year of interest
#' @param month: month of interest
#' @param day: day of interest
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
#' @param paralelled:  (comming soon)
#' @param BBlonMin Minimum latitude of bounding box
#' @param BBlonMax Maximum latitude of bounding box
#' @param BBlatMin Minimum longitude of bounding box
#' @param BBlatMax Maximum longitude of bounding box
#' @details This code is based in Martin Brandt's blog post and Claudia Vitolo Github: 
#' http://matinbrandt.wordpress.com/2013/09/04/automatically-downloading-and-processing-trmm-rainfall-data/
#' https://gist.github.com/cvitolo/9b5322be6fb940cc718a
#' more information about GPM see ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/IMERG_ATBD_V4.5.pdf
#' more information about Axel see http://www.linuxjournal.com/content/speed-your-downloads-axel
#' @examples 
#' downloadGPM(fiLocation = "/home/UseX/gpm/")
#'

downloadGPM <- function(fileLocation = "/home/senamhi-cesar/Escritorio/downloadgpm/",
                        url = "ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata/",
                        formatDownL="HDF5",
                        formatSave="geoTIFF",
                        year="2014",
                        month="03",
                        day="12",
                        latency = "early",
                        user="XXX@gmail.com",
                        password="XXX@gmail.com",
                        band=6,
                        paralelled=T,
                        BBlonMin = -86,
                        BBlonMax = -66,
                        BBlatMin = -19.25,
                        BBlatMax = 1.25
){
  
  if (!require(RCurl)) stop("Package RCurl is not installed")
  if (!require(raster)) stop("Package raster is not installed")
  if (!require(rgdal)) stop("Package rgdal is not installed")
  if (!require(gdalUtils)) stop("Package raster is not installed")
  #require(foreach)
  
  setwd(fileLocation)
  dir.create(paste(year,"-",month,"-",day,sep=""))
  setwd(paste(fileLocation,year,"-",month,"-",day,sep=""))
  
  if(url == "ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/"){
    
    myURL <-paste(url,"/",year,month,"/",sep="")
    filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
    filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
    selectedfilePaths <- filePaths[grep(filePaths,pattern=paste("*",year,month,day,"-",sep=""))] 
    x<- function(Url,bs)  system(paste("axel -n 3 ",Url," -o ",paste(fileLocation,year,"-",month,"-",day,sep=""),"/",bs,sep=""))
    mapply(x,paste("ftp://",user,":",password,"@",substr(selectedfilePaths,7,150),sep=""),basename(selectedfilePaths))}  
  
  if(url == "ftp://arthurhou.pps.eosdis.nasa.gov/gpmdata/"){
    if(formatDownL == "HDF5"){
      myURL <-paste(url,"/",year,"/",month,"/",day,"/imerg/",sep="")
      filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
      filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
      selectedfilePaths <- filePaths[grep(filePaths,pattern="*.HDF5")] 
      
      x<- function(Url,bs)  system(paste("axel -n 3 ",Url," -o ",paste(fileLocation,year,"-",month,"-",day,sep=""),"/",bs,sep=""))
      mapply(x,paste("ftp://",user,":",password,"@",substr(selectedfilePaths,7,150),sep=""),basename(selectedfilePaths))
    }  
    
    if(formatDownL == "GIS"){
      myURL <-paste(url,"/",year,"/",month,"/",day,"/gis/",sep="")
      filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE,userpwd="aybar1994@gmail.com:aybar1994@gmail.com")
      filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
      selectedfilePaths <- filePaths[grep(filePaths,pattern="*\\.tif$")] 
      x<- function(Url,bs)  system(paste("axel -n 3 ",Url," -o ",paste(fileLocation,year,"-",month,"-",day,sep=""),"/",bs,sep=""))
      mapply(x,paste("ftp://",user,":",password,"@",substr(selectedfilePaths,7,150),sep=""),basename(selectedfilePaths))}  
  }
  
  if (formatSave=="HDF5")  message(paste0("Download finish for the  ",year,"-",month,"-",day))
  baseraster <- raster(extent(-180,180,-90,90))
  res(baseraster) <- 0.1
  projection(baseraster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  if(formatSave=="geoTIFF")
    if(substr(list.files(),61,63)[1]=="tif"){ 
      file_path<-paste0(paste(fileLocation,"/",year,"-",month,"-",day,sep=""),"/",basename(list.files()))   
      
      z<-function(k){ 
        rl1<-as.matrix(raster(file_path[k]))
        mtrx = matrix(rl1, nrow=1800, ncol=3600)
        baseraster[]<-mtrx  
        prec<-crop(baseraster,extent(BBlonMin,BBlonMax,BBlatMin,BBlatMax) )
        prec<-prec*0.05
        writeRaster(prec,file_path[k],overwrite=T)}
      mapply(z,1:48)}
  
  
  if(substr(list.files(),57,63)[1]=="HDF5"){ 
    file_path<-paste0(paste(fileLocation,"/",year,"-",month,"-",day,sep=""),"/",basename(list.files()))   
    
    y<- function(a,b) system(paste0('gdal_translate ',get_subdatasets(a)[band]," ",b,"-",band,".tif"))
    mapply(y,file_path,basename(file_path))
    
    baseraster <- raster(extent(-180,180,-90,90))
    res(baseraster) <- 0.1
    projection(baseraster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    rl<-list.files(pattern = ".tif")
    
    z<-function(k){ 
      rl1<-t(as.matrix(raster(k)))
      mtrx = matrix(rl1, nrow=1800, ncol=3600)
      baseraster[]<-mtrx  
      flp<-flip(baseraster,direction = "y")
      prec<-crop(flp,extent(BBlonMin,BBlonMax,BBlatMin,BBlatMax) )
      writeRaster(prec/2,k,overwrite=T)
    }
    mapply(z,rl)
    file.remove(file_path)
  }
}
