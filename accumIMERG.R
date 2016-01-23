#' Data accumulation GEOTIFF- IMERG
#' @author Cesar Aybar Camacho 
#' @description This is the preliminary version for Data accumulation IMERG.Accumulates
#'               at different time steps data every 30 minutes from IMERG. 
#' @details You need to have the following libraries installed R: Raster, Rgdal and Foreach.
#' @param fileLocation: Folder where the data is located.
#' @param UTC="1200": Start Time accumulation
#' @param Timestep: It is supported accumulations every "3HR", "Daily" and "Monthly".
#' @param paralelled: You can define whether or not to use parallel programming multicore, using foreach and DoMC package.
#' @param cores: Set the number of cores to use.
#' @examples 
#' WINDONS: AccumGPM(InputLocation="C:/Users/Senamhi01/Desktop/accumulacion/datos",cores=2,OutputLocation="C://Users/Senamhi01/Desktop/accumulacion/mensual/",TimeStep="Monthly",UTC="1200")
#' LINUX: AccumGPM(InputLocation = "/home/senamhi-cesar/Escritorio/GPM-DATA/datos/",UTC = "1200",TimeStep = "Daily",paralelled = T,cores = 10,SO = "Linux",OutputLocation = "/home/senamhi-cesar/Escritorio/GPM-DATA/diario/")
AccumGPM<-function( 
  InputLocation= "C://Users/Papita/Desktop/practica/Imput/" ,
  UTC="1200",
  TimeStep=  "Monthly",
  paralelled=T,
  SO='Winbug',
  cores=12,
  OutputLocation="C://Users/Papita/Desktop/practica/Output/"
){
  setwd(OutputLocation)
  if (!require(raster)) stop("Package raster is not installed")
  if (!require(rgdal)) stop("Package rgdal is not installed")
  tt<-list.files(InputLocation,pattern = "*.tif",recursive = T,all.files = T,full.names = T)
  #select<-which(tt%in%list.files(InputLocation,pattern = "S210000",recursive = T,all.files = T,full.names = T))
  init<-substr(basename(tt[1]),32,35)
  diff<-as.numeric(UTC)-as.numeric(init)
  ndiff<-diff%/%100*2+(diff%%100)/30+1
  tt<-tt[ndiff:length(tt)]
  
  if(TimeStep=="3HR"){ 
    if (paralelled==T){ 
      if (SO=='Linux'){ 
       if (!require(foreach)) stop("Package foreach is not installed")
       if (!require(doMC)) stop("Package doMC is not installed")
       registerDoMC(cores) }
      if (SO=='Winbug'){ 
        if (!require(doSNOW)) stop("Package doSNOW is not installed")
        if (!require(foreach)) stop("Package foreach is not installed")
        cl<-makeCluster(cores)
        registerDoSNOW(cl)}
        
        foreach(i=1:(length(tt)%/%6))%dopar%{
          library(raster)
          writeRaster(sum(stack(tt[((i-1)*6+1):(i*6)]),na.rm = T),
                                  basename(tt[((i-1)*6+1):(i*6)])[6])}
          stopCluster(cl)}
      if(paralelled==F) for(i in 1:(length(tt)%/%6)) writeRaster(sum(stack(tt[((i-1)*6+1):(i*6)]),na.rm = T),
                                  basename(tt[((i-1)*6+1):(i*6)])[6])}
  

  if(TimeStep=="Daily"){ 
    if (paralelled==T){ 
      if (SO=='Linux'){ 
        if (!require(foreach)) stop("Package foreach is not installed")
        if (!require(doMC)) stop("Package doMC is not installed")
        registerDoMC(cores) }
      if (SO=='Winbug'){ 
        if (!require(doSNOW)) stop("Package doSNOW is not installed")
        if (!require(foreach)) stop("Package foreach is not installed")
        cl<-makeCluster(cores)
        registerDoSNOW(cl)}
        foreach(i=1:(length(tt)%/%48))%dopar%{ 
          library(raster)
          writeRaster(sum(stack(tt[((i-1)*48+1):(i*48)]),na.rm = T),paste0("IMERG.Daily.",
          substr(basename(tt[((i-1)*48+1):(i*48)])[1],22,29),".tif"))}
          stopCluster(cl)}
    if(paralelled==F) for(i in 1:(length(tt)%/%6)) writeRaster(sum(stack(tt[((i-1)*48+1):(i*48)]),na.rm = T),
          paste0("IMERG.Daily.",substr(basename(tt[((i-1)*48+1):(i*48)])[(i-1)*48+1],22,29),".tif"))
  }        
  
  if(TimeStep=="Monthly") { 
      lvl<-levels(factor(substr(basename(tt),22,27)))      
      if (paralelled==T){ 
        if (SO=='Linux'){ 
          if (!require(foreach)) stop("Package foreach is not installed")
          if (!require(doMC)) stop("Package doMC is not installed")
          registerDoMC(cores) }
        if (SO=='Winbug'){ 
          if (!require(doSNOW)) stop("Package doSNOW is not installed")
          if (!require(foreach)) stop("Package foreach is not installed")
          cl<-makeCluster(cores)
          registerDoSNOW(cl)}  
        foreach(i=1:length(lvl))%dopar%{
        library(raster)
        writeRaster(sum(stack(tt[grep(substr(basename(tt),22,27),pattern=lvl[i])]),na.rm = T),
        paste0("IMERG.Mounthly.",substr(basename(tt[grep(substr(basename(tt),22,27),pattern=lvl[i])])[1],22,27),".tif"))}
        stopCluster(cl)}
      
      if (paralelled==F){   
        for(i in 1:length(lvl)){ 
        writeRaster(sum(stack(tt[grep(substr(basename(tt),22,27),pattern=lvl[i])]),na.rm = T),
        paste0("IMERG.Mounthly.",substr(basename(tt[grep(substr(basename(tt),22,27),pattern=lvl[i])])[1],22,27),".tif"))}}}} 