#' Download files via axel
#' @param url A character string naming the URL of a resource to be downloaded.
#' @param destfile A character string with the name where the downloaded file will be saved.
#' @param n Specify maximum number of connections.
#' @param s Specify maximum speed (bytes per second).
#' @param quiet Leave stdout alone.
#' 
axel <- function(url,destfile,n,s,quiet){
  if (quiet) {
    down_file <- sprintf("axel -n %s -s %s %s -o %s --quiet",n,s,url,destfile)
  } else {
    down_file <- sprintf("axel -n %s -s %s %s -o %s",n,s,url,destfile)
  }

  system(down_file)
}

#' HDF5 to GEOTIFF using GDAL
#' @param hdf5 A character string naming the HDF5 file.
#' @param band A numeric, indicate the band to extract.
#' @param outfile A character string with the name where the geoTIFF will be saved.
#' @param removeHDF5 logical,Remove HD5 files after the conversion to geoTIFF.
#' 
gdal_translate <- function(hdf5,band,outfile,removeHDF5){
  gdal_exp <- sprintf('gdal_translate %s %s',
                      get_subdatasets(hdf5)[band],
                      outfile)
  system(gdal_exp)
  if (removeHDF5) {
    file.remove(hdf5)
  }
}

#' These functions provide information about the location 
#' of gdal_translate in your system.
#' 
gdaltranslate_exist = function(){
  msg <- try(system('gdal_translate --version',intern = T))
  if (class(msg) == 'try-error') {
    if (Sys.info()[1] == 'Linux') {
      stop('GDAL is not installed, please install it first')  
    } else if (Sys.info()[1] == 'Windows') {
      stop('GDAL should be declared as a system variable if you are using Windows')  
    } else {
      stop('Your SO is not supported, try installing GDAL')  
    }
  }
}
#' These functions provide information about the location 
#' of axel in your system.
axel_exist = function(){
  msg <- try(system('axel --version',intern = T))
  if (class(msg) == 'try-error') {
    if (Sys.info()[1] == 'Linux') {
      stop('axel is not installed, please install it first')  
    } else if (Sys.info()[1] == 'Windows') {
      stop('axel should be declared as a system variable if you are using Windows')  
    } else {
      stop('Your SO is not supported, try installing axel')  
    }
  }
}

#' Donwnload axel 2.4 for windows user
#' @param dirfile 
#' A character string with the dirname where axel.zip well be saved. 
#' @importFrom utils download.file 
#' @export
gpm_getaxel <- function(dirfile = getwd()){
  id <- '12T6R32-kAZPxbeCC3VdPx_KjqtxHWDt6'
  url <- 'https://drive.google.com/uc?authuser=0&id='
  axel_link <- sprintf('%s%s',url,id)
  download.file(axel_link,sprintf('%s%s',dirfile,'/axel.zip'))
}

