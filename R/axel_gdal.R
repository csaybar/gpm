#' Download files via axel
axel <- function(url,destfile,n,s,quiet){
  s = s*1000000 # mb to byte
  if (quiet) {
    down_file <- sprintf("axel -n %s -s %s %s -o %s --quiet",n,s,url,destfile)
  } else {
    down_file <- sprintf("axel -n %s -s %s %s -o %s",n,s,url,destfile)
  }

  system(down_file)
}

#' HDF5 to GEOTIFF using GDAL
gdal_translate <- function(hdf5,band,outfile,removeHDF5){
  gdal_exp <- sprintf('gdal_translate %s %s',
                      get_subdatasets(hdf5)[band],
                      outfile)
  system(gdal_exp)
  if (removeHDF5) {
    file.remove(hdf5)
  }
}
