Download and data management IMERG-GPM (R-function)
=========================================================
|||
|-----------------|----------------------------------------------------------------------------------|
| **Maintainer:** | Cesar Luis Aybar Camacho-                                                        |
| **Contact:**    | aybar1994@gmail.com                                                              |
| **Version:**    | 2016-01-24                                                                       |


### What is IMERG?: (Extracted from Guoqiang Tang, 2015)
Download the post-real time product of Day-1 Integrated Multi-satellitE Retrievals for Global Precipitation
Measurement (IMERG).IMERG is the Level 3 multi-satellite precipitation algorithm of GPM, which combines intermittent precipitation estimates from all constellation microwave sensors, IR-based observations from geosynchronous satellites, and monthly gauge precipitation data (Hou et al., 2014). Currently, IMERG is at its very early Day-1 stage. IMERG employs the 2014 version of the Goddard Profiling Algorithm (GPROF2014) to compute precipitation estimates from all passive microwave (PMW) sensors onboard GPM satellites, which is an improvement compared with TMPA (GPROF2010) (Huffman et al., 2014, 2015). IMERG ‘‘Final” run combines the GPCC Monitor-ing Product (currently Version 4) in the product, whose data source is limited to the Global Telecommunications System (GTS) with only about 7000 stations over the globe. The Full Data Reanalysis (currently Version 6) involves much more stations than the Monitoring Product, but only covers the period 1901–2010. The IMERG data were also downloaded from the PMM website ( http://pmm.nasa.gov/data-access/downloads/gpm).

##What makes these functions?
**download_GPM.R:**
It helps download information late, early and end run of IMERG, mainly supports the following ftp://jsimpson.pps.eosdis.nasa.gov/data/imerg/ and ftp: //arthurhou.pps.eosdis.nasa. gov / gpmdata / "axel download accelerator for download is used for the explanation of why can be reviewed http://www.linuxjournal.com/content/speed-your-downloads-axel the script can save the data in HDF5 or convert GeoTIFF for which executables GDAL so must be properly installed on your operating system was used.

**accumIMERG.R:**
Allows parallel accumulation of files IMERG 30 minutes from time steps of 3 hours, daily and monthly.

##How to use this function?

**download_GPM.R:** You need to have installed "Axel download accelerator application" and gdal.
          Obs: If you are using Windows OS executables it should be declared as environment variables.
          Download Linux axel: apt-get install axel
          Download windows axel: https://st0rage.org/~n2j3/?page_id=225717166
R libraries are used RCurl, raster, rgdal and GdalUtils, these must also be correctly installed.

**accumIMERG.R:** You need to have installed foreach, rgdal and raster packages.
