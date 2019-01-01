# gpm
gpm is a R package that is able to download IMERG v05B Final, Early and Late Run products using axel.

## Installing
``` r
devtools::install_github("csaybar/gpm")
```
## Example
``` r
library(gpm)

gpm_download(path = '~/gpm',
            user = 'you_user@gmail.com',
            password = 'you_user@gmail.com',
            dates = c('2018-01-01'),
            product = 'finalrun')
```

## Windows user
If you are using Windows, the executables of "GDAL" and "axel" must be set as PATH system variables. Acquire **axel** by following:

``` r
library(gpm)
gpm_getaxel()
```
