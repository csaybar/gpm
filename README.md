# gpm
gpm is a R package that is able to download IMERG v05B Final, Early and Late Run products

## Installation
devtools::install_github("csaybar/gpm")

## usage
gpm_download(path = '~/gpm',
            user = 'you_user@gmail.com',
            password = 'you_user@gmail.com',
            dates = c('2018-01-01'),
            product = 'early')
