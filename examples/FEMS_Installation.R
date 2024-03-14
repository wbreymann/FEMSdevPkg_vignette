# This script helps you to installs the package FEMS and all the packages needed 
# beforehand.

# Packages required by FEMS:
packageList <- c("timeDate", 
                 "timeSeries", 
                 "zoo", 
                 "httr", 
                 "lubridate",
                 "fmdates",
                 "jsonlite",
                 "readxl",
                 "data.tree",
                 "devtools"
                 )
# Installing these packages from CRAN:
install.packages(packageList)

# To install FEMS from public github
devtools::install_github("wbreymann/FEMS")

