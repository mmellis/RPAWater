\name{RPAWater-package}
\alias{RPAWater-package}
\alias{RPAWater}
\docType{package}
\title{RPA Water Projections}
\description{
Shifting code and data from 2010rpaMODEL.xlsx into R scripts and functions
}
\details{
\tabular{ll}{
Package: \tab RPAWater\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2018-02-27\cr
License: \tab GPL (>=3)\cr
}
This is just a start.  
}

\examples{       
projWater()
plotWaterProjections(dF=projWater())

projClimate()
plotClimateProjections(dF=projClimate())

}
