\name{projWater}
\alias{projWater}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
projWater
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
projWater(year = NULL, watershed = NULL, sector = NULL, scenario = NULL, model = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
Optional - vector of years to project.  Defaults to \code{year <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100) }
}
  \item{watershed}{
Optional - vector of watersheds to project.  Defaults to \code{watershed <- c(1401, 1402, 1403, 1501, 1502, 1503) }
}
  \item{sector}{
Optional - vector of watersheds to project. Defaults to \code{ sector <- c("dp", "ic", "thermo", "irrig","livestock") }
}
  \item{scenario}{
Optional - vector of watersheds to project. Defaults to \code{scenario <- c("a1b", "a2", "b2") }
}
  \item{model}{
Optional - vector of watersheds to project. Defaults to \code{watershed <- c("CGC.A1B", "CGC.A2", "CGC.B2", 
            "CSIRO.A1B", "CSIRO.A2", "CSIRO.B2", 
            "MIROC.A1B", "MIROC.A2", "HAD.B2") }
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}

\seealso{
\code{\link{plotWaterProjections}}
}
\examples{
projWater()
projWater(watershed=1501)

## The function is currently defined as
#function (year = NULL, watershed = NULL, sector = NULL, scenario = NULL, 
#    model = NULL) 
#{
#    if (is.null(year)) 
#        year <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 
#            2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 
#            2095, 2100)
#    if (is.null(watershed)) 
#        watershed <- c(1401, 1402, 1403, 1501, 1502, 1503)
#    if (is.null(sector)) 
#        sector <- c("dp", "ic", "thermo", "irrig", "livestock")
#    if (is.null(scenario)) 
#        scenario <- c("a1b", "a2", "b2")
#    if (is.null(model)) 
#        model <- c("CGC.A1B", "CGC.A2", "CGC.B2", "CSIRO.A1B", 
#            "CSIRO.A2", "CSIRO.B2", "MIROC.A1B", "MIROC.A2", 
#            "HAD.B2")
#    num_years <- length(year)
#    num_sheds <- length(watershed)
#    num_scenarios <- length(scenario)
#    num_models <- length(model)
#    wd <- expand.grid(year = year, ASR = watershed, sector = sector, 
#        scenario = scenario, model = model)
#    wd <- addDrivers(wd)
#    wd <- addInitialValues(wd, yr1 = year[1])
#    wd <- calcWPU(wd, yr1 = year[1])
#    wd <- calcWD(wd)
#    return(wd)
#  }
}
