\name{projClimate}
\alias{projClimate}
\title{
projClimate
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
projClimate()
projClimate(yr1 = 2010)
}
\arguments{
  \item{yr1}{
Starting year of projections.  Currently only 2010 is allowed.
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
\code{\link{plotClimateProjections}}
}
\examples{
projClimate()

## The function is currently defined as
#projClimate<-function(yr1=2010){
#  eta_precip <- 1.415
#  eta_et <- 0.778
#  
#  data(climate)
#  
#  dF<- climate %>% group_by(model, ASR) %>% 
#      mutate(wpu_climate=ifelse(year==yr1, 0,
#        eta_precip * 6 * ((precipdata[1]-precipdata)/6 + 
#               0.016*((precipdata/6)^2-(precipdata/6)^2))  +
#         eta_et * ((etdata-etdata[1])*365/10) ))
#  return(dF)
#}
}
