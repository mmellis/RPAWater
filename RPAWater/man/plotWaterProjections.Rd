\name{plotWaterProjections}
\alias{plotWaterProjections}
\title{
plotWaterProjections
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotWaterProjections()
plotWaterProjections(dF = projWater())
}
\arguments{
  \item{dF}{
Optional - data.frame of climate projections.  Otherwise created by \code{projWater()}
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
\code{\link{projWater}}
}
\examples{
projWaterProjections()

### The function is currently defined as
#plotWaterProjections<-function(dF=NULL){
#  if(is.null(dF)) dF<-projWater()
# message('Climate models not yet implemented')
#  p0<-ggplot(dF, aes(x=year, y=wd, colour=scenario, group=interaction(ASR,scenario)))+
#    geom_line()+facet_grid(sector~model,scales='free_y')
#  print(p0)
#  return(invisible(p0)) 
#}
}