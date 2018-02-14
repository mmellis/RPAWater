\name{plotClimateProjections}
\alias{plotClimateProjections}
\title{
plotClimateProjections
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotClimateProjections()
plotClimateProjections(dF = projClimate())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dF}{
Optional - data.frame of climate projections.  Otherwise created by \code{projClimate()}
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
\code{\link{projClimate}}
}
\examples{
plotClimateProjections()

### The function is currently defined as
#plotClimateProjections<-function(dF=NULL){
#  if(is.null(dF)) dF<-projClimate() 
#  p0<-ggplot(projClimate(), aes(x=year, y=wpu_climate, group=ASR))+
#    geom_line()+facet_grid(sector~model)
#  print(p0)
#  return(invisible(p0)) 
#}
}

