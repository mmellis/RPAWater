\name{climate}
\docType{data}
\alias{climate}
\title{Data for climate projections}
\description{
 A data.frame containing growth and decay rates for water use drivers by sector and region. 
}
\usage{drivers}
\format{Data.frame with 7 columns (model=climate model, ASR, year, pricipdata=precipitation projection, etdata=evapotranspiration data, sector - only 'dp' available)}
\source{See original script \code{data-raw/RPA-Water_MEllis.R}}
\section{Data}{\verb{
> summary(climate)
       model          ASR            year        precipdata        etdata             sector    
 CGC.A1B  :114   Min.   :1401   Min.   :2010   Min.   :-0.48   Min.   : 5.055   dp       :1026  
 CGC.A2   :114   1st Qu.:1402   1st Qu.:2030   1st Qu.:11.62   1st Qu.: 6.459   ic       :   0  
 CGC.B2   :114   Median :1452   Median :2055   Median :14.80   Median : 7.522   thermo   :   0  
 CSIRO.A1B:114   Mean   :1452   Mean   :2055   Mean   :15.08   Mean   : 7.788   irrig    :   0  
 CSIRO.A2 :114   3rd Qu.:1502   3rd Qu.:2080   3rd Qu.:18.36   3rd Qu.: 9.200   livestock:   0  
 CSIRO.B2 :114   Max.   :1503   Max.   :2100   Max.   :38.36   Max.   :12.689                   
 (Other)  :342                                                                               
}  }

\keyword{datasets}