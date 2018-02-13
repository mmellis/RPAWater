\name{drivers}
\docType{data}
\alias{drivers}
\title{Water use drivers}
\description{
  A data.frame containing 342 observations form two watersheds (14,15), 3 scenarios (a1b, a2, b2), projected out for 2010-2100.  Includes population projects (used to project water demand for domestic and public use) and income (used to project water demand for industrical and commerical use) 
}
\usage{drivers}
\format{Data.frame with 5 columns (ASR = unit, scenario, year (projected), pop, inc)}
\source{See original script \code{data-raw/RPA-Water_MEllis.R}}
\section{Data}{ \verb{
> head(drivers)
   ASR scenario year       pop        inc
1 1401      a1b 2010  199800.0   7.489330
2 1402      a1b 2010  425286.2  15.763071
3 1403      a1b 2010  328302.2   9.570906
4 1501      a1b 2010  298244.4   7.616871
5 1502      a1b 2010 2624666.3  95.173232
6 1503      a1b 2010 5963303.4 208.876114

} }

\keyword{datasets}