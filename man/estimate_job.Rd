% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs.R
\name{estimate_job}
\alias{estimate_job}
\title{Estimates job costs}
\usage{
estimate_job(con, job)
}
\arguments{
\item{con}{authenticated Connection}

\item{job}{the job or the id of the job}
}
\value{
JobCostsEstimation containing information how much money and time will be spent
}
\description{
Calls the back-end and asks for an approximation about the costs in money and how much time
will be required to finish the job and whether or not the job owners data download is already
included in the monetary costs.
}
