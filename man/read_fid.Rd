% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_fid}
\alias{read_fid}
\title{Read folder structure file}
\usage{
read_fid(fid_path = file.path("_meta", "fid.rds"))
}
\arguments{
\item{fid_path}{Path to the fid file. Defaults to \code{"_meta/fid.RDS"}}
}
\value{
list containing folder structure
}
\description{
Read the fid file from the \verb{_meta} directory
}
