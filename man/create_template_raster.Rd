% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_template_raster.R
\name{create_template_raster}
\alias{create_template_raster}
\title{Create a raster template from AOI}
\usage{
create_template_raster(
  aoi = fs::path(PEMprepr::read_fid()$dir_1010_vector$path_abs, "aoi_snapped.gpkg"),
  res = 25,
  filename = "template.tif",
  out_dir = PEMprepr::read_fid()$dir_1020_covariates$path_rel,
  write_output = TRUE,
  overwrite = TRUE
)
}
\arguments{
\item{aoi}{is a sf or terra::vect object bounding box created expanded
in aoi_snap function(e.g. polygon).  Should be a meter based
projection}

\item{res}{desired resolution of the raster (in meters)}

\item{filename}{text name of the output file. the default is template.tif}

\item{out_dir}{output path.  Note that the results will be placed in a
subfolder labelled with the resolution.}

\item{write_output}{should the template raster be written to disk?
If \code{TRUE} (default), will write to \code{out_dir}}

\item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
}
\value{
a terra raster
}
\description{
Sets the baseline raster template to align and standardize all raster predictor layers
This package assumes to data is in a metric equal area projection most likely BCAlbers
}
\details{
This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
Having the aoi set 100m break-points facilitates this.
}
\examples{
\dontrun{
create_template_raster(
    aoi = fs::path(PEMr::read_fid()$dir_1010_vector$path_abs, "aoi_snapped.gpkg"),
    res = 25,
    filename = "template.tif",
    out_dir = PEMr::read_fid()$dir_1020_covariates$path_rel,
    write_output = TRUE)
}
}
