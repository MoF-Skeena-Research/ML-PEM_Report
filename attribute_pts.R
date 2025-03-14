#' Add covariate predictor data to training point file
#'
#' @param data_pts a file gpkg of training points
#' @param cov_dir folder containing covariate files
#' @param write_output A `logical`if the sf object be written to disk?
#'     If `TRUE` (default), will write to `out_dir` under the appropriate resolution subfolder.
#' @param out_dir A character string of path which points to output location. A default
#'    location and name are applied in line with standard workflow.
#' @param out_name A character string of the output file name. Default is `allpoints_att.gpkg`
#' @return an sf object
#' @export
#' @examples
#' \dontrun{
#' tpoints_ne <- attribute_points(dat_pts, cov_dir)
#' }
# attribute_points <- function(data_pts,
#                              cov_dir,
#                              write_output = TRUE,
#                              out_dir = fs::path(PEMprepr::read_fid()$dir_20105020_clean_field_data$path_rel, out_name = "allpoints.gpkg"),
#                              out_name = "allpoints_att.gpkg") {
data_pts <- vect("./model_inputs/10_training_data/allpoints_fixed.gpkg")
  # get list of raster
  rastlist <- list.files("./model_inputs/20_covariates/model", pattern = ".sdat$|.tif$", recursive = T, full.names = T)
  ancDat <- terra::rast(rastlist)
  atts <- terra::extract(ancDat, data_pts)
  att_all <- dplyr::bind_cols(sf::st_as_sf(data_pts), atts)
  sf::st_write(att_all, "./model_inputs/10_training_data/allpoints_fixed_att2.gpkg", driver = "GPKG", append = FALSE)

