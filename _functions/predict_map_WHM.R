#' Predict Map from model
#'
#' @param model rf model file
#' @param out_dir location to export predicted tiles
#' @param tile_dir location of template tiles
#' @param rstack spatRast stack of all covars
#' @param probability TRUE or FALSE if probability rasters are to be exported
#' @param model_name_label name of the model file
#'
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' predict_map(rf_fit, out_dir, tile_dir, rstack, probability = FALSE)
#'}
predict_map <- function(model,
                        out_dir,
                        tile_dir,
                        rstack,
                        probability = FALSE,
                        model_name_label = "map.tif") {
  # extract fit
  rf_fit <- workflows::extract_fit_engine(model_fit)
  .pred_class <- rf_fit$forest$levels
  respNames <- as.data.frame(.pred_class) |>
    dplyr::mutate(pred_no = seq(1:length(.pred_class)))

  utils::write.csv(respNames, file.path(out_dir, "response_names.csv"), row.names = TRUE)

  ntiles <- list.files(tile_dir, full.names = T)
  a <- 0 ## running total of area complete
  ta <- sum(as.numeric(length(ntiles)))

  for (i in ntiles) {
    # i = ntiles[3]
    out_name <- basename(i)

    # create tracking message
    t <- terra::rast(file.path(i)) ## read in tile
    cli::cli_alert_info("working on {out_name} of {length(ntiles)}")
    cli::cli_alert_info("... loading data ...")

    # check if blank tile
    if (all(is.na(unique(terra::values(t)))) == TRUE) {
      cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
    } else {
      # crop the raster stack to tile extent
      tstack <- terra::crop(rstack, t)
      # convert to dataframe
      rsf <- as.data.frame(tstack)
      # get xy values
      # rsfxy <- terra::crds(tstack)

      # check if all values in columns are NA (ie not in study area)
      na_table <- as.data.frame(sapply(rsf, function(x) all(is.na(x))))

      if (any(na_table[, 1] == TRUE)) {
        cli::cli_alert_warning("Some variables with all NA values, skipping tile...")
      } else {
        # predict
        pfun <- \(...) {
          predict(...)$predictions
        }
        #map <- terra::predict(raster_stk2, BGCmodel, fun = pfun, na.rm = TRUE)
        pred <- terra::predict(tstack, rf_fit, fun = pfun, na.rm = TRUE)

        # write out probability layer
        if (probability == TRUE) {
          if (!dir.exists(file.path(out_dir, "probability"))) {
            dir.create(file.path(out_dir, "probability"))
          } else {
            cli::cli_alert_info("probability dir exists")
          }
          terra::writeRaster(pred, file.path(out_dir, "probability", out_name), filetype = 'GTiff', overwrite = TRUE)
          cli::cli_alert_success("writing probability tile")
        }

        # write out best class
        if (!dir.exists(file.path(out_dir, "best"))) {
          dir.create(file.path(out_dir, "best"))
        }

        pdfxy <- as.data.frame(pred, xy = TRUE, cells = FALSE)
        pdf <- pdfxy |> dplyr::select(-.data$x, -.data$y)
        pdfid <- pdfxy |> dplyr::select(.data$x, .data$y)

        best_class <- colnames(pdf)[apply(pdf[, 2:length(pdf)], 1, which.max)]

        r_out <- cbind(pdfid, as.factor(best_class))
        names(r_out) <- c("x", "y", ".pred_class")

        ## change the text values to numeric values.
        r_out <- dplyr::left_join(r_out, respNames, by = ".pred_class")
        r_out <- r_out |> dplyr::select(-".pred_class")

        cli::cli_alert_info("... exporting raster tiles...")

        out <- tidyterra::as_spatraster(r_out, crs = "epsg:3005")

        terra::writeRaster(out, fs::path(out_dir, "best", out_name), filetype = 'GTiff', overwrite = TRUE)
      }
    }

    ## * report progress -----
    a <- a + 1
    prog <- round(a / ta * 100, 0)
    cli::cli_alert_info("{prog} % complete")
  }

  cli::cli_alert_success("All predicted tiles generated")

  r_tiles <- list.files(fs::path(out_dir, "best"), pattern = ".tif$", full.names = TRUE)
  rsrc <- terra::sprc(r_tiles)
  m <- terra::mosaic(rsrc, fun = "min")
  terra::writeRaster(m, fs::path(out_dir, model_name_label), filetype = 'GTiff', overwrite = TRUE)

  if (probability == TRUE) {
    r_tiles <- list.files(fs::path(out_dir, "probability"), pattern = ".tif$", full.names = TRUE)
    rsrc <- terra::sprc(r_tiles)
    m <- terra::mosaic(rsrc, fun = "min")
    terra::writeRaster(m, fs::path(out_dir, paste0("probability_", model_name_label)), filetype = 'GTiff', overwrite = TRUE)
  }

  return(TRUE)
}
