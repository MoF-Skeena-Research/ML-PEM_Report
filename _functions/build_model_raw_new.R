#Sure To create a separate model and results for each `bgc`, you can loop through each unique `bgc` value and apply the existing model-building process. Here's how you can modify your function to include this loop:

build_model_raw_new <- function(trDat, model.name = "", model_type = "", extrarun = FALSE, extradat = NULL,
                                downsample = FALSE, downratio = 1000, smote = FALSE, use.neighbours = TRUE) {
  balance_name <- model.name

  # Get unique BGC labels
  bgc_labels <- unique(trDat$bgc)

  # Initialize a list to store results for each BGC
  all_results <- list()

  # Loop through each BGC label
  for (bgc in bgc_labels) {
    print(paste("Processing BGC:", bgc))
#bgc = "SBSmc2"
    # Filter data for the current BGC
    trDat_bgc <- trDat %>% filter(bgc == !!bgc) %>% arrange(slice)

    # Cross-validation loop based on slices
    slices <- unique(trDat_bgc$slice) %>% droplevels()

    # For all slices
    sresults <- foreach(k = levels(slices)) %do% {
      print(k)
     # k = levels(slices)[2]
      # Split into train and test based on 5-site slices
      BGC_train <- trDat_bgc %>%
        filter(!slice %in% k, position == "Orig") %>%
        filter(is.na(mapunit2) | mapunit2 == "") %>%
        dplyr::select(-slice, -bgc, -position, -mapunit2, -id, -transect_id) %>%
        mutate(across(tid, str_replace, " ", "")) %>%
        mutate(mapunit1 = factor(mapunit1)) %>%
        drop_na(mapunit1) %>%
        droplevels()

      # Merge in extra point data if required
      if (extrarun) {
        print("Adding extra points at each model build")
        BGC_train <- bind_rows(BGC_train, extradat)
      }

      # Check if enough data for each class to smote
      # MU_count <- BGC_train %>%
      #   count(mapunit1) %>%
      #   filter(n < 10) %>%
      #   pull(mapunit1)

      # Prepare test data
      BGC_test <- trDat_bgc %>%
        filter(slice %in% k)# %>%
        #filter(!mapunit1 %in% MU_count)

      if (!use.neighbours) {
        BGC_test <- BGC_test %>%
          filter(position == "Orig")
      }

      BGC_test_all <- BGC_test
      BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
      BGC_test <- BGC_test %>%
        dplyr::select(-slice, -mapunit2, -position, -transect_id) %>%
        mutate(mapunit1 = factor(mapunit1))

      # Define recipe and model
      if (!downsample) {
        null_recipe <- recipe(mapunit1 ~ ., data = BGC_train) %>%
          update_role(tid, new_role = "id variable")
      } else {
        null_recipe <- recipe(mapunit1 ~ ., data = BGC_train) %>%
          update_role(tid, new_role = "id variable") %>%
          step_downsample(mapunit1, under_ratio = downratio)
      }

      randf_spec <- rand_forest(trees = 151) %>%
        set_mode("classification") %>%
        set_engine("ranger", importance = "permutation", verbose = FALSE)

      pem_workflow <- workflow() %>%
        add_recipe(null_recipe) %>%
        add_model(randf_spec)

      # Fit the model
      PEM_rf1 <- fit(pem_workflow, BGC_train)
      final_fit <- extract_fit_parsnip(PEM_rf1)
      oob <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)

      # Predict test data
      test_target <- BGC_test_all %>%
        dplyr::select(id, mapunit1, mapunit2)
      test.pred <- predict(PEM_rf1, BGC_test) %>%
        cbind(test_target) %>% mutate_if(is.factor,as.character) %>%  distinct()
      test.pred <- test.pred %>%
        mutate(
          mapunit1 = ifelse(grepl("_\\D$", mapunit1), "nonfor", mapunit1),
          mapunit2 = ifelse(grepl("_\\D$", mapunit2), "nonfor", mapunit2),
          .pred_class = ifelse(grepl("_\\D$", .pred_class), "nonfor", .pred_class)
        )
      test.pred <- test.pred  %>%
        mutate_if(is.factor, as.character) %>%
        mutate_all(as.factor) %>%
        distinct()

      # Harmonize levels
      levs <- unique(c(levels(test.pred$mapunit1), levels(test.pred$.pred_class)))
      test.pred$mapunit1 <- factor(test.pred$mapunit1, levels = levs)
      test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)

      # Output test predictions
      test.pred.out <- test.pred %>%
        mutate(slice = k)

      acc.compare <- acc_metrix(test.pred, theta = 0.5) %>%
        mutate(slice = k,
               transect_no = BGC_test_transect_no,
               acc_type = "test_estimate",
               oob = oob,
               balance = balance_name) %>%
        dplyr::select(slice, mapunit1, balance, trans.sum, trans.tot, pred.tot, acc, kap, everything())

      return(list(acc.compare))
    }

    # Extract results from sresults
    acc_results <- lapply(sresults, function(x) x[[1]])
    acc <- as.data.frame(rbindlist(acc_results))
    acc <- acc %>%
      mutate(bgc = bgc)
    # Store results for the current BGC
    all_results[[bgc]] <- acc

    outDir = paste0("./models/", model_type ,"/acc_reports/")
    if (!dir.exists(fs::path(outDir))) {
      dir.create(fs::path(outDir))
    }
    # Write results to CSV
    write.csv(acc, file = paste0(outDir,"acc_", balance_name, "_", bgc, ".csv"))
  }

  return(all_results)
}


