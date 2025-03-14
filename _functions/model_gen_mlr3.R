# mlr and mlr3 testing
# Please note that this script is still under development, I just wanted to place an
# initial commit before finishing up for the day.

# model_gen_mlr <- function(traindat, target, feature_selection = "filter", outDir = ".", rseed = NA) {
#   library(mlr)
#   # Create the initial task, learner, and feature filter method
#   task <- makeClassifTask(
#     data = st_drop_geometry(traindat), 
#     target = target, 
#     coordinates = as.data.frame(st_coordinates(traindat))
#   )
#   
#   lrn <- makeLearner(
#     "classif.ranger",
#     num.trees = 100,                         ## number of trees DEFAULT: 500
#     num.threads = parallel::detectCores(),   ## CAUTION HERE: how many threads does your machine have?
#     importance = "impurity",                 ## collect var importance data
#     predict.type = "response"                    ## model will generate prob. and multi-class
#   )
#   
#   lrn_filter <- makeFilterWrapper(
#     learner = lrn, 
#     fw.method = "ranger_impurity", # Note this takes a very long time, perhaps use a different method
#   )
#   
#   if(feature_selection == "filter") {
#     # Create models using different percentages (between 1 variable and all variables)
#     # of the total variables, and use the model with the best result
#     ps <- makeParamSet(makeNumericParam(
#       id = "fw.perc", 
#       lower = 1 / getTaskNFeats(task), 
#       upper = 1
#     ))
#   } else if(feature_selection == "rfe") {
#     # Use recursive feature elimination to select the best model. 
#     ps <- makeParamSet(makeDiscreteParam(
#       id = "fw.abs", 
#       values = seq(getTaskNFeats(task), 1, -1)
#     ))
#   }
#   
#   lrn_tune <- makeTuneWrapper(
#     lrn_filter, 
#     resampling = makeResampleDesc("Holdout"), 
#     par.set = ps, 
#     control = makeTuneControlGrid(), 
#     show.info = TRUE
#   )
#   parallelMap::parallelStart(mode = "socket", cpus = parallel::detectCores())
#   mod <- mlr::train(lrn_tune, task)
#   parallelMap::parallelStop()
#   
#   model_ranger <- getLearnerModel(mod$learner.model$next.model$learner.model$next.model)
#   performance <- getTuneResult(mod)$y
#   features <- getFilteredFeatures(mod)
#   
#   # Create the resampling conditions for model evaluation
#   resampling_outer <- makeResampleDesc(
#     method = "SpRepCV",
#     reps = 5, 
#     folds = 10
#   )
#   
#   # We only want to resample the best model, not all models, so we extract the best
#   # learner and change model predicy types from "prob" to "response" to allow for
#   # resampling to occur (throws an error if left as prob)
#   resp <- mod$learner.model$next.model$learner
#   resp$predict.type <- "response"
#   resp$next.learner$predict.type <- "response"
#   
#   rr <- mlr::resample(
#     learner = resp, 
#     task = task, 
#     resampling = resampling_outer, 
#     models = FALSE, 
#     show.info = TRUE
#   )
#   
#   conf_mat <- as.data.frame.matrix(calculateConfusionMatrix(rr$pred)$result)
#   
# } 


# Please note that I have tried many different things to try to get probability estimation to work
# using the ranger package in mlr and mlr3 however it never seems to work when there are
# few data points for a class, hence why predict_type is response. Other learners have no
# issue using the prob predict_type

# Essentially the way I have this working is as follows: First, create the pipe
# operators which allows for subsetting certain percentages of the top variables
# (ex: a subset of the top 10%, 20%, etc. of variables) and then creates single
# models from the subsetted features. The model performance is evaluated for each
# and then the best model is fitted. This is the "tuning" step and it is performed
# on each of the input "traindat" dataframes. The other option for
# tuning is recursive feature elimination, however this would take much longer, 
# especially if there are a lot fo variables in a dataset. 
# After the best feature subset is found, 10 fold cross validation repeated 5 times 
# is performed in order to generate the model metrics

model_gen_mlr3 <- function(traindat, target, feature_selection = "filter") {
  library(mlr3verse)
  #traindat <- tpts
  #target <- "mapunit"
  #feature_selection = "filter"
  
  # perform input checks
  if(!is.list(traindat)) 
    stop("Please provide a list of dataframes for the 'traindat' argument.")
  
  if(!all(sapply(traindat, function(x) is.data.frame(x))))
    stop("List argument provided, but not all list objects are dataframes.")
  
  if(!is.character(target))
    stop("'target' argument is not a character vector. Additionally, 'target' should
         be of length 1 and should be the same for each listed dataframe.")
  
  # Check for X and Y columns. If present, a spatial cv will be performed
  if(all(sapply(traindat, function(x) 
    length(which(names(x) %in% c("X", "x"))) == 1 && length(which(names(x) %in% c("Y", "y"))) == 1))) {
    spatial <- TRUE
  } else {
    # remove any x and y columns if it isn't spatial
    spatial <- FALSE
    sp_index <- which(sapply(traindat, function(x) 
      length(which(names(x) %in% c("X", "Y"))) == 2 || length(which(names(x) %in% c("x", "y")))))
    
    traindat <- lapply(traindat, function(x) x[, names(x)[!names(x) %in% c("X", "Y", "x", "y")]])
  }
  
  # Create pipeop learner, note I had some issues with this, error included duplicated value
  # in "name" column, but if the mlr3spatiaotempcv package is not loaded here, this 
  # works fine. I submitted this as an issue to the mlr3pipelines page: 
  # https://github.com/mlr-org/mlr3pipelines/issues/368
  # The workaround is to restart R and rerun the po_lrn code without the task loaded
  
  # Note: don't use the mtry argument here, or problems will arrive later on
  lrn <- lrn(
    "classif.ranger", 
    num.trees = 100,
    num.threads = parallel::detectCores(),
    importance = "impurity",
    predict_type = "response"
  )
  
  po_lrn <- po("learner", lrn)
  
  # Create feature filter based on variable importance
  filter <- po("filter", filter = mlr3filters::flt("importance", learner = lrn))
  
  # Create process (new learner) for filtering the task
  glrn <- GraphLearner$new(filter %>>% po_lrn)
  
  # Create task from list of dataframes
  if(spatial == TRUE) {
    if(!("mlr3spatiotempcv" %in% installed.packages()[, "Package"]))
      devtools::install_github("mlr-org/mlr3spatiotempcv")
    
    library(mlr3spatiotempcv)
    message("Performing spatial sampling")
    
    tasks <- sapply(seq_along(traindat), function(x) {
      TaskClassifST$new(id = gsub(".gpkg$", "", basename(names(traindat[x]))), 
                        backend = traindat[[x]], 
                        target = target, 
                        coords_as_features = FALSE, 
                        crs = st_crs(3005)$proj4string, 
                        coordinate_names = c("X", "Y"))
    }, simplify = FALSE, USE.NAMES = TRUE)
    
    resampling_outer <- rsmp("repeated-spcv-coords", folds = 10, repeats = 5)
    
  } else {
    message("Performing non-spatial sampling")
    
    tasks <- sapply(seq_along(traindat), function(x) {
      TaskClassif$new(id = gsub(".gpkg$", "", basename(names(traindat[x]))), 
                        backend = traindat[[x]], 
                        target = target)
    }, simplify = FALSE, USE.NAMES = TRUE)
    
    resampling_outer <- rsmp("repeated_cv", folds = 10, repeats = 5)
  }
  
  if(feature_selection == "filter") {
    # Create filter parameters (i.e.: filter the top features of importance using
    # a given percentage of total features)
    param_set <- ParamSet$new(
      params = list(ParamDbl$new("importance.filter.frac", 
                                 lower = 1 / length(tasks[[1]]$feature_names), 
                                 upper = 1))
    )
    
    # Create the autotuner which will generate models using the top features of 
    # importance, using 1 feature and numbers between 1 and the total number of 
    # features (ex: if tuner has resolution 10, there will be 9 other values of 
    # features used in model creation. The best model will be used to create the 
    # resampling task.)
    feature_sel <- AutoTuner$new(
      learner = glrn, 
      resampling = rsmp("holdout"), 
      measures = msr("classif.ce"),                  
      tune_ps = param_set, 
      terminator = term("none"), 
      tuner = tnr("grid_search", resolution = 10)
    )
  } else if(feature_selection == "rfe") {
    
    if(!("mlr3fselect" %in% installed.packages()[, "Package"]))
      devtools::install_github("mlr-org/mlr3fselect")
    
    library(mlr3fselect)
    
    f_select <- fs("rfe")
    f_select$param_set$values$recursive <- TRUE # Recalculates importance each iteration
    
    # Create the feature selection learner. Models will be compared at each iteration
    # of a feature being removed. If the models don't improve after 5 iterations, then
    # feature selection stops, using the best evaluated model
    feature_sel <- AutoFSelect$new(
      learner = lrn,
      resampling = rsmp("holdout"), 
      measures = msr("classif.ce"), 
      terminator = term("stagnation", iters = 5, threshold = 1e-5), 
      fselect = f_select 
    )
  }
  
  feature_sel$store_tuning_instance <- TRUE
  
  design <- benchmark_grid(tasks, feature_sel, resamplings = rsmp("holdout", ratio = 1))
  bmr <- benchmark(design, store_models = TRUE)
  
  # Compare best results
  all_results <- lapply(1:bmr$n_resample_results, function(x) 
    bmr$resample_result(x))
  
  best_results <- lapply(all_results, function(x) 
    x$learners[[1]]$tuning_instance$result$perf)
  
  # Extract best model for evaluation
  mod <- bmr$resample_result(which.min(best_results))$learners[[1]]
  features <- mod$model$learner$model$importance$features
  
  ######################################################################
  # Perform resampling
  best_task <- tasks[[which.min(best_results)]]
  rr <- resample(
    task = best_task$select(features), 
    learner = lrn, 
    resampling = resampling_outer,
    store_models = TRUE
  )
  
  # Compare best results
  all_results_rr <- lapply(1:length(rr$learners), function(x) 
    rr$learners[[x]])
  
  best_results_rr <- lapply(all_results_rr, function(x) 
    x$model$prediction.error)
  
  best_model <- rr$learners[[which.min(best_results_rr)]]$model
  best_mod_train <- rr$resampling$train_set(which.min(best_results_rr))
  best_mod_test <- rr$resampling$test_set(which.min(best_results_rr))
  
  # Extract aggregated confusion matrix from all resampling iterations
  conf_mat <- as.data.frame.matrix(rr$prediction()$confusion)
  
  # Return a list of variables from the run
  return(list(benchmark = bmr, resampleresult = rr, bestmodel = best_model, 
              trainset = best_mod_train, testset = best_mod_test, confusion = conf_mat))
}
