
# function to run model with internal AA version 

modelt_intAA <- function(trDat, target, target2, tid, outDir = ".", mname = "model", rseed = NA, 
                      infiles = infiles, mmu = mmu) {
  
  library(cowplot)
  library(tidymodels)
  library(tidyverse)
  library(themis)
  
  # ## testing : GP
  # trDat = mpts       # data table
  # target = "target"  # primary call or target column name
  # target2 = "target2"
  # tid = "tid"          # transect_id column
  # outDir =  file.path(paste(out_dir, paste0(mname), sep = "/")) #output file
  # indata = indata    # name of input data file for reporting
  # rseed = 456        # seed
  # mmu = mmu
  # mname = mname      # model type for reporting

  # set up parallel processing 
 # cores <- parallel::detectCores()

  
  #mpts = mpts[, names(mpts)[grep("^b|^sen_|^n|^p[:digit:]", names(mpts), invert = T)]]
  #odata <- mpts
  
  
  # get a summary of the training points 

  table(trDat$target)
  
  # remove covars with Na values 
  trDat_all <- trDat[complete.cases(trDat[ , 5:length(trDat)]),]
  
  trDat <- trDat_all %>%
    dplyr::select(-c(target2, tid, bgc_cat))
  
  
  # 1: split into training and test data 3/4 training and testing 
  set.seed(1234)
  uni_split <-  initial_split(trDat, 
                              strata = target,
                              prop = 0.8)
  
  t_train <- training(uni_split)
  t_test <- testing(uni_split)
  
  # 2: set up cross validation with analysis and assessment data sets (analogous to old train and test)
  set.seed(345)
  pem_cvfold <- vfold_cv(t_train, #v = 10,
                         v = 3, 
                         repeats = 5, 
                         strata = target)
  
  
  # 3: set up preparation of data sets 
  
  uni_recipe <-
    recipe(target ~ ., data = t_train) %>%
    #step_corr(all_numeric()) %>% 
    step_dummy(all_nominal(),-all_outcomes()) %>%    # remove correlated covariates
    #step_center(all_numeric(), -all_outcomes()) %>%   # centre and scale all numeric values
    step_zv(all_numeric()) %>%          # remove values with no variance
   # step_normalize(all_numeric()) %>%   
    #step_scale(all_numeric()) %>%
    #step_zv()
    #step_medianimpute(??)
    #step_smote(target)%>%
    prep()
  

  # 4: create a workflow 
  pem_workflow <- workflow() %>%
    add_recipe(uni_recipe)
  
  
## 5: define the models to test : 
#  ranger_spec <- rand_forest(mtry = 3, trees = 500) %>%
#    set_mode("classification") %>%
#    set_engine("ranger", importance = "impurity") #or "permutations
  
  randf_spec <- rand_forest(trees = 500) %>%
    set_engine("randomForest") %>%
    set_mode("classification") 
 
  
  # to do create a grid for tuning 
  # https://www.tidymodels.org/start/case-study/
  # http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
  # 
  # rf_grid <- expand.grid(mtry = c(3, 4, 5), trees = c(100, 500))
  # 
  # # extract result
  # rf_tune_results <- pem_workflow %>%
  #   add_model(randf_spec) %>%
  #   tune_grid(resamples = pem_cvfold, #CV object
  #             grid = rf_grid, # grid of values to try
  #             metrics = metric_set(accuracy, roc_auc) # metrics we care about
  #   )
  # rf_tune_results %>%
  #   collect_metrics()
  # 
  # param_final <- rf_tune_results %>%
  #   select_best(metric = "accuracy")
  # param_final
  
  
  
  ## build model for prediction total train data
  
#  model_result <- pem_workflow %>% 
#    add_model(randf_spec) %>%
#    fit(training(uni_split))
  
 # model_result.var <- pull_workflow_fit(model_result)$fit
  
#  varimp <- as.data.frame(model_result.var$importance)
#  varimp
  

## https://github.com/tidymodels/tune/issues/159
# doParallel::registerDoParallel()
# foreach::getDoParWorkers()
# clusterEvalQ(cl, {library(tidymodels)})
 
 cv_results <-  pem_workflow %>%
    add_model(randf_spec) %>%
    fit_resamples(
      resamples = pem_cvfold,
      metrics = metric_set(roc_auc, accuracy, sensitivity,specificity ),
                           control = control_resamples(save_pred = TRUE)
    )
  
 
 #saveRDS(cv_results, file = file.path(outDir, "tmodel_cv_results.rds"))
 
 # accuracy measures 
 cv_acc <- collect_metrics(cv_results)
 cv_acc
 
 # positive predictive value 
 cv_results %>%
   collect_predictions() %>%
   #group_by(id)%>%
   ppv(target, .pred_class) # positive predictive value 
  
 # ROC curve
 cv_results %>%
   collect_predictions() %>%
   #group_by(id) %>%
   roc_curve(target, .pred_ESSFmc_01:.pred_SBSmc2_10) %>%
   autoplot()
 
 # confusion matrix
  cv_results %>%
   conf_mat_resampled() #%>%
   #autoplot(type = "heatmap")
 
  
 # Test the model with the predicted variable importance: 
 
  test_result <-  pem_workflow %>%
    add_model(randf_spec) %>%
    last_fit(uni_split)
  
  collect_metrics(test_result)
 
  collect_predictions(test_result) %>%
    conf_mat(target, .pred_class) %>%
    autoplot(type = "heatmap")

  #confusion matrix 
  collect_predictions(test_result)  %>%
    conf_mat(target, .pred_class) %>%
    pluck(1) %>%
    as_tibble () %>%
    ggplot(aes(Prediction, Truth, alpha = n)) +
    geom_tile(show.legend = FALSE) +
    geom_text(aes(label = n), colour = "white", alpha = 1, size = 8)
  
  test_result$.workflow[[1]] 
  
   # varible importance
  
   #varimp <- as.data.frame(test_results$fit$importance)
# References: 
# https://towardsdatascience.com/modelling-with-tidymodels-and-parsnip-bae2c01c131c
  
  
 
   
  # looking at predicted mapped surface  
  # Accuracy per mapped transect resample: 
   trDat_all 
   trID <- unique(trDat_all$tid) %>% as.character()
   
   
  
   
   ntrID <- length(trID)
   
   trAll <- trDat_all  
    
   table(trAll[, target])
   
   # get the weighing values for the test dataset
   MU_count <-  trAll %>% 
     dplyr::count(target) %>%
     mutate(prop = n/sum(n, na.rm = TRUE))
   
   bsRes <- foreach(it = trID, .combine = rbind) %do% {
       #it = trID[1] 
      testID = it
      testDat <- trAll %>% filter(tid == testID) 
      testDat <- data.table(testDat)
      
      trainDat <- trAll %>% filter(tid != testID) %>%
        filter(is.na(target2)) 
      trainDat <- data.table(trainDat)

      trainDat[,`:=`(target2 = NULL,tid = NULL)]
      numTr <- trainDat[,.(Num = .N), by = .(target)]
      #numTr[,New := as.integer(rescale(log(Num), to = c(50,800)))] # need to make this dynamic if using this.
    
      trainClean <- foreach(unit = numTr$target, .combine = rbind) %do% {
        dat <- trainDat[target == unit,]
        dat
      }
      
      trainClean <- unique(trainClean)
      trainClean[,target := as.factor(as.character(target))]
      
      ##create model
      mod <- ranger(target ~ ., data =  trainClean, 
                    num.trees = 501, importance = "impurity",
                    splitrule = "extratrees",
                    classification = T)
      
      # predict the model over "test" data area for mapunit1 
      temp <- predict(mod, data = testDat[,!c("target","target2", "tid")], predict.all = T)
      predMat <- as.data.table(temp$predictions)
      predMat[,pID := seq_along(1:nrow(predMat))]
      predMat[,target := testDat$target]
      predMat <- data.table::melt(predMat, id.vars = c("pID","target"))
      predMat <- predMat[,.(Num = .N), by = .(target,pID,value)]
      predMat[,Prop := Num/501] # proportion of trees with that response
      leg2 <- data.table(UnitName = mod$forest$levels, ID = seq_along(mod$forest$levels))
      predMat[leg2, PredUnit := i.UnitName, on = c(value = "ID")]
      predMat <- as.data.frame(predMat) %>%
        mutate(pVal1 = ifelse(target == PredUnit, 1,0))
      predMat <- data.table(predMat)
      predMat[fMat, fVal := i.fVal, on = c("target", PredUnit = "Pred")]
      #predMat[,TotProp := Prop * fVal]
      predMat <- predMat[complete.cases(predMat),]
      
      testDat$Pred <- predict(mod, data = testDat)$predictions
      testDat <- testDat[,.(target, target2,Pred)]
      testDat[fMat, fVal1 := i.fVal, on = c("target","Pred")]
      testDat[fMat, fVal2 := i.fVal, on = c(target2 = "target","Pred")]
      testDat[is.na(fVal2), fVal2 := 0]
      testDat[,fValMax := max(fVal1,fVal2), by = seq(1:nrow(testDat))]
      
      testDat <- as.data.frame(testDat) 
      testDat <- testDat %>%
        mutate(pVal1 = ifelse(target == Pred, 1,0)) %>%
        rowwise() %>%
        mutate(pVal2 = ifelse(target2 %in% Pred, 1,0),
               pValMax = pmax(pVal1, pVal2))
      
      testDat = data.table(testDat)
      
      res <- foreach(unit = unique(testDat$target), .combine = rbind) %do% {
        sub <- testDat[target == unit,]
        #sub2 <- predMat[target == unit,]
        out <- data.table(Unit = unit, 
                          Acc_P = sum(sub$pVal1)/nrow(sub),
                          Acc_PA = sum(sub$pValMax)/nrow(sub),
                          Acc_FP = sum(sub$fVal1)/nrow(sub),
                          Acc_FPA = sum(sub$fValMax)/nrow(sub))
                          #Acc3 = sum(sub2$TotProp)/length(unique(sub2$pID)))
        out
      }
      
      res[,It := it]
      res
    }
    
   # format the output data and summary data
   
    write.csv(bsRes, file.path(out_dir, "intAA_metrics_raw.csv"))
    
    # get summary data per map unit 
    bsRes_sum_per_mapunit <- bsRes %>%
      dplyr::group_by(Unit) %>%
      dplyr::summarise(mean_acc_prime = mean(Acc_P),
                sd_acc_prime = sd(Acc_P),
                mean_acc_prime_sec = mean(Acc_PA),
                sd_acc_prime_sec = sd(Acc_PA),
                mean_acc_prime_fuz = mean(Acc_FP), 
                sd_acc_prime_fuzzy = sd(Acc_FP),
                mean_acc_prime_sec_fuz = mean(Acc_FPA), 
                sd_acc_prime_sec_fuz = sd(Acc_FPA)) 
    
    write.csv( bsRes_sum_per_mapunit, file.path(out_dir, "intAA_metrics_summary.csv"))
    
    # get overall summary statistics
    
    overall_sum <- bsRes %>%
      left_join(MU_count, by = c("Unit" = "target")) %>%
      dplyr::group_by(It) %>%
      dplyr::summarise(mean_Acc_P = mean(Acc_P),
                       mean_Acc_P_wt = sum(Acc_P * prop),
                       mean_Acc_PA = mean(Acc_PA),
                       mean_Acc_PA_wt = sum(Acc_PA * prop),
                       mean_Acc_FP = mean(Acc_FP),
                       mean_Acc_FP_wt = sum(Acc_FP * prop),
                       mean_Acc_FPA = mean(Acc_FPA),
                       mean_Acc_FPA_wt = sum(Acc_FPA * prop)) %>%
     dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
      
    overall_sum_l = pivot_longer(overall_sum, cols = where(is.numeric), names_to = "Acc_type")
    
    overall_sum_l <- overall_sum_l %>%
      mutate(accuracy_measure = gsub("mean_Acc_", "", Acc_type)) %>%
      mutate(accuracy_measure = gsub("_wt", "", accuracy_measure)) %>%
      mutate(acc_measure = ifelse(str_detect(overall_sum_l$Acc_type , "_wt"),
                                  "mean_wt", "mean"))
    
    # make a plot of the results: 
    
    p2 <- ggplot(aes(y = value, x = acc_measure), data = overall_sum_l) + 
      facet_wrap(~accuracy_measure) + 
      ylim(0,1)+ 
      geom_bar(stat = "identity") +
      geom_hline( yintercept = 0.65,linetype="dashed", color = "red") +
      ggtitle("Overall overall map accuracy") +
      xlab("mean and weighted mean accuracy") + ylab("")
    
    p2
    
      
    # convert to lengthwise and plot results
    bsRes_long <- pivot_longer(bsRes, cols = c(Acc_P, Acc_PA, Acc_FP, Acc_FPA), names_to = "accuracy_measure")
    
    # note default box plot shows median and mean 
    
    min.mean.sd.max <- function(x) {
      r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    
    
    
    p1 <- ggplot(aes(y = value, x = factor(Unit)), data = bsRes_long)
    p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
      geom_jitter(position=position_jitter(width=.1), size=1, colour = "grey") + 
      facet_wrap(~accuracy_measure) +
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle("Internal map accuracy by mapunit") + 
      xlab("Mapunit") + ylab("Accuracy")+ 
      geom_hline(yintercept = 0.65,linetype="dashed", color = "red") 
    
    p1
    
    pout <- plot_grid(p1, p2)
    pout
    ggsave(file.path(out_dir, "intAA_plot.png"), width = 30, heigh = 20 , units = "cm")
    
}
    