
# Gens version of WHM testing 


# function to determine balancing parameters for recipe preparation. 
# Currently set for smote and or downsample combination. 
# Used to determine optimum balancing requirement for dataset. 
#extrarun = FALSE; use.neighbours = TRUE

balance_optimisation_raw <- function(trDat ,
                                     extrarun = FALSE,
                                     extradat = NULL,
                                     downsample = FALSE,
                                     smote = FALSE,
                                     use.neighbours = TRUE){
  
  ## testing lines:
  #trDat
  #extrarun = FALSE
  #extradat = NULL
  #downsample = FALSE
  #smote = FALSE
  #use.neighbours = TRUE
  ##
  
  balance_name = "raw"
  
  # Cross validation loop based on slices
  slices <- unique(trDat$slice) %>% droplevels()
  
  # for all slices
  sresults <- foreach(k = levels(slices)) %do% {
    
    #k = levels(slices)[1]
    ### split into train and test based on 5-site slices
    print(k)
    
    bgc_label = xx
    
    #  trDat <- trDat %>%
    #    dplyr::select(-bgc_cat)
    
    # training set - train only on pure calls
    BGC_train <- trDat %>%
      dplyr::filter(!slice %in% k, position %in% "Orig") %>%
      filter(is.na(target2) | target2 == "") %>%
      dplyr::select( -slice, -transect_id, -position, -id) %>%
      droplevels()
    
    #if(extra = FALSE) {remove(extra)}
    # merge in extra point data
    if(isTRUE(extrarun)) {
      print("adding extra points at each model build")
      BGC_train <- rbind(BGC_train, extradat)
    }
    
    # check if enough data for each class to smote
    MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
      pull(target)
    
    BGC_train <- BGC_train %>%
      dplyr::select(-target2) %>%  drop_na() %>% droplevels() %>% data.frame #%>%
    # mutate(tid = factor(tid)) %>%  droplevels()
    
    if (isTRUE(use.neighbours)) { # check this is going to work as filtered above for "Org"
      BGC_test <- trDat %>% filter(slice %in% k) %>%
        filter(!target %in% MU_count)
    } else {
      BGC_test <- trDat %>% filter(slice %in% k, position %in% "Orig") %>%
        filter(!target %in% MU_count)}
    
    BGC_test_all <- BGC_test # keep for the target2 alt call.
    BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
    BGC_test <- BGC_test %>%
      dplyr::select(-slice,-target2,-transect_id, -position, -id) %>%
      droplevels()
    
    # set recipe
    null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
      update_role(tid, new_role = "id variable")
    
    #randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 151) %>%
    randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>%
      set_mode("classification") %>%
      set_engine("ranger", importance = "permutation", verbose = FALSE)
    
    pem_workflow <- workflow() %>%
      add_recipe(null_recipe) %>%
      add_model(randf_spec)
    
    #######################################################
    PEM_rf1 <- fit(pem_workflow, BGC_train)
    final_fit <- extract_fit_parsnip(PEM_rf1)
    oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
    
    ######### Predict Test
    test_target <- BGC_test_all %>% dplyr::select(id, target, target2)
    test.pred <-  predict(PEM_rf1, BGC_test)
    test.pred <- cbind(test_target, test.pred) %>% distinct()
    test.pred <- test.pred %>%  mutate_if(is.factor, as.character)# %>%
    
    # switch out the predicted Nf units for "nonfor" catergory.
    test.pred <- test.pred %>%
      mutate(target = ifelse(target %in% nf_mapunits, "nonfor", target),
             target2 = ifelse(target2 %in% nf_mapunits, "nonfor", target2) ,
             .pred_class = ifelse(.pred_class  %in% nf_mapunits, "nonfor", .pred_class ))
    
    test.pred <- test.pred %>%  mutate_all(as.factor) %>% distinct()
    
    ###harmonize levels
    targ.lev <- levels(test.pred$target)
    pred.lev <- levels(test.pred$.pred_class)
    levs <- c(targ.lev, pred.lev) %>% unique()
    test.pred$target <- factor(test.pred$target, levels = levs)
    test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
    
    # output test predictions
    #test.pred.out <- test.pred %>% mutate(slice = k)
    
    acc.compare <- acc_metrix(test.pred, theta = 0.5) %>%
      mutate(slice = k,
             transect_no = BGC_test_transect_no,
             acc_type = "test_estimate",
             oob = oob,
             balance = balance_name,
             bgc = bgc_label)
    
    ##use function to add in theta
    # acc.theta <- theta_accuracy(acc.compare, theta = 0.5)
    # acc.compare <- cbind(acc.compare,acc.theta) %>% dplyr::select(unique(colnames(.)))
    acc.compare <- acc.compare  %>%
      dplyr::select( slice, target, balance, trans.sum, trans.tot, pred.tot, acc, kap, everything() )
    return(list(acc.compare))
    
  }
  
  # extract results from sresults
  acc_results <- lapply(sresults, function(x) x[[1]])
  acc <- as.data.frame(rbindlist(acc_results))
  
  write.csv(acc, file = file.path(bal_out_dir , paste0("acc_", balance_name,".csv")))
  
}


# balancing function for both downsample and smote. 
# needs to be converted back to options where only one or other is option (see 
# old code commented out below)


balance_optimisation <- function(trDat , 
                                 extrarun = FALSE, 
                                 extradat = NULL, 
                                 downsample = TRUE,
                                 ds_iterations = ds_iterations,
                                 smote = TRUE,
                                 smote_iterations = smote_iterations,
                                 use.neighbours = TRUE) {
  
  # testing lines 
  # trDat
  # extrarun = FALSE
  # extradat = NULL 
  # downsample = TRUE
  # ds_iterations = ds_iterations
  # smote = TRUE
  # smote_iterations = smote_iterations
  # use.neighbours = TRUE
  # 
  
  
  for(d in ds_iterations){
   # d = ds_iterations[1]
    print(d)
    
    for(i in smote_iterations){
      
      #i = smote_iterations[1]
      print(i)
      # 
      # test if enough data in the slice to use smoting 
      
      # set up the parameters for balancing
      downsample_ratio = d  # (0 - 100, Null = 1)
      smote_ratio = i    # 0 - 1, 1 = complete smote
      
      balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)
      
      # training set - train only on pure calls
      
      
      # Cross validation loop based on slices 
      slices <- unique(trDat$slice) %>% droplevels()
      
      # if(length(slices)<2){
      #   
      #   # switching to transect iteration instead of slices
      #   
      #   trDat_key <- trDat %>%
      #     dplyr::select(c(tid)) %>%
      #     distinct() %>%
      #     mutate(slice = as.factor(seq(1,length(tid),1)))
      #   
      #   trDat <- trDat %>%
      #     dplyr::select(-slice) %>%
      #     left_join(trDat_key)
      #   
      #   slices <- unique(trDat$slice) %>% droplevels()
      #   
      # }
      
      
      # for all slices
      sresults <- foreach(k = levels(slices)) %do% {
        
        #k = levels(slices)[5]
        label = paste(d, i, k, sep = "-")
        print(label)
        #print(k)  
        ### split into train and test based on 5-site slices
        
     
        # training set
        
        BGC_train <- trDat %>% 
          dplyr::filter(!slice %in% k, position %in% "Orig") %>%
         # filter(is.na(target2) | target2 == "") %>% 
          dplyr::select(-slice, -transect_id, -position, -id) 
        
        # merge in extra point data
        if(isTRUE(extrarun)) {
          print("adding extra points at each model build")
          BGC_train <- rbind(BGC_train, extradat)
        }
        # check if enough data for each class to smote
        
        MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
          pull(target) %>% droplevels()
        
        if(length(MU_count) > 0) {
          print("remove classes with very low training points from smoting test")
          BGC_train <- BGC_train %>% 
            filter(!target %in% MU_count) %>% droplevels()
        }
        
        
        BGC_train <- BGC_train %>%
          dplyr::select(-target2) %>%  
          drop_na() %>% 
          mutate(target = factor(target), 
                 tid = as.character(tid)) %>%  
          droplevels()
        
       
        # test set
        if (isTRUE(use.neighbours)) {
          BGC_test <- trDat %>% filter(slice %in% k) %>%
            filter(!target %in% MU_count)
        } else{ BGC_test <- trDat %>% filter(slice %in% k, position %in% "Orig") %>%
          filter(!target %in% MU_count)}
        
        BGC_test_all <- BGC_test # keep for the target2 alt call. 
        BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
        BGC_test <- BGC_test %>%
          dplyr::select(-slice,-target2,-transect_id)
        # 
        ############### Define test recipes and workflow ###################
        
        # null_recipe <- balance_optimum(indata = BGC_train, 
        #                                downsample = downsample,
        #                                downsample_ratio = downsample_ratio,
        #                                smote= smote, 
        #                                smote_ratio = smote_ratio,
        #                                update_role(tid, new_role = "id variable"))
        
        
        null_recipe <-  recipe(target ~ ., data = BGC_train) %>%
          update_role(tid, new_role = "id variable") %>%
          step_downsample(target, under_ratio = downsample_ratio) %>%
          step_smote(target, over_ratio = smote_ratio , neighbors = 2, skip = TRUE)
        
        randf_spec <- rand_forest(mtry = mtry, min_n = min_n, trees = 151) %>% 
          set_mode("classification") %>%
          set_engine("ranger", importance = "permutation", verbose = FALSE) 
        
        pem_workflow <- workflow() %>%
          add_recipe(null_recipe) %>%
          add_model(randf_spec)
        
        #######################################################
        PEM_rf1 <- fit(pem_workflow, BGC_train)
        
        #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
        final_fit <- extract_fit_parsnip(PEM_rf1)
        
        oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
        
        ######### Predict Test
        #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
        test_target <- BGC_test_all %>% dplyr::select(id, target, target2)
        
        test.pred <-  predict(PEM_rf1, BGC_test)
        test.pred <- cbind(test_target, test.pred) 
      
        # switch out the predicted Nf units for "nonfor" catergory.
        test.pred <- test.pred %>%
          mutate(target = ifelse(as.character(target) %in% nf_mapunits, "nonfor", as.character(target)),
                 target2 = ifelse(as.character(target2) %in% nf_mapunits, "nonfor", as.character(target2)) ,
                 .pred_class = ifelse(as.character(.pred_class)  %in% nf_mapunits, "nonfor", as.character(.pred_class)))
        
        test.pred <- test.pred %>%  mutate_if(is.character, as.factor)
        
        ###_______________________________________________________
        
        ###harmonize factor levels
        targ.lev <- levels(test.pred$target)
        targ2.lev <- levels(test.pred$target2)
        pred.lev <- levels(test.pred$.pred_class)
        levs <- c(targ.lev,targ2.lev, pred.lev) %>% unique()
        test.pred$target <- factor(test.pred$target, levels = levs)
        test.pred$target2 <- factor(test.pred$target2, levels = levs)
        test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
        
        # output test predictions
        
        test.pred.out <- test.pred %>% mutate(slice = k)
        acc.compare <- acc_metrix(test.pred) %>%
          mutate(slice = k,
                 transect_no = BGC_test_transect_no,
                 acc_type = "test_estimate", 
                 oob = oob, 
                 balance = balance_name,
                 bgc = bec.choose)
        ##use function to add in theta  
        # acc.theta <- theta_accuracy(acc.compare, theta = 0.5)
        # acc.compare <- cbind(acc.compare,acc.theta) %>% dplyr::select(unique(colnames(.)))
        acc.compare <- acc.compare  %>% 
          dplyr::select(bgc, slice, target,balance, trans.sum, trans.tot, pred.tot, acc, kap, everything() )
        return(list(acc.compare))
      }
      
      # extract results from sresults
      acc_results <- lapply(sresults, function(x) x[[1]])
      acc <- as.data.frame(rbindlist(acc_results))
      
      write.csv(acc, file = paste(bal_out_dir, paste0("acc_", balance_name,".csv"),sep = "/"))
      
      
    } # end of smote iteration
    
  } # end of downsample iteration 
  
}




#extradat = extra; extrarun = TRUE 
#trDat = trDat_all


##### OLD VERSION 

# 
# balance_optimum <- function(
#   indata = BGC_train,
#   downsample = TRUE,
#   downsample_ratio = 100, # (0 - 100, Null = 100)
#   smote = TRUE,
#   smote_ratio = 1    # 0 - 1, 1 = complete smote
# ){
#   
#   if(downsample == TRUE & smote == FALSE){
#     print("downsampling")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = downsample_ratio)
#     
#     
#   } else if(downsample == FALSE & smote == TRUE){
#     
#     print("smoting")
#     
#    # null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = smote_ratio, neighbors = 2)
#     
#   } else if (downsample == TRUE & smote == TRUE) {
#     print ("downsample and smoting")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = downsample_ratio) %>%
#       step_smote(target, over_ratio = smote_ratio , neighbors = 2)
#     
#   }
#   
#   else if (downsample == FALSE & smote == FALSE) {
#     print ("raw")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata)# %>%
#       #update_role(tid, new_role = "id variable") 
#   }
#   
# }
# 
# # testing the output 
# 
# #null_recipe <- balance_optimum(downsample = FALSE, smote= FALSE)
# #null_recipe <- balance_optimum(downsample = TRUE, smote= TRUE)
# #null_recipe <- balance_optimum(downsample = TRUE, smote= FALSE)    
# 
# #############################################################
# 
# # set up the recipe for the modelling 
# 
# set_recipe <- function(indata, ds_ratio = NA, sm_ratio = NA) {
#   
#   #indata = inmdata_all
#   #ds_ratio = NA
#   #sm_ratio = 40
#   
#   
#   if(is.na(ds_ratio) & is.na(sm_ratio)){
#     print("basic")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) #%>%
#       #update_role(tid, new_role = "id variable") 
#     
#   } else if(is.na(ds_ratio) & !is.na(sm_ratio)){
#     
#     print("smoting")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#       #update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2)
#     
#   } else if (!is.na(ds_ratio) & is.na(sm_ratio)) {
#     print ("downsample")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#      # update_role(tid, new_role = "id variable") %>%
#       step_downsample(target, under_ratio = ds_ratio)
#     
#   } else if (!is.na(ds_ratio) & !is.na(sm_ratio)) {
#     print ("downsample and smoting")
#     
#     #null_recipe <-
#     recipe(target ~ ., data = indata) %>%
#      # update_role(tid, new_role = "id variable") %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#     
#   }
#  
# } 
# 
# # set up the recipe for the final modelling 
#   
# set_final_recipe <- function(indata, ds_ratio = NA, sm_ratio = NA) {
#   
#   
#   if(is.na(ds_ratio) & is.na(sm_ratio)){
#     print("basic")
# 
#     recipe(target ~ ., data = indata) 
#     
#   } else if(is.na(ds_ratio) & !is.na(sm_ratio)){
#     
#     print("smoting")
# 
#     recipe(target ~ ., data = indata) %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2)
#     
#   } else if (!is.na(ds_ratio) & is.na(sm_ratio)) {
#     print ("downsample")
#     
#     recipe(target ~ ., data = indata) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#     
#   } else if (!is.na(ds_ratio) & !is.na(sm_ratio)) {
#     print ("downsample and smoting")
#     
#     recipe(target ~ ., data = indata) %>%
#       step_smote(target, over_ratio = sm_ratio, neighbors = 2) %>%
#       step_downsample(target, under_ratio = ds_ratio)
#     
#   }
#   
# } 
# 
# 
# 
# 
# ######################################################################
# # plot each of the outputs (downsample, smote and ds/ smote combinations)
# 
# create_dev_plot <- function(indata) {
#   
#   out_data <- indata %>% 
#     group_by(balance) %>%
#     summarise(mu_devation = sum(pred.obs.total),
#               mu_var = var(pred.obs.total),
#               mu_mean = mean(pred.obs.total),
#               mu_sd = sd(pred.obs.total)) 
#   
#   # simple plot of accuracy metrics 
#   dev_plot <- ggplot(indata, aes(x=target, y=pred.obs.pc)) + 
#     geom_bar(stat='identity',  aes(fill = pred.obs.type), width=.5) + 
#     coord_flip(ylim =c(-100, 110)) +  
#     facet_wrap(~balance) 
#   
#   # add text 
#   dat_text <- out_data
#   dat_text$label <- sprintf(
#     "%s, %s",
#     round(dat_text$mu_devation,0),
#     str_extract(dat_text$balance,"^.{0}")
#   )
#   
#   dev_plot_annotate <- dev_plot +
#     geom_text(data = dat_text,
#               size = 3,
#               mapping = aes(x = 8, y = 90, label = label),
#               colour = "blue") 
#   
#   return(dev_plot_annotate)      
# } 
# 
# 
# 
# ########################################################################
# 
# # Function to run optimising automation 
# 
# balance_optimisation <- function(trDat , downsample = TRUE, smote = FALSE, 
#                                  ds_iterations = c(15, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                                  smote_iterations = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)){
#   
#  trDat = trDat2
#  #downsample = FALSE
# #  smote = FALSE
# ## smote_iterations = 0.3
# # ds_iterations = 0.1
#   
#   if(downsample == FALSE & smote == FALSE){
#     
#     # run the raw model 
#     balance_name = "raw"
#     
#     # Cross validation loop based on slices 
#     slices <- unique(trDat$slice) %>% droplevels()
#     
#     
#     if(length(slices)<2){
# 
#     # switching to transect iteration instead of slices
# 
#       trDat_key <- trDat %>%
#         dplyr::select(c(tid)) %>%
#         distinct() %>%
#         mutate(slice = as.factor(seq(1,length(tid),1)))
# 
#       trDat <- trDat %>%
#         dplyr::select(-slice) %>%
#         left_join(trDat_key)
# 
#       slices <- unique(trDat$slice) %>% droplevels()
# 
#     }
# 
#     
#     # for all slices
#     sresults <- foreach(k = levels(slices)) %do% {
#       
#        #k = levels(slices)[1]
#       ### split into train and test based on 5-site slices
#       print(k)
#       
#       # training set
#       BGC_train <- trDat %>% dplyr::filter(!slice %in% k) %>%
#         filter(is.na(target2)) # train only on pure calls
#       BGC_train <- BGC_train %>%
#         dplyr::select(-slice, -target2, -transect_id) %>%
#         droplevels()
#       
#       # test set
#       BGC_test <- trDat %>% filter(slice %in% k) 
#       BGC_test_all <- BGC_test # keep for the target2 alt call. 
#       BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
#       BGC_test <- BGC_test %>%
#         dplyr::select(-slice,-target2,-transect_id)
#       
#       ############### Define test recipes and workflow ###################
#     
#       null_recipe <- 
#           recipe(target ~ ., data = BGC_train) %>%
#           update_role(tid, new_role = "id variable") 
#       
#       # 
#       # if(length(levels(slices))<5) {
#       #   vv = length(levels(slices))
#       #   # vv = 3
#       # } else {vv = 10}
#       # 
#       # # testing line#  vv = 3
#       # 
#       # set.seed(345)
#       # pem_cvfold <- group_vfold_cv(
#       #   BGC_train,
#       #   v = vv,
#       #   ### need to build a check for number of tids available to automatically reduce this number where necessary # problem with essfmcw
#       #   repeats = 5,
#       #   group = tid,
#       #   strata = target
#       # )
#       
#       
#       randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 200) %>% 
#         set_mode("classification") %>%
#         set_engine("ranger", importance = "permutation", verbose = FALSE) 
#     
#       pem_workflow <- workflow() %>%
#         add_recipe(null_recipe) %>%
#         add_model(randf_spec)
#       
#       #######################################################
#       PEM_rf1 <- fit(pem_workflow, BGC_train)
#       
#       #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
#       final_fit <- extract_fit_parsnip(PEM_rf1) # %>%pull(.predictions)
#         
#       oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
#       
#       ######### Predict Test
#       #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
#       test_target <- BGC_test_all %>% dplyr::select(target, target2)
#       
#       test.pred <-  predict(PEM_rf1, BGC_test)
#       test.pred <- cbind(test_target, test.pred) %>% 
#         mutate_if(is.character, as.factor)
#       # levels(train.pred$target)
#       
#       ###harmonize levels
#       targ.lev <- levels(test.pred$target)
#       pred.lev <- levels(test.pred$.pred_class)
#       levs <- c(targ.lev, pred.lev) %>% unique()
#       test.pred$target <- factor(test.pred$target, levels = levs)
#       test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
#       # output test predictions
#       
#       test.pred.out <- test.pred %>% mutate(slice = k)
#       acc.compare <- acc_metrix(test.pred) %>%
#         mutate(slice = k,
#                transect_no = BGC_test_transect_no,
#                acc_type = "test_estimate", 
#                oob = oob, 
#                balance = balance_name)
#       
#       return(list(acc.compare))
#     }
#     
#     # extract results from sresults
#     acc_results <- lapply(sresults, function(x) x[[1]])
#     acc <- as.data.frame(rbindlist(acc_results))
#     
#     dir.create(out_dir, showWarnings = FALSE)
#     
#     fwrite(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))
#     
#   }  
#   
#   # for the downsample and smote option: 
#   
#   if(downsample == TRUE & smote == TRUE){
#     
#     print("downsample and smote")
#     
#     for(d in ds_iterations){
#      d = ds_iterations[1]
#     print(d)
#     
#       for(i in smote_iterations){
#       i = smote_iterations[1]
#       print(i)
#       
#       # test if enough data in the slice to use smoting 
#   
#       # set up the parameters for balancing
#       downsample_ratio = d  # (0 - 100, Null = 1)
#       smote_ratio = i    # 0 - 1, 1 = complete smote
#       
#       if(downsample == TRUE & smote == FALSE) {
#         balance_name <- paste0("ds_", downsample_ratio )
#         
#       } else if(downsample == FALSE & smote == TRUE){
#         balance_name <- paste0("smote_", smote_ratio )
#         
#       } else if (downsample == TRUE & smote == TRUE) {
#         balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)
#         
#       } else if (downsample == FALSE & smote == FALSE) {
#         balance_name = "raw"
#       }
#       
#       # Cross validation loop based on slices 
#       slices <- unique(trDat$slice) %>% droplevels()
#       
#       if(length(slices)<2){
#         
#         # switching to transect iteration instead of slices
#         
#         trDat_key <- trDat %>%
#           dplyr::select(c(tid)) %>%
#           distinct() %>%
#           mutate(slice = as.factor(seq(1,length(tid),1)))
#         
#         trDat <- trDat %>%
#           dplyr::select(-slice) %>%
#           left_join(trDat_key)
#         
#         slices <- unique(trDat$slice) %>% droplevels()
#         
#       }
#       
#       
#       # for all slices
#       sresults <- foreach(k = levels(slices)) %do% {
#         
#          k = levels(slices)[1]
#         ### split into train and test based on 5-site slices
#         print(k)
#         
#         # training set
#         BGC_train <- trDat %>% dplyr::filter(!slice %in% k) %>%
#           filter(is.na(target2)) # train only on pure calls
#         
#         # check if enough data for each class to smote
#         
#         MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
#           pull(target)
#         
#         if(length(MU_count) != 0) {
#             print("remove classes with very low training points from smoting test")
#             BGC_train <- BGC_train %>% 
#               filter(!target %in% MU_count)
#             }
#         
#         BGC_train <- BGC_train %>%
#             dplyr::select(-slice, -target2, -transect_id)# %>% mutate(tid = factor(tid)) %>%  droplevels()
#           
#   
#         
#         # test set
#         BGC_test <- trDat %>% filter(slice %in% k) %>%
#           filter(!target %in% MU_count)
#         BGC_test_all <- BGC_test # keep for the target2 alt call. 
#         BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
#         BGC_test <- BGC_test %>%
#           dplyr::select(-slice,-target2,-transect_id)
#         
#         ############### Define test recipes and workflow ###################
#         
#         # null_recipe <- balance_optimum(indata = BGC_train, 
#         #                                downsample = downsample,
#         #                                downsample_ratio = downsample_ratio,
#         #                                smote= smote, 
#         #                                smote_ratio = smote_ratio,
#         #                                update_role(tid, new_role = "id variable"))
#         
#         null_recipe <- recipe(target ~., BGC_train, 
#                                        downsample = downsample,
#                                        downsample_ratio = downsample_ratio,
#                                        smote= smote, 
#                                        smote_ratio = smote_ratio)#, update_role(tid, new_role = "id variable"))
#         # null_recipe <-
#         #    recipe(target ~ ., data = BGC_train) %>%
#         #    update_role(tid, new_role = "id variable")  
#         
#         print(downsample)
#         print(downsample_ratio)
#         print(smote)
#         print(smote_ratio)
#         # 
#         # if(length(levels(slices))<5) {
#         #   vv = length(levels(slices))
#         #   # vv = 3
#         # } else {vv = 10}
#         # 
#         # # testing line#  vv = 3
#         # 
#         # set.seed(345)
#         # pem_cvfold <- group_vfold_cv(
#         #   BGC_train,
#         #   v = vv,
#         #   ### need to build a check for number of tids available to automatically reduce this number where necessary # problem with essfmcw
#         #   repeats = 5,
#         #   group = tid,
#         #   strata = target
#         # )
#         
#         randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 200) %>% 
#           set_mode("classification") %>%
#           set_engine("ranger", importance = "permutation", verbose = FALSE) 
#         
#         pem_workflow <- workflow() %>%
#           add_recipe(null_recipe) %>%
#           add_model(randf_spec)
#         
#         #######################################################
#         PEM_rf1 <- fit(pem_workflow, BGC_train)
#         
#         #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
#         final_fit <- extract_fit_parsnip(PEM_rf1)
#         
#         oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
#         
#         ######### Predict Test
#         #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
#         test_target <- BGC_test_all %>% dplyr::select(target, target2)
#         
#         test.pred <-  predict(PEM_rf1, BGC_test)
#         test.pred <- cbind(test_target, test.pred) %>% 
#           mutate_if(is.character, as.factor)
#         # levels(train.pred$target)
#         
#         ###harmonize levels
#         targ.lev <- levels(test.pred$target)
#         pred.lev <- levels(test.pred$.pred_class)
#         levs <- c(targ.lev, pred.lev) %>% unique()
#         test.pred$target <- factor(test.pred$target, levels = levs)
#         test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
#         # output test predictions
#         
#         test.pred.out <- test.pred %>% mutate(slice = k)
#         acc.compare <- acc_metrix(test.pred) %>%
#           mutate(slice = k,
#                  transect_no = BGC_test_transect_no,
#                  acc_type = "test_estimate", 
#                  oob = oob, 
#                  balance = balance_name)
#         
#         return(list(acc.compare))
#       }
#       
#       # extract results from sresults
#       acc_results <- lapply(sresults, function(x) x[[1]])
#       acc <- as.data.frame(rbindlist(acc_results))
#       
#       write.csv(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))
#       
#       
#     } # end of smote iteration
#     
#   } # end of downsample iteration 
# 
# } # end of the downsample and smote iterations 
# 
#   
# # for the downsample only optimisation 
#   
#   if(downsample == TRUE & smote == FALSE){
# 
#    print("downsample")
#    for(d in ds_iterations){
#     
#     #d = ds_iterations[1]
#     print(d)
#     
#       # set up the parameters for balancing
#       downsample_ratio = d  # (0 - 100, Null = 1)
#       smote_ratio = 1    # 0 - 1, 1 = complete smote
#       
#       if(downsample == TRUE & smote == FALSE) {
#         balance_name <- paste0("ds_", downsample_ratio )
#         
#       } else if(downsample == FALSE & smote == TRUE){
#         balance_name <- paste0("smote_", smote_ratio )
#         
#       } else if (downsample == TRUE & smote == TRUE) {
#         balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)
#         
#       } else if (downsample == FALSE & smote == FALSE) {
#         balance_name = "raw"
#       }
#       
#       
#       # Cross validation loop based on slices 
#       slices <- unique(trDat$slice) %>% droplevels()
#       
#       if(length(slices)<2){
#         
#         # switching to transect iteration instead of slices
#         
#         trDat_key <- trDat %>%
#           dplyr::select(c(tid)) %>%
#           distinct() %>%
#           mutate(slice = as.factor(seq(1,length(tid),1)))
#         
#         trDat <- trDat %>%
#           dplyr::select(-slice) %>%
#           left_join(trDat_key)
#         
#         slices <- unique(trDat$slice) %>% droplevels()
#         
#       }
#       
#       
#       # for all slices
#       sresults <- foreach(k = levels(slices)) %do% {
# 
#          #k = levels(slices)[1]
# 
#         ### split into train and test based on 5-site slices
#         print(k)
#         
#         # training set
#         BGC_train <- trDat %>% dplyr::filter(!slice %in% k) %>%
#           filter(is.na(target2)) # train only on pure calls
#         BGC_train <- BGC_train %>%
#           dplyr::select(-slice, -target2, -transect_id) %>%
#           droplevels()
#         
#         # test set
#         BGC_test <- trDat %>% filter(slice %in% k) 
#         BGC_test_all <- BGC_test # keep for the target2 alt call. 
#         BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
#         BGC_test <- BGC_test %>%
#           dplyr::select(-slice,-target2,-transect_id)
#         
#         ############### Define test recipes and workflow ###################
#         
#         null_recipe <- balance_optimum(indata = BGC_train, downsample = downsample,
#                                        downsample_ratio = downsample_ratio,
#                                        smote= smote, 
#                                        smote_ratio = smote_ratio)
#         # null_recipe <-
#         #    recipe(target ~ ., data = BGC_train) %>%
#         #    update_role(tid, new_role = "id variable")  
#         
#         print(downsample)
#         print(downsample_ratio)
#         #print(smote)
#         #print(smote_ratio)
#         
#         # 
#         # if(length(levels(slices))<5) {
#         #   vv = length(levels(slices))
#         #   # vv = 3
#         # } else {vv = 10}
#         # 
#         # # testing line#  vv = 3
#         # 
#         # set.seed(345)
#         # pem_cvfold <- group_vfold_cv(
#         #   BGC_train,
#         #   v = vv,
#         #   ### need to build a check for number of tids available to automatically reduce this number where necessary # problem with essfmcw
#         #   repeats = 5,
#         #   group = tid,
#         #   strata = target
#         # )
#         
#         randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 200) %>% 
#           set_mode("classification") %>%
#           set_engine("ranger", importance = "permutation", verbose = FALSE) 
#         
#         pem_workflow <- workflow() %>%
#           add_recipe(null_recipe) %>%
#           add_model(randf_spec)
#         
#         #######################################################
#         PEM_rf1 <- fit(pem_workflow, BGC_train)
#         
#         #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
#         final_fit <- extract_fit_parsnip(PEM_rf1)
#         
#         oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
#         
#         ######### Predict Test
#         #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
#         test_target <- BGC_test_all %>% dplyr::select(target, target2)
#         
#         test.pred <-  predict(PEM_rf1, BGC_test)
#         test.pred <- cbind(test_target, test.pred) %>% 
#           mutate_if(is.character, as.factor)
#         # levels(train.pred$target)
#         
#         ###harmonize levels
#         targ.lev <- levels(test.pred$target)
#         pred.lev <- levels(test.pred$.pred_class)
#         levs <- c(targ.lev, pred.lev) %>% unique()
#         test.pred$target <- factor(test.pred$target, levels = levs)
#         test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
#         # output test predictions
#         
#         test.pred.out <- test.pred %>% mutate(slice = k)
#         acc.compare <- acc_metrix(test.pred) %>%
#           mutate(slice = k,
#                  transect_no = BGC_test_transect_no,
#                  acc_type = "test_estimate", 
#                  oob = oob, 
#                  balance = balance_name)
#         
#         return(list(acc.compare))
#       }
#       
#       # extract results from sresults
#       acc_results <- lapply(sresults, function(x) x[[1]])
#       acc <- as.data.frame(rbindlist(acc_results))
#       
#       write.csv(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))
#       
#       
#     } # end of smote iteration
#     
#   } # end of downsample iteration 
#   
# ## Loop for smote only iterations
# 
#   if(downsample == FALSE & smote == TRUE){
#   
#   print("smote")
#   
#     for(i in smote_iterations){
#       #i = smote_iterations[1]
#       print(i)
#       
#       # set up the parameters for balancing
#       downsample_ratio = 1  # (0 - 100, Null = 1)
#       smote_ratio = i    # 0 - 1, 1 = complete smote
#       
#       if(downsample == TRUE & smote == FALSE) {
#         balance_name <- paste0("ds_", downsample_ratio )
#         
#       } else if(downsample == FALSE & smote == TRUE){
#         balance_name <- paste0("smote_", smote_ratio )
#         
#       } else if (downsample == TRUE & smote == TRUE) {
#         balance_name = paste0("ds_",downsample_ratio,"_sm_", smote_ratio)
#         
#       } else if (downsample == FALSE & smote == FALSE) {
#         balance_name = "raw"
#       }
#       
#       # Cross validation loop based on slices 
#       slices <- unique(trDat$slice) %>% droplevels()
# 
#         #k = levels(slices)[1]
# 
#         if(length(slices)<2){
#         
#         # switching to transect iteration instead of slices
#         
#         trDat_key <- trDat %>%
#           dplyr::select(c(tid)) %>%
#           distinct() %>%
#           mutate(slice = as.factor(seq(1,length(tid),1)))
#         
#         trDat <- trDat %>%
#           dplyr::select(-slice) %>%
#           left_join(trDat_key)
#         
#         slices <- unique(trDat$slice) %>% droplevels()
#         
#       }
#       
#          # for all slices
#       sresults <- foreach(k = levels(slices)) %do% {
# 
#         #k = levels(slices)[2]
# 
#         ### split into train and test based on 5-site slices
#         print(k)
#         
#         #trDat$tid <- as.factor(trDat$tid)
#         
#         
#         # training set
#         BGC_train <- trDat %>% dplyr::filter(!slice %in% k) %>%
#           filter(is.na(target2)) # train only on pure calls
# 
#             # check if enough data for each class to smote
#             
#             MU_count <- BGC_train %>% dplyr::count(target) %>% filter(n < 10) %>%
#               pull(target)
#             
#             if(length(MU_count) != 0) {
#               print("remove classes with very low training points from smoting test")
#               BGC_train <- BGC_train %>% 
#                 filter(!target %in% MU_count)
#             }
#             
#             BGC_train <- BGC_train %>%
#               dplyr::select(-slice, -target2, -transect_id) %>%
#               droplevels()
#         
#         # test set
#         BGC_test <- trDat %>% filter(slice %in% k) %>%
#           filter(!target %in% MU_count)  # MIGHT NEED TO CHECK THIS DOESNT CAUSE PROBLEMS 
#         BGC_test_all <- BGC_test # keep for the target2 alt call. 
#         BGC_test_transect_no <- length(unique(BGC_test_all$transect_id))
#         BGC_test <- BGC_test %>%
#           dplyr::select(-slice,-target2,-transect_id)
#         
#       
#        
#         ############### Define test recipes and workflow ###################
#         
#         null_recipe <- balance_optimum(BGC_train, 
#                                        downsample = downsample,
#                                        downsample_ratio = downsample_ratio,
#                                        smote= smote, 
#                                        smote_ratio = smote_ratio)
#        # print(smote)
#       #  print(smote_ratio)
#         # 
#         # if(length(levels(slices))<5) {
#         #   vv = length(levels(slices))
#         #   # vv = 3
#         # } else {vv = length(levels(slices))-1}
#         # 
#         # set.seed(345)
#         # pem_cvfold <- group_vfold_cv(
#         #   BGC_train,
#         #   v = vv,
#         #    repeats = 5,
#         #   group = tid,
#         #   strata = target
#         # )
#         # 
#         randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 200) %>% 
#           set_mode("classification") %>%
#           set_engine("ranger", importance = "permutation", verbose = FALSE) 
#       
#         pem_workflow <- workflow() %>%
#           add_recipe(null_recipe) %>%
#           add_model(randf_spec)
#         
#         PEM_rf1 <- fit(pem_workflow, BGC_train)
# 
#         #final_fit <- pull_workflow_fit(PEM_rf1) # %>%pull(.predictions)
#         
#         final_fit <- extract_fit_parsnip(PEM_rf1)
#         
#         oob  <- round(PEM_rf1$fit$fit$fit$prediction.error, 3)
#         
#         ######### Predict Test
#         #test_target <- as.data.frame(BGC_test$target) %>% rename(target = 1)
#         test_target <- BGC_test_all %>% dplyr::select(target, target2)
#         
#         test.pred <-  predict(PEM_rf1, BGC_test)
#         test.pred <- cbind(test_target, test.pred) %>% 
#           mutate_if(is.character, as.factor)
#         # levels(train.pred$target)
#         
#         ###harmonize levels
#         targ.lev <- levels(test.pred$target)
#         pred.lev <- levels(test.pred$.pred_class)
#         levs <- c(targ.lev, pred.lev) %>% unique()
#         test.pred$target <- factor(test.pred$target, levels = levs)
#         test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
#         # output test predictions
#         
#         test.pred.out <- test.pred %>% mutate(slice = k)
#         acc.compare <- acc_metrix(test.pred) %>%
#           mutate(slice = k,
#                  transect_no = BGC_test_transect_no,
#                  acc_type = "test_estimate", 
#                  oob = oob, 
#                  balance = balance_name)
#         
#         return(list(acc.compare))
# 
#         #}
#       }
# 
#       
#       # extract results from sresults
#       acc_results <- lapply(sresults, function(x) x[[1]])
#       acc <- as.data.frame(rbindlist(acc_results))
#       
#       write.csv(acc, file = paste(outDir, paste0("acc_", balance_name,".csv"),sep = "/"))
#       
#       }
#     } # end of smote iteration
#     
#   } # end of optimising function 
#   
# 
