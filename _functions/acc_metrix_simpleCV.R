
# This function is used to calculate accuracy metrics for cv and test estimates for simple 


# This currently includes: 
# accuracy, mcc, sensitity, specificity, precision, recall, fmeas, kappa
# aspatial_acc, aspatial_meanacc
# spat_p (same as accuracy), spat_fp (fuzzy), mcc_fp

# for test datasets the following are also generated: 
# spat_pa, spat_fpa,

library(yardstick)


acc_metrix_simpleCV <- function(data){
  
  ## testing lines
  #data <- cv_pred_sum 
  #data <- test.pred  

  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
  mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE)
  sens <- data %>% sens(target, .pred_class, na_rm = TRUE)
  spec <- data %>% yardstick::spec(target, .pred_class, na_rm = TRUE)
  fmeas <- data %>% f_meas(target, .pred_class, na.rm = TRUE)
  kap <- data %>% kap(target, .pred_class, na.rm = TRUE)
  
  #### 1) calculate some aspatial metrics
  aspatial_pred <- data  %>% 
    dplyr::select(.pred_class) %>% 
    group_by(.pred_class) %>% 
    dplyr::mutate(pred.tot = n()) %>% 
    ungroup() %>% distinct()
  
  aspatial_target <- data %>% 
    dplyr::select(target) %>% 
    group_by(target) %>% 
    dplyr::mutate(trans.tot = n()) %>% 
    ungroup() %>% distinct()
  
  aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) %>% 
    mutate_if(is.integer, funs(replace_na(., 0))) %>% 
    mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>% 
    dplyr::rowwise() %>% 
    mutate(trans.pro = (trans.tot/trans.sum), 
           pred.pro = (pred.tot/trans.sum)) %>%
    ungroup() 
  # work around to fix min() rowwise error
    dat <- as.data.table(aspatial_sum)
    dat[, aspat_p := pmin(trans.pro, pred.pro)]
    dat[, unit_pos_temp := pmin(trans.tot, pred.tot)]
    
    #mutate(unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
    
  aspatial_sum = as.data.frame(dat) %>%
    dplyr::select(c(-trans.pro, -pred.pro)) %>%
    mutate(aspat_p_mean = sum(aspat_p)) %>%
    rowwise() %>%
    mutate(unit_pos = unit_pos_temp/trans.tot)%>%
    drop_na() %>%
    dplyr::select(-unit_pos_temp)
  
  aspatial_results <- tribble(
    ~.metric, ~.estimator,  ~.estimate,
    "aspatial_acc", "aspatial",  min(aspatial_sum$aspat_p_mean),
    "aspatial_meanacc", "aspatial",  colMeans(aspatial_sum["unit_pos"]))
  
  # generate spatially explicit results for primary and prime/alternate
  xx <- data %>% tabyl(target, .pred_class)
  xy <- pivot_longer(xx, cols = !target) 
  
  # 2) generate primary accuracy 
  spat_p <- xy %>%
    filter(target == name) %>%
    mutate(spat_p = value ) %>%
    dplyr::select(target, spat_p)
  
  outsum <- left_join(aspatial_sum, spat_p, by = "target")
  
  # 3) generate the primary fuzzy calls: 
  
  spat_fp_df <- xy %>%
    left_join(fMat, by = c("target" = "target", "name" = "Pred")) %>%
    rowwise() %>%
    mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    mutate(spat_fpt = fVal * value)  %>%
    group_by(target) %>%
    mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
    dplyr::select(target, spat_fp) %>%
    distinct()
  
  outsum <- left_join(outsum, spat_fp_df, by = "target") 
  
  # 4) Generate fuzzy primary mcc call
  # https://en.wikipedia.org/wiki/Matthews_correlation_coefficient
     
  spat_fp_mcc <- xy %>%
    left_join(fMat, by = c("target" = "target", "name" = "Pred")) %>%
    rowwise() %>%
    mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    mutate(fVal = ifelse(fVal != 1, fVal/4, fVal)) %>%
    mutate(spat_fpt = fVal * value)
  
  mccf_cal <-foreach(mu = levels(xy$target), .combine=rbind) %do% {
    #mu <- levels(xy$target)[1] # testing line 
    # calc tn
    tn <- xy %>% filter(target != mu & name != mu) %>% pull(value) %>% sum()
    # calculate (tp, fn, fp)
    tpfpfn <- spat_fp_mcc %>%
      filter(target == mu | name == mu) %>%
      mutate(tp = ifelse(target == mu & name != mu, spat_fpt, 0)) %>%
      mutate(tp = ifelse(target == mu & name == mu, spat_fpt, tp)) %>%
      mutate(tp = ifelse(target != mu & name == mu, spat_fpt, tp)) %>% 
     # mutate(tp = ifelse(target != mu & name == mu & fVal>0 & fVal <1,
                    #     fVal *, tp))))
      mutate(fn = ifelse(target == mu & fVal>0 & fVal <1, value * (1-fVal),0)) %>%
      mutate(fp = ifelse(target != mu & fVal>0 & fVal <1, value * (1-fVal),0)) %>%
      mutate(fn = ifelse(target == mu & fVal == 0, value, fn)) %>%
      mutate(fp = ifelse(name == mu & fVal == 0, value, fp))
    
   # write.csv(tpfpfn, "test_acc.csv")
    
    tp <- tpfpfn %>% pull(tp) %>% sum()
    fp <- tpfpfn %>% pull(fp) %>% sum()
    fn <- tpfpfn %>% pull(fn) %>% sum()
    
    #output for each level 
    out <- tibble(mu, tp, tn, fp, fn)
  }
  
  # calculate mcc
  mccf_cal <- mccf_cal %>%
    rowwise() %>%
    mutate(pred = tp + fp,
           actual = tp + fn,
           num = pred * actual,
           pred_sq = pred^2,
           actual_sq = actual^2)
  
  # calculate the total correct and total no.  
  mccf_tp <- sum(mccf_cal$tp)
  mccf_tot <- sum(mccf_cal$actual)
  
  numerator <- (mccf_tp*mccf_tot) - sum(mccf_cal$num)
  denom <- sqrt((mccf_tot^2 - sum(mccf_cal$pred_sq)) *
                  (mccf_tot^2 - sum(mccf_cal$actual_sq)))
  
  mcc_fp = numerator/denom
  
  mcc_df <- tibble(target = levels(xy$target), 
                   mcc_fp = mcc_fp)
  
  outsum <- left_join(outsum, mcc_df, by = "target") 
  
  # extract spatial secondary call match - note this only applies to test data sets not cv data
  
  if(length(data)==3){ 
    
    # spatially explicit calls: 
    spat_pa <- data %>%
      filter(!is.na(target2)) %>%
      filter(target != .pred_class)
    
    # # check if there are any calls alt points 
     if(nrow(spat_pa) == 0){
       
       spat_pa <- spat_p %>% mutate(spat_pa = 0 ) %>% dplyr::select(-spat_p) 
       mcc_pa_df  <- spat_p %>% mutate(mcc_pa = 0) %>% dplyr::select(-spat_p) 
       mcc_fpa_df  <- spat_p %>% mutate(mcc_fpa = 0) %>% dplyr::select(-spat_p)
       spat_fpa_df <- spat_p %>% mutate(spat_fpa = 0) %>% dplyr::select(-spat_p)
       
       
     } else {
       
      # 5) calculate spatial prime / alt call accuracy 
      spat_pa <- spat_pa %>%
        tabyl(target2, .pred_class) %>%
        pivot_longer(cols = !target2) %>%
        filter(target2 == name) %>%
        mutate(target = target2, 
               spat_pa = value) %>%
        dplyr::select(target, spat_pa)
    
      # 6) calculate the mcc based on prime/alt call
      levs <- c(levels(data$target), levels(data$.pred_class), levels(data$target2)) %>% unique()
      padata <- data %>%
        mutate(target = factor(target, levels = levs),
               target2 = factor(target2, levels = levs),
               .pred_class = factor(.pred_class, levels = levs)) 
        
      spat_pa_mcc <- padata %>%
        mutate(best_target = case_when(
          target2 == .pred_class ~ target2,
          TRUE ~ as.factor(target)
        )) %>%
         dplyr::select(-c(target, target2))%>%
         rename(target = best_target) %>%
         dplyr::select(target, .pred_class) %>%
         droplevels()
          
       xxpa <- spat_pa_mcc %>% tabyl(target, .pred_class)
       xypa <- pivot_longer(xxpa, cols = !target) 
       
      mcc_pa_cal <-foreach(mu = levels(xypa$target), .combine=rbind) %do% {
          #mu <- levels(xypa$target)[1]
          tp <- xypa %>% filter(target == mu & name == mu) %>% pull(value)
          tn <- xypa %>% filter(target != mu & name != mu) %>% pull(value) %>% sum()
          fp <- xypa %>% filter(target != mu & name == mu) %>% pull(value) %>% sum()
          fn <- xypa %>% filter(target == mu & name != mu) %>% pull(value) %>% sum()
          out <- tibble(mu, tp, tn, fp, fn)
          }
        
       mcc_pa_tp <- sum(mcc_pa_cal$tp)
       mcc_pa_tot <- rowSums(mcc_pa_cal[1, c(2:5)]) # grab first row and check 
         
       mcc_pa_cal <- mcc_pa_cal %>%
           rowwise() %>%
           mutate(pred = tp + fp,
                  actual = tp + fn,
                  num = pred * actual,
                  pred_sq = pred^2,
                  actual_sq = actual^2)
       
       numerator_pa <- (mcc_pa_tp*mcc_pa_tot) - sum(mcc_pa_cal$num)
       denom_pa <- sqrt((mcc_pa_tot^2 - sum(mcc_pa_cal$pred_sq)) *
                       (mcc_pa_tot^2 - sum(mcc_pa_cal$actual_sq)))
       
       mcc_pa = numerator_pa/denom_pa
       
       mcc_pa_df <- tibble(target = levels(xypa$target), 
                        mcc_pa = mcc_pa)
   
      
    # 7) generate fuzzy prime / alt calls : 
    
    spat_fpa_raw <- data %>%
      left_join(fMat, by = c("target" = "target", ".pred_class" = "Pred")) %>%
      left_join(fMat, by = c("target2" = "target", ".pred_class" = "Pred")) %>%
      mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      rowwise() %>%
      mutate(targetMax = ifelse(fVal.x >= fVal.y , target, target2)) %>%
      dplyr::select(targetMax, .pred_class) %>%
      tabyl(targetMax,  .pred_class) %>%
      pivot_longer(cols = !targetMax) %>%
      left_join(fMat, by = c("targetMax" = "target", "name" = "Pred")) %>%
      rowwise() %>%
      mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      mutate(spat_fpat = fVal * value) 
      
    spat_fpa_df <- spat_fpa_raw  %>%
      group_by(targetMax) %>%
      mutate(spat_fpa = sum(spat_fpat)) %>%
      dplyr::select(target = targetMax, spat_fpa) %>%
      distinct()
    
  
    # 8) Generate mcc fuzzy alt/primary calls values 
    
    spat_fpa_mcc <-  spat_fpa_raw %>%
      rename(target = targetMax)
    
    mccfpa_cal <-foreach(mu = unique(spat_fpa_mcc$target), .combine=rbind) %do% {
      #mu <- unique(spat_fpa_mcc$target)[1] # testing line 
      # calc tn
      tn <- xy %>% filter(target != mu & name != mu) %>% pull(value) %>% sum()
      # calculate (tp, fn, fp)
      tpfpfn <- spat_fpa_mcc %>%
        filter(target == mu | name == mu) %>%
        mutate(tp = ifelse(target == mu & name != mu, spat_fpat, 0)) %>%
        mutate(tp = ifelse(target == mu & name == mu, spat_fpat, tp)) %>%
        mutate(tp = ifelse(target != mu & name == mu, spat_fpat, tp)) %>% 
        # mutate(tp = ifelse(target != mu & name == mu & fVal>0 & fVal <1,
        #     fVal *, tp))))
        mutate(fn = ifelse(target == mu & fVal>0 & fVal <1, value * (1-fVal),0)) %>%
        mutate(fp = ifelse(target != mu & fVal>0 & fVal <1, value * (1-fVal),0)) %>%
        mutate(fn = ifelse(target == mu & fVal == 0, value, fn)) %>%
        mutate(fp = ifelse(name == mu & fVal == 0, value, fp))
      
      # write.csv(tpfpfn, "test_acc.csv")
      
      tp <- tpfpfn %>% pull(tp) %>% sum()
      fp <- tpfpfn %>% pull(fp) %>% sum()
      fn <- tpfpfn %>% pull(fn) %>% sum()
      
      #output for each level 
      out <- tibble(mu, tp, tn, fp, fn)
    }
    
    # calculate mcc
    mccfpa_cal <- mccfpa_cal %>%
      rowwise() %>%
      mutate(pred = tp + fp,
             actual = tp + fn,
             num = pred * actual,
             pred_sq = pred^2,
             actual_sq = actual^2)
    
    # calculate the total correct and total no.  
    mccf_tp <- sum(mccfpa_cal$tp)
    mccf_tot <- sum(mccfpa_cal$actual)
    
    numerator <- (mccf_tp*mccf_tot) - sum(mccfpa_cal$num)
    denom <- sqrt((mccf_tot^2 - sum(mccfpa_cal$pred_sq)) *
                    (mccf_tot^2 - sum(mccfpa_cal$actual_sq)))
    
    mcc_fpa = numerator/denom
    
    mcc_fpa_df <- tibble(target = levels(xy$target), 
                     mcc_fpa = mcc_fpa)
    
      }
    
    outsum <- left_join(outsum, spat_pa, by = "target") 
    outsum <- left_join(outsum, mcc_pa_df, by = "target")  
    outsum <- left_join(outsum, mcc_fpa_df, by = "target") 
    outsum <- left_join(outsum, spat_fpa_df, by = "target") %>%
    rowwise() %>%
    dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE)) 
  } 
  
  cv_metrics <- bind_rows (acc,  mcc, sens, spec, kap, fmeas,  aspatial_results ) # acc_bal,jind, ppv, precision, recall, kap, fmean, sens, spec, jind) %>% mutate_if(is.character, as.factor)
  
  cv_metrics_wide <- cv_metrics %>%
    dplyr::select(-.estimator) %>%
    mutate(.estimate = .estimate *100) %>%
    pivot_wider( names_from = .metric, values_from = .estimate)
  
  outsum <- cbind(outsum, cv_metrics_wide)
  outsum
  
}
