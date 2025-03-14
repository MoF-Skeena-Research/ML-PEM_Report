acc_metrix <- function(data){
  
  # data <- test.pred  
  
  # do we need to group by fold?
  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
  mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE)
  sens <- data %>% sens(target, .pred_class, na_rm = TRUE)
  spec <- data %>% spec(target, .pred_class, na_rm = TRUE)
  prec <- data %>% precision(target, .pred_class, na.rm = TRUE)
  recall <- data %>% recall(target, .pred_class, na.rm = TRUE)
  fmeas <- data %>% f_meas(target, .pred_class, na.rm = TRUE)
  kap <- data %>% kap(target, .pred_class, na.rm = TRUE)
  
  # ###some aspatial metrics
  aspatial_pred <- data  %>% 
    dplyr::select(.pred_class) %>% 
    group_by(.pred_class) %>% 
    mutate(pred.tot = n()) %>% 
    ungroup() %>% distinct()
  
  aspatial_target <- data %>% 
    dplyr::select(target) %>% 
    group_by(target) %>% 
    mutate(trans.tot = n()) %>% 
    ungroup() %>% distinct()
  
  aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) %>% 
    mutate_if(is.integer, funs(replace_na(., 0))) %>% 
    mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>% # check if map sum == transect_sum?
    rowwise() %>% 
    mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
    ungroup() %>%
    mutate(aspat_p_mean = sum(aspat_p))
  
  aspatial_sum <- aspatial_sum %>% 
    rowwise()%>%
    mutate(unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
    drop_na()
  
  aspatial_results <- tribble(
    ~.metric, ~.estimator,  ~.estimate,
    "aspatial_acc", "aspatial",  min(aspatial_sum$aspat_p_mean),
    "aspatial_meanacc", "aspatial",  colMeans(aspatial_sum["unit_pos"]))
  
  # generate spatially explicit results for primary and prime/alternate
  xx <- data %>% tabyl(target, .pred_class)
  xy <- pivot_longer(xx, cols = !target) 
  
  # generate primate accuracy 
  spat_p <- xy %>%
    filter(target == name) %>%
    mutate(spat_p = value ) %>%
    dplyr::select(target, spat_p)
  
  outsum <- left_join(aspatial_sum, spat_p, by = "target")
  
  # generate the primary fuzzy calls: 
  spat_fp_df <- xy %>%
    left_join(fMat, by = c("target" = "target", "name" = "Pred")) %>%
    rowwise() %>%
    mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    mutate(spat_fpt = fVal * value) %>%
    group_by(target) %>%
    mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
    dplyr::select(target, spat_fp) %>%
    distinct()
  
  outsum <- left_join(outsum, spat_fp_df, by = "target") 
  
  # extract spatial secondary call match 
  if(length(data) == 3){ 
    
    # spatially explicit calls: 
    spat_pa <- data %>%
      filter(!is.na(target2)) %>%
      filter(target != .pred_class)
    
    if(nrow(spat_pa) == 0){
      
      spat_pa <- spat_p %>%
        mutate(spat_pa = 0 ) %>%
        dplyr::select(-spat_p)
      # outsum <- left_join(outsum, spat_pa, by = "target") 
    } else {
      
      spat_pa <- spat_pa %>%
        tabyl(target2, .pred_class) %>%
        pivot_longer(cols = !target2) %>%
        filter(target2 == name) %>%
        mutate(target = target2, 
               spat_pa = value) %>%
        dplyr::select(target, spat_pa)
    }
    
    outsum <- left_join(outsum, spat_pa, by = "target") 
    
    # generate fuzzy secondary calls : 
    spat_fpa_df <- data %>%
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
      mutate(spat_fpat = fVal * value) %>%
      group_by(targetMax) %>%
      mutate(spat_fpa = sum(spat_fpat)) %>%
      dplyr::select(target = targetMax, spat_fpa) %>%
      distinct()
    
    outsum <- left_join(outsum,  spat_fpa_df, by = "target") %>%
      rowwise() %>%
      mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE))
  } 
  
  cv_metrics <- bind_rows (acc,  mcc, sens, spec, prec, kap, fmeas, recall, aspatial_results ) # acc_bal,jind, ppv, precision, recall, kap, fmean, sens, spec, jind) %>% mutate_if(is.character, as.factor)
  
  cv_metrics_wide <- cv_metrics %>%
    dplyr::select(-.estimator) %>%
    mutate(.estimate = .estimate *100) %>%
    pivot_wider( names_from = .metric, values_from = .estimate)
  
  outsum <- cbind(outsum, cv_metrics_wide)
  outsum
  ##______________ add alt-call metrics
  ##______________ add in fuzzy-call metrics     
  
}
