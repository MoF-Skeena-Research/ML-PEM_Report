
# This function is used to calculate accuracy metrics for cv and test estimates

# This currently includes: 
# accuracy, mcc, sensitity, specificity, precision, recall, fmeas, kappa
# aspatial_acc, aspatial_meanacc
# spat_p (same as accuracy), spat_fp (fuzzy), mcc_fp

# for test datasets the following are also generated: 
# spat_pa, spat_fpa,

library(yardstick)
library(janitor)


acc_metrix <- function(data){
  
  ## testing lines
  #data <- test.pred.out 
  data <- test.pred  
  data <- data %>% drop_na(target)
  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
  mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE)
  #sens <- data %>% sens(target, .pred_class, na_rm = TRUE)
  #spec <- data %>% yardstick::spec(target, .pred_class, na_rm = TRUE)
  #prec <- data %>% precision(target, .pred_class, na.rm = TRUE)
  #recall <- data %>% recall(target, .pred_class, na.rm = TRUE)
  #fmeas <- data %>% f_meas(target, .pred_class, na.rm = TRUE)
  kap <- data %>% kap(target, .pred_class, na.rm = TRUE)
  
  
  #2) calculate aspatial metrics (overall and mapunit % correct)
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
    # dplyr::mutate_if(is.integer, funs(replace_na(., 0))) %>% 
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>% 
    dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
    ungroup() %>%
    mutate(aspat_p_overall = sum(aspat_p)) #%>%
    #dplyr::select(-aspat_p)
  
  trans.sum <- unique(aspatial_sum$trans.sum)
    
  aspatial_sum <- aspatial_sum %>% 
    rowwise()%>%
    dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0))%>%
    ungroup()%>%
    dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
           spat_p_overall =  acc$.estimate)

  # generate spatially explicit results for primary and prime/alternate
  xx <- data %>% tabyl(target, .pred_class)
  xy <- pivot_longer(xx, cols = !target) 
  
  # 2) generate  spatial primary accuracy 
  spat_p <- xy %>%
    filter(target == name) %>%
    mutate(spat_p_correct = value ) %>%
    dplyr::select(target, spat_p_correct) 
  
  outsum <- left_join(aspatial_sum, spat_p, by = "target")
  
  # generate spatial primary mean accuracy 
  outsum <- outsum %>%
    rowwise()%>%
    mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    ungroup() %>%
    mutate(spat_p_meanacc = mean(spat_p_unit_pos))

  # 3) generate the spatial primary fuzzy calls: 
  spat_fp_df <- xy %>%
    left_join(fMat, by = c("target" = "target", "name" = "Pred")) %>%
    rowwise() %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    dplyr::mutate(spat_fpt = fVal * value)  %>%
    ungroup() %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
    dplyr::select(target, spat_fp) %>%
    distinct() 
   
  outsum <- left_join(outsum, spat_fp_df, by = "target") 
  
  outsum <- outsum %>%
    dplyr::mutate(spat_pf_overall = (sum(spat_fp)/trans.sum)) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    rowwise() %>%
    dplyr::mutate(spat_pf_unit_pos = min(spat_fp, trans.tot)/trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    ungroup() %>%
      mutate(spat_pf_meanacc = mean(spat_pf_unit_pos)) 
    
  # generate aspatial primary fuzzy call: 
  
  aspat_fp_df <- outsum %>%
    dplyr::select(target, trans.tot, pred.tot, spat_fp, spat_p_correct) %>%  
    rowwise() %>%
    dplyr::mutate(aspat_p_min_correct = min(trans.tot, pred.tot), 
           aspat_fp_extra = spat_fp - spat_p_correct,
           aspat_fp_total = aspat_p_min_correct + aspat_fp_extra,
           aspat_fp_pred = min((aspat_fp_total/trans.sum), (trans.tot/trans.sum)),
           aspat_fp_unit_pos = min(trans.tot, aspat_fp_total)/trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    ungroup() %>%
    dplyr::mutate(aspat_fp_overall = sum(aspat_fp_pred),
           aspat_fp_meanacc = mean(aspat_fp_unit_pos)) %>%
    dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_p_correct, aspat_p_min_correct)) 
                  
  outsum <- left_join(outsum, aspat_fp_df, by = "target") 

    
    # for the test data comparison we can calculate the alternate calls also
    
  if(length(data)==3){ 
    
    # spatially explicit calls: 
    spat_pa <- data %>%
      filter(!is.na(target2)) %>%
      filter(target != .pred_class)
    
    # # check if there are any calls alt points 
     if(nrow(spat_pa) > 0){
      
      # 5) calculate spatial prime / alt call accuracy 
      spat_pa <- spat_pa %>%
        tabyl(target2, .pred_class) %>%
        pivot_longer(cols = !target2) %>%
        filter(target2 == name) %>%
        dplyr::mutate(target = target2, 
               spat_pa_correct = value) %>%
        dplyr::select(target, spat_pa_correct) 
      
    # # 7) generate spatial fuzzy prime / alt calls : 

    spat_fpa_raw <- data %>%
      left_join(fMat, by = c("target" = "target", ".pred_class" = "Pred")) %>%
      left_join(fMat, by = c("target2" = "target", ".pred_class" = "Pred")) %>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      rowwise() %>%
      dplyr::mutate(targetMax = ifelse(fVal.x >= fVal.y , target, target2)) %>%
      dplyr::select(targetMax, .pred_class) %>%
      tabyl(targetMax,  .pred_class) %>%
      pivot_longer(cols = !targetMax) %>%
      left_join(fMat, by = c("targetMax" = "target", "name" = "Pred")) %>%
      rowwise() %>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      dplyr::mutate(fVal = ifelse(targetMax %in% name, 1, fVal)) %>% ## for units not listed in the fuzzy matrix
      dplyr::mutate(spat_fpat = fVal * value)

    spat_fpa_df <- spat_fpa_raw  %>%
      group_by(targetMax) %>%
      dplyr::mutate(spat_fpa = sum(spat_fpat)) %>%
      dplyr::select(target = targetMax, spat_fpa) %>%
      distinct()

    # calculate spatial pa calculations
    
    outsum <- left_join(outsum, spat_pa, by = "target") %>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      rowwise() %>%
      dplyr::mutate(spat_pa_total = (spat_pa_correct + spat_p_correct)) %>%
      ungroup() 
    
    outsum <- outsum %>%
      dplyr::mutate(spat_pa_overall = (sum(spat_pa_total)/trans.sum))%>%
      rowwise() %>%
      dplyr::mutate(spat_pa_unit_pos = min(spat_pa_total,trans.tot)/trans.tot)%>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      ungroup() %>%
      dplyr::mutate(spat_pa_meanacc = mean(spat_pa_unit_pos)) 
    
    # calculate aspatial pa calcs
    
    aspat_pa_df <- outsum %>%
      dplyr::select(target, trans.tot, pred.tot, spat_fp, spat_pa_correct) %>%
      rowwise() %>%
      dplyr::mutate(aspat_pa_min_correct = min(trans.tot, pred.tot), 
             aspat_pa_extra = spat_pa_correct,
             aspat_pa_total = aspat_pa_min_correct + aspat_pa_extra,
             aspat_pa_pred = min((aspat_pa_total/trans.sum), (trans.tot/trans.sum)),
             aspat_pa_unit_pos = min(trans.tot, aspat_pa_total)/trans.tot) %>%
      ungroup()%>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      dplyr::mutate(aspat_pa_overall = sum(aspat_pa_pred),
             aspat_pa_meanacc = mean(aspat_pa_unit_pos)) %>%
      dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_pa_correct, aspat_pa_min_correct)) 
    
    outsum <- left_join(outsum, aspat_pa_df, by = "target") 
     
    #calculate spatial fpa metrics 
    outsum <-  outsum %>%
      left_join(spat_fpa_df, by = "target") %>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      dplyr::mutate(spat_fpa_overall = sum(spat_fpa)/trans.sum) %>%
      rowwise() %>%
      dplyr::mutate(spat_fpa_unit_pos = min(spat_fpa,trans.tot)/trans.tot)%>%
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
      ungroup() %>%
      dplyr::mutate(spat_fpa_meanacc = mean(spat_fpa_unit_pos)) 
    
    #calculate aspatial fpa metrics 
      
    aspat_fpa_df <- outsum %>%
        dplyr::select(target, trans.tot, pred.tot, spat_p_correct, spat_fpa) %>%
        rowwise() %>%
        dplyr::mutate(aspat_fpa_min_correct = min(trans.tot, pred.tot), 
               aspat_fpa_extra = spat_fpa - spat_p_correct,
               aspat_fpa_total = aspat_fpa_min_correct + aspat_fpa_extra,
               aspat_fpa_pred = min((aspat_fpa_total/trans.sum), (trans.tot/trans.sum)),
               aspat_fpa_unit_pos = min(trans.tot, aspat_fpa_total)/trans.tot) %>%
        ungroup()%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(aspat_fpa_overall = sum(aspat_fpa_pred),
               aspat_fpa_meanacc = mean(aspat_fpa_unit_pos)) %>%
        dplyr::select( -c(trans.tot, pred.tot, spat_p_correct, aspat_fpa_min_correct,spat_fpa)) 
      
      outsum <- left_join(outsum, aspat_fpa_df, by = "target")
      outsum$accuracy <- acc$.estimate
      outsum$kappa <- kap$.estimate
      outsum$mcc <- mcc$.estimate
      
     } else {
       
       print ("no secondary calls in test dataset")
       
       fix_cols <- outsum %>%
         mutate(spat_pa_correct = 0,
                spat_pa_total = 0, 
                "spat_pa_overall" = 0 ,  
                "spat_pa_unit_pos" = 0,
                "spat_pa_meanacc" = 0 ,    
                "aspat_pa_extra"= 0 ,      
                "aspat_pa_total"= 0 ,      
                "aspat_pa_pred" = 0 ,     
                "aspat_pa_unit_pos" = 0 ,  
                "aspat_pa_overall" = 0 ,  
                "aspat_pa_meanacc" = 0 ,   
                "spat_fpa" = 0 ,        
                "spat_fpa_overall" = 0 ,   
                "spat_fpa_unit_pos" = 0 ,  
                "spat_fpa_meanacc" = 0 ,         
                 "aspat_fpa_extra"= 0 ,     
                "aspat_fpa_total"= 0 ,    
                "aspat_fpa_pred" = 0 ,     
                "aspat_fpa_unit_pos"= 0 , 
                "aspat_fpa_overall"= 0 ,   
                "aspat_fpa_meanacc" = 0 )
       
       outsum <- fix_cols
       
     }

    #rowwise() %>%
    #dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE)) 
  } 
    #write.csv(outsum, "test26.csv")
 outsum 

}


#test <- acc_metrix(test.pred)
#cv <- acc_metrix(cv_pred_sum)
#setdiff(names(test),names(cv))


# function to calculate the weighted metrics

weight_by_transect_no <- function(acc) {
  
    acc_sum <- acc %>%
 #     acc_sum <- acc %>%
        dplyr::filter(acc_type == "test_estimate") %>%
           mutate(across(ends_with("overall"), ~.x *100)) %>%
           mutate(across(ends_with("meanacc"), ~.x *100)) %>%
           dplyr::select(slice, acc_type, transect_no,
                         aspat_p_overall,  aspat_p_meanacc, 
                         aspat_fp_overall,  aspat_fp_meanacc,
                         spat_p_overall, spat_p_meanacc,
                         spat_pf_overall,  spat_pf_meanacc, 
                         aspat_pa_overall,  aspat_pa_meanacc,
                         aspat_fpa_overall, aspat_fpa_meanacc,
                         spat_pa_overall,  spat_pa_meanacc,
                         spat_fpa_overall, spat_fpa_meanacc ) %>%
         distinct() 
  
       # calculate the weighted mean and st dev summary 
       acc_wt_ave <- acc_sum %>%
         summarise(mutate(across(where(is.numeric), ~ weighted.mean(.x, transect_no, na.rm = FALSE)))) %>%
         pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "ave_wt") %>%
         dplyr::filter(!accuracy_type %in% c("slice", "transect_no"))
       
       acc_wt_sd <- acc_sum %>%         
         summarise(mutate(across(where(is.numeric), ~ sqrt(wtd.var(.x, transect_no, na.rm = FALSE))))) %>%
         pivot_longer(cols = where(is.numeric), names_to = "accuracy_type", values_to = "sd_wt") %>%
         dplyr::filter(!accuracy_type %in% c("slice", "transect_no"))
       
       acc_wt_sum <- left_join(acc_wt_ave, acc_wt_sd ) %>%
         filter(!accuracy_type == "transect_no") 
       
       return(acc_wt_sum)

}


# function to calculate the theta metrics 

theta_accuracy <- function(acc, theta = 0.5){
  
 # acc <- temp
#  theta = 0.1
  
   acc <- acc %>%
     dplyr::select(target,	trans.tot, pred.tot, trans.sum, pred.tot, #slice,
               spat_p_unit_pos, spat_pa_unit_pos,  spat_pf_unit_pos, spat_fpa_unit_pos,
               aspat_p_unit_pos, aspat_pa_unit_pos, aspat_fp_unit_pos, aspat_fpa_unit_pos)
  
  acc_theta <- acc %>%
    #group_by(slice) %>%
    mutate(no.classes = length(target)) %>%
    ungroup()
  
  # calculate the theta_spat_p
  
  acc_theta <- acc_theta %>%
    rowwise() %>%
    mutate(theta_wt = theta * (1/no.classes) + (1-theta)* (pred.tot/trans.sum)) %>%
    mutate(spat_p_unit_pos_raw = theta_wt *  spat_p_unit_pos,
           spat_pa_unit_pos_raw = theta_wt *  spat_pa_unit_pos,
           spat_pf_unit_pos_raw  = theta_wt * spat_pf_unit_pos,
           spat_fpa_unit_pos_raw = theta_wt * spat_fpa_unit_pos,
           aspat_p_unit_pos_raw = theta_wt * aspat_p_unit_pos, 
           aspat_pa_unit_pos_raw = theta_wt * aspat_pa_unit_pos,
           aspat_fp_unit_pos_raw = theta_wt * aspat_fp_unit_pos,
           aspat_fpa_unit_pos_raw = theta_wt *   aspat_fpa_unit_pos) 
    
    acc_theta_raw <- acc_theta %>%
      #group_by(slice) %>%
      mutate(spat_p_theta = sum(spat_p_unit_pos_raw),
             spat_pa_theta = sum(spat_pa_unit_pos_raw),
             spat_pf_theta = sum(spat_pf_unit_pos_raw),
             spat_fpa_theta = sum(spat_fpa_unit_pos_raw),
             aspat_p_theta = sum(aspat_p_unit_pos_raw),
             aspat_pa_theta = sum(aspat_pa_unit_pos_raw),
             aspat_fp_theta = sum(aspat_fp_unit_pos_raw),
             aspat_fpa_theta = sum(aspat_fpa_unit_pos_raw))
             
    
    noi <-  names(acc_theta_raw)[str_detect(names(acc_theta_raw),"_theta")]
    
    acc_theta_raw <- acc_theta_raw %>%
       dplyr::select(c(target,  all_of(noi)))#slice,
            
 return(acc_theta_raw)
}



# Generate accuracy metrics for Non-forest model. This outputs basic metrics for 
# primary call only : 

acc_metrix_nf <- function(data){
    
    #data <- cv_pred_sum 
    
    # Run accuracy metrics
    acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
    kap <- data %>% kap(target, .pred_class, na.rm = TRUE)
    
    #2) calculate aspatial metrics (overall and mapunit % correct)
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
      dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>% 
      dplyr::mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
      ungroup() %>%
      mutate(aspat_p_overall = sum(aspat_p)) #%>%
    #dplyr::select(-aspat_p)
    
    trans.sum <- unique(aspatial_sum$trans.sum)
    
    aspatial_sum <- aspatial_sum %>% 
      rowwise()%>%
      dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
      drop_na() %>%
      dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
                    spat_p_overall =  acc$.estimate,
                    kap = kap$.estimate)
    
    # generate spatially explicit results for primary and prime/alternate
    xx <- data %>% tabyl(target, .pred_class)
    xy <- pivot_longer(xx, cols = !target) 
    
    # 2) generate primary accuracy 
    spat_p <- xy %>%
      filter(target == name) %>%
      mutate(spat_p_correct = value ) %>%
      dplyr::select(target, spat_p_correct) 
    
    outsum <- left_join(aspatial_sum, spat_p, by = "target")
    
    outsum <- left_join( aspatial_sum, spat_p, by = "target") %>%
      mutate(model = "nf")
    
    outsum <- outsum %>%
      rowwise()%>%
      mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
      drop_na() %>%
      ungroup() %>%
      mutate(spat_p_meanacc = mean(spat_p_unit_pos))

    return(outsum)
}



###################################################################################

library(yardstick)
library(janitor)


acc_metrix_loo <- function(data, uss){
  
  uss_all = uss
  ## testing lines
  #data <- cv_pred_sum 
  data <- test.pred  
  
  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
  mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE)
  #sens <- data %>% sens(target, .pred_class, na_rm = TRUE)
  #spec <- data %>% yardstick::spec(target, .pred_class, na_rm = TRUE)
  #prec <- data %>% precision(target, .pred_class, na.rm = TRUE)
  #recall <- data %>% recall(target, .pred_class, na.rm = TRUE)
  #fmeas <- data %>% f_meas(target, .pred_class, na.rm = TRUE)
  kap <- data %>% kap(target, .pred_class, na.rm = TRUE)
  
  
  #2) calculate aspatial metrics (overall and mapunit % correct)
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
  
  aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) 
  
  # add missing levels 
    if(length(uss_all) == length(aspatial_sum$target)){
      print("all levels included")
    } else {
      
      missing_mu <- setdiff(uss,  aspatial_sum$target)
      missing_mu_df <- tibble(
        target = as.factor(missing_mu),
        trans.tot = 0,
        pred.tot = 0
      )
      aspatial_sum <- bind_rows(aspatial_sum, missing_mu_df) 
    }
  
  
  aspatial_sum <- aspatial_sum %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>% 
    # dplyr::mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>% 
    dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
    ungroup() %>%
    dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>% 
    mutate(aspat_p_overall = sum(aspat_p)) #%>%
  #dplyr::select(-aspat_p)
  
  trans.sum <- unique(aspatial_sum$trans.sum)
  
  aspatial_sum <- aspatial_sum %>% 
    rowwise()%>%
    dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0))%>%
    ungroup()%>%
    dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
                  spat_p_overall =  acc$.estimate)
  
  # generate spatially explicit results for primary and prime/alternate
  xx <- data %>% tabyl(target, .pred_class)
  xy <- pivot_longer(xx, cols = !target) 
  
  # 2) generate  spatial primary accuracy 
  spat_p <- xy %>%
    filter(target == name) %>%
    mutate(spat_p_correct = value ) %>%
    dplyr::select(target, spat_p_correct) 
  
  outsum <- left_join(aspatial_sum, spat_p, by = "target")
  
  # generate spatial primary mean accuracy 
  outsum <- outsum %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    rowwise()%>%
    mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
    dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>% 
    ungroup() %>%
    mutate(spat_p_meanacc = mean(spat_p_unit_pos))
  
  # 3) generate the primary fuzzy calls: 
  spat_fp_df <- xy %>%
    left_join(fMat, by = c("target" = "target", "name" = "Pred")) %>%
    rowwise() %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    dplyr::mutate(spat_fpt = fVal * value)  %>%
    ungroup() %>%
    dplyr::group_by(target) %>%
    dplyr::mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
    dplyr::select(target, spat_fp) %>%
    distinct() 
  
  outsum <- left_join(outsum, spat_fp_df, by = "target") %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) 
  
  outsum <- outsum %>%
    dplyr::mutate(spat_pf_overall = (sum(spat_fp)/trans.sum)) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    rowwise() %>%
    dplyr::mutate(spat_pf_unit_pos = min(spat_fp, trans.tot)/trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    ungroup() %>%
    mutate(spat_pf_meanacc = mean(spat_pf_unit_pos)) 
  
  # generate aspatial primary fuzzy call: 
  
  aspat_fp_df <- outsum %>%
    dplyr::select(target, trans.tot, pred.tot, spat_fp, spat_p_correct) %>%  
    rowwise() %>%
    dplyr::mutate(aspat_p_min_correct = min(trans.tot, pred.tot), 
                  aspat_fp_extra = spat_fp - spat_p_correct,
                  aspat_fp_total = aspat_p_min_correct + aspat_fp_extra,
                  aspat_fp_pred = min((aspat_fp_total/trans.sum), (trans.tot/trans.sum)),
                  aspat_fp_unit_pos = min(trans.tot, aspat_fp_total)/trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
    ungroup() %>%
    dplyr::mutate(aspat_fp_overall = sum(aspat_fp_pred),
                  aspat_fp_meanacc = mean(aspat_fp_unit_pos)) %>%
    dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_p_correct, aspat_p_min_correct)) 
  
  outsum <- left_join(outsum, aspat_fp_df, by = "target") 
  
  
  # for the test data compariosn we can calculate the alternate calls also
  
  if(length(data)==3){ 
    
    # spatially explicit calls: 
    spat_pa <- data %>%
      filter(!is.na(target2)) %>%
      filter(target != .pred_class)
    
    # # check if there are any calls alt points 
    if(nrow(spat_pa) > 0){
      
      # 5) calculate spatial prime / alt call accuracy 
      spat_pa <- spat_pa %>%
        tabyl(target2, .pred_class) %>%
        pivot_longer(cols = !target2) %>%
        filter(target2 == name) %>%
        dplyr::mutate(target = target2, 
                      spat_pa_correct = value) %>%
        dplyr::select(target, spat_pa_correct) 
      
      # # 7) generate spatial fuzzy prime / alt calls : 
      
      spat_fpa_raw <- data %>%
        left_join(fMat, by = c("target" = "target", ".pred_class" = "Pred")) %>%
        left_join(fMat, by = c("target2" = "target", ".pred_class" = "Pred")) %>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        rowwise() %>%
        dplyr::mutate(targetMax = ifelse(fVal.x >= fVal.y , target, target2)) %>%
        dplyr::select(targetMax, .pred_class) %>%
        tabyl(targetMax,  .pred_class) %>%
        pivot_longer(cols = !targetMax) %>%
        left_join(fMat, by = c("targetMax" = "target", "name" = "Pred")) %>%
        rowwise() %>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(spat_fpat = fVal * value)
      
      spat_fpa_df <- spat_fpa_raw  %>%
        group_by(targetMax) %>%
        dplyr::mutate(spat_fpa = sum(spat_fpat)) %>%
        dplyr::select(target = targetMax, spat_fpa) %>%
        distinct()
      
      # calculate spatial pa calculations
      
      outsum <- left_join(outsum, spat_pa, by = "target") %>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        rowwise() %>%
        dplyr::mutate(spat_pa_total = (spat_pa_correct + spat_p_correct)) %>%
        ungroup() 
      
      outsum <- outsum %>%
        dplyr::mutate(spat_pa_overall = (sum(spat_pa_total)/trans.sum))%>%
        rowwise() %>%
        dplyr::mutate(spat_pa_unit_pos = min(spat_pa_total,trans.tot)/trans.tot)%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        ungroup() %>%
        dplyr::mutate(spat_pa_meanacc = mean(spat_pa_unit_pos)) 
      
      # calculate aspatial pa calcs
      
      aspat_pa_df <- outsum %>%
        dplyr::select(target, trans.tot, pred.tot, spat_fp, spat_pa_correct) %>%
        rowwise() %>%
        dplyr::mutate(aspat_pa_min_correct = min(trans.tot, pred.tot), 
                      aspat_pa_extra = spat_pa_correct,
                      aspat_pa_total = aspat_pa_min_correct + aspat_pa_extra,
                      aspat_pa_pred = min((aspat_pa_total/trans.sum), (trans.tot/trans.sum)),
                      aspat_pa_unit_pos = min(trans.tot, aspat_pa_total)/trans.tot) %>%
        ungroup()%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(aspat_pa_overall = sum(aspat_pa_pred),
                      aspat_pa_meanacc = mean(aspat_pa_unit_pos)) %>%
        dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_pa_correct, aspat_pa_min_correct)) 
      
      outsum <- left_join(outsum, aspat_pa_df, by = "target") 
      
      #calculate spatial fpa metrics 
      outsum <-  outsum %>%
        left_join(spat_fpa_df, by = "target") %>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(spat_fpa_overall = sum(spat_fpa)/trans.sum) %>%
        rowwise() %>%
        dplyr::mutate(spat_fpa_unit_pos = min(spat_fpa,trans.tot)/trans.tot)%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        ungroup() %>%
        dplyr::mutate(spat_fpa_meanacc = mean(spat_fpa_unit_pos)) 
      
      #calculate aspatial fpa metrics 
      
      aspat_fpa_df <- outsum %>%
        dplyr::select(target, trans.tot, pred.tot, spat_p_correct, spat_fpa) %>%
        rowwise() %>%
        dplyr::mutate(aspat_fpa_min_correct = min(trans.tot, pred.tot), 
                      aspat_fpa_extra = spat_fpa - spat_p_correct,
                      aspat_fpa_total = aspat_fpa_min_correct + aspat_fpa_extra,
                      aspat_fpa_pred = min((aspat_fpa_total/trans.sum), (trans.tot/trans.sum)),
                      aspat_fpa_unit_pos = min(trans.tot, aspat_fpa_total)/trans.tot) %>%
        ungroup()%>%
        dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
        dplyr::mutate(aspat_fpa_overall = sum(aspat_fpa_pred),
                      aspat_fpa_meanacc = mean(aspat_fpa_unit_pos)) %>%
        dplyr::select( -c(trans.tot, pred.tot, spat_p_correct, aspat_fpa_min_correct,spat_fpa)) 
      
      outsum <- left_join(outsum, aspat_fpa_df, by = "target") 
      
    } else {
      
      print ("no secondary calls in test dataset")
      
      fix_cols <- outsum %>%
        mutate(spat_pa_correct = 0,
               spat_pa_total = 0, 
               "spat_pa_overall" = 0 ,  
               "spat_pa_unit_pos" = 0,
               "spat_pa_meanacc" = 0 ,    
               "aspat_pa_extra"= 0 ,      
               "aspat_pa_total"= 0 ,      
               "aspat_pa_pred" = 0 ,     
               "aspat_pa_unit_pos" = 0 ,  
               "aspat_pa_overall" = 0 ,  
               "aspat_pa_meanacc" = 0 ,   
               "spat_fpa" = 0 ,        
               "spat_fpa_overall" = 0 ,   
               "spat_fpa_unit_pos" = 0 ,  
               "spat_fpa_meanacc" = 0 ,         
               "aspat_fpa_extra"= 0 ,     
               "aspat_fpa_total"= 0 ,    
               "aspat_fpa_pred" = 0 ,     
               "aspat_fpa_unit_pos"= 0 , 
               "aspat_fpa_overall"= 0 ,   
               "aspat_fpa_meanacc" = 0 )
      
      outsum <- fix_cols
      
    }
    
    #rowwise() %>%
    #dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE)) 
  } 
  #write.csv(outsum, "test26.csv")
  outsum
}

