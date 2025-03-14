
# This function is used to calculate accuracy metrics for cv and test estimates

# This currently includes: 
# accuracy, mcc, sensitity, specificity, precision, recall, fmeas, kappa
# aspatial_acc, aspatial_meanacc
# spat_p (same as accuracy), spat_fp (fuzzy), mcc_fp

# for test datasets the following are also generated: 
# spat_pa, spat_fpa,

library(yardstick)
library(janitor)
library(dplyr)

acc_metrix <- function(data, theta = 0.5) {
  
  #data <- test.pred.out 
  data <- test.pred
   theta = 0.5
  ## pick highest value neighbour
    ### selects max value between primary and secondary calls
   
   data1 <- left_join(data,fMat, by = c("target" = "target", ".pred_class" = "Pred")) %>% 
     mutate_if(is.character, as.factor) %>% replace(is.na(.), 0) %>% mutate(p_fVal = fVal) %>% dplyr::select(-fVal)
   data2 <- left_join(data1,fMat, by = c("target2" = "target", ".pred_class" = "Pred")) %>% 
     mutate_if(is.character, as.factor) %>% replace(is.na(.), 0) %>% mutate(alt_fVal = fVal)
   ### selects neighbour with max value
   data <- data2 %>% rowwise() %>%  mutate(pa_fVal = max(p_fVal, alt_fVal)) %>% 
     group_by(id)  %>%  
     top_n(1, abs(pa_fVal)) %>% distinct(id, .keep_all = TRUE) %>% 
     data.frame %>% dplyr::select(-fVal, -alt_fVal)
   
   data <- data %>% mutate_if(is.factor, as.character) %>%
     mutate(p_Val = ifelse(target == .pred_class, 1, 0 ),
            alt_Val = ifelse(target2 == .pred_class, 1, 0 )) %>% rowwise() %>% 
     mutate(pa_Val = max(p_Val, alt_Val)) %>%  dplyr::select(-alt_Val) %>% 
     mutate_if(is.character, as.factor) %>% data.frame %>% 
     add_count(target, name = 'trans.tot') %>% add_count(.pred_class, name = "pred.tot")
   
    # data <- left_join(data,fMat, by = c("target" = "target", ".pred_class" = "Pred")) %>% 
    #   mutate_if(is.character, as.factor) %>% replace(is.na(.), 0) %>% mutate(p_fVal = fVal) %>% dplyr::select(-fVal)
    # data <- data %>% mutate_if(is.factor, as.character) %>%
    #                   mutate(p_Val = ifelse(target == .pred_class, 1, 0 ))%>% rowwise() %>% 
    #                   #mutate(pa_Val = max(p_Val, alt_Val)) %>%  dplyr::select(-alt_Val) %>% 
    #                   mutate_if(is.character, as.factor) %>% data.frame %>% 
    #  add_count(target, name = 'trans.tot') %>% add_count(.pred_class, name = "pred.tot")
targ.lev <- as.data.frame(levels(data$target)) %>% rename(levels = 1) %>% droplevels()
pred.lev <- as.data.frame(levels(data$.pred_class)) %>% rename(levels = 1)%>% droplevels()
add.pred.lev <- anti_join(pred.lev, targ.lev,  by = "levels")
 data <- data %>% mutate(target.new = ifelse(.pred_class %in% add.pred.lev, as.character(.pred_class), as.character(target)),
                         trans.tot.new = ifelse(.pred_class %in% add.pred.lev, 0, trans.tot)) %>% 
   mutate_if(is.character, as.factor) %>% mutate(trans.tot = trans.tot.new)
#   mutate(pred.new = ifelse(target.new %in% add.pred.lev, as.character(target), as.character(.pred_class))) %>% 
#     mutate(target = target.new, .pred_class = pred.new) 
    ###harmonize factor levels
    targ.lev <- levels(data$target) 
    pred.lev <- levels(data$.pred_class)
    levs <- c(targ.lev, pred.lev) %>% unique()
    data$target <- factor(data$target, levels = levs)
        data$.pred_class <- factor(data$.pred_class, levels = levs)
    
  data <- data %>% drop_na(target) %>% mutate(no.classes = length(levs)) %>% 
    dplyr::select(-pred.tot, -trans.tot.new)
  
  ### 1)machine learning stats
  acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  %>%  dplyr::select(.estimate) %>% as.numeric %>% round(3)
  mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE) %>%  dplyr::select(.estimate) %>% as.numeric %>% round(3)
  #sens <- data %>% sens(target, .pred_class, na_rm = TRUE)
  #spec <- data %>% yardstick::spec(target, .pred_class, na_rm = TRUE)
  #prec <- data %>% precision(target, .pred_class, na.rm = TRUE)
  #recall <- data %>% recall(target, .pred_class, na.rm = TRUE)
  #fmeas <- data %>% f_meas(target, .pred_class, na.rm = TRUE)
  kap <- data %>% kap(target, .pred_class, na.rm = TRUE) %>%  dplyr::select(.estimate) %>% as.numeric %>% round(3)
  ###2) spatial stats
  spatial_acc <- data %>% mutate(trans.sum = n(), acc = acc, kap = kap ) %>%
###here the problem is differing number of target vs .pred_class     
     group_by(target.new) %>% 
        #dplyr::mutate(trans.tot = n()) %>% 
    # mutate(no.classes = length(unique(target.new))) %>% 
    dplyr::mutate(spat_p_correct = sum(p_Val)) %>% 
    dplyr::mutate(spat_pa_correct = sum(pa_Val)) %>% 
    dplyr::mutate(spat_pf_correct = sum(p_fVal)) %>% 
    dplyr::mutate(spat_paf_correct = sum(pa_fVal)) %>% 
    dplyr::select(-.pred_class, -target,  -p_fVal,  -p_Val ) %>% #-id, -target2,-pa_Val-pa_fVal,
    ungroup() %>% distinct() %>%   
    dplyr::mutate(spat_p = spat_p_correct/trans.tot) %>% 
    dplyr::mutate(spat_pa = spat_pa_correct/trans.tot ) %>% 
    dplyr::mutate(spat_pf = spat_pf_correct/trans.tot) %>% 
    dplyr::mutate(spat_paf = spat_paf_correct/trans.tot) %>% 
    dplyr::mutate(across(where(is.numeric), ~ replace(.,is.nan(.),0))) %>% 
    dplyr::mutate(across(where(is.numeric), ~ replace(.,is.infinite(.),0))) %>% 
            mutate( spat_p_theta1 = mean(spat_p), 
                    spat_p_theta0 = sum(spat_p_correct)/trans.sum, #,
                    spat_pa_theta1 = mean(spat_pa), 
                    spat_pa_theta0 = sum(spat_pa_correct)/trans.sum,
                    spat_paf_theta1 = mean(spat_paf), 
                    spat_paf_theta0 = sum(spat_paf_correct)/trans.sum) %>% 
    rowwise() %>%
    mutate(spat_p_theta_wt = theta * (1/no.classes) + (1-theta) * (spat_p_correct/trans.sum),
           spat_pa_theta_wt = theta * (1/no.classes) + (1-theta)* (spat_pa_correct/trans.sum),
          spat_paf_theta_wt = theta * (1/no.classes) + (1-theta)* (spat_paf_correct/trans.sum),
           spat_p_theta_work = spat_p_theta_wt * spat_p, #,
          spat_pa_theta_work = spat_pa_theta_wt * spat_pa,
          spat_paf_theta_work = spat_paf_theta_wt * spat_paf) %>% 
    ungroup() %>% 
    dplyr::mutate(spat_p_theta.5 = sum(spat_p_theta_work),
                  spat_pa_theta.5 = sum(spat_pa_theta_work),
                  spat_paf_theta.5 = sum(spat_paf_theta_work)) %>% 
    dplyr::select(-spat_p_theta_wt, -spat_p_theta_work,-spat_pa_theta_wt, -spat_pa_theta_work, -spat_paf_theta_wt, -spat_paf_theta_work) %>% #,  %>% 
    rename(target = target.new)
 
  #3) calculate aspatial metrics (overall and mapunit % correct)
  aspatial_target <- data  %>% dplyr::select(target) %>% 
    dplyr::add_count(target, name = 'trans.tot') %>% distinct() %>% 
    dplyr::mutate(trans.sum = sum(trans.tot))
 
   aspatial_pred <- data  %>% dplyr::select(.pred_class) %>% 
     dplyr::add_count(.pred_class, name = 'pred.tot') %>% 
    distinct() 
   
   aspatial_acc <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) %>% 
     mutate(no.classes = length(unique(target))) %>% 
   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>% 
     dplyr::rowwise() %>% 
     dplyr::mutate(aspat_p = min((trans.tot/trans.tot),(pred.tot/trans.tot)),
                   aspat_p_wtd= min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
     dplyr::mutate(across(where(is.numeric), ~ replace(.,is.nan(.),0))) %>% 
     ungroup() %>%
     mutate(aspat_p_theta0 = sum(aspat_p_wtd),
            aspat_p_theta1 = mean(aspat_p)) %>% 
        rowwise() %>%
     mutate(aspat_p_theta_wt = theta * (1/no.classes) + (1-theta)* (trans.tot/trans.sum)) %>% #
     mutate(aspat_p_theta_work = aspat_p_theta_wt * aspat_p_wtd) %>% 
     dplyr::mutate(across(where(is.numeric), ~ replace(.,is.nan(.),0))) %>% 
     ungroup() %>% 
     dplyr::mutate(aspat_p_theta.5 = sum(aspat_p_theta_wt * aspat_p)) %>% 
     dplyr::select(-aspat_p_theta_wt, -aspat_p_theta_work, -no.classes, -trans.sum, -trans.tot) %>% 
   ungroup() %>% distinct() 
   
   
   
   
accuracy_stats <- left_join(spatial_acc, aspatial_acc, by = 'target')
###calculate paf aspatial statistics  
# aspat_fpa_df <- accuracy_stats %>%
#   dplyr::select(target, trans.sum, no.classes, trans.tot, pred.tot, spat_p_correct, spat_paf_correct) %>%
#   rowwise() %>%
#   dplyr::mutate(aspat_paf_min_correct = min(trans.tot, pred.tot), 
#                 aspat_paf_extra = spat_paf_correct - spat_p_correct,
#                 aspat_paf_total = aspat_paf_min_correct + aspat_paf_extra,
#                 aspat_paf_pred = min((aspat_paf_total/trans.tot), (trans.tot/trans.tot)),
#                 aspat_paf_pred2= min((aspat_paf_total/trans.sum), (trans.tot/trans.sum)),                
#                 aspat_paf_unit_pos = min(trans.tot, aspat_paf_total)) %>%  
#   ungroup()%>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   dplyr::mutate(aspat_paf_theta0 = sum(aspat_paf_unit_pos/trans.sum),
#                 aspat_paf_theta1 = mean(aspat_paf_pred)) %>%
#   rowwise() %>%
#   mutate(aspat_paf_theta_wt = theta * (1/no.classes) + (1-theta)* (trans.tot/trans.sum)) %>% #
#   mutate(aspat_paf_theta_work = aspat_paf_theta_wt * aspat_paf_pred2) %>% 
#   ungroup() %>% 
#   dplyr::mutate(aspat_paf_theta.5 = sum(aspat_paf_theta_wt * aspat_paf_pred)) %>% 
#   dplyr::select(-aspat_paf_theta_wt, -aspat_paf_theta_work) %>% 
#   ungroup() %>% distinct() %>% 
#   dplyr::select(target, aspat_paf_theta0, aspat_paf_theta.5, aspat_paf_theta1) 
# 
# accuracy_stats <- left_join(accuracy_stats, aspat_fpa_df, by = 'target') %>% 
#   dplyr::select(target, trans.sum, trans.tot, pred.tot, no.classes, everything())
accuracy_stats

}

 


