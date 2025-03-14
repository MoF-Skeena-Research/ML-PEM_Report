
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
#data <- test.pred;    theta = 0.5
acc_metrix <- function(data, theta = 0.5) {



  ## pick highest value neighbour
  ### selects max value between primary and secondary calls
  data1 <- left_join(data, fMat, by = c("mapunit1" = "target", ".pred_class" = "Pred")) %>%
    mutate(fVal = ifelse(mapunit1 ==.pred_class, 1, fVal)) %>% mutate_if(is.character, as.factor) %>%
   replace(is.na(.), 0) %>%
    mutate(p_fVal = fVal) %>%
    dplyr::select(-fVal)
  data2 <- left_join(data1, fMat, by = c("mapunit2" = "target", ".pred_class" = "Pred")) %>%
    mutate(fVal = ifelse(mapunit1 ==.pred_class, 1, fVal)) %>% mutate_if(is.character, as.factor) %>%
    replace(is.na(.), 0) %>%
    mutate(alt_fVal = fVal)
  ### selects neighbour with max value
  data <- data2 %>%
    rowwise() %>%
    mutate(pa_fVal = max(p_fVal, alt_fVal)) %>%
    group_by(id) %>%
    top_n(1, abs(pa_fVal)) %>%
    distinct(id, .keep_all = TRUE) %>%
    data.frame() %>%
    dplyr::select(-fVal, -alt_fVal)

  data <- data %>%
    mutate_if(is.factor, as.character) %>%
    mutate(
      p_Val = ifelse(mapunit1 == .pred_class, 1, 0),
      alt_Val = ifelse(mapunit2 == .pred_class, 1, 0)
    ) %>%
    rowwise() %>%
    mutate(pa_Val = max(p_Val, alt_Val)) %>%
    dplyr::select(-alt_Val) %>%
    mutate_if(is.character, as.factor) %>%
    data.frame() %>%
    add_count(mapunit1, name = "trans.tot") %>%
    add_count(.pred_class, name = "pred.tot") # %>% group_by(mapunit1) %>%
  # dplyr::mutate(pred.tot_pa = sum(pa_Val)) %>% ungroup()

  targ.lev <- as.data.frame(levels(data$mapunit1)) %>%
    dplyr::rename(levels = 1) %>%
    droplevels()
  pred.lev <- as.data.frame(levels(data$.pred_class)) %>%
    dplyr::rename(levels = 1) %>%
    droplevels()
  add.pred.lev <- anti_join(pred.lev, targ.lev, by = "levels")
  data <- data %>%
    mutate(
      mapunit1.new = ifelse(.pred_class %in% add.pred.lev, as.character(.pred_class), as.character(mapunit1)),
      trans.tot.new = ifelse(.pred_class %in% add.pred.lev, 0, trans.tot)
    ) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(trans.tot = trans.tot.new)
  #   mutate(pred.new = ifelse(mapunit1.new %in% add.pred.lev, as.character(mapunit1), as.character(.pred_class))) %>%
  #     mutate(mapunit1 = mapunit1.new, .pred_class = pred.new)
  ### harmonize factor levels
  targ.lev <- levels(data$mapunit1)
  pred.lev <- levels(data$.pred_class)
  levs <- c(targ.lev, pred.lev) %>% unique()
  data$mapunit1 <- factor(data$mapunit1, levels = levs)
  data$.pred_class <- factor(data$.pred_class, levels = levs)

  data <- data %>%
    drop_na(mapunit1) %>%
    mutate(no.classes = length(levs)) %>%
    dplyr::select(-pred.tot, -trans.tot.new)

  ### 1)machine learning stats
  acc <- data %>%
    accuracy(mapunit1, .pred_class, na_rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  mcc <- data %>%
    mcc(mapunit1, .pred_class, na_rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  # sens <- data %>% sens(mapunit1, .pred_class, na_rm = TRUE)
  # spec <- data %>% yardstick::spec(mapunit1, .pred_class, na_rm = TRUE)
  # prec <- data %>% precision(mapunit1, .pred_class, na.rm = TRUE)
  # recall <- data %>% recall(mapunit1, .pred_class, na.rm = TRUE)
  # fmeas <- data %>% f_meas(mapunit1, .pred_class, na.rm = TRUE)
  kap <- data %>%
    kap(mapunit1, .pred_class, na.rm = TRUE) %>%
    dplyr::select(.estimate) %>%
    as.numeric() %>%
    round(3)
  ### 2) spatial stats
  spatial_acc <- data %>%
    mutate(trans.sum = n(), acc = acc, kap = kap) %>%
    ### here the problem is differing number of mapunit1 vs .pred_class
    group_by(mapunit1.new) %>%
    # dplyr::mutate(trans.tot = n()) %>%
    # mutate(no.classes = length(unique(mapunit1.new))) %>%
    dplyr::mutate(spat_p_correct = sum(p_Val)) %>%
    dplyr::mutate(spat_pa_correct = sum(pa_Val)) %>%
    dplyr::mutate(spat_pf_correct = sum(p_fVal)) %>%
    dplyr::mutate(spat_paf_correct = sum(pa_fVal)) %>%
    dplyr::select(-id, -mapunit2, -.pred_class, -mapunit1, -p_fVal, -pa_fVal, -p_Val, -pa_Val) %>%
    ungroup() %>%
    distinct() %>%
    dplyr::mutate(spat_p = spat_p_correct / trans.tot) %>%
    dplyr::mutate(spat_pa = spat_pa_correct / trans.tot) %>%
    dplyr::mutate(spat_pf = spat_pf_correct / trans.tot) %>%
    dplyr::mutate(spat_paf = spat_paf_correct / trans.tot) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.infinite(.), 0))) %>%
    mutate(
      spat_p_theta1 = mean(spat_p),
      spat_p_theta0 = sum(spat_p_correct) / trans.sum,
      spat_pa_theta1 = mean(spat_pa),
      spat_pa_theta0 = sum(spat_pa_correct) / trans.sum,
      spat_paf_theta1 = mean(spat_paf),
      spat_paf_theta0 = sum(spat_paf_correct) / trans.sum
    ) %>%
    rowwise() %>%
    mutate(
      spat_p_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_p_correct / trans.sum),
      spat_pa_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_pa_correct / trans.sum),
      spat_paf_theta_wt = theta * (1 / no.classes) + (1 - theta) * (spat_paf_correct / trans.sum),
      spat_p_theta_work = spat_p_theta_wt * spat_p,
      spat_pa_theta_work = spat_pa_theta_wt * spat_pa,
      spat_paf_theta_work = spat_paf_theta_wt * spat_paf
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      spat_p_theta.5 = sum(spat_p_theta_work),
      spat_pa_theta.5 = sum(spat_pa_theta_work),
      spat_paf_theta.5 = sum(spat_paf_theta_work)
    ) %>%
    dplyr::select(-spat_p_theta_wt, -spat_p_theta_work, -spat_pa_theta_wt, -spat_pa_theta_work, -spat_paf_theta_wt, -spat_paf_theta_work) %>%
    dplyr::rename(mapunit1 = mapunit1.new)

  # 3) calculate aspatial metrics (overall and mapunit % correct)
  aspatial_target <- data %>%
    dplyr::select(mapunit1) %>%
    dplyr::add_count(mapunit1, name = "trans.tot") %>%
    distinct() %>%
    dplyr::mutate(trans.sum = sum(trans.tot))
  #
  aspatial_pred <- data %>%
    dplyr::select(.pred_class) %>%
    dplyr::add_count(.pred_class, name = "pred.tot") %>%
    distinct()

  aspatial_acc <- left_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class")) %>%
    dplyr::select(mapunit1, trans.tot, pred.tot) %>% # group_by(mapunit1) %>%
    # mutate(p_correct = sum(p_Val),
    # pa_correct = sum(pa_Val),
    # paf_correct = sum(pa_fVal)) %>%
    # ungroup() %>%
    dplyr::mutate(trans.sum = sum(trans.tot)) %>% # dplyr::select(-p_Val, -pa_Val, -p_fVal, -pa_fVal) %>% distinct %>%

    # %>%
    mutate(no.classes = length(unique(mapunit1))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aspat_p = min((trans.tot / trans.tot), (pred.tot / trans.tot)),
      aspat_p_wtd = min((trans.tot / trans.sum), (pred.tot / trans.sum))
    ) %>%
    #                 aspat_pa = min((trans.tot/trans.tot),(pa_correct/trans.tot)),
    #                 aspat_pa_wtd= min((trans.tot/trans.sum),(pa_correct/trans.sum))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    ungroup() %>%
    mutate(
      aspat_p_theta0 = sum(aspat_p_wtd),
      aspat_p_theta1 = mean(aspat_p)
    ) %>%
    #            aspat_pa_theta0 = sum(aspat_pa_wtd),
    #           aspat_pa_theta1 = mean(aspat_pa)) %>%
    rowwise() %>%
    mutate(aspat_p_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    mutate(aspat_p_theta_work = aspat_p_theta_wt * aspat_p_wtd) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    ungroup() %>%
    dplyr::mutate(aspat_p_theta.5 = sum(aspat_p_theta_wt * aspat_p)) %>%
    dplyr::select(-aspat_p_theta_wt, -aspat_p_theta_work, -no.classes, -trans.sum, -trans.tot) %>%
    ungroup() %>%
    distinct()
  #### --- primary plus alternate
  data_pa <- data %>%
    mutate_if(is.factor, as.character) %>%
    mutate(mapunit1 = ifelse(mapunit2 == .pred_class, as.character(mapunit2), as.character(mapunit1))) %>%
    mutate_if(is.character, factor)

  aspatial_target <- data_pa %>%
    dplyr::select(mapunit1) %>% # %>% group_by(mapunit1) %>%
    dplyr::add_count(mapunit1, name = "trans.tot") %>%
    distinct()
  #
  aspatial_pred <- data_pa %>%
    dplyr::select(.pred_class) %>%
    dplyr::add_count(.pred_class, name = "pred.tot") %>%
    distinct()

  aspatial_acc_pa <- left_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class")) %>%
    dplyr::select(mapunit1, trans.tot, pred.tot) %>% # group_by(mapunit1) %>%
    # mutate(p_correct = sum(p_Val),
    # pa_correct = sum(pa_Val),
    # paf_correct = sum(pa_fVal)) %>%
    # ungroup() %>%
    dplyr::mutate(trans.sum = sum(trans.tot)) %>% # dplyr::select(-p_Val, -pa_Val, -p_fVal, -pa_fVal) %>% distinct %>%

    # %>%
    mutate(no.classes = length(unique(mapunit1))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      aspat_pa = min((trans.tot / trans.tot), (pred.tot / trans.tot)),
      aspat_pa_wtd = min((trans.tot / trans.sum), (pred.tot / trans.sum))
    ) %>%
    #                 aspat_pa = min((trans.tot/trans.tot),(pa_correct/trans.tot)),
    #                 aspat_pa_wtd= min((trans.tot/trans.sum),(pa_correct/trans.sum))) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    ungroup() %>%
    mutate(
      aspat_pa_theta0 = sum(aspat_pa_wtd),
      aspat_pa_theta1 = mean(aspat_pa)
    ) %>%
    #            aspat_pa_theta0 = sum(aspat_pa_wtd),
    #           aspat_pa_theta1 = mean(aspat_pa)) %>%
    rowwise() %>%
    mutate(aspat_pa_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    mutate(aspat_pa_theta_work = aspat_pa_theta_wt * aspat_pa_wtd) %>%
    dplyr::mutate(across(where(is.numeric), ~ replace(., is.nan(.), 0))) %>%
    ungroup() %>%
    dplyr::mutate(aspat_pa_theta.5 = sum(aspat_pa_theta_wt * aspat_pa)) %>%
    dplyr::select(-aspat_pa_theta_wt, -aspat_pa_theta_work, -no.classes, -trans.sum, -trans.tot) %>%
    ungroup() %>%
    distinct() %>%
    dplyr::select(-pred.tot)

  aspatial_acc2 <- left_join(aspatial_acc, aspatial_acc_pa, by = "mapunit1")


  accuracy_stats <- left_join(spatial_acc, aspatial_acc2, by = "mapunit1")
  ### calculate paf aspatial statistics
  aspat_fpa_df <- accuracy_stats %>%
    dplyr::select(mapunit1, trans.sum, no.classes, trans.tot, pred.tot, spat_p_correct, spat_paf_correct) %>%
    rowwise() %>%
    dplyr::mutate(
      aspat_paf_min_correct = min(trans.tot, pred.tot),
      aspat_paf_extra = spat_paf_correct - spat_p_correct,
      aspat_paf_total = aspat_paf_min_correct + aspat_paf_extra,
      aspat_paf_pred = min((aspat_paf_total / trans.tot), (trans.tot / trans.tot)),
      aspat_paf_pred2 = min((aspat_paf_total / trans.sum), (trans.tot / trans.sum)),
      aspat_paf_unit_pos = min(trans.tot, aspat_paf_total)
    ) %>%
    ungroup() %>%
    dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    dplyr::mutate(
      aspat_paf_theta0 = sum(aspat_paf_unit_pos / trans.sum),
      aspat_paf_theta1 = mean(aspat_paf_pred)
    ) %>%
    rowwise() %>%
    mutate(aspat_paf_theta_wt = theta * (1 / no.classes) + (1 - theta) * (trans.tot / trans.sum)) %>% #
    mutate(aspat_paf_theta_work = aspat_paf_theta_wt * aspat_paf_pred2) %>%
    ungroup() %>%
    dplyr::mutate(aspat_paf_theta.5 = sum(aspat_paf_theta_wt * aspat_paf_pred)) %>%
    dplyr::select(-aspat_paf_theta_wt, -aspat_paf_theta_work) %>%
    ungroup() %>%
    distinct() %>%
    dplyr::select(mapunit1, aspat_paf_theta0, aspat_paf_theta.5, aspat_paf_theta1)

  accuracy_stats <- left_join(accuracy_stats, aspat_fpa_df, by = "mapunit1") %>%
    dplyr::select(mapunit1, trans.sum, trans.tot, pred.tot, no.classes, everything())
  accuracy_stats
}

# #2) calculate aspatial metrics (overall and mapunit % correct)
# aspatial_pred <- data  %>%
#   dplyr::select(.pred_class) %>%
#   group_by(.pred_class) %>%
#   dplyr::mutate(pred.tot = n()) %>%
#   ungroup() %>% distinct()
#
# aspatial_target <- data %>%
#   dplyr::select(mapunit1) %>%
#   group_by(mapunit1) %>%
#   dplyr::mutate(trans.tot = n()) %>%
#   ungroup() %>% distinct()
#
# aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class")) %>%
#   # dplyr::mutate_if(is.integer, funs(replace_na(., 0))) %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
#   ungroup() %>%
#   mutate(aspat_p_overall = sum(aspat_p)) #%>%
#
# aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class")) %>%
#   # dplyr::mutate_if(is.integer, funs(replace_na(., 0))) %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
#   ungroup() %>%
#   mutate(aspat_p_overall = sum(aspat_p)) #%>%
#   #dplyr::select(-aspat_p)
#
# trans.sum <- unique(aspatial_sum$trans.sum)
#
# aspatial_sum <- aspatial_sum %>%
#   rowwise()%>%
#   dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
#   dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>%
#   dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0))%>%
#   ungroup()%>%
#   dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
#          spat_p_overall =  acc$.estimate)
#
#
# # generate spatially explicit results for primary and prime/alternate
# xx <- data %>% tabyl(mapunit1, .pred_class)
# xy <- pivot_longer(xx, cols = !mapunit1)
#
# # 2) generate  spatial primary accuracy
# spat_p <- xy %>%
#   filter(mapunit1 == name) %>%
#   mutate(spat_p_correct = value ) %>%
#   dplyr::select(mapunit1, spat_p_correct)
#
# outsum <- left_join(aspatial_sum, spat_p, by = "mapunit1")
#
#
# # generate spatial primary mean accuracy
# outsum <- outsum %>%
#   rowwise()%>%
#   mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
#   dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>%
#   ungroup() %>%
#   mutate(spat_p_meanacc = mean(spat_p_unit_pos))
#
# # 3) generate the spatial primary fuzzy calls:
# spat_fp_df <- xy %>%
#   left_join(fMat, by = c("mapunit1" = "mapunit1", "name" = "Pred")) %>%
#   rowwise() %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   dplyr::mutate(spat_fpt = fVal * value)  %>%
#   ungroup() %>%
#   dplyr::group_by(mapunit1) %>%
#   dplyr::mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
#   dplyr::select(mapunit1, spat_fp) %>%
#   distinct()
#
# outsum <- left_join(outsum, spat_fp_df, by = "mapunit1")
#
# outsum <- outsum %>%
#   dplyr::mutate(spat_pf_overall = (sum(spat_fp)/trans.sum)) %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   rowwise() %>%
#   dplyr::mutate(spat_pf_unit_pos = min(spat_fp, trans.tot)/trans.tot) %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   ungroup() %>%
#     mutate(spat_pf_meanacc = mean(spat_pf_unit_pos))
#
# # generate aspatial primary fuzzy call:
#
# aspat_fp_df <- outsum %>%
#   dplyr::select(mapunit1, trans.tot, pred.tot, spat_fp, spat_p_correct) %>%
#   rowwise() %>%
#   dplyr::mutate(aspat_p_min_correct = min(trans.tot, pred.tot),
#          aspat_fp_extra = spat_fp - spat_p_correct,
#          aspat_fp_total = aspat_p_min_correct + aspat_fp_extra,
#          aspat_fp_pred = min((aspat_fp_total/trans.sum), (trans.tot/trans.sum)),
#          aspat_fp_unit_pos = min(trans.tot, aspat_fp_total)/trans.tot) %>%
#   dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#   ungroup() %>%
#   dplyr::mutate(aspat_fp_overall = sum(aspat_fp_pred),
#          aspat_fp_meanacc = mean(aspat_fp_unit_pos)) %>%
#   dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_p_correct, aspat_p_min_correct))
#
# outsum <- left_join(outsum, aspat_fp_df, by = "mapunit1")
#
#
#   # for the test data compariosn we can calculate the alternate calls also
#
# #if(length(data)==3){
#
#   # spatially explicit calls:
#   spat_pa <- data %>%
#     filter(!is.na(mapunit2)) %>%
#     filter(mapunit1 != .pred_class)
#
#   # # check if there are any calls alt points
#    if(nrow(spat_pa) > 0){
#
#     # 5) calculate spatial prime / alt call accuracy
#     spat_pa <- spat_pa %>%
#       tabyl(mapunit2, .pred_class) %>%
#       pivot_longer(cols = !mapunit2) %>%
#       filter(mapunit2 == name) %>%
#       dplyr::mutate(mapunit1 = mapunit2,
#              spat_pa_correct = value) %>%
#       dplyr::select(mapunit1, spat_pa_correct)
#
#   # # 7) generate spatial fuzzy prime / alt calls :
#
#   spat_fpa_raw <- data %>%
#     left_join(fMat, by = c("mapunit1" = "mapunit1", ".pred_class" = "Pred")) %>%
#     left_join(fMat, by = c("mapunit2" = "mapunit1", ".pred_class" = "Pred")) %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     rowwise() %>%
#     #dplyr::mutate(targetMaxnew  = max(fVal.x , fVal.y)) %>%
#     dplyr::mutate(targetMax = ifelse(fVal.x >= fVal.y , mapunit1, mapunit2)) %>%
#     dplyr::select(targetMax, .pred_class) %>%
#     tabyl(targetMax,  .pred_class) %>%
#     pivot_longer(cols = !targetMax) %>%
#     left_join(fMat, by = c("targetMax" = "mapunit1", "name" = "Pred")) %>%
#     rowwise() %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     dplyr::mutate(fVal = ifelse(targetMax %in% name, 1, fVal)) %>% ## for units not listed in the fuzzy matrix
#     dplyr::mutate(spat_fpat = fVal * value)
#
#   spat_fpa_df <- spat_fpa_raw  %>%
#     group_by(targetMax) %>%
#     dplyr::mutate(spat_fpa = sum(spat_fpat)) %>%
#     dplyr::select(mapunit1 = targetMax, spat_fpa) %>%
#     distinct()
#
#   # calculate spatial pa calculations
#
#   outsum <- left_join(outsum, spat_pa, by = "mapunit1") %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     rowwise() %>%
#     dplyr::mutate(spat_pa_total = (spat_pa_correct + spat_p_correct)) %>%
#     ungroup()
#
#   outsum <- outsum %>%
#     dplyr::mutate(spat_pa_overall = (sum(spat_pa_total)/trans.sum))%>%
#     rowwise() %>%
#     dplyr::mutate(spat_pa_unit_pos = min(spat_pa_total,trans.tot)/trans.tot)%>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     ungroup() %>%
#     dplyr::mutate(spat_pa_meanacc = mean(spat_pa_unit_pos))
#
#   # calculate aspatial pa calcs
#
#   aspat_pa_df <- outsum %>%
#     dplyr::select(mapunit1, trans.tot, pred.tot, spat_fp, spat_pa_correct) %>%
#     rowwise() %>%
#     dplyr::mutate(aspat_pa_min_correct = min(trans.tot, pred.tot),
#            aspat_pa_extra = spat_pa_correct,
#            aspat_pa_total = aspat_pa_min_correct + aspat_pa_extra,
#            aspat_pa_pred = min((aspat_pa_total/trans.sum), (trans.tot/trans.sum)),
#            aspat_pa_unit_pos = min(trans.tot, aspat_pa_total)/trans.tot) %>%
#     ungroup()%>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     dplyr::mutate(aspat_pa_overall = sum(aspat_pa_pred),
#            aspat_pa_meanacc = mean(aspat_pa_unit_pos)) %>%
#     dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_pa_correct, aspat_pa_min_correct))
#
#   outsum <- left_join(outsum, aspat_pa_df, by = "mapunit1")
#
#   #calculate spatial fpa metrics
#   outsum <-  outsum %>%
#     left_join(spat_fpa_df, by = "mapunit1") %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     dplyr::mutate(spat_fpa_overall = sum(spat_fpa)/trans.sum) %>%
#     rowwise() %>%
#     dplyr::mutate(spat_fpa_unit_pos = min(spat_fpa,trans.tot)/trans.tot)%>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     ungroup() %>%
#     dplyr::mutate(spat_fpa_meanacc = mean(spat_fpa_unit_pos))
#
#   #calculate aspatial fpa metrics
#
#   aspat_fpa_df <- outsum %>%
#       dplyr::select(mapunit1, trans.tot, pred.tot, spat_p_correct, spat_fpa) %>%
#       rowwise() %>%
#       dplyr::mutate(aspat_fpa_min_correct = min(trans.tot, pred.tot),
#              aspat_fpa_extra = spat_fpa - spat_p_correct,
#              aspat_fpa_total = aspat_fpa_min_correct + aspat_fpa_extra,
#              aspat_fpa_pred = min((aspat_fpa_total/trans.sum), (trans.tot/trans.sum)),
#              aspat_fpa_unit_pos = min(trans.tot, aspat_fpa_total)/trans.tot) %>%
#       ungroup()%>%
#       dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#       dplyr::mutate(aspat_fpa_overall = sum(aspat_fpa_pred),
#              aspat_fpa_meanacc = mean(aspat_fpa_unit_pos)) %>%
#       dplyr::select( -c(trans.tot, pred.tot, spat_p_correct, aspat_fpa_min_correct,spat_fpa))
#
#     outsum <- left_join(outsum, aspat_fpa_df, by = "mapunit1")
#     outsum$accuracy <- acc$.estimate
#     outsum$kappa <- kap$.estimate
#     outsum$mcc <- mcc$.estimate
# outsum <- outsum %>% dplyr::select(slice, mapunit1,	trans.tot, pred.tot, trans.sum, pred.tot, accuracy, kappa, everything())
# #                                          spat_p_unit_pos, spat_pa_unit_pos,  spat_fpa_unit_pos,
# #                                          aspat_p_unit_pos, aspat_pa_unit_pos,  aspat_fpa_unit_pos)
# outsum$mcc <- mcc$.estimate

# } else {
#
#   print ("no secondary calls in test dataset")
#
#   fix_cols <- outsum %>%
#     mutate("spat_pa_correct" = 0,
#            "spat_pa_total" = 0,
#            "spat_pa_overall" = 0 ,
#            "spat_pa_unit_pos" = 0,
#            "spat_pa_meanacc" = 0 ,
#            "spat_fpa" = 0 ,
#            "spat_fpa_overall" = 0 ,
#            "spat_fpa_unit_pos" = 0 ,
#            "spat_fpa_meanacc" = 0 ,
#            "aspat_pa_extra"= 0 ,
#            "aspat_pa_total"= 0 ,
#            "aspat_pa_pred" = 0 ,
#            "aspat_pa_unit_pos" = 0 ,
#            "aspat_pa_overall" = 0 ,
#            "aspat_pa_meanacc" = 0 ,
#           "aspat_fpa_extra"= 0 ,
#            "aspat_fpa_total"= 0 ,
#            "aspat_fpa_pred" = 0 ,
#            "aspat_fpa_unit_pos"= 0 ,
#            "aspat_fpa_overall"= 0 ,
#            "aspat_fpa_meanacc" = 0 )

# outsum <- fix_cols

#    }

# rowwise() %>%
# dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE))
# }
# write.csv(outsum, "test26.csv")
# outsum

# }


# test <- acc_metrix(test.pred)
# cv <- acc_metrix(cv_pred_sum)
# setdiff(names(test),names(cv))


# function to calculate the weighted metrics

weight_by_transect_no <- function(acc) {
  acc_sum <- acc %>%
    #     acc_sum <- acc %>%
    dplyr::filter(acc_type == "test_estimate") %>%
    mutate(across(ends_with("overall"), ~ .x * 100)) %>%
    mutate(across(ends_with("meanacc"), ~ .x * 100)) %>%
    dplyr::select(
      slice, acc_type, transect_no,
      aspat_p_overall, aspat_p_meanacc,
      # aspat_fp_overall,  aspat_fp_meanacc,
      spat_p_overall, spat_p_meanacc,
      spat_pf_overall, spat_pf_meanacc,
      aspat_pa_overall, aspat_pa_meanacc,
      aspat_fpa_overall, aspat_fpa_meanacc,
      spat_pa_overall, spat_pa_meanacc,
      spat_fpa_overall, spat_fpa_meanacc
    ) %>%
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

  acc_wt_sum <- left_join(acc_wt_ave, acc_wt_sd) %>%
    filter(!accuracy_type == "transect_no")

  return(acc_wt_sum)
}


# function to calculate the theta metrics

# theta_accuracy <- function(acc, theta = 0.5){
#
#  # acc <- temp
# #  theta = 0.1
#
#    acc <- acc %>%
#      dplyr::select(mapunit1,	trans.tot, pred.tot, trans.sum, pred.tot, slice,
#                spat_p_unit_pos, spat_pa_unit_pos,   spat_fpa_unit_pos, #spat_pf_unit_pos,
#                aspat_p_unit_pos, aspat_pa_unit_pos,  aspat_fpa_unit_pos)#aspat_fp_unit_pos,
#
#   acc_theta <- acc %>%
#     group_by(slice) %>%
#     mutate(no.classes = length(mapunit1)) %>%
#     ungroup()
#
#   # calculate the theta_spat_p
#
#   acc_theta <- acc_theta %>%
#     rowwise() %>%
#     mutate(theta_wt = theta * (1/no.classes) + (1-theta)* (pred.tot/trans.sum)) %>%
#     mutate(spat_p_unit_pos_raw = theta_wt *  spat_p_unit_pos,
#            spat_pa_unit_pos_raw = theta_wt *  spat_pa_unit_pos,
#            #spat_pf_unit_pos_raw  = theta_wt * spat_pf_unit_pos,
#            spat_fpa_unit_pos_raw = theta_wt * spat_fpa_unit_pos,
#            aspat_p_unit_pos_raw = theta_wt * aspat_p_unit_pos,
#            aspat_pa_unit_pos_raw = theta_wt * aspat_pa_unit_pos,
#            #aspat_fp_unit_pos_raw = theta_wt * aspat_fp_unit_pos,
#            aspat_fpa_unit_pos_raw = theta_wt *   aspat_fpa_unit_pos)
#
#     acc_theta_raw <- acc_theta %>%
#       group_by(slice) %>%
#       mutate(spat_p_theta = sum(spat_p_unit_pos_raw),
#              spat_pa_theta = sum(spat_pa_unit_pos_raw),
#              #spat_pf_theta = sum(spat_pf_unit_pos_raw),
#              spat_fpa_theta = sum(spat_fpa_unit_pos_raw),
#              aspat_p_theta = sum(aspat_p_unit_pos_raw),
#              aspat_pa_theta = sum(aspat_pa_unit_pos_raw),
#              #aspat_fp_theta = sum(aspat_fp_unit_pos_raw),
#              aspat_fpa_theta = sum(aspat_fpa_unit_pos_raw))
#
#
#     noi <-  names(acc_theta_raw)[str_detect(names(acc_theta_raw),"_theta")]
#
#     acc_theta_raw <- acc_theta_raw %>%
#        dplyr::select(c(mapunit1, slice, all_of(noi)))
#
#  return(acc_theta_raw)
# }
#


# Generate accuracy metrics for Non-forest model. This outputs basic metrics for
# primary call only :

# acc_metrix_nf <- function(data){
#
#     #data <- cv_pred_sum
#
#     # Run accuracy metrics
#     acc <- data %>% accuracy(mapunit1, .pred_class, na_rm = TRUE)
#     kap <- data %>% kap(mapunit1, .pred_class, na.rm = TRUE)
#
#     #2) calculate aspatial metrics (overall and mapunit % correct)
#     aspatial_pred <- data  %>%
#       dplyr::select(.pred_class) %>%
#       group_by(.pred_class) %>%
#       dplyr::mutate(pred.tot = n()) %>%
#       ungroup() %>% distinct()
#
#     aspatial_target <- data %>%
#       dplyr::select(mapunit1) %>%
#       group_by(mapunit1) %>%
#       dplyr::mutate(trans.tot = n()) %>%
#       ungroup() %>% distinct()
#
#     aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class")) %>%
#       dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#       dplyr::mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>%
#       dplyr::rowwise() %>%
#       dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
#       ungroup() %>%
#       mutate(aspat_p_overall = sum(aspat_p)) #%>%
#     #dplyr::select(-aspat_p)
#
#     trans.sum <- unique(aspatial_sum$trans.sum)
#
#     aspatial_sum <- aspatial_sum %>%
#       rowwise()%>%
#       dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
#       drop_na() %>%
#       dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
#                     spat_p_overall =  acc$.estimate,
#                     kap = kap$.estimate)
#
#     # generate spatially explicit results for primary and prime/alternate
#     xx <- data %>% tabyl(mapunit1, .pred_class)
#     xy <- pivot_longer(xx, cols = !mapunit1)
#
#     # 2) generate primary accuracy
#     spat_p <- xy %>%
#       filter(mapunit1 == name) %>%
#       mutate(spat_p_correct = value ) %>%
#       dplyr::select(mapunit1, spat_p_correct)
#
#     outsum <- left_join(aspatial_sum, spat_p, by = "mapunit1")
#
#     outsum <- left_join( aspatial_sum, spat_p, by = "mapunit1") %>%
#       mutate(model = "nf")
#
#     outsum <- outsum %>%
#       rowwise()%>%
#       mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
#       drop_na() %>%
#       ungroup() %>%
#       mutate(spat_p_meanacc = mean(spat_p_unit_pos))
#
#     return(outsum)
# }
#
#
#
# ###################################################################################
#
# library(yardstick)
# library(janitor)
#
#
# acc_metrix_loo <- function(data, uss){
#
#   uss_all = uss
#   ## testing lines
#   #data <- cv_pred_sum
#   data <- test.pred
#
#   acc <- data %>% accuracy(mapunit1, .pred_class, na_rm = TRUE)
#   mcc <- data %>%  mcc(mapunit1, .pred_class, na_rm = TRUE)
#   #sens <- data %>% sens(mapunit1, .pred_class, na_rm = TRUE)
#   #spec <- data %>% yardstick::spec(mapunit1, .pred_class, na_rm = TRUE)
#   #prec <- data %>% precision(mapunit1, .pred_class, na.rm = TRUE)
#   #recall <- data %>% recall(mapunit1, .pred_class, na.rm = TRUE)
#   #fmeas <- data %>% f_meas(mapunit1, .pred_class, na.rm = TRUE)
#   kap <- data %>% kap(mapunit1, .pred_class, na.rm = TRUE)
#
#
#   #2) calculate aspatial metrics (overall and mapunit % correct)
#   aspatial_pred <- data  %>%
#     dplyr::select(.pred_class) %>%
#     group_by(.pred_class) %>%
#     dplyr::mutate(pred.tot = n()) %>%
#     ungroup() %>% distinct()
#
#   aspatial_target <- data %>%
#     dplyr::select(mapunit1) %>%
#     group_by(mapunit1) %>%
#     dplyr::mutate(trans.tot = n()) %>%
#     ungroup() %>% distinct()
#
#   aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("mapunit1" = ".pred_class"))
#
#   # add missing levels
#     if(length(uss_all) == length(aspatial_sum$mapunit1)){
#       print("all levels included")
#     } else {
#
#       missing_mu <- setdiff(uss,  aspatial_sum$mapunit1)
#       missing_mu_df <- tibble(
#         mapunit1 = as.factor(missing_mu),
#         trans.tot = 0,
#         pred.tot = 0
#       )
#       aspatial_sum <- bind_rows(aspatial_sum, missing_mu_df)
#     }
#
#
#   aspatial_sum <- aspatial_sum %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     # dplyr::mutate(trans.sum = sum(trans.tot, na.rm = TRUE)) %>%
#     dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(aspat_p = min((trans.tot/trans.sum),(pred.tot/trans.sum))) %>%
#     ungroup() %>%
#     dplyr::mutate(trans.sum = sum(pred.tot, na.rm = TRUE)) %>%
#     mutate(aspat_p_overall = sum(aspat_p)) #%>%
#   #dplyr::select(-aspat_p)
#
#   trans.sum <- unique(aspatial_sum$trans.sum)
#
#   aspatial_sum <- aspatial_sum %>%
#     rowwise()%>%
#     dplyr::mutate(aspat_p_unit_pos = min(trans.tot, pred.tot)/trans.tot) %>%
#     dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>%
#     dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), 0))%>%
#     ungroup()%>%
#     dplyr::mutate(aspat_p_meanacc = mean(aspat_p_unit_pos),
#                   spat_p_overall =  acc$.estimate)
#
#   # generate spatially explicit results for primary and prime/alternate
#   xx <- data %>% tabyl(mapunit1, .pred_class)
#   xy <- pivot_longer(xx, cols = !mapunit1)
#
#   # 2) generate  spatial primary accuracy
#   spat_p <- xy %>%
#     filter(mapunit1 == name) %>%
#     mutate(spat_p_correct = value ) %>%
#     dplyr::select(mapunit1, spat_p_correct)
#
#   outsum <- left_join(aspatial_sum, spat_p, by = "mapunit1")
#
#   # generate spatial primary mean accuracy
#   outsum <- outsum %>%
#     dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>%
#     rowwise()%>%
#     mutate(spat_p_unit_pos = spat_p_correct/trans.tot) %>%
#     dplyr::mutate_if(is.numeric, ~replace_na(., 0)) %>%
#     ungroup() %>%
#     mutate(spat_p_meanacc = mean(spat_p_unit_pos))
#
#   # 3) generate the primary fuzzy calls:
#   spat_fp_df <- xy %>%
#     left_join(fMat, by = c("mapunit1" = "mapunit1", "name" = "Pred")) %>%
#     rowwise() %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     dplyr::mutate(spat_fpt = fVal * value)  %>%
#     ungroup() %>%
#     dplyr::group_by(mapunit1) %>%
#     dplyr::mutate(spat_fp = sum(spat_fpt, na.rm = TRUE)) %>%
#     dplyr::select(mapunit1, spat_fp) %>%
#     distinct()
#
#   outsum <- left_join(outsum, spat_fp_df, by = "mapunit1") %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0)))
#
#   outsum <- outsum %>%
#     dplyr::mutate(spat_pf_overall = (sum(spat_fp)/trans.sum)) %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     rowwise() %>%
#     dplyr::mutate(spat_pf_unit_pos = min(spat_fp, trans.tot)/trans.tot) %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     ungroup() %>%
#     mutate(spat_pf_meanacc = mean(spat_pf_unit_pos))
#
#   # generate aspatial primary fuzzy call:
#
#   aspat_fp_df <- outsum %>%
#     dplyr::select(mapunit1, trans.tot, pred.tot, spat_fp, spat_p_correct) %>%
#     rowwise() %>%
#     dplyr::mutate(aspat_p_min_correct = min(trans.tot, pred.tot),
#                   aspat_fp_extra = spat_fp - spat_p_correct,
#                   aspat_fp_total = aspat_p_min_correct + aspat_fp_extra,
#                   aspat_fp_pred = min((aspat_fp_total/trans.sum), (trans.tot/trans.sum)),
#                   aspat_fp_unit_pos = min(trans.tot, aspat_fp_total)/trans.tot) %>%
#     dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#     ungroup() %>%
#     dplyr::mutate(aspat_fp_overall = sum(aspat_fp_pred),
#                   aspat_fp_meanacc = mean(aspat_fp_unit_pos)) %>%
#     dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_p_correct, aspat_p_min_correct))
#
#   outsum <- left_join(outsum, aspat_fp_df, by = "mapunit1")
#
#
#   # for the test data compariosn we can calculate the alternate calls also
#
#   if(length(data)==3){
#
#     # spatially explicit calls:
#     spat_pa <- data %>%
#       filter(!is.na(mapunit2)) %>%
#       filter(mapunit1 != .pred_class)
#
#     # # check if there are any calls alt points
#     if(nrow(spat_pa) > 0){
#
#       # 5) calculate spatial prime / alt call accuracy
#       spat_pa <- spat_pa %>%
#         tabyl(mapunit2, .pred_class) %>%
#         pivot_longer(cols = !mapunit2) %>%
#         filter(mapunit2 == name) %>%
#         dplyr::mutate(mapunit1 = mapunit2,
#                       spat_pa_correct = value) %>%
#         dplyr::select(mapunit1, spat_pa_correct)
#
#       # # 7) generate spatial fuzzy prime / alt calls :
#
#       spat_fpa_raw <- data %>%
#         left_join(fMat, by = c("mapunit1" = "mapunit1", ".pred_class" = "Pred")) %>%
#         left_join(fMat, by = c("mapunit2" = "mapunit1", ".pred_class" = "Pred")) %>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         rowwise() %>%
#         dplyr::mutate(targetMax = ifelse(fVal.x >= fVal.y , mapunit1, mapunit2)) %>%
#         dplyr::select(targetMax, .pred_class) %>%
#         tabyl(targetMax,  .pred_class) %>%
#         pivot_longer(cols = !targetMax) %>%
#         left_join(fMat, by = c("targetMax" = "mapunit1", "name" = "Pred")) %>%
#         rowwise() %>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         dplyr::mutate(spat_fpat = fVal * value)
#
#       spat_fpa_df <- spat_fpa_raw  %>%
#         group_by(targetMax) %>%
#         dplyr::mutate(spat_fpa = sum(spat_fpat)) %>%
#         dplyr::select(mapunit1 = targetMax, spat_fpa) %>%
#         distinct()
#
#       # calculate spatial pa calculations
#
#       outsum <- left_join(outsum, spat_pa, by = "mapunit1") %>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         rowwise() %>%
#         dplyr::mutate(spat_pa_total = (spat_pa_correct + spat_p_correct)) %>%
#         ungroup()
#
#       outsum <- outsum %>%
#         dplyr::mutate(spat_pa_overall = (sum(spat_pa_total)/trans.sum))%>%
#         rowwise() %>%
#         dplyr::mutate(spat_pa_unit_pos = min(spat_pa_total,trans.tot)/trans.tot)%>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         ungroup() %>%
#         dplyr::mutate(spat_pa_meanacc = mean(spat_pa_unit_pos))
#
#       # calculate aspatial pa calcs
#
#       aspat_pa_df <- outsum %>%
#         dplyr::select(mapunit1, trans.tot, pred.tot, spat_fp, spat_pa_correct) %>%
#         rowwise() %>%
#         dplyr::mutate(aspat_pa_min_correct = min(trans.tot, pred.tot),
#                       aspat_pa_extra = spat_pa_correct,
#                       aspat_pa_total = aspat_pa_min_correct + aspat_pa_extra,
#                       aspat_pa_pred = min((aspat_pa_total/trans.sum), (trans.tot/trans.sum)),
#                       aspat_pa_unit_pos = min(trans.tot, aspat_pa_total)/trans.tot) %>%
#         ungroup()%>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         dplyr::mutate(aspat_pa_overall = sum(aspat_pa_pred),
#                       aspat_pa_meanacc = mean(aspat_pa_unit_pos)) %>%
#         dplyr::select( -c(trans.tot, pred.tot, spat_fp, spat_pa_correct, aspat_pa_min_correct))
#
#       outsum <- left_join(outsum, aspat_pa_df, by = "mapunit1")
#
#       #calculate spatial fpa metrics
#       outsum <-  outsum %>%
#         left_join(spat_fpa_df, by = "mapunit1") %>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         dplyr::mutate(spat_fpa_overall = sum(spat_fpa)/trans.sum) %>%
#         rowwise() %>%
#         dplyr::mutate(spat_fpa_unit_pos = min(spat_fpa,trans.tot)/trans.tot)%>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         ungroup() %>%
#         dplyr::mutate(spat_fpa_meanacc = mean(spat_fpa_unit_pos))
#
#       #calculate aspatial fpa metrics
#
#       aspat_fpa_df <- outsum %>%
#         dplyr::select(mapunit1, trans.tot, pred.tot, spat_p_correct, spat_fpa) %>%
#         rowwise() %>%
#         dplyr::mutate(aspat_fpa_min_correct = min(trans.tot, pred.tot),
#                       aspat_fpa_extra = spat_fpa - spat_p_correct,
#                       aspat_fpa_total = aspat_fpa_min_correct + aspat_fpa_extra,
#                       aspat_fpa_pred = min((aspat_fpa_total/trans.sum), (trans.tot/trans.sum)),
#                       aspat_fpa_unit_pos = min(trans.tot, aspat_fpa_total)/trans.tot) %>%
#         ungroup()%>%
#         dplyr::mutate(across(where(is.numeric), ~ replace_na(.,0))) %>%
#         dplyr::mutate(aspat_fpa_overall = sum(aspat_fpa_pred),
#                       aspat_fpa_meanacc = mean(aspat_fpa_unit_pos)) %>%
#         dplyr::select( -c(trans.tot, pred.tot, spat_p_correct, aspat_fpa_min_correct,spat_fpa))
#
#       outsum <- left_join(outsum, aspat_fpa_df, by = "mapunit1")
#
#     } else {
#
#       print ("no secondary calls in test dataset")
#
#       fix_cols <- outsum %>%
#         mutate(spat_pa_correct = 0,
#                spat_pa_total = 0,
#                "spat_pa_overall" = 0 ,
#                "spat_pa_unit_pos" = 0,
#                "spat_pa_meanacc" = 0 ,
#                "aspat_pa_extra"= 0 ,
#                "aspat_pa_total"= 0 ,
#                "aspat_pa_pred" = 0 ,
#                "aspat_pa_unit_pos" = 0 ,
#                "aspat_pa_overall" = 0 ,
#                "aspat_pa_meanacc" = 0 ,
#                "spat_fpa" = 0 ,
#                "spat_fpa_overall" = 0 ,
#                "spat_fpa_unit_pos" = 0 ,
#                "spat_fpa_meanacc" = 0 ,
#                "aspat_fpa_extra"= 0 ,
#                "aspat_fpa_total"= 0 ,
#                "aspat_fpa_pred" = 0 ,
#                "aspat_fpa_unit_pos"= 0 ,
#                "aspat_fpa_overall"= 0 ,
#                "aspat_fpa_meanacc" = 0 )
#
#       outsum <- fix_cols
#
#     }
#
#     #rowwise() %>%
#     #dplyr::mutate(spat_pa = sum(spat_pa, spat_p, na.rm = TRUE))
#   }
#   #write.csv(outsum, "test26.csv")
#   outsum
# }
