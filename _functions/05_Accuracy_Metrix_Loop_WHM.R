
## need to read in the lookup tables is using this script

library(data.table)
library(knitr)
library(cowplot)
library(tidymodels)
library(tidyverse)
library(themis)
library(ggplot2)
library(gridExtra)
library(janitor)
require(magicfor)
require(ggthemes)
require(ggthemr)
require(ggsci)
require(ggpubr)
require(patchwork)
require(hrbrthemes)

## function to run accuracy metrics given a table with 2 columns representing target truth and predicted class
acc_metrix <- function(data){
 data <- test.pred  
acc <- data %>% accuracy(target, .pred_class, na_rm = TRUE)  
 mcc <- data %>%  mcc(target, .pred_class, na_rm = TRUE)
  
 # ###some aspatial metrics
  aspatial_pred <- data  %>% dplyr::select(.pred_class) %>% group_by(.pred_class) %>% mutate(pred.ratio = n()) %>%ungroup() %>% distinct()
  aspatial_target <- data  %>% dplyr::select(target) %>% group_by(target) %>% mutate(targ.ratio = n())%>%ungroup() %>% distinct()
  aspatial_sum <- full_join(aspatial_target, aspatial_pred, by = c("target" = ".pred_class")) %>% mutate_if(is.integer, funs(replace_na(., 0))) %>% rowwise() %>% mutate(Min = min(targ.ratio, pred.ratio))
  .estimate <- colSums(aspatial_sum[,4])/colSums(aspatial_sum[,2])
  .metric= "aspatial_acc"
  .estimator= "aspatial"
  aspatial_acc <- data.frame(.metric, .estimator, .estimate)
  # 
   aspatial_sum <- aspatial_sum %>% mutate(unit_pos = Min/targ.ratio) %>% drop_na()
  mean_acc <- colMeans(aspatial_sum[5])
  .estimate <- mean_acc
  .metric= "aspatial_meanacc"
  .estimator= "aspatial"
  aspatial_meanacc <- data.frame(.metric, .estimator, .estimate)
  
  cv_metrics <- bind_rows (acc,  mcc, aspatial_acc, aspatial_meanacc)# acc_bal,jind, ppv, precision, recall, kap, fmean, sens, spec, jind) %>% mutate_if(is.character, as.factor)
  ##______________ add alt-call metrics
  ##______________ add in fuzzy-call metrics     
  ##______________  add in metrics by map unit aspatial
  #unit_metrics
}
#cv_metrics <- acc_metrix(test.pred)

###Read Data
trDat <-  fread("D:/GitHub/PEM_Methods_DevX/Deception_AOI/1_map_inputs/trainingData/att_5m/Stage1_cleaned_5m_pts_data.csv")
table(trDat[, target])
tpts <- trDat
### removes points that are not forested
MU.for <- map.key %>% dplyr::select(MapUnit, Type) %>% filter(Type == "For") %>% distinct()
MU.for <- MU.for$MapUnit
tpts <- tpts %>% filter(target %in% MU.for)
MU_count2 <- tpts %>% dplyr::count(target)
# filter groups less than 20 pts
#MU_count <- mpts %>% dplyr::count(target) %>% filter(n > 20) 

trDat <- tpts
# calculate summary of raw training data set to check set going into loop
trDat_sum <- trDat %>%
  dplyr::group_by(target) %>%
  summarise(freq = n()) %>%
  mutate(prop = round(freq/sum(freq),3))

ggplot(trDat, aes(target)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90))


trDat_all <- trDat[complete.cases(trDat[ , 5:length(trDat)]),]

#####################  BGC Loop
BGCDat <- trDat_all %>% filter(bgc == "SBSmc2")  %>% dplyr::select(-bgc, -pt_id) %>% mutate_if(is.character, as.factor) %>% mutate_if(is.integer, as.numeric)
slices <- unique(BGCDat$slice) %>% droplevels()


####################### CV Loop

magic_for(print, silent = TRUE)

for (k in levels(slices)){
  #test_train_MU_ratio <- foreach(k = iter, .combine = 'rbind') %do% {
  #k = "SBSmc2_3"
  #test_slice = "SBSmc2_1"
  ### separate train vs test based on 5-site slices
  
  #############Build slices
  BGC_train <- BGCDat %>% filter(!slice %in% k)  ## filter for pure calls only
  BGC_train$target2 <- BGC_train$target2 %>% na_if("")
  BGC_train <- BGC_train%>% filter(is.na(target2)) %>% dplyr::select(-slice, -target2) %>% droplevels()
            
  BGC_test <- BGCDat %>% filter(slice %in% k) %>% dplyr::select(-slice, -target2)
  
  trDat_sum <- BGC_train %>%
    dplyr::group_by(target) %>%
    summarise(freq = n()) %>%
    mutate(prop = round(freq/sum(freq),3))
  trDat_sum$test_slice <- k
  #print (trDat_sum)
  
###############Define test recipes and workflow ###################
null_recipe <-
  recipe(target ~ ., data = BGC_train) %>%
  update_role(tid, new_role = "id variable") %>% 
  #step_corr(all_numeric()) %>%        # remove correlated covariates
  #step_dummy(all_nominal(),-all_outcomes()) %>%   
  #step_zv(all_numeric()) %>%          # remove values with no variance
  prep()
summary(null_recipe)

balance_recipe1 <-  recipe(target ~ ., data =  BGC_train) %>%
  update_role(tid, new_role = "id variable") %>% 
  #step_corr(all_numeric()) %>%        # remove correlated covariates
  #step_dummy(all_nominal(),-all_outcomes()) %>%    
  # step_zv(all_numeric()) %>% # remove values with no variance
  step_downsample(target, under_ratio = 25) %>%
  step_smote(target, over_ratio = 1, neighbors = 2) %>% 
  prep()


set.seed(345)
pem_cvfold <- group_vfold_cv(BGC_train, 
                             v = 10, ### need to build a check for number of tids available to automatically reduce this number where necessary
                             repeats = 5, 
                             group = tid,
                             strata = target)

summary(pem_cvfold)

randf_spec <- rand_forest(mtry = 10, min_n = 2, trees = 200) %>% ## trees = 200 is approximately good metrics improve by 1% going 100 -> 200 but go down at higher ntrees
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation", verbose = FALSE) #or "permutations

# pem_workflow <- workflow() %>%
#   add_recipe(null_recipe) %>%
#   add_model(randf_spec)

pem_workflow <- workflow() %>%
  add_recipe(balance_recipe1) %>%
  add_model(randf_spec)
#######################################################

set.seed(4556)
doParallel::registerDoParallel() # note when smoting with fit_resample you cant use parrallel process or will cause error

cv_results <- fit_resamples(
  pem_workflow, 
  resamples = pem_cvfold, 
  # metrics = cv_metric_set, 
  control = control_resamples(save_pred = TRUE))

# collect metrics  
cv_metrics <- cv_results  %>% collect_metrics(summarize = FALSE)
cv_metrics_sum <- cv_results %>% collect_metrics()

# collect predictions
cv_pred <- cv_results %>% collect_predictions(summarize = FALSE)
cv_pred_sum <- cv_results %>% collect_predictions(summarize = TRUE)
cv_pred_sum <- cv_pred_sum %>% dplyr::select(target, .pred_class)

identical(levels(cv_pred_sum$target),levels(cv_pred_sum$.pred_class))
## CV model accuracy metrics
cv_pred_sum <- as.data.frame(cv_pred_sum)
#cv_pred_sum %>% bal_accuracy(target, .pred_class)
cv_metrics <- acc_metrix(cv_pred_sum)


## now build final train model and predict test data and compare acc_metrix to cv results.
## Cannot use the lastfit due to having a non-standard split of test and train

PEM_rf1 <- fit(
  pem_workflow, 
  BGC_train)

######### Predict Test
test_target <-as.data.frame(BGC_test$target) %>% rename(target = 1)
test.pred <-  predict(PEM_rf1, BGC_test)
test.pred <- cbind(test_target, test.pred) %>% mutate_if(is.character, as.factor)
# levels(train.pred$target)

###harmonize levels
targ.lev <- levels(test.pred$target); pred.lev <- levels(test.pred$.pred_class)
levs <- c(targ.lev, pred.lev) %>% unique()
test.pred$target <- factor(test.pred$target, levels = levs)
test.pred$.pred_class <- factor(test.pred$.pred_class, levels = levs)
# 
# train.acc <- acc_metrix(train.pred) %>% rename(train = .estimate)
test.acc <- acc_metrix(test.pred) %>% dplyr::select(.estimate) %>% rename(test = 1)
#print(test.acc)
## compare cv stats to test stats  
acc.compare <- cbind(cv_metrics, test.acc)
print (acc.compare)
}

x <- magic_result_as_dataframe()
Accuracy.metrix <- unnest(x, cols = c(acc.compare))

Accuracy.metrix2 <- Accuracy.metrix %>% dplyr::select(.metric, .estimate, test) %>% pivot_longer(cols = -.metric, names_to = "test") %>% 
  mutate(.metric= fct_relevel(.metric, "accuracy", "mcc", "aspatial_acc","aspatial_meanacc"))
Accuracy.metrix2$model <- "balanced"
Accuracy.metrix_SBS_balanced <- Accuracy.metrix2

p_SBSmc2_balanced <- ggplot(aes(y = value, x = .metric, fill = test), data = Accuracy.metrix2) +
   geom_boxplot() +
   geom_hline(yintercept = 65,linetype ="dashed", color = "red") +
   theme(axis.text.x = element_text(angle = 90)) +
   xlab("Accuracy Measure") + 
  ylab("Accuracy Value") +
  geom_hline(yintercept = .65, linetype="dashed", color = "red") +
  scale_fill_few("Light", name = "Source", labels = c("cv","test")) + #
  
   ylim(0, 1)+
  theme_few(base_size = 10)+
  #labs(title = "Comparison of cv versus test accuracy metrics for raw data ESSFmc, Deception Lake",
  #     subtitle = "10 kfold/ 5 repeated cv. Train/Test split leave-one-out cLHS 5-site slice",
   #    caption = "Figure.")

p_SBSmc2_balanced


SBS_compare <- rbind(Accuracy.metrix_SBS_balanced, Accuracy.metrix_SBSmc2raw) %>% unite(test2, c('model','test'))

p_SBSmc2 <- ggplot(aes(y = value, x = .metric, fill = test2), data = SBS_compare) +
  geom_boxplot() + #show.legend = FALSE
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Accuracy Measure") + 
  ylab("Accuracy Value") +
  geom_hline(yintercept = .65, linetype="dashed", color = "red") +
  #scale_fill_few("Light") + #, name = "Source", labels = c("cv","test"
  scale_fill_ipsum()+
  ylim(0, 1)+
  theme_few(base_size = 10) #+ #
  #ggtitle ('SBSmc2')
  # labs(title = "Comparison of cv versus test accuracy metrics for raw data SBSmc2, Deception Lake",
  #      subtitle = "10 kfold/ 5 repeated cv. Train/Test split leave-one-out cLHS 5-site slice",
  #      caption = "Figure.")

p_SBSmc2 


ESSF_compare <- rbind(Accuracy.metrix_ESSF_balanced, Accuracy.metrix_ESSFraw) %>% unite(test2, c('model','test'))

p_ESSF <- ggplot(aes(y = value, x = .metric, fill = test2), data = ESSF_compare ) +
  geom_boxplot() +
  geom_hline(yintercept = 65,linetype ="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Accuracy Measure") + 
  ylab("") +
  geom_hline(yintercept = .65, linetype="dashed", color = "red") +
  #scale_fill_few("Light") + #, name = "Source", labels = c("cv","test"
  scale_fill_ipsum(name = "Model_metric source")+
  ylim(0, 1)+
  theme_few(base_size = 10)# + #
  #ggtitle ('ESSFmc')
  # labs(title = ,
  #      subtitle = "10 kfold/ 5 repeated cv. Train/Test split leave-one-out cLHS 5-site slice",
  #      caption = "Figure. Comparison of cv versus test accuracy metrics for Deception Lake")

p_ESSF
acc_plot_comp <- ggarrange(p_SBSmc2, p_ESSF + remove ("x.text"), common.legend = TRUE, legend = "right", labels = c("SBSmc2", "ESSFmc"), hjust = -1,vjust = 1.8, font.label = list(size = 10))
acc_plot_comp2 <- annotate_figure(acc_plot_comp ,
                #top = text_grob(),
                bottom = text_grob('Figure 1. Comparison of cv versus test accuracy metrics for raw data and balanced data', color = "black",
                                   hjust = 1.4, x = 1, face = "plain", size = 10),
                #left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                #right = text_grob(bquote("Superscript: ("*kg~NH[3]~ha^-1~yr^-1*")"), rot = 90),
                #fig.lab = 'Figure 1. Comparison of cv versus test accuracy metrics for raw data and balanced data' , fig.lab.face = "plain", fig.lab.pos = "bottom.left"
                )
acc_plot_comp2 
ggsave(filename = "./Deception_AOI/3_maps_analysis/Report_output/Compared_accuracy.jpg", acc_plot_comp2, width = 8, height = 4, dpi = 600, units = "in", device='jpg')
# 
# patchwork <- p_SBSmc2 | p_ESSF + plot_layout()
# patchwork + plot_annotation(
#   title = 'Comparison of cv versus test accuracy metrics for raw data and balanced data',
#   subtitle = 'Deception Lake study area',
#   caption = "10 kfold/ 5 repeated cv. Train/Test split leave-one-out cLHS 5-site slice"
# )



#ggexport(patchwork, filename = "Map accuracy2.pdf")
