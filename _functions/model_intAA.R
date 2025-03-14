
# initial protype script - now superseeded my model_gen_tidy.Rmd and R version 

# function to run model with internal AA version 

model_intAA <- function(trDat, target, target2, tid, outDir = ".", mname = "model", rseed = NA, 
                      infiles = infiles, mmu = mmu) {
  
  library(cowplot)
  
  # # # testing : GP
  # trDat = mpts       # data table
  # target = "target"  # primary call or target column name
  # target2 = "target2"
  # tid = "tid"          # transect_id column
  # outDir =  file.path(paste(out_dir, paste0(mname), sep = "/")) #output file
  # indata = indata    # name of input data file for reporting
  # rseed = 456        # seed
  # mmu = mmu
  # mname = mname      # model type for reporting
 
  
   # Testing - tmp filter for values with correct fuzzyness 
   trDat <- trDat[grep("SBSmc2",trDat$target),] %>% 
     droplevels() %>%
     as.data.table()
   
   #Read in and format data:
   trID <- unique(trDat$tid) %>% as.character()
   ntrID <- length(trID)
   trAll <- trDat  
    
   table(trAll[, target])
   # get the weighing values for the test dataset
   MU_count <-  trAll %>% 
     dplyr::count(target) %>%
     mutate(prop = n/sum(n, na.rm = TRUE))
   
   #iteration based on site (id)
    
   #bsRes <- foreach(it = 1:ntrID, .combine = rbind) %do% {
      #testID <- sample(trID,1)
   bsRes <- foreach(it = trID, .combine = rbind) %do% {
      # it = trID[1] 
      testID = it
      testDat_sf <- trDat[trDat$tid == testID,]
      testDat <- trAll[tid == testID,]
      
      trainDat <- trAll[tid != testID,]
      trainDat <- trainDat[is.na(target2),]
      trainDat[,`:=`(target2 = NULL,tid = NULL)]
      numTr <- trainDat[,.(Num = .N), by = .(target)]
      numTr[,New := as.integer(rescale(log(Num), to = c(50,800)))] # need to make this dynamic if using this.
      
      
      ###clhs to reduce tps within unit
      trainClean <- foreach(unit = numTr$target, .combine = rbind) %do% {
      #  # test line
      #  #   unit = "SBSmc2_05"
      #       if(numTr[target == unit,(Num)] > numTr[target == unit,(New)]){
      #        dat <- trainDat[target == unit,]
      #  dat <- trainDat[target == unit,]
      #  
      #        lhs <- clhs(dat[,!c("target", "tid","target2")], size = numTr[target == unit,(New)], 
      #                    use.cpp = T, simple = F,iter = 5000)
      #        res <- as.data.table(lhs$sampled_data)
      #        res[,target := unit]
      #      }else{
        dat <- trainDat[target == unit,]
        dat
         #}
      }
      
      trainClean <- unique(trainClean)
      trainClean[,target := as.factor(as.character(target))]
      
      ##create model
      mod <- ranger(target ~ ., data =  trainClean, 
                    num.trees = 501,importance = "impurity",
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
      
      testDat$Pred <- predict(mod,data = testDat)$predictions
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
    
    

    # Acc3 = metric to asess fuzzyiness, includes fuzzyness of calls + prop of calls that called that unit (proportion). 
    
    
    
    
    