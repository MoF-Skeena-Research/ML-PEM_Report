
# function to run model with internal AA version 

model_extAA <- function(trDat, target, target2, tid, outDir = ".", mname = "model", rseed = NA, 
                      infiles = infiles, mmu = mmu) {
  
  # # testing : GP
 #  trDat = mpts       # data table
#   target = "target"  # primary call or target column name
#   target2 = "mapunit2"
#   tid = "tid"          # transect_id column
#   outDir =  file.path(paste(out_dir, paste0(mname), sep = "/")) #output file
#   indata = indata    # name of input data file for reporting 
#   rseed = 456        # seed 
#   mmu = mmu
#   mname = mname      # model type for reporting 
    
   # load library if needed 
   library(data.table)
   library(scales)
   library(clhs)
  
   # Testing - tmp filter for values with correct fuzzyness 
   trDat <- trDat[grep("SBSmc2",trDat$target),] %>% 
     droplevels() %>%
     as.data.table()
   
   
   #Read in and format data:
   trID <- unique(trDat$tid) %>% as.character()
   trAll <- trDat  
    
   table(trAll[, target])
    
   bsRes <- foreach(it = 1:10, .combine = rbind) %do% {
      testID <- sample(trID,1)
      testDat_sf <- trDat[trDat$tid == testID,]
      testDat <- trAll[tid == testID,]
      #testDat <- testDat[,!c("tid","target2")]
      trainDat <- trAll[tid != testID,]
      trainDat <- trainDat[is.na(target2),]
      trainDat[,`:=`(target2 = NULL,tid = NULL)]
      numTr <- trainDat[,.(Num = .N), by = .(target)]
      numTr[,New := as.integer(rescale(log(Num), to = c(50,800)))]
      
      
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
      #trainData <- trainClean[,!"tid", "target2"]
      
      ##create model
      mod <- ranger(target ~ ., data =  trainClean, 
                    num.trees = 501,importance = "impurity",
                    splitrule = "extratrees",
                    classification = T)
      
      temp <- predict(mod, data = testDat[,!c("target","target2", "tid")], predict.all = T)
      predMat <- as.data.table(temp$predictions)
      predMat[,pID := seq_along(1:nrow(predMat))]
      predMat[,target := testDat$target]
      predMat <- data.table::melt(predMat, id.vars = c("pID","target"))
      predMat <- predMat[,.(Num = .N), by = .(target,pID,value)]
      predMat[,Prop := Num/501]
      leg2 <- data.table(UnitName = mod$forest$levels, ID = seq_along(mod$forest$levels))
      predMat[leg2, PredUnit := i.UnitName, on = c(value = "ID")]
      predMat[fMat, fVal := i.fVal, on = c("target", PredUnit = "Pred")]
      predMat[,TotProp := Prop * fVal]
      predMat <- predMat[complete.cases(predMat),]
      
      testDat$Pred <- predict(mod,data = testDat)$predictions
      testDat <- testDat[,.(target, target2,Pred)]
      testDat[fMat, fVal1 := i.fVal, on = c("target","Pred")]
      testDat[fMat, fVal2 := i.fVal, on = c(target2 = "target","Pred")]
      testDat[is.na(fVal2), fVal2 := 0]
      testDat[,fValMax := max(fVal1,fVal2), by = seq(1:nrow(testDat))]
    
      
      res <- foreach(unit = unique(testDat$target), .combine = rbind) %do% {
        sub <- testDat[target == unit,]
        sub2 <- predMat[target == unit,]
        out <- data.table(Unit = unit, Acc1 = sum(sub$fVal1)/nrow(sub),
                          Acc2 = sum(sub$fValMax)/nrow(sub))
                          #Acc3 = sum(sub2$TotProp)/length(unique(sub2$pID)))
        out
      }
      res[,It := it]
      res
    }
    
    write.csv(bsRes, file.path(out_dir, "externalAA_metrics_raw.csv"))
   
    bsRes_sum <- bsRes %>%
      group_by(Unit)%>%
      summarise(mean_acc1 = mean(Acc1), 
                sd_acc1 = sd(Acc1),
                mean_acc2 = mean(Acc2), 
                sd_acc2 = sd(Acc2))#,
                #mean_acc3 = mean(Acc3), 
                #sd_acc3 = sd(Acc3)) 
                
    write.csv(bsRes_sum, file.path(out_dir, "externalAA_metrics_summary.csv"))
    
    
}

    # Acc3 = metric to asess fuzzyiness, includes fuzzyness of calls + prop of calls that called that unit (proportion). 
    
    
    
    
    