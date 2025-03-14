### code chunk taken from ClimateWNA modelling scripts.
## Original Concept and Scripting by W.H. MacKenzie//Scripting vastly improved by K. Daust



### r remove outlier points by mahalanobis distance
removeOutlier <- function(dat, alpha){
  temp <- as.data.table(dat)
  temp[,MD := mahalanobis(.SD,center = colMeans(.SD),cov = pseudoinverse(cov(.SD)), inverted = T), by = BGC]
  ctf <- qchisq(1-alpha, df = ncol(temp)-1)
  temp <- temp[MD < ctf,]
  return(temp)
}

#Xoutlier2 <- Xoutlier %>% filter(BGC == "IDFdk1")
#XX1 <- removeOutlier(Xoutlier, alpha = .01, numIDvars = 1) ###set alpha for removal of outlieres (2.5% = 3SD)
###outlier function requires 1 column labeled BGC and no other id columns
XX <- removeOutlier(Xoutlier, alpha = 0.4) ###set alpha for removal of outlieres (2.5% = 3SD)
XX1 <- XX %>% select(ID1) %>% left_join(IDVars) ###write csv to see where the plots now occur

X0 <- XX %>% select(ID1) %>% left_join(X0) ### join the reduced points to original variable data
X0 <- as.data.table(X0)

count_tp <- X0 %>% dplyr::count(BGC) ## calculate the number of training points per unit in raw data
X0 <- X0 %>% select(BGC, everything())

#X0 <- X0 %>% mutate_if (is.factor, as.character) %>% mutate_if(is_integer,as.numeric) %>% mutate_if (is.character, as.factor)

## identify units that are over or undersampled

X0[,Num := .N, by = .(BGC)]
X0_OK <- as.data.table(X0[Num > 100 & Num < 500,]) ## for these we just use raw sample set
X0[,LogNum := log(Num,base = 10)] ## log10 the training point count
X0[Num <= 100, New := scales::rescale(LogNum, to = c(50,100))] ## rescale to generate the number of training points to SMOTE generate where  too few unit
X0[Num >= 500, New := scales::rescale(LogNum, to = c(500,1000))] ## rescale to generate a number to down sample by cLHS where too many
X0[,New := as.integer(New)]

X0temp <- as.data.table(X0[Num >= 500,])
X0temp$BGC <- as.factor(X0temp$BGC)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)
###Down sammple the units with too many points
X0_big <- foreach(unit = unique(X0temp$BGC), .combine = rbind,
                  .packages = c("clhs","data.table")) %dopar% {
                    cat("Processing",unit,"\n")
                    temp <- X0temp[BGC == unit,]
                    num <- temp$New[1]
                    ##nz <- nearZeroVar(temp, names=T) ##do we need this?
                    lhs <- clhs(temp[,!c("BGC","Num","LogNum","New")],size = (num -2), iter = 1000, use.cpp = T, simple = F)
                    res <- lhs$sampled_data
                    res$BGC <- unit
                    res
                  }

X0temp <- as.data.table(X0[Num <= 100,])# %>% filter (!(BGC == "ESSFxh_WA" | BGC == "IMAun_UT" | BGC == "MHun" ))
X0_small <- X0temp %>% select(-New, -Num, -LogNum)
### upsample the units with too few points
foreach(unit = unique(X0temp$BGC), .combine = rbind, .packages = c("clhs","data.table")) %do% {
  cat("Processing",unit,"\n")
  temp <- X0temp[BGC == unit,] %>% mutate_if(is.integer,as.numeric)
  temp$BGC <- as.numeric(as.factor(temp$BGC))
  num <- (temp$New[1])/temp$Num[1]
  temp <- as.data.table(temp)
  tempSMOTE <- smotefamily::SMOTE(temp[,!c("BGC","Num","LogNum","New")],
                                  temp$BGC, K = 2, dup_size = ceiling(num))
  newCases <- temp$New[1] - temp$Num[1]
  synData <- tempSMOTE$syn_data
  synData <- synData[sample(1:nrow(synData), size = newCases, replace = F),]
  synData$class <- NULL
  synData$BGC <- unit
  synData
}

X0_OK[,Num := NULL]
#X0_OK<- X0_OK %>% select(-New,  -LogNum)
XAll <- rbind(X0_small,X0_OK,X0_big) %>% select(-ID1) ### stick the OK, too few and too many training point sets back together
#XAll <- X0  ###unalanced data set
XAll <- as.data.table(XAll)

XAll[,BGC := as.factor(BGC)]
XAll$BGC  <- droplevels(XAll$BGC)

```