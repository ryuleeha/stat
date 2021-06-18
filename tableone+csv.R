tabone<-function(vars,                                      # character vector of variable names
                 strata,                                    # character vector of variable names
                 data,                                      # data frame
                 factorVars,                                # variables to be transformed to factors
                 filename,
                 includeNA     = FALSE,                     # include NA as a category (categoricals only)
                 test          = TRUE,                      # whether to include p-values
                 ## Test configuration for categorical data
                 testApprox    = chisq.test,                # function for approximation test
                 argsApprox    = list(correct = TRUE),      # arguments passed to testApprox
                 testExact     = fisher.test,               # function for exact test
                 argsExact     = list(workspace = 2*10^5),  # arguments passed to testExact
                 ## Test configuration for continuous data
                 testNormal    = oneway.test,               # test for normally distributed variables
                 argsNormal    = list(var.equal = TRUE),    # arguments passed to testNormal
                 testNonNormal = kruskal.test,              # test for nonnormally distributed variables
                 argsNonNormal = list(NULL),                # arguments passed to testNonNormal
                 smd           = TRUE,                      # whether to include standardize mean differences
                 addOverall    = FALSE,
                 ## print.TableOne
                 # Number of digits to show
                 quote         = FALSE,       # Whether to show quotes
                 
                 ## Common options
                 missing       = FALSE, # Not implemented yet
                 explain       = TRUE,  # Whether to show explanation in variable names
                 printToggle   = TRUE,  # Whether to print the result visibly
                 p.test          = TRUE,  # Whether to add p-values
                 p.smd           = FALSE, # Whether to add standardized mean differences
                 noSpaces      = FALSE, # Whether to remove spaces for alignments
                 padColnames   = FALSE, # Whether to pad column names for alignments
                 varLabels     = FALSE, # Whether to show variable labels instead of names.
                 
                 ## Categorical options
                 format        = c("fp","f","p","pf")[1], # Format f_requency and/or p_ercent
                 showAllLevels = FALSE, # Show all levels of a categorical variable
                 cramVars      = NULL,  # Which 2-level variables to show both levels in one row
                 dropEqual     = FALSE, # Do not show " = second level" for two-level variables
                 exact         = NULL,  # Which variables should be tested with exact tests
                 
                 ## Continuous options
                 nonnormal     = NULL,  # Which variables should be treated as nonnormal
                 minMax        = FALSE, # Whether to show median
                 
                 formatOptions = list(scientific = FALSE),... # Options for formatting
) {
  
  
  f.vars<-as.factor(factorVars)
  if (!is.factor(vars)) {
    
    
    
  }
  tab<-tableone::CreateTableOne(vars,strata,data,factorVars,
                                includeNA,test,testApprox,
                                argsApprox,testExact,argsExact,
                                testNormal,argsNormal,testNonNormal,
                                argsNonNormal,smd,addOverall)
  
  tabprint<-print(x=tab,catDigits = 1, contDigits = 2, pDigits = 3,quote,missing,explain,printToggle,
                  test=p.test,smd=p.smd,noSpaces,padColnames,varLabels,format,showAllLevels,cramVars,
                  dropEqual,exact,nonnormal,minMax,formatOptions)
  
  write.csv(tabprint,paste(filename,".csv",sep=""))
}



library(survival)
data(pbc)
vars<-c("time", "status", "trt", "age", "sex", "ascites", "hepato", 
        "spiders", "edema", "bili", "chol", "albumin", "copper", "alk.phos", 
        "ast", "trig", "platelet", "protime", "stage")
group<-c("trt")
b<-c("bili")
t1<-tabone(vars=vars,strata=group,factorVars = group,data=pbc,p.test=FALSE,filename = "pbctab")
class(t1)
