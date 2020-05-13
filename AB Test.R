
#########################################
#########                       #########
#########      A/B Testing      #########
#########                       #########
#########################################


# load the functions needed to run the test
source('AB Functions.R')

###  INPUTS  ####

# expectations based on historical results, percent value of success whether it's opens or clicks or whatever the test may be.
# Change these inputs to your typical conversion rates and variance (variance is just the Standard Deviation squared)
A.priorSucces <- 0.0468
A.priorVariance <- 0.09617667
B.priorSucces <- 0.0468
B.priorVariance <- 0.09617667
#C.priorSucces <- 0.0468
#C.priorVariance <- 0.09617667


### Results of current test
A.success <- 8
A.total <- 649
B.success <- 10
B.total <- 632

## It's A/B/C test uncomment and fill these 2 lines in
#C.success <- 14
#C.total <- 135

## succes rate of each group
print(paste0("Group A Converts at ", round((A.success/A.total)*100, 2), "%"))
print(paste0("Group B Converts at ", round((B.success/B.total)*100, 2), "%"))
#print(paste0("Group C Converts at ", round((C.success/C.total)*100, 2), "%"))


# Run A/B Test
twoGroups(A.priorSucces, A.priorVariance, B.priorSucces, B.priorVariance, 
          A.success, A.total, B.success, B.total, yvalue = 125) # if you need to adjust the graph size change yvalue



## It's A/B/C test uncomment and fill these 2 lines in
# Run A/B/C test
threeGroups(A.priorSucces, A.priorVariance, B.priorSucces, B.priorVariance, C.priorSucces, C.priorVariance,
            A.success, A.total, B.success, B.total, C.success, C.total, yvalue = 75)
