::::::::::::::
expectedInflammation.R
::::::::::::::
options(java.parameters="-DSIMULATION_THREAD_COUNT = 128")
options(java.mem=1024*8)
library(REFSfs)
library(data.table)

ensloc <- "/home/jacobja9/gbp/run4/Results/model/NetFragFile.txt.protobuf"
ens <- fsReadModel(ensloc)

load("/home/jacobja9/gbp/data/refsReady_bl_012418.RData")

colnames(refs_ready_bl)[ grep("anti", colnames(refs_ready_bl))] <-
 gsub( "-", "_", colnames(refs_ready_bl)[ grep("anti", colnames(refs_ready_bl))])

biomarker <- "anti_3317_33"
biomarker <- "clin_num_ALT"

biomarker <- "anti_3317_33"
biomarkers <- c("anti_2744_57", "anti_3331_8", "anti_4160_49")

biomarker <- biomarkers[3]
biomarker <- "anti_5688_65"


biomarker.values <- quantile(refs_ready_bl[[ biomarker ]], seq(.1,0.9,.1))
outcome <- "Lobular.Inflammation.Score"

experimentData <- data.frame(
    experiment = as.character(c(biomarker)),
    condition = as.character(biomarker.values),
    variable = as.character(c(biomarker)),
    value = c(biomarker.values))

dataused = refs_ready_bl[, fsGrepVars(ens, ".*")]

simulation <- fsSimulateOverwrite(ens = ens,
    c(outcome), baselineData = dataused[, fsFixedVars(ens)], sampleSize = 5,
    conditionNames = as.character(biomarker.values),
    experimentData = experimentData, reportMeans = FALSE)

simulation <- simulation$result[[1]]
simulation <- as.data.table(simulation)

#Set up sample indicator for aggregation purposes later on
sampleSize = 5
num_experiments = length(biomarker.values)
num_patients    = dim(refs_ready_bl)[1]
num_networks    = 128
sampleSize_ind           = data.frame(matrix(ncol = 1, nrow = sampleSize))
sampleSize_ind$sample    =  c(1:sampleSize)
sampleSize_ind           = rbindlist(rep(list(sampleSize_ind),num_experiments*num_patients*num_networks))
sampleSize_ind[,1]       = NULL

simulation <- as.data.frame(cbind( sampleSize_ind, simulation))
simulation <- data.table(simulation)
df <- simulation[, lapply(.SD, mean), by = list(condition, network, sample)]

df %>% group_by(condition) %>% mutate( lower = quantile(output, 0.025), upper = quantile(output, 0.975), median = median(output)) %>% select( condition, lower, upper, media
n) -> df_results

unique( df_results[c("condition", "lower", "upper", "median")])


::::::::::::::
expectedNAFLD.R
::::::::::::::
options(java.parameters="-DSIMULATION_THREAD_COUNT = 128")
options(java.mem=1024*8)
library(REFSfs)
library(data.table)

ensloc <- "/home/jacobja9/gbp/run4/Results/model/NetFragFile.txt.protobuf"
ens <- fsReadModel(ensloc)

load("/home/jacobja9/gbp/data/refsReady_bl_012418.RData")

colnames(refs_ready_bl)[ grep("anti", colnames(refs_ready_bl))] <-
 gsub( "-", "_", colnames(refs_ready_bl)[ grep("anti", colnames(refs_ready_bl))])

biomarker <- "anti_3317_33"
biomarker <- "clin_num_ALT"

biomarker <- "anti_3317_33"
biomarkers <- c("anti_2744_57", "anti_3331_8", "anti_4160_49")

biomarker <- biomarkers[3]
biomarker <- "anti_5688_65"
biomarker <- "anti_4430_44"


biomarker.values <- quantile(refs_ready_bl[[ biomarker ]], seq(.1,0.9,.1))
outcome <- "group7"

experimentData <- data.frame(
    experiment = as.character(c(biomarker)),
    condition = as.character(biomarker.values),
    variable = as.character(c(biomarker)),
    value = c(biomarker.values))

dataused = refs_ready_bl[, fsGrepVars(ens, ".*")]

simulation <- fsSimulateOverwrite(ens = ens,
    c(outcome), baselineData = dataused[, fsFixedVars(ens)], sampleSize = 5,
    conditionNames = as.character(biomarker.values),
    experimentData = experimentData, reportMeans = FALSE)

simulation <- simulation.orig
simulation <- simulation$result[[1]]
#Set up sample indicator for aggregation purposes later on
sampleSize = 5
num_experiments = length(biomarker.values)
num_patients    = dim(refs_ready_bl)[1]
num_networks    = 128
sampleSize_ind           = data.frame(matrix(ncol = 1, nrow = sampleSize))
sampleSize_ind$sample    =  c(1:sampleSize)
sampleSize_ind           = rbindlist(rep(list(sampleSize_ind),num_experiments*num_patients*num_networks))
sampleSize_ind[,1]       = NULL
simulation <- as.data.frame(cbind( sampleSize_ind, simulation))
as.data.frame(simulation) -> simulation
pmf1               = simulation$pmf[,5]
pmf0               = simulation$pmf[,3]
simulation$pmf     = NULL
simulation$pmf2  = pmf1
simulation$pmf1  = pmf0

simulation$output = NULL
simulation <- as.data.table(simulation)
df <- simulation[, lapply(.SD, mean), by = list(condition, network, sample)]

 df$odds <- df$pmf2/df$pmf1


df %>% group_by(condition) %>% mutate( lower = quantile(odds, 0.025), upper = quantile(odds, 0.975), median = median(odds)) %>% select( condition, lower, upper, median) ->
df_results

unique( df_results[c("condition", "lower", "upper", "median")])

ggplot( unique( df_results[c("condition", "lower", "upper", "median")]), aes( x=condition, y=median))+geom_point() + geom_errorbar(aes( ymin=lower, ymax=upper), width = 0.1
)

