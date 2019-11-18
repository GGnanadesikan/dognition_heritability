#Linear mixed model analysis for Gnanadesikan et al. 
#"Estimating the Heritability of Cognitive Traits Across Dog Breeds 
#Reveals Highly Heritable Inhibitory Control and Communication Factors"
#
#Note: this is a single iteration of the model (one of the 1000 iterations),
#and includes a no-covariates version and a version controlling for weight.
#
#Author: GEG
#Date: 11/16/19
#####################################################################
library(NAM)
#####################################################################
load("Gnanadesikan_data.RData")
#cog_samp contains the cognitive data: factor scores for 540 individuals, 15/breed.
#gen_exp is the k-matrix, a relatedness matrix based on the breed-avg IBS

#####################################################################
#No Covariates Version
#####################################################################
#Loop over factors
for (i in 1:fac_num){
  #Extract phenotype of interest (scores on a single factor)
  behavior <- as.matrix(cog_samp[,i+2])
  #Model
  trait_reml = reml(y = behavior[,1], X = model.matrix(~1, intercepts), K = gen_exp, Z = z_full)
  #Save the Vg, Ve, and h2
  results[i, 2:4] <- trait.reml$VC
}
results
#####################################################################
#Controlling for weight
#####################################################################
#Loop over factors
for (i in 1:fac_num){
  #Extract phenotype of interest (scores on a single factor)
  behavior <- as.matrix(cog_samp[,i+2])
  #Model
  trait_reml = reml(y = behavior[,1], X = model.matrix(~1 + weights, intercepts), K = gen_exp, Z = z_full)
  #Save the Vg, Ve, and h2
  results_weight[i, 2:4] <- trait.reml$VC
}
results_weight