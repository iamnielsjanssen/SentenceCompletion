# R-ANALYSIS SCRIPT FOR 
#
# If you use this code for analysis that is published in an indexed journal or repository, please cite the following article:
#
# Janssen, N. & Lopéz-Peréz, P. J., Determinants of response latencies in the sentence completion task.
#
# Niels Janssen, Sept. 2017.


rm(list=ls(all=TRUE)) 
setwd('set/to/path') # put correct path here!
dt = read.table("data_matrix_SC_v1.0.txt", header = TRUE)

nrow(dt) # = 5703 total cleaned trials


####
# STEP 1 - new variables
####

# 1. Frequency of the response (LexFreq, here: logLexespC)
# 2. Length of the response (RLen, here: num_phonC)
# 3. Length of the sentence (SLen, here: slenC)
# 4. Phonological neighborhood of the response (PNS, here: logPNC)
# 5. Lexical Diversity of the participant (LexDiv, here: scohortC)
# 6. Response counts (RespCounts, here: logcountsC)
# 7. Sentence Cohort size (CohortSize, here: cohortsizeC)
# 8. Articulatory 1 (Plosive, here: init_phonC)
# 9. Articulatory 2 (Voiced, here: voicedC)
#
# Strategy here is to for try to determine the by subject random slopes, and leave the by-item slopes empty
# then after getting the minimal model for by-subjects, start with a full mode by-items and trim that down.
# This avoids starting with a full model that does not converge-

require(lme4)

# Do Bates et al. (2015) first by-subjects
model1s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + voicedC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
summary(model1s)

# remove by-subject num_phonC, loglexespC
model2s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+cohortsizeC + logcountsC + logPNC + slenC + voicedC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
anova(model1s,model2s) # not-significant, so simpler model2s justified.
summary(model2s)

# remove by-subject cohortsize
model3s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+logcountsC + logPNC + slenC + voicedC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
anova(model2s,model3s) # not-significant, so simpler model3s justified.
summary(model3s)

# remove by-subject slenC
model4s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+logcountsC + logPNC + voicedC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
anova(model3s,model4s) # not-significant, so simpler model3s justified.
summary(model4s)

# remove by-subject voicedC
model5s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+logcountsC + logPNC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
anova(model4s,model5s) # not-significant, so simpler model3s justified.
summary(model5s)

# remove by-subject logPNC
model6s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+logcountsC + init_phonC||subject) + (1|item), data = dt, REML = FALSE)
anova(model5s,model6s) # not-significant, so simpler model3s justified.
summary(model6s)

# remove by-subject init_phonC
model7s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+ logcountsC||subject) + (1|item), data = dt, REML = FALSE)
anova(model6s,model7s) # not-significant, so simpler model3s justified.
summary(model7s)

# remove by-subject logcountsC
model8s = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1|item), data = dt, REML = FALSE)
anova(model7s,model8s) # significant! model7s is final model
summary(model8s)

####
# same thing with by items
model1i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + loglexespC + logPNC + num_phonC + scohortC + voicedC + init_phonC||item), data = dt, REML = FALSE)
summary(model1i)

# remove by-item scohort
model2i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + loglexespC + logPNC + num_phonC + voicedC + init_phonC||item), data = dt, REML = FALSE)
anova(model1i,model2i) # not significant, removal justified.
summary(model2i)

# remove by-item loglexespC
model3i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + logPNC + num_phonC + voicedC + init_phonC||item), data = dt, REML = FALSE)
anova(model2i,model3i) # not significant, removal justified.
summary(model3i)

# remove by-item logPNC
model4i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + num_phonC + voicedC + init_phonC||item), data = dt, REML = FALSE)
anova(model3i,model4i) # not significant, removal justified.
summary(model4i)

# remove by-item init_phonC
model5i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + num_phonC + voicedC ||item), data = dt, REML = FALSE)
anova(model4i,model5i) # not significant, removal justified.
summary(model5i)

# remove by-item voicedC
model5i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model4i,model5i) # not significant, removal justified.
summary(model5i)

# remove by-item num_phonC or logcountsC
model6i = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1|subject) + (1+ logcountsC + num_phonC||item), data = dt, REML = FALSE)
anova(model5i,model6i) # significant! removal NOT justified. Final model is 5i.
summary(model5i)

###
# combine final models from subjects and items
# repeat procedure for fixed effects

model1 = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + logPNC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
summary(model1)

# remove fixed effect logPNC
model2 = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + num_phonC + slenC + scohortC + voicedC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model1, model2) # not signiticant, removal justified.
summary(model2)

# remove fixed effect voicedC
model3 = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + num_phonC + slenC + scohortC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model2, model3) # not signiticant, removal justified.
summary(model3)

# remove fixed effect num_phonC
model4 = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model3, model4) # not signiticant, removal justified.
summary(model4)

# remove fixed effect slenC
model5 = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + scohortC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model4, model5) # signiticant! removal NOT justified. model4 is final model.
summary(model5)

# testing for removal of significant effects in final model
model4a = lmer(logRT ~ cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE)
anova(model4, model4a) 

# test for accumulation
anova(model1, model4)
anova(model1s, model7s)
anova(model1i, model5i)

# check condition index
source("mer-utils.R") # from Florian Jaeger website
kappa.mer(model4) # condition index is 2.00

# get p-values
require(afex)
mixed(logRT ~ cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logcountsC||subject) + (1+ logcountsC + num_phonC ||item), data = dt, REML = FALSE, method = "S")



####
# STEP 2 - existing variables
####

# 1. Cloze Probability (here: logagreeC)
# 2. Response Ratio (here: logstsratioC)
# 3. LSA Strength (here: logLSAC)
# 4. LSA Entropy (here: loglsaentC)

# effect of cloze-prob by itself
model4a1 = lmer(logRT ~ logagreeC + (1+ logagreeC|subject) + (1+ logagreeC |item), data = dt, REML = FALSE)
model4a2 = lmer(logRT ~ logagreeC + (1+ logagreeC|subject) + (1|item), data = dt, REML = FALSE)
mixed(logRT ~ logagreeC + (1+ logagreeC|subject) + (1|item), data = dt, REML = FALSE, method="S")
anova(model4a1,model4a2)
summary(model4a1)

# effect of cloze-prob with new vars
model4a = lmer(logRT ~ logagreeC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC+ logcountsC||subject) + (1+ logagreeC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logagreeC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC+ logcountsC||subject) + (1+ logagreeC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4a)


# effect of response ratio by itself
model4b1 = lmer(logRT ~ logstsratioC+ (1+ logstsratioC|subject) + (1+ logstsratioC |item), data = dt, REML = FALSE)
model4b2 = lmer(logRT ~ logstsratioC+ (1|subject) + (1|item), data = dt, REML = FALSE)
mixed(logRT ~ logstsratioC+ (1|subject) + (1|item), data = dt, REML = FALSE, method="S")
anova(model4b1,model4b2)
summary(model4b1)

# effect of response ratio
model4b = lmer(logRT ~ logstsratioC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC+ logcountsC||subject) + (1+ logstsratioC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logstsratioC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC+ logcountsC||subject) + (1+ logstsratioC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4b)


# effect of lsa strength by itself
model4c1 = lmer(logRT ~ logLSAC + (1+ logLSAC|subject) + (1+ logLSAC|item), data = dt, REML = FALSE)
model4c2 = lmer(logRT ~ logLSAC + (1+ logLSAC|subject) + (1|item), data = dt, REML = FALSE)
mixed(logRT ~ logLSAC + (1+ logLSAC|subject) + (1|item), data = dt, REML = FALSE, method="S")
anova(model4c1,model4c2)
summary(model4c1)

# effect of lsa strength
model4c = lmer(logRT ~ logLSAC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC+ logcountsC||subject) + (1+ logLSAC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logLSAC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC+ logcountsC||subject) + (1+ logLSAC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4c)


# effect of lsa ent by itself
model4d1 = lmer(logRT ~ loglsaentC + (1+ loglsaentC|subject) + (1+ loglsaentC|item), data = dt, REML = FALSE)
model4d2 = lmer(logRT ~ loglsaentC + (1+ loglsaentC|subject) + (1|item), data = dt, REML = FALSE)
mixed(logRT ~ loglsaentC + (1+ loglsaentC|subject) + (1+ loglsaentC|item), data = dt, REML = FALSE, method="S")
anova(model4d1, model4d2)
summary(model4d1)

# effect of lsa ent
model4d = lmer(logRT ~ loglsaentC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC+ logcountsC||subject) + (1+ loglsaentC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ loglsaentC + cohortsizeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC+ logcountsC||subject) + (1+ loglsaentC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4d)


####
# Examining why no effects of the four vars when combined with new variables
# Look at correlation matrix
####

corvars = c("logagreeC","logstsratioC","logLSAC","loglsaentC","cohortsizeC", "logcountsC", "loglexespC", "slenC", "scohortC")
cormat = round(cor(dt[,corvars]),2)
cormat # note high correlations between existing vars and response counts and cohort size


####
# Leaving out Cohort Size
####

# effect of cloze-prob, leaving out cohort size
model4a = lmer(logRT ~ logagreeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC+ logcountsC||subject) + (1+ logagreeC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logagreeC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC+ logcountsC||subject) + (1+ logagreeC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4a)

# effect of response ratio, leaving out cohort size
model4b = lmer(logRT ~ logstsratioC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC+ logcountsC||subject) + (1+ logstsratioC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logstsratioC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC+ logcountsC||subject) + (1+ logstsratioC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4b)

# effect of lsa strength, leaving out cohort size
model4c = lmer(logRT ~ logLSAC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC+ logcountsC||subject) + (1+ logLSAC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logLSAC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC+ logcountsC||subject) + (1+ logLSAC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4c)

# effect of lsa ent, leaving out cohort size
model4d = lmer(logRT ~ loglsaentC  + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC+ logcountsC||subject) + (1+ loglsaentC + logcountsC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ loglsaentC + logcountsC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC+ logcountsC||subject) + (1+ loglsaentC + logcountsC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4d)

####
# Leaving out response counts
####

# effect of cloze-prob, leaving out response counts
model4a = lmer(logRT ~ logagreeC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC||subject) + (1+ logagreeC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logagreeC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logagreeC|subject) + (1+ logagreeC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4a)

# effect of response ratio, leaving out response counts
model4b = lmer(logRT ~ logstsratioC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC||subject) + (1+ logstsratioC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logstsratioC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logstsratioC||subject) + (1+ logstsratioC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4b)

# effect of lsa strength, leaving out response counts
model4c = lmer(logRT ~ logLSAC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC||subject) + (1+ logLSAC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ logLSAC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ logLSAC||subject) + (1+ logLSAC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4c)

# effect of lsa ent, leaving out response counts
model4d = lmer(logRT ~ loglsaentC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC||subject) + (1+ loglsaentC + num_phonC ||item), data = dt, REML = FALSE)
mixed(logRT ~ loglsaentC + cohortsizeC + loglexespC + slenC + scohortC + init_phonC + (1+ loglsaentC||subject) + (1+ loglsaentC + num_phonC ||item), data = dt, REML = FALSE, method="S")
summary(model4d)

