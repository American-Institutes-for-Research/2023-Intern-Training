############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
# to load the package	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat",	
                            package = "NAEPprimer"))	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", 	
                            package="NAEPprimer"))	
	
showCutPoints(sdf)	
############################################### Slide 11	
ach <- achievementLevels("composite", data = sdf, 	
                         returnCumulative = TRUE)	
ach	
############################################### Slide 12	
ach$discrete	
ach$cumulative	
  achievementLevels("composite", data = sdf, 	
                    returnCumulative = TRUE, 	
                    cutpoints = c(250,300,350))	
############################################### Slide 13	
ach1 <- achievementLevels(c("composite", "dsex"), data = sdf)	
ach1$discrete	
############################################### Slide 14	
ach2 <- achievementLevels(c("composite", "dsex"), 	
                          aggregateBy = "dsex", data = sdf)	
ach2$discrete	
############################################### Slide 15	
ach3 <- achievementLevels(c("composite", "dsex"), 	
                          aggregateBy = "composite", data = sdf)	
ach3$discrete	
############################################### Slide 16	
dsex_iep <- achievementLevels(c("composite", "dsex", "iep"),	
                              aggregateBy = c("dsex", "iep"), 	
                              data = sdf)	
searchSDF("dsex",data = sdf, levels = TRUE)	
searchSDF("iep",data = sdf, levels = TRUE)	
############################################### Slide 17	
dsex_iep$discrete	
############################################### Slide 18	
############################################### Slide 19	
# for reference	
help(package = "EdSurvey")	
searchSDF("text", sdf)	
levelsSDF("myvar", sdf)	
############################################### Slide 20	
exerciseAL <- achievementLevels(c("composite", "b018201"), 	
                                aggregateBy = c("b018201"),	
                                data = sdf, returnCumulative = TRUE)	
exerciseAL$cumulative	
############################################### Slide 21	
############################################### Slide 22	
per <- percentile("composite", percentiles = c(25,50,75), data = sdf)	
per	
############################################### Slide 23	
############################################### Slide 24	
percentile("composite", percentiles = c(10,25,50,75,90),	
                           weightVar = "origwt", data = sdf)	
# bonus, using dplyr	
library(dplyr)	
lsdf <- sdf %>% getData(varnames = c("composite", "sdracem", "origwt"), 	
                        addAttributes = TRUE) 	
sapply(levels(lsdf$sdracem), function(x){	
  percentile(data = lsdf[lsdf$sdracem %in% x,], "composite",	
             percentiles = c(10,25,50,75,90), 	
             weightVar = "origwt")$estimate})	
############################################### Slide 25	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
############################################### Slide 29	
mathGap <- gap(variable = "composite", data = sdf,	
               groupA = dsex %in% "Male", 	
               groupB = dsex %in% "Female")	
mathGap	
############################################### Slide 30	
############################################### Slide 31	
# for reference	
help(package = "EdSurvey")	
searchSDF("text", sdf)	
levelsSDF("myvar", sdf)	
############################################### Slide 32	
exerciseGap <- gap(variable = "composite", data = sdf,	
                  groupA = ell3 %in% "Yes", groupB = ell3 %in% "No",	
                   achievementLevel = c("Proficient"))	
exerciseGap	
############################################### Slide 33	
############################################### Slide 34	
############################################### Slide 35	
############################################### Slide 36	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
lm1 <- lm.sdf(composite ~ b017451,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 37	
lm2 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt', data = sdf)	
############################################### Slide 38	
summary(lm2)	
############################################### Slide 39	
summary(lm2, src = TRUE)	
############################################### Slide 40	
lm3 <- lm.sdf(composite ~ dsex + b017451, 	
              weightVar = 'origwt',	
              relevels = list(dsex = "Female"), data = sdf)	
############################################### Slide 41	
summary(lm3)	
############################################### Slide 42	
############################################### Slide 43	
lmexercise2 <- lm.sdf(composite ~ b017101 + b018201,	
                      weightVar = 'origwt', data = sdf)	
summary(lmexercise2)	
############################################### Slide 44	
############################################### Slide 45	
############################################### Slide 46	
mmlA <- mml.sdf(algebra ~ dsex, data=sdf, weightVar="origwt")	
############################################### Slide 47	
############################################### Slide 48	
summary(mmlA)	
############################################### Slide 49	
############################################### Slide 50	
logit1 <- logit.sdf(I(b013801 %in% ">100") ~ dsex,	
                    weightVar = 'origwt', data = sdf)	
############################################### Slide 51	
summary(logit1)	
############################################### Slide 52	
oddsRatio(logit1)	
############################################### Slide 53	
############################################### Slide 54	
logitexercise1 <- logit.sdf(I(lep %in% "Yes") ~ b018201,	
                          weightVar = 'origwt', data = sdf)	
summary(logitexercise1)	
############################################### Slide 55	
############################################### Slide 56	
vignette("introduction", package="EdSurvey")	
	
# There are additional functions that we couldn't cover!	
drawPVs() #Generates PVs based on the mml.sdf conditioning model	
cor.sdf() # Bivariate correlations using "Pearson", "Spearman", "polychoric", or "polyserial" methods	
edsurveyTable2pdf() # creating production ready summary tables	
cbind(), rbind(), append(), merge() # useful functions in processing data	
help(package = "EdSurvey")	
############################################### Slide 57	
