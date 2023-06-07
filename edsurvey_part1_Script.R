############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
#install EdSurvey	
# you may need to get rtools	
install.packages("EdSurvey")	
devtools::install_github("American-Institutes-for-Research/tidyEdSurvey")	
# to load the package	
library(EdSurvey)	
library(tidyEdSurvey)	
library(dplyr)	
############################################### Slide 13	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 14	
############################################### Slide 15	
############################################### Slide 16	
############################################### Slide 17	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
############################################### Slide 18	
math17 <- readNAEP("//path_to_directory/Data/M48NT2AT.dat")	
############################################### Slide 19	
############################################### Slide 20	
############################################### Slide 21	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
############################################### Slide 25	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
show(sdf)	
############################################### Slide 29	
dim(sdf)	
############################################### Slide 30	
colnames(sdf)	
############################################### Slide 31	
searchSDF("education", sdf)	
searchSDF("b003501", sdf, levels = TRUE)	
############################################### Slide 32	
levelsSDF("b018201", sdf)	
############################################### Slide 33	
View(showCodebook(sdf))	
############################################### Slide 34	
table(sdf$b003501)	
sdf$mother_hs_grad <- ifelse(sdf$b003501 %in% 	
                               c("Graduated H.S.","Some ed after H.S."),1,0)	
table(sdf$mother_hs_grad)	
############################################### Slide 35	
showPlausibleValues(sdf)	
showPlausibleValues(sdf, verbose = TRUE)	
############################################### Slide 36	
showWeights(sdf)	
showWeights(sdf, verbose = TRUE)	
############################################### Slide 37	
############################################### Slide 38	
############################################### Slide 39	
############################################### Slide 40	
############################################### Slide 41	
############################################### Slide 42	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 43	
# Note: head returns the first 6 rows of a data frame	
head(gddat)	
############################################### Slide 44	
gddat <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 45	
subsetSDF <- subset(sdf, dsex %in% c("Male"))	
dim(sdf)	
dim(subsetSDF)	
############################################### Slide 46	
sdf2 <- recode.sdf(sdf, recode =	
                     list(b017451 = list(from = c("Never or hardly ever", "Once every few weeks"),	
                                         to = c("Infrequently")),	
                          b017451 = list(from = c("Every day"),	
                                        to = c("Frequently")))	
                   )	
searchSDF("b017451", sdf2, levels = TRUE)	
############################################### Slide 47	
sdf2 <- rename.sdf(sdf2, oldnames = "b017451",	
                   newnames = "studytalkfrequency")	
searchSDF("studytalkfrequency", sdf2, levels = TRUE)	
############################################### Slide 48	
############################################### Slide 49	
############################################### Slide 50	
############################################### Slide 51	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
summary2(sdf, "composite")	
############################################### Slide 52	
summary2(sdf, "composite", weightVar = NULL)	
############################################### Slide 53	
summary2(sdf, "b017451")	
############################################### Slide 54	
summary2(sdf, "b017451", omittedLevels = TRUE)	
############################################### Slide 55	
############################################### Slide 56	
es1 <- edsurveyTable(composite ~ dsex + b017451, data = sdf)	
es1$data	
############################################### Slide 57	
es2 <- edsurveyTable(composite ~ dsex + b017451, data = sdf, pctAggregationLevel = 0)	
es2	
############################################### Slide 58	
############################################### Slide 59	
edexercise <- edsurveyTable(composite ~ iep + b013801,	
                            weightVar = 'origwt', data = sdf)	
edexercise	
############################################### Slide 60	
############################################### Slide 61	
attach(sdf)	
sdf %>% 	
  group_by(sdracem) %>%	
  summarise(avg1 = mean(mrpcm1,na.rm=TRUE),	
            avg2 = mean(mrpcm2,na.rm=TRUE),	
            avg3 = mean(mrpcm3,na.rm=TRUE),	
            avg4 = mean(mrpcm4,na.rm=TRUE),	
            avg5 = mean(mrpcm5,na.rm=TRUE),	
            achievement = (avg1+avg2+avg3+avg4+avg5)/5) %>%	
  select(sdracem,achievement)	
	
############################################### Slide 62	
sdf %>%	
  mutate(sdracem = case_match(as.character(sdracem), 	
                              "Asian/Pacific Island" ~ "As/PacIs", 	
             "Amer Ind/Alaska Natv" ~ "AmInd/AkNat", 	
             .default = as.character(sdracem))) %>%	
  ggplot(.,aes(y=sdracem,x=mrpcm1,fill=sdracem,	
               alpha=0.6)) +	
  geom_violin(show.legend = FALSE,draw_quantiles = 0.5) +	
  theme(axis.title.y = element_blank()) +	
  xlab("1st Plausible Value")	
  	
############################################### Slide 63	
############################################### Slide 64	
	
sdf %>% 	
    mutate(prof = case_when(	
                  between(mrpcm1, 0, 261.99) ~ "Below Basic",	
                  between(mrpcm1, 262, 298.99) ~ "Basic",	
                  between(mrpcm1, 299, 332.99) ~ "Proficient",	
                  between(mrpcm1, 333, 1e4) ~ "Advanced")) %>%	
	
    ggplot(.,aes(x=mrpcm1,fill=prof,color=prof)) +	
      facet_wrap(vars(sdracem),scales="free_y") +	
      geom_density(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..],alpha=0.6),	
               position="stack",bw=2,show.legend = FALSE) +	
      scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090","#ff6361"),	
                    guide = guide_none()) +	
      scale_colour_manual(values = c("#003f5c", "#58508d", "#bc5090","#ff6361"), 	
                      guide = guide_none()) +	
      theme_minimal() +	
      theme(axis.title.x = element_blank(),	
            axis.title.y = element_blank(),	
            axis.text.y = element_blank()) +	
      geom_vline(xintercept = c(262,299,333),	
                 color=rep(c("#58508d","#ff6361","#003f5c"),6),	
                 linewidth=1.1) +	
      annotate(geom = "text",label = "Basic",x = 262,y = 0.003,angle = 90,vjust = 1) +	
      annotate(geom = "text",label = "Proficient",x = 299,y = 0.003,angle = 90,vjust = 1) +	
      annotate(geom = "text",label = "Advanced",x = 333,y = 0.003,angle = 90,vjust = 1)	
	
############################################### Slide 65	
