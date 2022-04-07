### Data summaries and visualizations 
# A. Clason & Ingrid Farnell
# March, 2022

#libraries
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.,
ls <- append(ls,c("ggplot2","ggarrange"))
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

#Read in data that went into the Random Forest analysis:
ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn")
datPath <- "./Inputs/" #"C:/Users/farne/Documents/"

Chutanli <- fread(paste0(datPath,"G41607dat270.csv"))
Chutanli <- Chutanli %>%
  mutate_at((colnames(Chutanli)[colnames(Chutanli) %in% ctg_variables]), factor) %>%
  dplyr::select(-"V1") #can remove if you remembered to save without row.names
Chutanli[,dNBR := dNBR*1000]

Tezzeron <- fread(paste0(datPath,"G51632dat270.csv"))
Tezzeron <- Tezzeron %>%
  mutate_at((colnames(Tezzeron)[colnames(Tezzeron) %in% ctg_variables]), factor)%>%
  dplyr::select(-"V1")
Tezzeron[,dNBR := dNBR*1000]

Shovel <- fread(paste0(datPath,"R11498dat270.csv"))
Shovel <- Shovel %>%
  mutate_at((colnames(Shovel)[colnames(Shovel) %in% ctg_variables]), factor)%>%
  dplyr::select(-"V1")
Shovel[,dNBR := dNBR*1000]

Verdun <- fread(paste0(datPath,"R11796dat270.csv"))
Verdun <- Verdun %>%
  mutate_at((colnames(Verdun)[colnames(Verdun) %in% ctg_variables]), factor)%>%
  dplyr::select(-"V1")
Verdun[,dNBR := dNBR*1000]

Island <- fread(paste0(datPath,"R11921dat270.csv"))
Island <- Island %>%
  mutate_at((colnames(Island)[colnames(Island) %in% ctg_variables]), factor)%>%
  dplyr::select(-"V1")
Island[,dNBR := dNBR*1000]

Nadina <- fread(paste0(datPath,"R21721dat270.csv"))
Nadina <- Nadina %>%
  mutate_at((colnames(Nadina)[colnames(Nadina) %in% ctg_variables]), factor)%>%
  dplyr::select(-"V1")
Nadina[,dNBR := dNBR*1000]

Chutanli_sf = sf::st_as_sf(Chutanli, coords = c("x", "y"))
Tezzeron_sf = sf::st_as_sf(Tezzeron, coords = c("x", "y"))
Shovel_sf = sf::st_as_sf(Shovel, coords = c("x", "y"))
Verdun_sf = sf::st_as_sf(Verdun, coords = c("x", "y"))
Island_sf = sf::st_as_sf(Island, coords = c("x", "y"))
Nadina_sf = sf::st_as_sf(Nadina, coords = c("x", "y"))

#Important covariates in Nadina
a <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")

b <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PineCov))+
  geom_smooth(aes(y=dNBR,x=PineCov),method="gam")

c <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=BASAL_AREA))+
  geom_smooth(aes(y=dNBR,x=BASAL_AREA),method="gam")

d <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=maxT))+
  geom_smooth(aes(y=dNBR,x=maxT),method="gam")

e <- ggplot(Nadina)+
  geom_boxplot(aes(y=dNBR,x=Spaced))

f <- ggplot(Nadina)+
  geom_boxplot(aes(y=dNBR,x=DebrisPiled))

ggarrange(a,b,c,d,e,f)

## Shovel
a <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=CROWN_CLOS))+
  geom_smooth(aes(y=dNBR,x=CROWN_CLOS),method="gam")

b <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=isi))+
  geom_smooth(aes(y=dNBR,x=isi),method="gam")

c <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=HistoricFires))+
  geom_smooth(aes(y=dNBR,x=HistoricFires),method="gam")

d <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=SpruceCov))+
  geom_smooth(aes(y=dNBR,x=SpruceCov),method="gam")

e <- ggplot(Shovel)+
  geom_boxplot(aes(y=dNBR,x=BroadBurn))

f <- ggplot(Shovel)+
  geom_boxplot(aes(y=dNBR,x=DebrisMade))

ggarrange(a,b,c,d,e,f)


#plantation Age for all fires:
a <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Nadina")

b <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Shovel")

c <- ggplot(Island)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Island")

d <- ggplot(Verdun)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Verdun")

e <- ggplot(Tezzeron)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Tezzeron")

f <- ggplot(Chutanli)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Chutanli")

ggarrange(a,b,c,d,e,f)



####Running some simplified versions of the models to get our heads around the right approach

#--- DALEX
#simplified:
dat <- as.data.table(Chutanli_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]

task = TaskClassif$new("Chutanli", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Chutanli - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Chutanli_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
#dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]


task = TaskClassif$new("Chutanli", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Chutanli - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Chutanli_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Chutanli", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Chutanli",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")

########################################################################################################
#simplified:
dat <- as.data.table(Shovel_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]

task = TaskClassif$new("Shovel", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Shovel - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Shovel_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
#dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]


task = TaskClassif$new("Shovel", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Shovel - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Shovel_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Shovel", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Shovel",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")
########################################################################################################
#simplified:
dat <- as.data.table(Nadina_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,"UnburnLow",
                               ifelse(dNBRCAT==2,"UnburnLow","Burned")))]

task = TaskClassif$new("Nadina", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",
              predict_type = "prob",
              num.threads=20)
learner$train(task)

model = Predictor$new(learner, data = dat, y = "dNBRCAT")

x.interest = data.frame(x[1,])  
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)

train_set = sample(task$nrow, 0.8 * task$nrow) # train on 80% of data
test_set = setdiff(seq_len(task$nrow), train_set)

learner$predict_type = "prob"
# re-fit the model
learner$train(task, row_ids = train_set)
# rebuild prediction object
prediction = learner$predict(task, row_ids = test_set)
as.data.table(prediction)
autoplot(prediction)
model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(dat$dNBRCAT),
                         label = "Nadina - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = TRUE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Probability") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Nadina_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]

task = TaskClassif$new("Nadina", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model = Predictor$new(learner, data = dat, y = "dNBRCAT")

x.interest = data.frame(x[1,])  
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Nadina - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Nadina_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Nadina", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Nadina",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")

####continuous no plantation age
dat <- as.data.table(Nadina_sf_con)
dat <- dat[,-c("PlantAge","geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Nadina", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Nadina",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")
########################################################################################################

#simplified:
dat <- as.data.table(Verdun_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]

task = TaskClassif$new("Verdun", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Verdun - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Verdun_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
#dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]


task = TaskClassif$new("Verdun", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Verdun - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Verdun_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Verdun", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Verdun",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")
########################################################################################################

#simplified:
dat <- as.data.table(Island_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]

task = TaskClassif$new("Island", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Island - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Island_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
#dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]


task = TaskClassif$new("Island", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Island - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Island_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Island", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Island",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")
########################################################################################################

#simplified:
dat <- as.data.table(Tezzeron_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]

task = TaskClassif$new("Tezzeron", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Tezzeron - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


#simplified:
dat <- as.data.table(Tezzeron_sf_cat)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBRCAT")]
#dat[,dNBRCAT:=as.factor(ifelse(dNBRCAT==1,0,1))]


task = TaskClassif$new("Tezzeron", backend = dat, target = "dNBRCAT")
learner = lrn("classif.ranger",predict_type = "prob")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = as.numeric(as.character(dat$dNBRCAT)),
                         label = "Tezzeron - cat",
                         colorize = FALSE,
                         verbose=TRUE)

# Dataset level exploration
# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
head(model_vi)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")


####continuous
dat <- as.data.table(Tezzeron_sf_con)
dat <- dat[,-c("geometry")]
x <- dat[,-c("dNBR")]

task = TaskRegr$new("Tezzeron", backend = dat, target = "dNBR")
learner = lrn("regr.ranger")
learner$train(task)

model_exp = explain_mlr3(learner,
                         data = x, # provide data without y 
                         y = dat$dNBR,
                         label = "Tezzeron",
                         colorize = FALSE,
                         verbose=TRUE)

# Importance of variables - permutation based importance
model_vi = model_parts(model_exp)
plot(model_vi, show_boxplots = FALSE)

# Partial dependence plots of top variables
selected_variables =c("PlantAge", "PineCov", "CROWN_CLOS", "BASAL_AREA", "SpruceCov", "conifCov", "decidCov", "Brushed")
pd = model_profile(model_exp, 
                   variables = selected_variables)$agr_profiles
pd
plot(pd) +
  scale_y_continuous("Estimated dNBR") +
  ggtitle("Partial Dependence profiles for selected variables")
########################################################################################################

########################################################################################################

library(ggpubr)
a <- ggplot(data=Chutanli, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  xlab(NULL)+
  ggtitle("Chutlani")

b <- ggplot(data=Shovel, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Shovel")

c <- ggplot(data=Nadina, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Nadina")

d <- ggplot(data=Island, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ggtitle("Island")

e <- ggplot(data=Verdun, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Verdun")

f <- ggplot(data=Tezzeron, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Tezzeron")

ggarrange(a,b,c,d,e,f)


a <- ggplot(data=Chutanli, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="gam")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  xlab(NULL)+
  ggtitle("Chutlani")

b <- ggplot(data=Shovel, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Shovel")

c <- ggplot(data=Nadina, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Nadina")

d <- ggplot(data=Island, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ggtitle("Island")

e <- ggplot(data=Verdun, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="gam")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Verdun")

f <- ggplot(data=Tezzeron, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Tezzeron")

ggarrange(a,b,c,d,e,f)







