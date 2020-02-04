library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(caret)
library(dvmisc)
library(rsq)
library(boot)
library(fastDummies)


names(d) %>% tail


#import
p   <- 'C:/Users/lucas/Documents/MLapp-yhat-challenge/'
f1  <- paste0(p,'1data/nlsy training set')
f2  <- paste0(p,'1data/nlsy test set'    )
f1J <- paste0(p,'1data/train_clean_Juan.csv')
f2J <- paste0(p,'1data/test_clean_Juan.csv' )
fo1 <- paste0(p,'1data/train_clean.csv')
fo2 <- paste0(p,'1data/test_clean.csv' )
l1  <- paste0(p,'1data/labelvars.do')
l2  <- paste0(p,'1data/Variable info.xlsx')
fdic <- paste0(p,'dicionary.csv')


#making variable dictionary
# l <- fread(l1,header = F)[,.(V3,V4)]
#  l %>% setnames('V3','var_orig')
#  l %>% setnames('V4','varlabel')
# l[,varname:=''] 
# l[,vartype:='']      # n = numeric, c= cathegorical
# l[,varcreated:='']
# l[,varformula:='']
# l[,varyear:=(str_extract(varlabel,'\\d{4}') %>% as.numeric)]
# l[,varlabel_aux:=(varlabel %>% str_replace('\\d+','') %>% str_replace('\\d+',''))]
# l <- l[,.(varname,var_orig,vartype,varcreated,varformula,varyear,varlabel,varlabel_aux)]
# 
# l2 <- read_excel(l2,sheet = 'Raw Names') %>% data.table
# l2[,var_orig:= str_remove(VarCode,'\\.')]
# l3 <- l[l2,on='var_orig']
# l3[varyear!=SurveyYear] %>% View
# l3[str_detect(varlabel,'INCOME_GROSS') ,vartype:='n']
# l3[str_detect(varlabel,'INCOME_FAMILY'),vartype:='n']
# l3[str_detect(varlabel,'NET_WORTH')    ,vartype:='n']
# l3 %>% fwrite(fdic)
dic <- fread(fdic) %>% .[varname!='inc_mean_fam']


d <- fread(f1)
T <- fread(f2)


#Encoding NAs 
encode_NA <- function(x) x %>% na_if(-5) %>% na_if(-4) %>% na_if(-3) %>% na_if(-2) %>% na_if(-1)
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

d <- d[, lapply(.SD, encode_NA)]
T <- T[, lapply(.SD, encode_NA)]


#Harmonize variable names
d %>% setnames(dic[varname!='']$var_orig,dic[varname!='']$varname)
T %>% setnames(dic[varname!='']$var_orig,dic[varname!='']$varname,skip_absent=TRUE)


#Drop missings in drinks
d <- d[!is.na(drinks)]


#new variables
d[,female:=(gender==2) %>% as.numeric]
T[,female:=(gender==2) %>% as.numeric]

d[,age:=2015-birthyear]
T[,age:=2015-birthyear]
  d[,.(drinks,inc_fam1997)][, lapply(.SD, encode_NA)][,.N,inc_fam1997][order(-N)]

#family background income:
d[age==31, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1997','inc_fam1998','inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003')]
d[age==32, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1998','inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004')]
d[age==33, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005')]
d[age==34, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005','inc_fam2006')]
d[age==35, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005','inc_fam2006','inc_fam2007')]
d[,inc_fam_16t22_MI:=inc_fam_16t22 %>% is.na %>% as.numeric]
d[,inc_fam_16t22   :=inc_fam_16t22 %>% NA2mean]
  #d[,.N,inc_fam_16t22][order(-N)]
  #d[,.N,inc_fam_16t22_MI]
  #d[age==31, .(inc_fam_16t22,inc_fam_16t22_MI,inc_fam1997,inc_fam1998,inc_fam1999,inc_fam2000,inc_fam2001,inc_fam2002,inc_fam2003)] %>% View


T[age==31, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1997','inc_fam1998','inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003')]
T[age==32, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1998','inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004')]
T[age==33, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam1999','inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005')]
T[age==34, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2000','inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005','inc_fam2006')]
T[age==35, inc_fam_16t22:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2001','inc_fam2002','inc_fam2003','inc_fam2004','inc_fam2005','inc_fam2006','inc_fam2007')]
T[,inc_fam_16t22_MI:=inc_fam_16t22 %>% is.na %>% as.numeric]
T[,inc_fam_16t22   :=inc_fam_16t22 %>% NA2mean]



#income last 5 years
d[, inc_fam_2010t2015:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2010','inc_fam2011','inc_fam2013','inc_fam2015')]
d[,inc_fam_2010t2015_MI:=inc_fam_2010t2015 %>% is.na %>% as.numeric]
d[,inc_fam_2010t2015   :=inc_fam_2010t2015 %>% NA2mean]
  #d[,.N,inc_fam_2010t2015][order(-N)]
  #d[,.N,inc_fam_2010t2015_MI]
  #d[,.(inc_fam_2010t2015,inc_fam_2010t2015_MI,inc_fam2010,inc_fam2011,inc_fam2013,inc_fam2015)]

T[, inc_fam_2010t2015:=rowMeans(.SD,na.rm = TRUE), .SDcols = c('inc_fam2010','inc_fam2011','inc_fam2013','inc_fam2015')]
T[,inc_fam_2010t2015_MI:=inc_fam_2010t2015 %>% is.na %>% as.numeric]
T[,inc_fam_2010t2015   :=inc_fam_2010t2015 %>% NA2mean]






#Integrating Juan's data:

dJ <- fread(f1J)
dJ[,V1:=NULL]
dJ <- dJ[health_status_2015>=0]
names(dJ)
dJ[,.N,ave_pov_2010t2015][order(-N)]
dJ[,ave_pov_2010t2015_MI:=ave_pov_2010t2015 %>% is.na %>% as.numeric]
dJ[,ave_pov_2010t2015   :=ave_pov_2010t2015 %>% NA2mean]


TJ <- fread(f2J)
TJ[,V1:=NULL]
names(TJ)
TJ[,.N,ave_pov_2010t2015][order(-N)]
TJ[,ave_pov_2010t2015_MI:=ave_pov_2010t2015 %>% is.na %>% as.numeric]
TJ[,ave_pov_2010t2015   :=ave_pov_2010t2015 %>% NA2mean]



dF <- d[,.(diag.id,drinks,age,female,race,inc_fam_16t22, inc_fam_16t22_MI, inc_fam_2010t2015, inc_fam_2010t2015_MI)]
TF <- T[,.(diag.id,       age,female,race,inc_fam_16t22, inc_fam_16t22_MI, inc_fam_2010t2015, inc_fam_2010t2015_MI)]

dF <- dF[dJ,on='diag.id']
TF <- TF[TJ,on='diag.id']



#Exporting clean version
dF %>% fwrite(fo1)
TF %>% fwrite(fo2)




fill.NAs
model.matrix

names(d)
d[,.N,drinks][order(-N)]
d[,.N,birthyear]
d[,.N,age]
d[,.N,race]
d[,.N,gender]
d[,.N,female]


d[drinks>0] %>% lm(drinks ~ race + gender + age,.) %>% summary %>% .$adj.r.squared



control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


dF <- dF %>% dummy_cols(select_columns=c('health_status_2015'))

dF %>% lm(drinks ~ 
            age + factor(race) + female + 
            inc_fam_16t22 + inc_fam_16t22_MI + inc_fam_2010t2015 + inc_fam_2010t2015_MI + 
            ave_hh_size_2010t2015 + ave_pov_2010t2015 + ave_totwork_h + 
            kids6_2015 + ave_kids6 + kids6_mi + nkids_mi + factor(marriage_status) +
            factor(health_status_2015),.) %>% 
  summary %>% .$adj.r.squared

mse <- function(x) mean(x^2)
dF %>% lm(drinks ~ 
            age + factor(race) + female + 
            inc_fam_16t22 + inc_fam_16t22_MI + inc_fam_2010t2015 + inc_fam_2010t2015_MI + 
            ave_hh_size_2010t2015 + ave_pov_2010t2015 + ave_totwork_h + 
            kids6_2015 + ave_kids6 + kids6_mi + nkids_mi + factor(marriage_status) +
            factor(health_status_2015),.) %>% 
  get_mse( var.estimate = FALSE)
  summary %>% .$residuals %>% mse
  

dT[health_status_2015==99,health_status_2015]   
poisson.model <- dF %>% 
      glm(drinks ~ 
            age + factor(race) + female + 
            inc_fam_16t22 + inc_fam_16t22_MI + inc_fam_2010t2015 + inc_fam_2010t2015_MI + 
            ave_hh_size_2010t2015 + ave_pov_2010t2015 + ave_totwork_h + 
            kids6_2015 + ave_kids6 + kids6_mi + nkids_mi + factor(marriage_status) +
            health_status_2015_3 + health_status_2015_2 + health_status_2015_1 +
            health_status_2015_4 + health_status_2015_5 + health_status_2015_99,
          .,family="poisson")
poisson.model %>% rsq(adj=TRUE)
poisson.model %>% get_mse( var.estimate = FALSE)

cv.err.10 <- dF %>% cv.glm(poisson.model,K=10)

muhat <- poisson.model$fitted
poisson.diag <- glm.diag(poisson.model)
mean((poisson.model$y-muhat)^2/(1-poisson.diag$h)^2)

cv.err <- dF %>% cv.glm(poisson.model,K=10)
cv.err$delta


  
summary %>% .$residuals %>% mse


install.packages('rsq')


glm(Monarchs ~ Garden,
    data=Data,
    family="poisson")
glm


