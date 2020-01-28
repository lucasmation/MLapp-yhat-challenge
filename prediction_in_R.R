library(tidyverse)
library(stringr)
library(readxl)
library(data.table)
library(caret)


library(reticulate)

#import
p <- 'C:/Users/lucas/Documents/MLapp-yhat-challenge/'
f1 <- paste0(p,'1data/nlsy training set')
f2 <- paste0(p,'1data/nlsy test set')
l1 <- paste0(p,'1data/labelvars.do')
l2 <- paste0(p,'1data/Variable info.xlsx')
fdic <- paste0(p,'dicionary.csv')

d <- fread(f1)
T <- fread(f2)

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
dic <- fread(fdic)

#now export and add variable types by hand


d[,.N,R1204600][order(R1204600)] # %>% View

la var R1204500 "CV_INCOME_GROSS_YR 1997"

names(l3)

dictionary
var_orig, name, label, var2016, frequency (y, m , atemporal), type (c,n,i), created, formula
 
names(d)
d %>% setnames('U1031900','drinks')
d %>% setnames('R0536300','gender')
d %>% setnames('R1482600','race')
d %>% setnames('R0536402','age')
d[,.N,drinks]
d[,.N,age]
d[,.N,race]
d[,.N,gender]

d[drinks>0] %>% lm(drinks ~ race + gender + age,.) %>% summary %>% .$adj.r.squared


[10:03 AM, 1/28/2020] Juan Harris MCapp: R0536402
[10:03 AM, 1/28/2020] Juan Harris MCapp: gender
[10:03 AM, 1/28/2020] Juan Harris MCapp: Ethinicity
[10:04 AM, 1/28/2020] Juan Harris MCapp: 'R1482600'
[10:04 AM, 1/28/2020] Juan Harris MCapp: 



d %>% str %>% View

d$drinks
d$E5031701
d$R0070400
d[,.N,(drinks<0)]
d[,.N,(E5031701)][order(-N)]
d[,.N,(E5031702)][order(-N)]

d[drinks>=0 & R0070400>=0,.(drinks,R0070400)] %>% cor()
T$U1031900
U1031900 (# DRINKS PER DAY LAST 30DAYS).

mepsid patient

YEAR
MEPSID 
EXPTOT
expot
exptot
T1[,ln_exptot:=log(exptot)]

T %>% ggplot(aes(exptot)) + geom_density()
T1 %>% ggplot(aes(exptot)) + geom_density() + scale_x_log10()
T1 %>% ggplot(aes(ln_exptot)) + geom_density()
T1[,.N,exptot==0]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
