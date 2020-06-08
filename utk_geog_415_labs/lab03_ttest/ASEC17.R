library(tidyverse)
data_url <- 'http://thedataweb.rm.census.gov/pub/cps/march/asec2017_pubuse.dat.gz'
data_dir <- '~/Dropbox/classes/Geog415/geog415_s18/data/cps'
data_file <- file.path(data_dir, 'asec2017.dat.gz')
if(!dir.exists(data_dir)) dir.create(data_dir)
download.file(url = data_url, destfile = data_file)
#R.utils::gunzip(data_file, file.path(data_dir, 'asec2017.dat'))

Pdata <- read_fwf(data_file, fwf_cols(PRECORD = c(1,1),
                             H_SEQ = c(2,6),
                             PPPOS = c(7,8),
                             A_LINENO = c(11,12),
                             A_EXPRRP = c(15,16),
			     PERRP = c(17,18),
                             A_AGE = c(19,20),
                             A_SPOUSE = c(22,23),
                             A_SEX = c(24,24),
                             A_HGA = c(25,26),
			     PRDTRACE = c(27,28),
			     PEHSPNON = c(31,31),
			     A_FAMNUM = c(33,34),
			     PF_SEQ = c(48,49),
			     PE_COHAB = c(50,51),
			     A_WKSTAT = c(202,202),
                             WSAL = c(364,370))) %>% 
  filter(PRECORD==3)

Pdata <- Pdata %>%
  mutate(A_HGA = recode(A_HGA,
                       '00' = 0,
                       '31' = 0,
                       '32' = 3,
                       '33' = 5.5,
                       '34' = 7.5,
                       '35' = 9,
                       '36' = 10,
                       '37' = 11,
                       '38' = 12,
                       '39' = 12,
                       '40' = 13,
                       '41' = 14,
                       '42' = 14,
                       '43' = 16,
                       '44' = 18,
                       '45' = 20,
                       '46' = 21,
                       .default = NA_real_)) %>%
  mutate(A_EXPRRP2 = recode(A_EXPRRP,
                      '01' = 'Ref w/relatives',
                      '02' = 'Ref w/out relatives',
                      '03' = 'Husband',
                      '04' = 'Wife',
                      .default = 'Other')) %>%
  mutate(RACE_R = recode(PRDTRACE,
			 '01' = 'White',
			 '02' = 'Black',
			 '06' = 'Black',
			 '10' = 'Black',
			 '11' = 'Black',
			 '12' = 'Black',
			 '16' = 'Black',
			 '17' = 'Black',
			 '18' = 'Black',
			 '23' = 'Black',
			 .default = 'Other')) %>%
  mutate(A_AGE = as.numeric(A_AGE)) %>%
  mutate(WSAL = as.numeric(WSAL))

Pdata %>% 
    dplyr::select(H_SEQ,
	   PF_SEQ,
	   A_LINENO,
	   A_SEX = A_SEX, 
	   A_SPOUSE = A_SPOUSE, 
	   PE_COHAB,
	   EDUC = A_HGA, 
	   RRP = A_EXPRRP2, 
	   PERRP,
	   HISPANIC = PEHSPNON,
	   RACE_R, 
	   A_AGE, 
	   A_WKSTAT,
	   WSAL) -> Pdata3


# Filter on unmarried people age 25-29.
Pdata2 <- Pdata %>% 
    filter(A_AGE >25 & A_AGE<=29) %>%
    filter(A_SPOUSE == '00') 

%>%
    filter(A_WKSTAT == "2")

Pdata3 %>% write_csv('~/Dropbox/classes/Geog415/geog415_s18/R/Lab3_ttest/cps2017.csv')

Pdata2 %>% group_by(A_SEX) %>%
    summarize(INC = mean(WSAL), EDUC = mean(A_HGA), n=n())

lm(WSAL ~ A_SEX  , data=Pdata2) %>% summary()
lm(WSAL ~ A_SEX + A_HGA +A_AGE + A_SEX*A_HGA, data=Pdata2) %>% summary()

# Find household with multiple members:
Pdata2 %>%
    group_by(H_SEQ) %>%
    mutate(n=n()) %>%
    ungroup()  %>%
    filter(n>1)



# Filter on married couples:
P_married <- Pdata %>% filter(A_SPOUSE!='00')

# For women age 25 and over, the relationship between age and education level 
Pdata %>% filter(A_SEX == "2", AGE >= 25) %>% select(AGE, EDUC) %>% summarize_all(c('mean','sd'))

# Find single, employed women, 30-34, and plot income on age 
Pop_wom <- Pdata %>% filter(A_SEX == "2", AGE>=30, SAL>0, EDUC>0, AGE<35, A_SPOUSE=='00')

Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>%
  ggplot(data=., aes(x=factor(EDUC, levels=c(10:21),labels=c(10:21)), y=(SAL/10000))) + 
  geom_boxplot()+
  scale_x_discrete(drop=FALSE)+
  labs(x='Education', y='Income ($10,000s)')+
  scale_y_continuous(limits=c(0,30))

Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>%
  ggplot(data=., aes(x=EDUC, y=SAL/10000)) + geom_point(alpha=.1)

Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>%
  ggplot(data=., aes(x=EDUC, y=log(SAL))) + geom_point(alpha=.01) + geom_smooth(method='loess')

Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>%
  ggplot(data=., aes(x=factor(EDUC, levels=c(10:21),labels=c(10:21)), y=log(SAL))) + 
  geom_boxplot(outlier.alpha=.1) +
  scale_x_discrete(drop=FALSE)+
  labs(x='Education', y='log Income')+
  scale_y_continuous(limits=c(2,15))


my_lm1 <- Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>% 
  lm(I(SAL/10000)~EDUC, data=.) 
broom::augment(my_lm1) %>%
  ggplot(aes(x=EDUC, y=.resid)) + geom_point(alpha=.1)

broom::augment(my_lm1) %>%
  ggplot(aes(x=EDUC, y=.resid)) + geom_point(alpha=.05) + scale_y_continuous(limits=c(-20,20))
broom::augment(my_lm1) %>%
  ggplot(aes(x=factor(EDUC, levels=c(10:21),labels=c(10:21)), y=.resid)) + 
  geom_boxplot() + scale_y_continuous(limits=c(-20,20))

broom::augment(my_lm1) %>% mutate(EDUC=factor(EDUC, levels=c(10:21),labels=c(10:21))) %>% 
  group_by(EDUC) %>%
  summarize(mn = mean(.resid), sd = sd(.resid)) %>%
  ggplot(aes(x=EDUC, y=mn)) + 
  geom_errorbar(aes(ymin=mn-2*sd, ymax=mn+2*sd), width=0) +
  geom_errorbar(aes(ymin=mn-.5*sd, ymax=mn+.5*sd), width=0, color='white') +
  geom_point() + 
  scale_x_discrete(drop=FALSE)
  



my_lm2 <- Pop_wom %>% mutate(EDUC = ifelse(EDUC<12, 10, EDUC)) %>% 
  lm(log(SAL)~EDUC, data=.)
broom::augment(my_lm2)  %>% 
  ggplot(aes(x=EDUC, y=.resid)) + geom_point(alpha=.1)
  

broom::augment(my_lm2) %>% mutate(EDUC=factor(EDUC, levels=c(10:21),labels=c(10:21))) %>% 
  group_by(EDUC) %>%
  summarize(mn = mean(.resid), sd = sd(.resid)) %>%
  ggplot(aes(x=EDUC, y=mn)) + 
  geom_errorbar(aes(ymin=mn-2*sd, ymax=mn+2*sd), width=0) +
  geom_errorbar(aes(ymin=mn-.5*sd, ymax=mn+.5*sd), width=0, color='white') +
  geom_point() + 
  scale_x_discrete(drop=FALSE)

