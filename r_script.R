#Q1
factors_period1 = read.csv('F-F_Research_Data_5_Factors_mm_2x3_63-93.csv')
Rf_period1 = factors_period1$RF
Ri_period1 = read.csv('25_Portfolios_5x5_AVWR_mm_63-93.csv')[,2:26]
exret_period1 = Ri_period1 - Rf_period1
exret_period1_mean = matrix(nrow = 5, ncol = 5)
exret_period1_std_dev = matrix(nrow = 5, ncol = 5)
for (i in 1:25){
  exret_period1_mean[i] = mean(exret_period1[,i])
  exret_period1_std_dev[i] = sd(exret_period1[,i])
}
exret_period1_mean = t(exret_period1_mean)
exret_period1_std_dev = t(exret_period1_std_dev)

#Q2
library(moments)
mktexret_period1 = factors_period1$Mkt.RF
smb_period1 = factors_period1$SMB
hml_period1 = factors_period1$HML
Q2_res = matrix(nrow = 4, ncol = 5)
Q2_data = cbind(mktexret_period1,smb_period1,hml_period1)
Q2_res[1,2:5] = c("mean","std dev","skewness","excess kurtosis")
Q2_res[2:4,1] = c("mkt","smb","hml")
for (i in 1:3){
  Q2_res[i+1,2] = mean(Q2_data[,i])
  Q2_res[i+1,3] = sd(Q2_data[,i])
  Q2_res[i+1,4] = skewness(Q2_data[,i])
  Q2_res[i+1,5] = kurtosis(Q2_data[,i])-3
}

#Q3
model1_a = matrix(0,5,5)
model1_ta = matrix(0,5,5)
model1_b = matrix(0,5,5)
model1_tb = matrix(0,5,5)
model1_s = matrix(0,5,5)
model1_ts = matrix(0,5,5)
model1_h = matrix(0,5,5)
model1_th = matrix(0,5,5)
model1_Rsq = matrix(0,5,5)
for (i in 1:25){
  model1_period1 = lm(exret_period1[,i]~factors_period1$Mkt.RF+factors_period1$SMB+factors_period1$HML)
  model1_a[i] = summary(model1_period1)$coefficients[1,1]
  model1_ta[i] = summary(model1_period1)$coefficients[1,3]
  model1_b[i] = summary(model1_period1)$coefficients[2,1]
  model1_tb[i] = summary(model1_period1)$coefficients[2,3]
  model1_s[i] = summary(model1_period1)$coefficients[3,1]
  model1_ts[i] = summary(model1_period1)$coefficients[3,3]
  model1_h[i] = summary(model1_period1)$coefficients[4,1]
  model1_th[i] = summary(model1_period1)$coefficients[4,3]
  model1_Rsq[i] = summary(model1_period1)$r.squared
}
model1_a = t(model1_a)
model1_ta = t(model1_ta)
model1_b = t(model1_b)
model1_tb = t(model1_tb)
model1_s = t(model1_s)
model1_ts = t(model1_ts)
model1_h = t(model1_h)
model1_th = t(model1_th)
model1_Rsq = t(model1_Rsq)

#Q4
library(lmtest)
library(sandwich)
model1_white_ta = matrix(0,5,5)
model1_white_tb = matrix(0,5,5)
model1_white_ts = matrix(0,5,5)
model1_white_th = matrix(0,5,5)
model1_NeweyWest_ta = matrix(0,5,5)
model1_NeweyWest_tb = matrix(0,5,5)
model1_NeweyWest_ts = matrix(0,5,5)
model1_NeweyWest_th = matrix(0,5,5)
for (i in 1:25){
  #same model in Q3
  model1_period1 = lm(exret_period1[,i]~factors_period1$Mkt.RF+factors_period1$SMB+factors_period1$HML)
  
  #White heteroskedasticity
  model1_white_ta[i] = coeftest(model1_period1, vcov=vcovHC, type=c("HC"))[1,3]
  model1_white_tb[i] = coeftest(model1_period1, vcov=vcovHC, type=c("HC"))[2,3]
  model1_white_ts[i] = coeftest(model1_period1, vcov=vcovHC, type=c("HC"))[3,3]
  model1_white_th[i] = coeftest(model1_period1, vcov=vcovHC, type=c("HC"))[4,3]
  
  #NeweyWest heteroskedasticity and auto correlation (serial corr??)
  model1_NeweyWest_ta[i] = coeftest(model1_period1, vcov.=NeweyWest)[1,3]
  model1_NeweyWest_tb[i] = coeftest(model1_period1, vcov.=NeweyWest)[2,3]
  model1_NeweyWest_ts[i] = coeftest(model1_period1, vcov.=NeweyWest)[3,3]
  model1_NeweyWest_th[i] = coeftest(model1_period1, vcov.=NeweyWest)[4,3]
}
#TRUE = rej H0 = significant
model1_white_rej_ta = (t(model1_white_ta) > qnorm(0.975) | t(model1_white_ta) < qnorm(0.025))
model1_white_rej_tb = (t(model1_white_tb) > qnorm(0.975) | t(model1_white_tb) < qnorm(0.025))
model1_white_rej_ts = (t(model1_white_ts) > qnorm(0.975) | t(model1_white_ts) < qnorm(0.025))
model1_white_rej_th = (t(model1_white_th) > qnorm(0.975) | t(model1_white_th) < qnorm(0.025))

model1_NeweyWest_rej_ta = (t(model1_NeweyWest_ta) > qnorm(0.975) | t(model1_NeweyWest_ta) < qnorm(0.025))
model1_NeweyWest_rej_tb = (t(model1_NeweyWest_tb) > qnorm(0.975) | t(model1_NeweyWest_tb) < qnorm(0.025))
model1_NeweyWest_rej_ts = (t(model1_NeweyWest_ts) > qnorm(0.975) | t(model1_NeweyWest_ts) < qnorm(0.025))
model1_NeweyWest_rej_th = (t(model1_NeweyWest_th) > qnorm(0.975) | t(model1_NeweyWest_th) < qnorm(0.025))

#to compare with model1 of Q3
model1_rej_ta = (model1_ta > qnorm(0.975) | model1_ta < qnorm(0.025))
model1_rej_tb = (model1_tb > qnorm(0.975) | model1_tb < qnorm(0.025))
model1_rej_ts = (model1_ts > qnorm(0.975) | model1_ts < qnorm(0.025))
model1_rej_th = (model1_th > qnorm(0.975) | model1_th < qnorm(0.025))

#TRUE = significance not modified by heteroskedasticity = no heteroskedasticity
model1_ta_sig_white = (model1_rej_ta == model1_white_rej_ta)
model1_tb_sig_white = (model1_rej_tb == model1_white_rej_tb)
model1_ts_sig_white = (model1_rej_ts == model1_white_rej_ts)
model1_th_sig_white = (model1_rej_th == model1_white_rej_th)

#TRUE = significance not modified by heteroskedasticity and auto corr = no heteroskedasticity and auto corr
model1_ta_sig_NeweyWest = (model1_rej_ta == model1_NeweyWest_rej_ta)
model1_tb_sig_NeweyWest = (model1_rej_tb == model1_NeweyWest_rej_tb)
model1_ts_sig_NeweyWest = (model1_rej_ts == model1_NeweyWest_rej_ts)
model1_th_sig_NeweyWest = (model1_rej_th == model1_NeweyWest_rej_th)

#Q5
#D=0 in 1963-1993, D=1 in 1994-2016
D = (read.csv('25_Portfolios_5x5_AVWR_mm_63-16.csv')[,1]>="199401")
factors_period2 = read.csv('F-F_Research_Data_5_Factors_2x3_63-16.csv')[D==TRUE,]
Rf_period2 = factors_period2$RF
Ri_period2 = read.csv('25_Portfolios_5x5_AVWR_mm_63-16.csv')[D==TRUE,][,2:26]
exret_period2 = Ri_period2 - Rf_period2
mktexret_period2 = factors_period2$Mkt.RF
smb_period2 = factors_period2$SMB
hml_period2 = factors_period2$HML

model1_period2_a = matrix(0,5,5)
model1_period2_ta = matrix(0,5,5)
model1_period2_b = matrix(0,5,5)
model1_period2_tb = matrix(0,5,5)
model1_period2_s = matrix(0,5,5)
model1_period2_ts = matrix(0,5,5)
model1_period2_h = matrix(0,5,5)
model1_period2_th = matrix(0,5,5)
model1_period2_Rsq = matrix(0,5,5)
for (i in 1:25){
  model1_period2 = lm(exret_period2[,i]~factors_period2$Mkt.RF+factors_period2$SMB+factors_period2$HML)
  model1_period2_a[i] = summary(model1_period2)$coefficients[1,1]
  model1_period2_ta[i] = summary(model1_period2)$coefficients[1,3]
  model1_period2_b[i] = summary(model1_period2)$coefficients[2,1]
  model1_period2_tb[i] = summary(model1_period2)$coefficients[2,3]
  model1_period2_s[i] = summary(model1_period2)$coefficients[3,1]
  model1_period2_ts[i] = summary(model1_period2)$coefficients[3,3]
  model1_period2_h[i] = summary(model1_period2)$coefficients[4,1]
  model1_period2_th[i] = summary(model1_period2)$coefficients[4,3]
  model1_period2_Rsq[i] = summary(model1_period2)$r.squared
}
model1_period2_a = t(model1_period2_a)
model1_period2_ta = t(model1_period2_ta)
model1_period2_b = t(model1_period2_b)
model1_period2_tb = t(model1_period2_tb)
model1_period2_s = t(model1_period2_s)
model1_period2_ts = t(model1_period2_ts)
model1_period2_h = t(model1_period2_h)
model1_period2_th = t(model1_period2_th)
model1_period2_Rsq = t(model1_period2_Rsq)

# parameters' value change between two periods ? a sort of unstable
# would be best if could compare on graph..
model1_period2_a
model1_a
model1_period2_b
model1_b
model1_period2_s
model1_s
model1_period2_h
model1_h

# t tests
# TRUE = rej H0 = significant
model1_period2_rej_ta = (model1_period2_ta > qnorm(0.975) | model1_period2_ta < qnorm(0.025))
model1_period2_rej_tb = (model1_period2_tb > qnorm(0.975) | model1_period2_tb < qnorm(0.025))
model1_period2_rej_ts = (model1_period2_ts > qnorm(0.975) | model1_period2_ts < qnorm(0.025))
model1_period2_rej_th = (model1_period2_th > qnorm(0.975) | model1_period2_th < qnorm(0.025))

#to compare with model1_period1 of Q3
model1_rej_ta = (model1_ta > qnorm(0.975) | model1_ta < qnorm(0.025))
model1_rej_tb = (model1_tb > qnorm(0.975) | model1_tb < qnorm(0.025))
model1_rej_ts = (model1_ts > qnorm(0.975) | model1_ts < qnorm(0.025))
model1_rej_th = (model1_th > qnorm(0.975) | model1_th < qnorm(0.025))

#TRUE = significance not modified by period = another sort of unstable (to verify with result of Chow test later)
model1_period_ta_sig = (model1_rej_ta == model1_period2_rej_ta)
model1_period_tb_sig = (model1_rej_tb == model1_period2_rej_tb)
model1_period_ts_sig = (model1_rej_ts == model1_period2_rej_ts)
model1_period_th_sig = (model1_rej_th == model1_period2_rej_th)

#Chow test starts
chowtest_fvalue = matrix(ncol = 5, nrow = 5)
factors_wholeperiod = read.csv('F-F_Research_Data_5_Factors_2x3_63-16.csv')
Rf_wholeperiod = factors_wholeperiod$RF
Ri_wholeperiod = read.csv('25_Portfolios_5x5_AVWR_mm_63-16.csv')[,2:26]
exret_wholeperiod = Ri_wholeperiod - Rf_wholeperiod
mktexret_wholeperiod = factors_wholeperiod$Mkt.RF
smb_wholeperiod = factors_wholeperiod$SMB
hml_wholeperiod = factors_wholeperiod$HML

for (i in 1:25){
  model_unrestr = lm(exret_wholeperiod[,i]~mktexret_wholeperiod+smb_wholeperiod+hml_wholeperiod+D*mktexret_wholeperiod+D*smb_wholeperiod+D*hml_wholeperiod)
  ssru = sum(model_unrestr$residuals^2)
  
  model_period1 = lm(exret_period1[,i]~factors_period1$Mkt.RF+factors_period1$SMB+factors_period1$HML)
  model_period2 = lm(exret_period2[,i]~factors_period2$Mkt.RF+factors_period2$SMB+factors_period2$HML)
  ssru_period1=sum(model_period1$residuals^2)
  ssru_period2=sum(model_period2$residuals^2)
  ssrutotal=ssru_period1+ssru_period2 #equals to ssru
  
  model_restr = lm(exret_wholeperiod[,i]~factors_wholeperiod$Mkt.RF+factors_wholeperiod$SMB+factors_wholeperiod$HML)
  ssrr = sum(model_restr$residuals^2)
  
  nrestr = 4
  upf = (ssrr-ssrutotal)/nrestr
  lowf = ssrutotal/(length(exret_wholeperiod[,i])-length(model_unrestr$coefficients))
  chowtest_fvalue[i] = upf/lowf
  #anova(model_unrestr, model_restr)[2,5] is the same as upf/lowf !
}
chowtest_fvalue = t(chowtest_fvalue)

threshold=qf(0.95, nrestr, length(exret_wholeperiod[,25])-length(model_unrestr$coefficients))
stable = (chowtest_fvalue<=threshold)

#Q6
#D=0 in 1963-1993, D=1 in 1994-2017
D = (read.csv('25_Portfolios_5x5_AVWR_mm_63-17.csv')[,1]>="199401")
factors_period2 = read.csv('F-F_Research_Data_5_Factors_2x3_63-17.csv')[D==TRUE,]
Rf_period2 = factors_period2$RF
Ri_period2 = read.csv('25_Portfolios_5x5_AVWR_mm_63-17.csv')[D==TRUE,][,2:26]
exret_period2 = Ri_period2 - Rf_period2
mktexret_period2 = factors_period2$Mkt.RF
smb_period2 = factors_period2$SMB
hml_period2 = factors_period2$HML
rmw_period2 = factors_period2$RMW
cma_period2 = factors_period2$CMA

model2_period2_Rsq = matrix(0,5,5)
model2_period2_ta = matrix(0,5,5)
model2_period2_tb = matrix(0,5,5)
model2_period2_ts = matrix(0,5,5)
model2_period2_th = matrix(0,5,5)
model2_period2_tr = matrix(0,5,5)
model2_period2_tc = matrix(0,5,5)
model2_period2_white_ta = matrix(0,5,5)
model2_period2_white_tb = matrix(0,5,5)
model2_period2_white_ts = matrix(0,5,5)
model2_period2_white_th = matrix(0,5,5)
model2_period2_white_tr = matrix(0,5,5)
model2_period2_white_tc = matrix(0,5,5)
model2_period2_NeweyWest_ta = matrix(0,5,5)
model2_period2_NeweyWest_tb = matrix(0,5,5)
model2_period2_NeweyWest_ts = matrix(0,5,5)
model2_period2_NeweyWest_th = matrix(0,5,5)
model2_period2_NeweyWest_tr = matrix(0,5,5)
model2_period2_NeweyWest_tc = matrix(0,5,5)

for (i in 1:25){
  model2_period2 = lm(exret_period2[,i]~mktexret_period2+smb_period2+hml_period2+rmw_period2+cma_period2)
  model2_period2_Rsq[i] = summary(model2_period2)$r.squared
  
  #t statistics
  model2_period2_ta[i] = summary(model2_period2)$coefficients[1,3]
  model2_period2_tb[i] = summary(model2_period2)$coefficients[2,3]
  model2_period2_ts[i] = summary(model2_period2)$coefficients[3,3]
  model2_period2_th[i] = summary(model2_period2)$coefficients[4,3]
  model2_period2_tr[i] = summary(model2_period2)$coefficients[5,3]
  model2_period2_tc[i] = summary(model2_period2)$coefficients[6,3]
  
  #White heteroskedasticity
  model2_period2_white_ta[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[1,3]
  model2_period2_white_tb[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[2,3]
  model2_period2_white_ts[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[3,3]
  model2_period2_white_th[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[4,3]
  model2_period2_white_tr[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[5,3]
  model2_period2_white_tc[i] = coeftest(model2_period2, vcov=vcovHC, type=c("HC"))[6,3]
  
  #NeweyWest heteroskedasticity and auto correlation (serial corr??)
  model2_period2_NeweyWest_ta[i] = coeftest(model2_period2, vcov.=NeweyWest)[1,3]
  model2_period2_NeweyWest_tb[i] = coeftest(model2_period2, vcov.=NeweyWest)[2,3]
  model2_period2_NeweyWest_ts[i] = coeftest(model2_period2, vcov.=NeweyWest)[3,3]
  model2_period2_NeweyWest_th[i] = coeftest(model2_period2, vcov.=NeweyWest)[4,3]
  model2_period2_NeweyWest_tr[i] = coeftest(model2_period2, vcov.=NeweyWest)[5,3]
  model2_period2_NeweyWest_tc[i] = coeftest(model2_period2, vcov.=NeweyWest)[6,3]
}
# to talk about Rsq
model2_period2_Rsq = t(model2_period2_Rsq)

model2_period2_ta = t(model2_period2_ta)
model2_period2_tb = t(model2_period2_tb)
model2_period2_ts = t(model2_period2_ts)
model2_period2_th = t(model2_period2_th)
model2_period2_tr = t(model2_period2_tr)
model2_period2_tc = t(model2_period2_tc)

#individually significance: t tests
# TRUE = rej H0 = individually significant
model2_period2_ta_sig = (model2_period2_ta>qnorm(0.975) | model2_period2_ta<qnorm(0.025))
model2_period2_tb_sig = (model2_period2_tb>qnorm(0.975) | model2_period2_tb<qnorm(0.025))
model2_period2_ts_sig = (model2_period2_ts>qnorm(0.975) | model2_period2_ts<qnorm(0.025))
model2_period2_th_sig = (model2_period2_th>qnorm(0.975) | model2_period2_th<qnorm(0.025))
model2_period2_tr_sig = (model2_period2_tr>qnorm(0.975) | model2_period2_tr<qnorm(0.025))
model2_period2_tc_sig = (model2_period2_tc>qnorm(0.975) | model2_period2_tc<qnorm(0.025))

# t tests results to compare with Q5
# t tests of Q5
# TRUE = rej H0 = significant
model1_period2_rej_ta = (model1_period2_ta > qnorm(0.975) | model1_period2_ta < qnorm(0.025))
model1_period2_rej_tb = (model1_period2_tb > qnorm(0.975) | model1_period2_tb < qnorm(0.025))
model1_period2_rej_ts = (model1_period2_ts > qnorm(0.975) | model1_period2_ts < qnorm(0.025))
model1_period2_rej_th = (model1_period2_th > qnorm(0.975) | model1_period2_th < qnorm(0.025))

#is there heteroskedasticity after added SMW AMC ?
#TRUE = rej H0 = significant
model2_period2_white_rej_ta = (t(model2_period2_white_ta) > qnorm(0.975) | t(model2_period2_white_ta) < qnorm(0.025))
model2_period2_white_rej_tb = (t(model2_period2_white_tb) > qnorm(0.975) | t(model2_period2_white_tb) < qnorm(0.025))
model2_period2_white_rej_ts = (t(model2_period2_white_ts) > qnorm(0.975) | t(model2_period2_white_ts) < qnorm(0.025))
model2_period2_white_rej_th = (t(model2_period2_white_th) > qnorm(0.975) | t(model2_period2_white_th) < qnorm(0.025))
model2_period2_white_rej_tr = (t(model2_period2_white_tr) > qnorm(0.975) | t(model2_period2_white_tr) < qnorm(0.025))
model2_period2_white_rej_tc = (t(model2_period2_white_tc) > qnorm(0.975) | t(model2_period2_white_tc) < qnorm(0.025))

#is there heteroskedasticity and auto corr after added SMW AMC ?
#TRUE = rej H0 = significant
model2_period2_NeweyWest_rej_ta = (t(model2_period2_NeweyWest_ta) > qnorm(0.975) | t(model2_period2_NeweyWest_ta) < qnorm(0.025))
model2_period2_NeweyWest_rej_tb = (t(model2_period2_NeweyWest_tb) > qnorm(0.975) | t(model2_period2_NeweyWest_tb) < qnorm(0.025))
model2_period2_NeweyWest_rej_ts = (t(model2_period2_NeweyWest_ts) > qnorm(0.975) | t(model2_period2_NeweyWest_ts) < qnorm(0.025))
model2_period2_NeweyWest_rej_th = (t(model2_period2_NeweyWest_th) > qnorm(0.975) | t(model2_period2_NeweyWest_th) < qnorm(0.025))
model2_period2_NeweyWest_rej_tr = (t(model2_period2_NeweyWest_tr) > qnorm(0.975) | t(model2_period2_NeweyWest_tr) < qnorm(0.025))
model2_period2_NeweyWest_rej_tc = (t(model2_period2_NeweyWest_tc) > qnorm(0.975) | t(model2_period2_NeweyWest_tc) < qnorm(0.025))

#after added SMW AMC, are significances of parameters modified by heteroskedasticity ?
#TRUE = significance not modified by heteroskedasticity = no heteroskedasticity
model2_period2_ta_sig_white = (model2_period2_ta_sig == model2_period2_white_rej_ta)
model2_period2_tb_sig_white = (model2_period2_tb_sig == model2_period2_white_rej_tb)
model2_period2_ts_sig_white = (model2_period2_ts_sig == model2_period2_white_rej_ts)
model2_period2_th_sig_white = (model2_period2_th_sig == model2_period2_white_rej_th)
model2_period2_tr_sig_white = (model2_period2_tr_sig == model2_period2_white_rej_tr)
model2_period2_tc_sig_white = (model2_period2_tc_sig == model2_period2_white_rej_tc)

#after added SMW AMC, are significances of parameters modified by heteroskedasticity and auto corr?
#TRUE = significance not modified by heteroskedasticity and auto corr = no heteroskedasticity and auto corr
model2_period2_ta_sig_NeweyWest = (model2_period2_ta_sig == model2_period2_NeweyWest_rej_ta)
model2_period2_tb_sig_NeweyWest = (model2_period2_tb_sig == model2_period2_NeweyWest_rej_tb)
model2_period2_ts_sig_NeweyWest = (model2_period2_ts_sig == model2_period2_NeweyWest_rej_ts)
model2_period2_th_sig_NeweyWest = (model2_period2_th_sig == model2_period2_NeweyWest_rej_th)
model2_period2_tr_sig_NeweyWest = (model2_period2_tr_sig == model2_period2_NeweyWest_rej_tr)
model2_period2_tc_sig_NeweyWest = (model2_period2_tc_sig == model2_period2_NeweyWest_rej_tc)

# after added SMW AMC, will influence of heteroskedasticity and auto corr on significances of other parameters, change ?
# to compute model1_period2_ta_sig_white, etc. and model1_period2_ta_sig_NeweyWest, etc.
# and to compare them with model2_period2_ta_sig_white, etc. and model2_period2_ta_sig_NeweyWest, etc. obtained above
model1_period2_white_ta = matrix(0,5,5)
model1_period2_white_tb = matrix(0,5,5)
model1_period2_white_ts = matrix(0,5,5)
model1_period2_white_th = matrix(0,5,5)
model1_period2_NeweyWest_ta = matrix(0,5,5)
model1_period2_NeweyWest_tb = matrix(0,5,5)
model1_period2_NeweyWest_ts = matrix(0,5,5)
model1_period2_NeweyWest_th = matrix(0,5,5)

for (i in 1:25){
  model1_period2 = lm(exret_period2[,i]~factors_period2$Mkt.RF+factors_period2$SMB+factors_period2$HML)
  
  #White heteroskedasticity
  model1_period2_white_ta[i] = coeftest(model1_period2, vcov=vcovHC, type=c("HC"))[1,3]
  model1_period2_white_tb[i] = coeftest(model1_period2, vcov=vcovHC, type=c("HC"))[2,3]
  model1_period2_white_ts[i] = coeftest(model1_period2, vcov=vcovHC, type=c("HC"))[3,3]
  model1_period2_white_th[i] = coeftest(model1_period2, vcov=vcovHC, type=c("HC"))[4,3]
  
  #NeweyWest heteroskedasticity and auto correlation (serial corr??)
  model1_period2_NeweyWest_ta[i] = coeftest(model1_period2, vcov.=NeweyWest)[1,3]
  model1_period2_NeweyWest_tb[i] = coeftest(model1_period2, vcov.=NeweyWest)[2,3]
  model1_period2_NeweyWest_ts[i] = coeftest(model1_period2, vcov.=NeweyWest)[3,3]
  model1_period2_NeweyWest_th[i] = coeftest(model1_period2, vcov.=NeweyWest)[4,3]
}

#is there heteroskedasticity in model1_period2 of Q5 ?
#TRUE = rej H0 = significant
model1_period2_white_rej_ta = (t(model1_period2_white_ta) > qnorm(0.975) | t(model1_period2_white_ta) < qnorm(0.025))
model1_period2_white_rej_tb = (t(model1_period2_white_tb) > qnorm(0.975) | t(model1_period2_white_tb) < qnorm(0.025))
model1_period2_white_rej_ts = (t(model1_period2_white_ts) > qnorm(0.975) | t(model1_period2_white_ts) < qnorm(0.025))
model1_period2_white_rej_th = (t(model1_period2_white_th) > qnorm(0.975) | t(model1_period2_white_th) < qnorm(0.025))

#is there heteroskedasticity and auto corr in model1_period2 of Q5 ?
#TRUE = rej H0 = significant
model1_period2_NeweyWest_rej_ta = (t(model1_period2_NeweyWest_ta) > qnorm(0.975) | t(model1_period2_NeweyWest_ta) < qnorm(0.025))
model1_period2_NeweyWest_rej_tb = (t(model1_period2_NeweyWest_tb) > qnorm(0.975) | t(model1_period2_NeweyWest_tb) < qnorm(0.025))
model1_period2_NeweyWest_rej_ts = (t(model1_period2_NeweyWest_ts) > qnorm(0.975) | t(model1_period2_NeweyWest_ts) < qnorm(0.025))
model1_period2_NeweyWest_rej_th = (t(model1_period2_NeweyWest_th) > qnorm(0.975) | t(model1_period2_NeweyWest_th) < qnorm(0.025))

# in model1_period2 of Q5, are significances of parameters modified by heteroskedasticity ?
#TRUE = significance not modified by heteroskedasticity = no heteroskedasticity
model1_period2_ta_sig_white = (model1_period2_rej_ta == model1_period2_white_rej_ta)
model1_period2_tb_sig_white = (model1_period2_rej_tb == model1_period2_white_rej_tb)
model1_period2_ts_sig_white = (model1_period2_rej_ts == model1_period2_white_rej_ts)
model1_period2_th_sig_white = (model1_period2_rej_th == model1_period2_white_rej_th)

# to compare with model2_period2_ta_sig_white, etc., ex:
model1_period2_ta_sig_white == model2_period2_ta_sig_white

# in model1_period2 of Q5, are significances of parameters modified by heteroskedasticity and auto corr?
#TRUE = significance not modified by heteroskedasticity and auto corr = no heteroskedasticity and auto corr
model1_period2_ta_sig_NeweyWest = (model1_period2_rej_ta == model1_period2_NeweyWest_rej_ta)
model1_period2_tb_sig_NeweyWest = (model1_period2_rej_tb == model1_period2_NeweyWest_rej_tb)
model1_period2_ts_sig_NeweyWest = (model1_period2_rej_ts == model1_period2_NeweyWest_rej_ts)
model1_period2_th_sig_NeweyWest = (model1_period2_rej_th == model1_period2_NeweyWest_rej_th)

# to compare with model2_period2_ta_sig_NeweyWest, etc., ex:
model1_period2_ta_sig_NeweyWest == model2_period2_ta_sig_NeweyWest

#RMW, CMA jointly significance: F test
model2_period2_ftest = matrix(0,5,5)
for (i in 1:25){
  i = 1
  model2_period2_restr = lm(exret_period2[,i]~mktexret_period2+smb_period2+hml_period2)
  model2_period2_unrestr = lm(exret_period2[,i]~mktexret_period2+smb_period2+hml_period2+rmw_period2+cma_period2)
  nrestr=2
  model2_period2_ssrr=sum(model2_period2_restr$residuals^2)
  model2_period2_ssru=sum(model2_period2_unrestr$residuals^2)
  upf=(model2_period2_ssrr-model2_period2_ssru)/nrestr
  lowf=(model2_period2_ssru/(length(mktexret_period2)-length(model2_period2_unrestr$coefficients)))
  model2_period2_ftest[i]=upf/lowf
  #anova(model2_period2_restr,model2_period2_unrestr) is the same
}
model2_period2_ftest = t(model2_period2_ftest)

#TRUE = rej H0 = RMW, CMA jointly significant
threshold = qf(0.95,nrestr,length(mktexret_period2)-length(model2_period2_unrestr$coefficients))
model2_period2_ftest_sig = (model2_period2_ftest >= threshold)

#Q7
model = lm(exret_period1[,1]~factors_period1$Mkt.RF+factors_period1$SMB+factors_period1$HML)
d1 = density(model$residuals)
d2 = density(rnorm(1000000,0,sd(model$residuals)))
colors = c("red", "blue")
labels = c("residuals", "normal")
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "x", ylab = "Density", main = "Comparison of distributions")
lines(d1, col = colors[1])
lines(d2, lty = 2, col = colors[2])
legend("topright", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 2), col=colors)
