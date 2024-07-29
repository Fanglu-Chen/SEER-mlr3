setwd("")#工作目录

#用于训练模型的数据
data<-read.csv("Finall Data_NPC.csv")
#用于训练模型的变量名
surv_data<-data[,c("survivalmonths", "event_os","agerecodewithsingleagesand","sex",
                   "race_1","marital_status_1","mhi","region","his_type","graderecodethru",
                   "tumor_size1","therapy","summary_clinical_stage")]

#因子化
surv_data$sex<-factor(surv_data$sex,levels=c("Male","Female"))
surv_data$his_type<-factor(surv_data$his_type,levels = c("UNKC","BSCC","DNKC","KSCC","Others"))
surv_data$race_1<-factor(surv_data$race_1,levels = c("White","Black","Others","Unknown"))
surv_data$marital_status_1<-factor(surv_data$marital_status_1,levels = c("Married","Unmarried","Unknown"))
surv_data$region<-factor(surv_data$region,levels = c("Urban","Rural","Unknown"))
surv_data$graderecodethru<-factor(surv_data$graderecodethru,
                                  levels = c("Grade I",
                                             "Grade II",
                                             "Grade III",
                                             "Grade IV",
                                             "Unknown"))
surv_data$summary_clinical_stage<-factor(surv_data$summary_clinical_stage,
                                         levels = c("Early"  ,
                                                    "Medium"  ,"Local-Distant NPC",
                                                    "Distant"  ,        
                                                    "Unknown"))
surv_data$therapy<-factor(surv_data$therapy,
                          levels = c("RT+CT","CT",
                                     "RT","Surgery","RT+CT+Surgery","Others" ))
surv_data$mhi<-factor(surv_data$mhi,
                      levels = unique(surv_data$mhi))
surv_data$tumor_size1<-factor(surv_data$tumor_size1,
                              levels =c("0-30mm","30-60mm",  ">60mm","Unknown" ))

#随机森林，感兴趣的时间点为1年至20年
library(randomForestSRC)
set.seed(12)
rsf_model <- rfsrc(Surv(survivalmonths, event_os) ~ agerecodewithsingleagesand+sex+
                     race_1+marital_status_1+mhi+region+his_type+graderecodethru+
                     tumor_size1+therapy+summary_clinical_stage, data = surv_data,
                   splitrule="logrank",ntime = c(seq(12,240,12)))


#随机生成一个新的观测
set.seed(42)  
sex_levels <- c("Male", "Female")
his_type_levels <- c("UNKC", "BSCC", "DNKC", "KSCC", "Others")
race_levels <- c("White", "Black", "Others", "Unknown")
marital_status_levels <- c("Married", "Unmarried", "Unknown")
region_levels <- c("Urban", "Rural", "Unknown")
graderecodethru_levels <- c("Grade I", "Grade II", "Grade III", "Grade IV", "Unknown")
clinical_stage_levels <- c("Early", "Medium", "Local-Distant NPC", "Distant", "Unknown")
therapy_levels <- c("RT+CT", "CT", "RT", "Surgery", "RT+CT+Surgery", "Others")
mhi_levels <- c(">=70000", "50000-69999", "<50000") 
tumor_size_levels <- c("0-30mm", "30-60mm", ">60mm", "Unknown")
random_observation <- data.frame(
  agerecodewithsingleagesand = sample(1:90, 1),  
  sex = factor(sample(sex_levels, 1), levels=sex_levels),
  race_1 = factor(sample(race_levels, 1), levels=race_levels),
  marital_status_1 = factor(sample(marital_status_levels, 1), levels=marital_status_levels),
  mhi = factor(sample(mhi_levels, 1), levels=mhi_levels),
  region = factor(sample(region_levels, 1), levels=region_levels),
  his_type = factor(sample(his_type_levels, 1), levels=his_type_levels),
  graderecodethru = factor(sample(graderecodethru_levels, 1), levels=graderecodethru_levels),
  tumor_size1 = factor(sample(tumor_size_levels, 1), levels=tumor_size_levels),
  therapy = factor(sample(therapy_levels, 1), levels=therapy_levels),
  summary_clinical_stage = factor(sample(clinical_stage_levels, 1), levels=clinical_stage_levels)
)
#预测
predict_prob <- predict(rsf_model, newdata = random_observation)$survival
print(predict_prob)#分别其是1-20年的生存概率
#柱状图
plot_data<-data.frame(Year=1:20,predict_prob=c(predict_prob))
library(ggplot2)
ggplot(plot_data, aes(x = Year, y = predict_prob)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Years", y = "Survival Probability", title = "Survival Probability by Year") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
  theme_minimal()

#绘制5年生存曲线
#5-year overall survival curve
library(survival)
library(survminer)
km_fit <- survfit(Surv(survivalmonths, event_os) ~ 1,data = surv_data)
year5 <- ggsurvplot(
  km_fit,
  data = surv_data,
  xlim = c(0, 60), 
  xlab = "Survival Months", 
  break.time.by = 12, 
  surv.median.line = "hv" 
)
print(year5)
#绘制10年生存曲线
year10 <- ggsurvplot(
  km_fit,
  data = surv_data,
  xlim = c(0, 120), 
  xlab = "Survival Months", 
  break.time.by = 12, 
  surv.median.line = "hv" 
)
print(year10)
#按照不同的基线特征分层
#agerecodewithsingleagesand分层为agegroup
surv_data$agegroup<-ifelse(surv_data$agerecodewithsingleagesand<=47,"<=47",
                           ifelse(surv_data$agerecodewithsingleagesand<=59,"47-59",
                                  ">59"))
surv_data$agegroup<-factor(surv_data$agegroup,levels=c("<=47","47-59",">59"))
#agegroup需分别替换为其它变量("sex","race_1","marital_status_1","mhi","region","his_type","graderecodethru",
#"tumor_size1","therapy","summary_clinical_stage")
km_strata <- survfit(Surv(survivalmonths, event_os) ~agegroup ,data = surv_data)
year10strata <- ggsurvplot(
  km_strata,
  data = surv_data,
  xlim = c(0, 120), 
  xlab = "Survival Months", 
  break.time.by = 12, 
  surv.median.line = "hv" 
)
print(year10strata)

