# DUC ANH NGUYEN
# ALY 6040 - ASSIGNMENT 02

rm(list=ls())
while (!is.null(dev.list()))  dev.off() 

# ========================== Import packages =========================
library(pastecs)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(tidyverse)
library(ggridges)
library(scales)
library(gridExtra)
library(GGally)
library(hrbrthemes)
source("ztheme.R")

# =========================== Import data ============================
df <- read.csv('ALY6040_Module2_Boston_Housing_Price.csv')

# =========================== Summary data ===========================
summary_df <- round(stat.desc(df), 2)
summary_df
#write.csv(summary_df, 'summary_housing.csv')

# ============================ Plot data =============================
# ===== Boxplot + ggplot geometries – density violin for finding outliers 
# Boxplot
df_fullbox <- gather(df, "variable", "value", c(1:12))
df_fullbox$variable <- gsub("[.]"," ",df_fullbox$variable)
df_fullbox$variable <- factor(df_fullbox$variable, 
                              c('CRIM','ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD','PTRATIO','LSTAT','MEDV'))

ggplot(df_fullbox,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=0.6)+
  geom_jitter(aes(color=variable),size=1,alpha=.2)+
  scale_y_continuous(breaks=seq(0,100,10))+
  guides(fill=FALSE,color=FALSE)+
  labs(x="",
       y="Value",
       caption="Boston Housing Price")+
  coord_flip()+
  z_theme()


# ===== Histogram + Density
# Histogram
a1<-ggplot(df, aes(x=CRIM)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a2<-ggplot(df, aes(x=ZN)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a3<-ggplot(df, aes(x=INDUS)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a4<-ggplot(df, aes(x=CHAS)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a5<-ggplot(df, aes(x=NOX)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a6<-ggplot(df, aes(x=RM)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a7<-ggplot(df, aes(x=AGE)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a8<-ggplot(df, aes(x=DIS)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a9<-ggplot(df, aes(x=RAD)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a10<-ggplot(df, aes(x=PTRATIO)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a11<-ggplot(df, aes(x=LSTAT)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
a12<-ggplot(df, aes(x=MEDV)) + geom_histogram(aes(y = ..density..), bins = 20, alpha = 0.7, fill = "#333333") + geom_density(fill = "#ff4d4d", alpha = 0.5) + theme(panel.background = element_rect(fill = '#ffffff')) + theme_ipsum()
grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12, nrow=3)

# Density
ggplot(df_fullbox,aes(y=variable,x=value))+
  geom_density_ridges(aes(fill=variable, alpha=3/4))+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Boston Housing Variables",
       x="Value",
       y="",
       caption="Boston Housing Price")+
  z_theme()


# ===== Regression between variables
ggplot2::theme_set(ggplot2::theme_classic())

my_fn <- function(data, mapping, method="p", use="pairwise", ...){
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  # calculate colour based on correlation value
  colFn <- colorRampPalette(c("#ffffff", "#ffb3b3", "#ff0000"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  ggally_cor(data = data, mapping = mapping, ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
}

df %>% select(c('CRIM','ZN','INDUS')) %>% ggpairs(., lower = list(continuous = wrap("smooth",alpha = 0.3, size=0.1)))+ theme_map()

ggpairs(df,
        upper = list(continuous = my_fn),
        lower = list(continuous = wrap("smooth",alpha = 0.3, size=0.1)))  #+theme_map() #+ theme_pander()



# ===== Correlation Matrix: 
corr <- round(cor(df), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Boston Housing Price", 
           ggtheme=theme_bw)


