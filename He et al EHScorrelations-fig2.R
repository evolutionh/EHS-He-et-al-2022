##fig 2
library(tidyverse)
library(wesanderson)
library(dplyr)
library(ggplot2)
##
## plot correlations between predictors
##
# get upper triangle of the correlation matrix
# (source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization) see Thomas et al 2016
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


d = FarmVM1msb %>%
  select(PhelpedMe,breedingF0712, BF, PChdinHH,  pAvgR,Age )

cor_vars = c("Husband helped?","Reproducing women","Reproductive-age women","Husband's children", 
                         "Relatedness", "Age")


# calculate correlations and p-values
cormats = d %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.,type = "spearman")

# format p-values
cormat.p = cormats$P %>% 
  round(., 2) %>% 
  get_upper_tri(.) %>% 
  reshape2::melt(.)

# format correlations and merge with p-values
cormat = cormats$r %>% 
  round(., 2) %>% 
  get_upper_tri(.) %>% 
  reshape2::melt(.) %>% 
  rename(r = value) %>% 
  left_join(cormat.p, by=c("Var1", "Var2")) %>% 
  rename(p = value)

# rename variables
cormat$Var1 = factor(cormat$Var1)
levels(cormat$Var1) = cor_vars
cormat$Var2 = factor(cormat$Var2)
levels(cormat$Var2) = cor_vars



ggplot(data = cormat, aes(Var2, Var1, fill = p)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = r), size=3) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", na.value="grey75",
                       midpoint = 0.05, limit = c(0, 0.1), space = "Lab", 
                       name="P-value") +
  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 11, hjust = 1)) +
  coord_fixed() +
  xlab("") +
  ylab("")

# point out where p < 0.01 (rather than show it as p = 0)
cormat$p = as.character(cormat$p)
cormat$p = ifelse(cormat$p == "0", "< 0.01", cormat$p)
cormat
