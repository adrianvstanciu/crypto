# --- PCA financial and technological literacy
if (!require(ggforce)) {
  install.packages("ggforce")
  require(ggforce)
}

# - packagges
library(psych)
library(haven)
library(sjlabelled)
library(dplyr)
library(tidyverse)
library(factoextra)
library(ggforce)

# - data
load("data/df.Rdata")

# - source scripts
source("scripts/helper_functions.R")


# script
# --- selects items
literacy_items <- df_ipst %>%
  dplyr::select(where(is.numeric)) %>%
  names() %>%
  str_subset(pattern = "^kryftl")

# --- pca centered
pca <- df_ipst %>% haven::zap_labels() %>% 
  select(all_of(literacy_items)) %>% 
  psych::principal(nfactors = 2, rotate = "varimax")

pca[["loadings"]] %>% 
  as.matrix.data.frame(rownames.force = TRUE) %>% 
  as_tibble(rownames = "item") %>% 
  ggplot(., aes(x = V1, y = V2)) +  
  geom_text(aes(x = V1, y = V2, label = item), nudge_x = 0.075) +
  geom_point() +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5))

pca$Structure

# version 2
pcav2<-df_ipst %>% haven::zap_labels() %>% 
  select(all_of(literacy_items)) %>% 
  prcomp(center=TRUE,scale=TRUE)

summary(pcav2)

# - screeplot to determine number of components

screeplot(pcav2, type="lines", npcs=7, 
                   main="Screeplot of the first 6 PCs")
abline(h=1, col="red",lty=5)
legend("topright",
       legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5,cex=0.6)

# - cumulative variance plot
cumpro<-cumsum(pcav2$sdev^2 / sum(pcav2$sde^2))
plot(cumpro,
     xlab="PCA #", 
     ylab="Amount of explained variance",
     main="Cumulative variance plot")
abline(v=2, col="blue",lty=5)
abline(h=0.505, col="blue", lty=5)
legend("bottomright",
       legend=c("Cut-off @ PCA2"),
       col=c("blue"),lty=5,cex=0.6)

# -- plots

df_ipst_v2<-df_ipst %>% select(sex,edu,employ,krybst01,krybsz01) %>%  
  naturalize_labelled_df()

df_ipst$sex<-as.factor(df_ipst$sex)
df_ipst$edu<-as.factor(df_ipst$edu)
df_ipst$krybst01 <- as.factor(df_ipst$krybst01)
df_ipst$krybsz01 <- as.factor(df_ipst$krybsz01)



# this is how it works using ggplot, but it requires more coding
tmp_pca_gender<-fviz_pca_ind(pcav2, 
             label="none")

plot_gender_pca<-ggplot(cbind(tmp_pca_gender$data,
                              df_ipst_v2[,"sex_fct"])) + 
  geom_point(aes(x=x,
                 y=y,
                 col=sex_fct)) + 
  geom_mark_ellipse(aes(x=x,
                        y=y,
                        fill=sex_fct,
                        label=sex_fct),
                    expand= unit(0.5,"mm"),
                    label.buffer=unit(-5,"mm")) +
  labs(title="Scatter plot for the 2PC on financial and technological literacy",
       subtitle="Grouping variable is Gender") +
  mytheme


# --------
# --- for gender
pca_sex<-fviz_pca_ind(pcav2, 
                      geom.ind = "point", 
                      pointshape = 21, 
                      pointsize = 2, 
                      fill.ind = df_ipst$sex, 
                      col.ind = "black", 
                      palette = "jco", 
                      addEllipses = TRUE,
                      label = "var",
                      col.var = "black",
                      repel = TRUE,
                      legend.title = "Gender") +
  labs(title="2PC solution based on financial and technological literacy",
       subtitle="Grouping variable is Gender") +
  theme(plot.title = element_text(hjust = 0.5))

# --- for education
pca_edu<-fviz_pca_ind(pcav2, 
               geom.ind = "point", 
               pointshape = 21, 
               pointsize = 2, 
               fill.ind = df_ipst$edu, 
               col.ind = "black", 
               palette = "jco", 
               addEllipses = TRUE,
               label = "var",
               col.var = "black",
               repel = TRUE,
               legend.title = "Education") +
  labs(title="2PC solution based on financial and technological literacy",
       subtitle="Grouping variable is Education") +
  theme(plot.title = element_text(hjust = 0.5))

pca_cryptohold <- fviz_pca_ind(pcav2, 
                               geom.ind = "point", 
                               pointshape = 21, 
                               pointsize = 2, 
                               fill.ind = df_ipst$krybst01, 
                               col.ind = "black", 
                               palette = "jco", 
                               addEllipses = TRUE,
                               label = "var",
                               col.var = "black",
                               repel = TRUE,
                               legend.title = "Holds Crypto") +
  labs(title="2PC solution based on financial and technological literacy",
       subtitle="Grouping variable is <Currently Holds Cryptocurrency>") +
  theme(plot.title = element_text(hjust = 0.5))

pca_cryptoholdfutur <- fviz_pca_ind(pcav2, 
                               geom.ind = "point", 
                               pointshape = 21, 
                               pointsize = 2, 
                               fill.ind = df_ipst$krybsz01, 
                               col.ind = "black", 
                               palette = "jco", 
                               addEllipses = TRUE,
                               label = "var",
                               col.var = "black",
                               repel = TRUE,
                               legend.title = "Crypto in the future") +
  labs(title="2PC solution based on financial and technological literacy",
       subtitle="Grouping variable is <Intends to hold cryptocurrency in the future>") +
  theme(plot.title = element_text(hjust = 0.5))

# --- extract variable labels for educational and technological literacy questions

labels_literacy <- df %>% 
  select(starts_with("kryftl")) %>%  
  register_labels()

# --- calculate cronbach alpha and construct scales

literacy <- list(
  financial_satisfaction = c("kryftl02","kryftl07"),
  financial_opportunism = c("kryftl01","kryftl05","kryftl06")
)
financial_satisfaction = c("kryftl02","kryftl07")
financial_opportunism = c("kryftl01","kryftl05","kryftl06")

cronb_lit <- tibble(
  variable = vector("character",0),
  alpha = vector("numeric",0),
  ci_low = vector("numeric",0),
  ci_high = vector("numeric",0)
)


## cronbach alpha table for loop ---
# - pvq dimensions
for (i in literacy){
  
  name = i
  # name <- deparse(substitute(i))
  tmp <- df_ipst %>% 
    filter(krybws01==1) %>% # only those who've heard of crypto
    dplyr::select(i) %>% 
    ltm::cronbach.alpha(.,na.rm=TRUE,CI=TRUE,B=100)
  
  cronb_lit <- cronb_lit %>% add_row(
    variable = name,
    alpha = tmp$alpha,
    ci_low = tmp$ci[1],
    ci_high = tmp$ci[2]
  )
  
  rm(tmp)
  cronb_lit <- cronb_lit %>% distinct(., alpha, .keep_all = T)
  
}
cronb_lit$indexe <- names(literacy)
cronb_lit <- cronb_lit %>%relocate(indexe, .before=alpha) %>% subset(.,select=-variable) %>% round_df()


# --- save data
save(list=c("pca","pcav2",
            "pca_sex","pca_edu","pca_cryptohold","cronb_lit",
            "pca_cryptoholdfutur","labels_literacy"),
     file="data/pca.Rdata")