library(ggplot2)
library(tidyverse)
library(dplyr)

# -------------------- PCA with prcomp() ----------------------

# generate fake data
d_blop <- matrix(nrow = 100, ncol = 10)

colnames(d_blop) <- c(
  paste("wt", 1:5, sep = ""),
  paste("ko", 1:5, sep = "")
)

row.names(d_blop) <- paste("gene", 1:100, sep = "")

for (i in 1:100) {
  wt.values <- rpois(5, lambda = sample(10:1000, size = 1))
  ko.values <- rpois(5, lambda = sample(10:1000, size = 1))
  
  d_blop[i, ] <- c(wt.values, ko.values)
}

# calculate pca using prcomp. t is to transpose column into rows.
d_blop.pca <- prcomp(t(d_blop), scale. = TRUE)

# Plot the data using PC1 and PC2
plot(d_blop.pca$x[,1], d_blop.pca$x[,2])

# get the variance between the variables
d_blop.pca.var <- d_blop.pca$sdev^2

# get the variance percentage
d_blop.pca.var.perc <- round(d_blop.pca.var/sum(d_blop.pca.var)*100,1)

# create scree plot 
dblop.pca.screeplot <- barplot(d_blop.pca.var.perc, xlab = "Principal Component",
                               main = "Scree Plot", ylab = "Percentage Variation")

# Create dataset for pca
d_blop.pca.data <- data.frame(Sample= row.names(d_blop.pca$x),
                          X=d_blop.pca$x[,1],
                          Y=d_blop.pca$x[,2])

# create pca with ggplot
ggplot(data = d_blop.pca.data, aes(x=X, y=Y, label=Sample))+
  geom_text()+
  ggtitle("PCA graph")+
  xlab(paste0("PC1 - ", d_blop.pca.var.perc[1], " %", sep=""))+
  ylab(paste0("PC2 - ", d_blop.pca.var.perc[2], " %", sep=""))

# use loading score to determine largest effect  
d_blop.pca.loadingscores <- d_blop.pca$rotation[,1]
genes_scores <- abs(d_blop.pca.loadingscores) 
genes_scores.ranked <- sort(genes_scores, decreasing = TRUE) 
top_10_genes <- names(genes_scores.ranked[1:10])
d_blop.pca$rotation[top_10_genes,1]

# ---------------------- PCA with ggfortify -----------------------


library(ggfortify)

d_iris <- iris %>% 
  select_if(is.numeric)

autoplot(prcomp(d_iris), data = iris, colour='Species') %>% 
  plotly::ggplotly()
  # ggpubr::fill_palette("jco")+      # Indiviual fill color
  # ggpubr::color_palette("npg") 

# --------------------- correlation matrix with ggally---------------

library(GGally)

d_mtcars <- mtcars %>% select_if(is.numeric)

ggpairs(d_mtcars)

ggpairs(iris, columns = 1:4,  ggplot2:: aes(color= Species ))

plot(d_mtcars)


# -------------- heatmap with ggplot2 ---------------

# geom_raster might be quicker, but I haven't tried yet


library(reshape2) # melt

# create correlation mat
mtcars.corr <- round(cor(mtcars), 2)
mtcars.melt <- melt(mtcars.corr)

mtcars.melt %>% 
  ggplot(aes(Var1, Var2, fill=value)) + geom_tile()
 
# ------------------------ heat map with heatmaply ---------------

library(heatmaply)

heatmaply(d_mtcars, grid_color = 'white', hide_colorbar = TRUE, dendrogram = 'none')


# ----------------- density plot with ggplot ------------------------

ggplot(mtcars, aes(cyl, wt)) +
  stat_density2d(aes(fill= ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis()

# -------------------- create a map ----------------------
maps::canada.cities

maps::us.cities

# --------------- Logistic regression -----------------

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header = FALSE)

colnames(data) <- c(
  "age",
  "sex",
  "cp", 
  "trestbps", 
  "chol", 
  "fbs",  
  "restecg", 
  "thalach", 
  "exang",    
  "oldpeak", 
  "slope", 
  "ca", 
  "thal", 
  "hd"
)

data %>% 
  mutate(sex = case_when(sex== 0 ~'F', sex== 1 ~"M"))



# ----------------- Random forest --------------------



# ---------------- ROC and AUC -----------------------

library(pROC)




# ----------------------- MDS and PCoA ----------------------------




