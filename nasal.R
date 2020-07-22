library(cluster)
library(useful)

filename <- "nasal_cavity_skeleton_cold_climate.csv"
dane <- read.csv(filename, header = TRUE, sep = ";", dec = ",", 
                 stringsAsFactors = FALSE)

colSums(is.na(dane))

table(dane$SERIES)
daneTrain <- dane[, c("ANTER_AREA", "POST_AREA", "N_PR", "N_NS","G_OP","EU_EU",
                      "MIN_AXIS_ANT","MAJ_AXIS_ANT","ANT_PERIM","ANT_COMPACT",
                      "ANT_SH_FACT","L_POST_AREA", "L_POST_MIN_A", "L_POST_MAJ_A", 
                      "L_POST_PERIM", "L_POST_COMPA", "L_POST_SH_FA", 
                      "R_POST_AREA", "R_POST_MIN_A", "R_POST_MAJ_A", "R_POST_PERIM", 
                      "R_POST_COMP", "R_POST_SH_FA")]
set.seed(238625)

# szacowanie liczby clustrów - 11?
numClBest <- FitKMeans(daneTrain, max.clusters=20, nstart=25,seed=238625)
numClBest

# inne szacowanie liczby clustrów - 6?
theGap <- clusGap(daneTrain, FUNcluster=pam, K.max=20)
gapDF <- as.data.frame(theGap$Tab)
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
  geom_line(aes(y=gap), color="orange") +
  geom_point(aes(y=gap), color="red") +
  labs(x="Number of Clusters", y="Gap")

# grupowanie
daneK3 <- kmeans(x=daneTrain, centers=6, nstart = 25)
daneK3

plot.kmeans(daneK3, data=daneTrain)
plot(daneK3, data=dane, class="SERIES")
