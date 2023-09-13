library(tidyverse)
library(vroom)
library(skimr)
library(DataExplorer)
library(patchwork)
bike <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/train.csv',
               show_col_types = FALSE)
test <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/test.csv',
               show_col_types = FALSE)
sample <- vroom('C:/BYU/2023(5) Fall/STAT 348/KaggleBikeShare/sampleSubmission.csv',
               show_col_types = FALSE)
skimr::skim(bike)
DataExplorer::plot_correlation(bike)

plot1 <- ggplot(bike)+
  geom_point(mapping =aes(x=atemp, y=count)) +
  theme(aspect.ratio = 1)
plot2 <- ggplot(data = bike) +
  geom_boxplot(mapping = aes(x = count))+
  theme(aspect.ratio = 1)
plot3 <-ggplot(data = bike)+
  geom_col(mapping = aes(x =as.factor(season), y =count )) +
  theme(aspect.ratio = 1)
plot4 <- ggplot(bike)+
  geom_point(mapping =aes(x=humidity, y=count)) +
  theme(aspect.ratio = 1)
(plot1 + plot4) / (plot2 + plot3)
