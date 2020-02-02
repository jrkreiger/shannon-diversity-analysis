library(tidyverse)
library(readxl)
xl <- read_excel("6000b_2.xlsx")
head(xl)
xl2 <- xl %>% select(su, material, class, quantity)
head(xl2)
xl3 <- xl2 %>%
  group_by(su, material) %>%
  summarize(quantity = sum(quantity))
head(xl3)
xl4 <- xl3 %>%
  spread(key = material, value = quantity)
xl4[is.na(xl4)] <- 0
xl4
xl5 <- xl4
xl5$id <- seq.int(nrow(xl5))
su_key <- xl5 %>%
  select(id, su)
su_key
xl5$su <- NULL
xl5$id <- NULL


my_data <- xl5
library(vegan)
library(maptools)
data(my_data)

# Set option to print a maximum of 3 significant digits.
options(digits = 3)

# View the data
View(my_data)

# Calculate the total number of artifacts of all types in each assemblage (row-wise sums).
(N <- rowSums(my_data))

# Calculate the total number of artifacts of each type across all assemblages (column-wise sums).
(T <- colSums(my_data))

# Calculate percentage of each type of artifact by assemblage.
my_data.pct <- my_data/N*100

# Calculate mean percentage of each artifact type across assemblages.
(Mp <- colMeans(my_data.pct))

# Sum the artifact types represented in each assemblage (richness).
(S <- specnumber(my_data))

# Sum the assemblages containing each artifact type (ubiquity).
(U <- specnumber(my_data, MARGIN = 2))

# Calculate percentage of assemblages that have each type of artifact (divide ubiquity by number of assemblages).
(Up <- U/length(N)*100)

# Calculate Shannon diversity measure (information entropy; high diversity/entropy means more types overall and that artifacts are spread more evenly over the types).
(Shannon_diversity <- diversity(my_data))

# Calculate Simpson index (i.e., probability that two artifacts drawn randomly will represent different types).
(D1 <- diversity(my_data, index = "simpson"))

# Shannon diversity index and Simpson index are usually highly correlated.
(correlation <- cor(Shannon_diversity, D1))

# Give effective number of species for Simpson index (i.e., the number of species that, if evenly distributed, would produce the same diversity index as the sample).
(D2 <- diversity(my_data, index = "invsimpson"))

# Give effective number of species for Shannon diversity index.
(Hmax <- exp(Shannon_diversity))

# Calculate correlation of D2 and Hmax (these measures are typically strongly correlated).
(correlation_2 <- cor(D2, Hmax))

# Calculate Pielou's J (Shannon diversity index divided by natural log of richness). Values will range from 0 to 1.
(Pielou_J <- Shannon_diversity/log(S))

# Calculate the ratio of effective species to richness. A value of 1 means that the sample is as even as it can be.
(E <- Hmax / S)

# Plot assemblages by Shannon diversity index (x-axis) and Pielou's J (y-axis). X values represent diversity; y values represent evenness. Points in the upper right quadrant have high diversity and high evenness. 
plot(Shannon_diversity, Pielou_J, main = "Shannon diversity index (x-axis) v. Pielou's J (y-axis)", pch = ifelse(N > 10, 16, 1))
abline(h = median(Pielou_J), v = median(Shannon_diversity), lty = 2)
pointLabel(Shannon_diversity, Pielou_J, rownames(my_data), cex = 0.75)
#leg.txt <- c(as.expression(bquote(Tools <= 10)), as.expression(bquote(Tools > 10)))
#legend("bottomright", leg.txt, pch = c(1, 16))

