library(readxl)
library(data.table)
library(reshape2)
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)

set.seed(123)
# Make sure you change these to your correct file path
whites <- read_excel("CSCI 4890 Data Mining and Warehousing/Project/wine_quality_clean.xlsx", 
                                 sheet = "white")
reds <- read_excel("CSCI 4890 Data Mining and Warehousing/Project/wine_quality_clean.xlsx", 
                                 sheet = "red")
reds$outcome = 'red'
whites$outcome = 'white'
wine = rbind.data.frame(reds, whites)
# wine$density = NULL
# Factorize wine dataset
wine$outcome = as.factor(wine$outcome)

# Create test and train data-sets
wine$id = 1:nrow(wine)
train = wine %>% sample_frac(0.80)
test = wine %>% anti_join(wine,train,by='id')

# Create Tree
# without density
wineTree = rpart(outcome~.-(id+density), data=train)
rpart.plot(wineTree, tweak=1.1)
  # Prediction
pWine = predict(wineTree, train, type='class')
  # Confusion Matrix
confusionMatrix(data=pWine, train$outcome)

# With density
wineTreeDensity = rpart(outcome~.-id, data=train)
rpart.plot(wineTreeDensity)
# Prediction
pWineDensity = predict(wineTreeDensity, train, type='class')
# Confusion Matrix
confusionMatrix(data=pWineDensity, train$outcome)


# Regression
# import the data-set, change this to your file path/correct sheet 
whiteStd = read_excel("CSCI 4890 Data Mining and Warehousing/Project/wine_quality_clean.xlsx", 
                      sheet = "white-standardized")
redStd = read_excel("CSCI 4890 Data Mining and Warehousing/Project/wine_quality_clean.xlsx", 
                    sheet = "red-standardized")

# Linear Models 
# Model 1: Quality vs Residual Sugar 

model1_white = lm(quality ~ `residual sugar`, data=whiteStd) 
summary(model1_white)

model1_red = lm(quality ~ `residual sugar`, data=redStd)
summary(model1_red)
  # Shows that Residual Sugar is significant in White wines, but Reds when it
  # comes to quality. This makes sense as White wines are sweeter than Reds.

# Model 2: Finding other significant variables for both Reds and Whites

model2_white = lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` +
              `residual sugar` + chlorides + `free sulfur dioxide` +
              `total sulfur dioxide` + density + pH + sulphates + alcohol,
            data=whiteStd) 
summary(model2_white)
 # Significant values include:
  # High significance: Citric Acid, Residual Sugar, Density, PH, Sulfates, alcohol
  # Medium significance: Fixed Acidity

model2_red = lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` +
              `residual sugar` + chlorides + `free sulfur dioxide` +
              `total sulfur dioxide` + density + pH + sulphates + alcohol,
            data=redStd) 
summary(model2_red)
  # Significant values include:
    # High significance: Volatile Acidity, Chlorides, Total Sulfur Dioxide,
      # Sulfates, Alcohol
    # Low significance: Free sulfur Dioxide, pH

# Model 3: Only include significant variables for both white and red wines
model3_white = lm(quality ~ `fixed acidity` + `citric acid` + `residual sugar` +
                  density + pH + sulphates + alcohol, data=whiteStd)
summary(model3_white)

model3_red = lm(quality ~ `volatile acidity` + chlorides +
                  `free sulfur dioxide` + `total sulfur dioxide` + pH +
                  sulphates + alcohol, data=redStd)
summary(model3_red)

# Correlation values and graphs will show influence of each variable on Wine
# Quality for their respective types of wine. 
# These graphs will show what higher rated wines should have ideally for each
# set of significant variable.

# Correlation Graphs
library(corrr)
library(knitr)
# White
whiteQualityCor = whiteStd %>%
  correlate() %>%
  focus(quality)
colnames(whiteQualityCor)[colnames(whiteQualityCor)=='term'] = 'variable'
  # Table of Values
kable(whiteQualityCor[order(whiteQualityCor$quality),])
  # Bar plot
whiteQualityCor %>%
  mutate(variable = factor(variable, levels = variable[order(quality)])) %>%
  ggplot(aes(x = variable, y = quality, fill=variable)) +
  geom_bar(stat = "identity") + ylab("Correlation with Quality") +
  xlab("Variable") + ggtitle('Correlations to White Wine Quality') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Red
redQualityCor = redStd %>%
  correlate() %>%
  focus(quality)
colnames(redQualityCor)[colnames(redQualityCor)=='term'] = 'variable'
  # Table of Values
kable(redQualityCor[order(redQualityCor$quality),])
  # Bar plot
redQualityCor %>%
  mutate(variable = factor(variable, levels = variable[order(quality)])) %>%
  ggplot(aes(x = variable, y = quality, fill=variable)) +
  geom_bar(stat = "identity") + ylab("Correlation with Quality") +
  xlab("Variable") + ggtitle('Correlations to Red Wine Quality') +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# Clustering
# Determine whether clustering can determine wine type accurately
 # Find centers of Red Wine
redCenter = kmeans(reds, k=1)
  # Find centers of White Wine
whiteCenter = kmeans(whites, k=1)

# Run k-means over the combined data-set using the two centers as k
totalClus = kmeans(wine, c(reds, whites))