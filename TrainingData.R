library(caret)
library(stringr)
library(reshape2)
library(tree)
library(dplyr)
library(readr)
CollegeCr <- read_csv("/Users/eudoc/Documents/Statistics/STAT580/Project2/CollegeCr.csv")
Edwards <- read_csv("/Users/eudoc/Documents/Statistics/STAT580/Project2/Edwards.csv")
OldTown <- read_csv("/Users/eudoc/Documents/Statistics/STAT580/Project2/OldTown.csv")
College_test<-read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/CollegeCr.test.csv")
Edwards_test<-read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/Edwards.test.csv")
OldTown_test<-read_csv("C:/Users/eudoc/Documents/Statistics/STAT580/Project2/OldTown.test.csv")
#Delete Problem Rows
CollegeCr<-CollegeCr[-30,] #yrsold before yrbuilt
Edwards<-Edwards[-16,] #number of bathrooms confusing
# Add ID 
num_rows<-nrow(CollegeCr)
CollegeCr$ID <- c(1:num_rows)
CollegeCr$Neigh<-c("College")
CollegeCr$House<-do.call(paste,c(CollegeCr[c("Neigh","ID")],sep=""))
CollegeCr <- CollegeCr[ , ! names(CollegeCr) %in% c("ID")]
CollegeCr <- CollegeCr %>% relocate(House, .before = OverallQual)

num_rows<-nrow(Edwards)
Edwards$ID <- c(1:num_rows)
Edwards$Neigh<-c("Edwards")
Edwards$House<-do.call(paste,c(Edwards[c("Neigh","ID")],sep=""))
Edwards <- Edwards[ , ! names(Edwards) %in% c("ID")]
Edwards <- Edwards %>% relocate(House, .before = Electrical)

num_rows<-nrow(OldTown)
OldTown$ID <- c(1:num_rows)
OldTown$Neigh<-c("OldTown")
OldTown$House<-do.call(paste,c(OldTown[c("Neigh","ID")],sep=""))
OldTown <- OldTown[ , ! names(OldTown) %in% c("ID")]
OldTown <- OldTown %>% relocate(House, .before = Foundation)

num_rows<-nrow(College_test)
College_test$ID <- c(1:num_rows)
College_test$Neigh<-c("CollegeTest")
College_test$House<-do.call(paste,c(College_test[c("Neigh","ID")],sep=""))
College_test <- College_test[ , ! names(College_test) %in% c("ID")]
College_test <- College_test %>% relocate(House, .before = uniqueID)

num_rows<-nrow(Edwards_test)
Edwards_test$ID <- c(1:num_rows)
Edwards_test$Neigh<-c("EdwardsTest")
Edwards_test$House<-do.call(paste,c(Edwards_test[c("Neigh","ID")],sep=""))
Edwards_test <- Edwards_test[ , ! names(Edwards_test) %in% c("ID")]
Edwards_test <- Edwards_test %>% relocate(House, .before = uniqueID)

num_rows<-nrow(OldTown_test)
OldTown_test$ID <- c(1:num_rows)
OldTown_test$Neigh<-c("OldTownTest")
OldTown_test$House<-do.call(paste,c(OldTown_test[c("Neigh","ID")],sep=""))
OldTown_test <- OldTown_test[ , ! names(OldTown_test) %in% c("ID")]
OldTown_test <- OldTown_test %>% relocate(House, .before = uniqueID)

#Merge Data Sets
df_list<-list(CollegeCr,Edwards,OldTown,College_test,Edwards_test,OldTown_test)
train_data<-Reduce(function(x,y) merge(x,y,all=TRUE),df_list)
View(train_data)

#Rearrange SalePrice to beginning of data set
train_data <- train_data %>% relocate(SalePrice, .before = OverallQual)

#Find duplicates and remove them
dups2<-train_data[(duplicated(train_data[c("SalePrice","YrSold","YearBuilt","LotInfo")]) | duplicated(train_data[c("SalePrice","YrSold","YearBuilt","LotInfo")], fromLast = TRUE)), ]
train_data<- train_data[-c(64, 70, 321), ]

#Find Houses with year built and year sold errors
train_data$results = ifelse(train_data$YearBuilt > train_data$YrSold, 'Error',
                      ifelse(train_data$YearBuilt < train_data$YrSold, 'Fine', 'None'))
train_data[train_data$results == "Error", ]
train_data <- subset (train_data, select = -results)

#Look for zero variance to remove
nearZeroVar(train_data, saveMetrics = TRUE)
sum(is.na(train_data$BsmtCond))
sum(is.na(train_data$BsmtCond))

#investigate near zero variance features
table(train_data$Heating, useNA = 'always')
p <- ggplot(train_data, aes(x=Heating, y=SalePrice)) + 
  geom_boxplot()
p
table(train_data$RoofMatl, useNA = 'always')
p1 <- ggplot(train_data, aes(x=RoofMatl, y=SalePrice)) + 
  geom_boxplot()
p1
#remove BsmtCond, Utilities and RoofMat1 due to zero variance or near zero variance
train_data_nzero <- subset(train_data, select = -c(BsmtCond, Utilities, RoofMatl))

#Remove 'story' in House style
table(train_data_nzero$HouseStyle, useNA = 'always')
train_data_nzero$house_story<-gsub("[a-zA-Z ]", "", train_data_nzero$HouseStyle)
train_data_nzero <- train_data_nzero[ , ! names(train_data_nzero) %in% c("HouseStyle")]

#BsmtUnfSF not found in all data sets, removed after checking for importance
with(OldTown, plot(BsmtUnfSF, SalePrice))
train_data_nzero <- train_data_nzero[ , ! names(train_data_nzero) %in% c("BsmtUnfSF")]

#Separate String Variables
Lot_infor<-colsplit(train_data_nzero$LotInfo, ";", names = c("Lotconfig",
                                                             "LotShape","LotArea","LotFront"))
Exterior<-colsplit(train_data_nzero$Exterior, ";", names = c("Exterior1st","ExtQual","ExtCond"))

# number of rows in data frame
num_rows = nrow(train_data_nzero)
num_rows2 = nrow(Lot_infor)
num_rows3 = nrow(Exterior)
# creating ID column vector
train_data_nzero$ID <- c(1:num_rows)
Lot_infor$ID <- c(1:num_rows2)
Exterior$ID <- c(1:num_rows3)
#Merge Datasets
df_list2<-list(train_data_nzero,Lot_infor,Exterior)
train_clean<-Reduce(function(x,y) merge(x,y,all=TRUE),df_list2)
View(train_clean)
#Remove concatenated variables
train_clean <- train_clean[ , ! names(train_clean) %in% c("Exterior","LotInfo")]
#Missing values
sapply(train_clean, function(x) sum(is.na(x)))
#Remove KitchenAbvGr since it is not in all data sets
train_clean <- train_clean[ , ! names(train_clean) %in% c("KitchenAbvGr")]
#Remove LotFront
with(train_clean, plot(LotFront, SalePrice))
train_clean <- train_clean[ , ! names(train_clean) %in% c("LotFront")] 

train_dummy<-data.frame(train_clean)

#Remove Missing Values
sapply(train_clean, function(x) sum(is.na(x)))
train_clean$BsmtQual[is.na(train_clean$BsmtQual)] <- "None"
train_clean$GarageType[is.na(train_clean$BsmtQual)] <- "None"
train_clean$BsmtFinType1[is.na(train_clean$BsmtFinType1)] <- "None"
                                                                                                                         


#Create dummy variables 
#Central air y=1, n=0
train_dummy$CentralAir <- ifelse(train_dummy$CentralAir == "Y", 1, 0)
#GarageTypes
table(train_dummy$GarageType, useNA = 'always')
train_dummy$GarType_Att <- ifelse(train_dummy$GarageType == "Attchd", 1, 0)
train_dummy$GarType_Det <- ifelse(train_dummy$GarageType == "Detchd", 1, 0)
train_dummy$GarType_Att[is.na(train_dummy$GarType_Att)] <- 0
train_dummy$GarType_Det[is.na(train_dummy$GarType_Det)] <- 0
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("GarageType")] 
#SaleType
table(train_dummy$SaleType, useNA = 'always')
train_dummy$SaleType <- ifelse(train_dummy$SaleType == "WD", 1, 0)
#RoofStyle Gable=1 NonGable = 0
table(train_dummy$RoofStyle, useNA = 'always')
train_dummy$RoofStyle <- ifelse(train_dummy$RoofStyle == "Gable", 1, 0)
#Electrical SBrkr=1 Fuse=0
table(train_dummy$Electrical, useNA = 'always')
train_dummy$Electrical <- ifelse(train_dummy$Electrical == "SBrkr", 1, 0)
#Foundation
table(train_dummy$Foundation, useNA = 'always')
p2 <- ggplot(train_dummy, aes(x=Foundation, y=SalePrice)) + 
  geom_boxplot()
p2
train_dummy$Foundation_Brktil <- ifelse(train_dummy$Foundation == "BrkTil", 1, 0)
train_dummy$Foundation_CBlock <- ifelse(train_dummy$Foundation == "CBlock", 1, 0)
train_dummy$Foundation_PConc <- ifelse(train_dummy$Foundation == "PConc", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("Foundation")] 
#BldgType
table(train_dummy$BldgType, useNA = 'always')
p3 <- ggplot(train_dummy, aes(x=BldgType, y=SalePrice)) + 
  geom_boxplot()
p3
train_dummy$BldgType_1Fam <- ifelse(train_dummy$BldgType == "1Fam", 1, 0)
train_dummy$BldgType_2fmCon <- ifelse(train_dummy$BldgType == "2fmCon", 1, 0)
train_dummy$BldgType_Duplex <- ifelse(train_dummy$BldgType == "Duplex", 1, 0)
train_dummy$BldgType_Twnhs <- ifelse(train_dummy$BldgType == "Twnhs", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("BldgType")] 
#BsmtQual
table(train_dummy$BsmtQual, useNA = 'always')
train_dummy$BsmtQual[is.na(train_dummy$BsmtQual)] <- "NA"
train_dummy$BsmtQual_Ex <- ifelse(train_dummy$BsmtQual == "Ex", 1, 0)
train_dummy$BsmtQual_Fa <- ifelse(train_dummy$BsmtQual == "Fa", 1, 0)
train_dummy$BsmtQual_Gd <- ifelse(train_dummy$BsmtQual == "Gd", 1, 0)
train_dummy$BsmtQual_TA <- ifelse(train_dummy$BsmtQual == "TA", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("BsmtQual")] 
#HeatingQC
table(train_dummy$HeatingQC, useNA = 'always')
train_dummy$HeatingQC_Ex <- ifelse(train_dummy$HeatingQC == "Ex", 1, 0)
train_dummy$HeatingQC_Fa <- ifelse(train_dummy$HeatingQC == "Fa", 1, 0)
train_dummy$HeatingQC_Gd <- ifelse(train_dummy$HeatingQC == "Gd", 1, 0)
train_dummy$HeatingQC_TA <- ifelse(train_dummy$HeatingQC == "TA", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("HeatingQC")]
#BsmtFinType1
table(train_dummy$BsmtFinType1, useNA = 'always')
train_dummy$BsmtFinType1[is.na(train_dummy$BsmtFinType1)] <- "NA"
#Extcond Code
table(train_dummy$ExtCond, useNA = 'always')
train_dummy$ExtCond <- ifelse(train_dummy$ExtCond == "Gd", 1, 0)
#ExtQual  Code
table(train_dummy$ExtQual, useNA = 'always')
train_dummy$ExtQual <- ifelse(train_dummy$ExtQual == "Gd", 1, 0)
#Exterior1st Code
table(train_dummy$Exterior1st, useNA = 'always')
train_dummy$Ext1st_met <- ifelse(train_dummy$Exterior1st == "MetalSd", 1, 0)
train_dummy$Ext1st_vinyl <- ifelse(train_dummy$Exterior1st == "VinylSd", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("Exterior1st")] 
#LotShape Code
table(train_dummy$LotShape, useNA = 'always')
train_dummy$LotShape_reg <- ifelse(train_dummy$LotShape == "Reg", 1, 0)
train_dummy$LotShape_ir1 <- ifelse(train_dummy$LotShape == "IR1", 1, 0)
train_dummy$LotShape_ir2 <- ifelse(train_dummy$LotShape == "IR2", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("LotShape")]
#Lotconfig code
table(train_dummy$Lotconfig, useNA = 'always')
train_dummy$Lotconfig_Inside <- ifelse(train_dummy$Lotconfig == "Inside", 1, 0)
train_dummy$Lotconfig_corner <- ifelse(train_dummy$Lotconfig == "Corner", 1, 0)
train_dummy$Lotconfig_culdsac <- ifelse(train_dummy$Lotconfig == "CulDSac", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("Lotconfig")]
#KitchenQuality code
table(train_dummy$KitchenQual, useNA = 'always')
train_dummy$KitchenQual_Ex <- ifelse(train_dummy$KitchenQual == "Ex", 1, 0)
train_dummy$KitchenQual_Fa <- ifelse(train_dummy$KitchenQual == "Fa", 1, 0)
train_dummy$KitchenQual_Gd <- ifelse(train_dummy$KitchenQual == "Gd", 1, 0)
train_dummy$KitchenQual_TA <- ifelse(train_dummy$KitchenQual == "TA", 1, 0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("KitchenQual")]
#PavedDrive code
table(train_dummy$PavedDrive, useNA = 'always')
train_dummy$PavedDrive_Y<-ifelse(train_dummy$PavedDrive=="Y",1,0)
train_dummy$PavedDrive_N<-ifelse(train_dummy$PavedDrive=="N",1,0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("PavedDrive")]
#Heating code
table(train_dummy$Heating, useNA = 'always')
train_dummy$Heating_GasA<-ifelse(train_dummy$Heating=="GasA",1,0)
train_dummy$Heating_GasW<-ifelse(train_dummy$Heating=="GasW",1,0)
train_dummy$Heating_Grav<-ifelse(train_dummy$Heating=="Grav",1,0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("Heating")]
#BsntFinType1 code
table(train_dummy$BsmtFinType1, useNA = 'always')
train_dummy$BsmtFinType1_ALQ<-ifelse(train_dummy$BsmtFinType1=="ALQ",1,0)
train_dummy$BsmtFinType1_BLQ<-ifelse(train_dummy$BsmtFinType1=="BLQ",1,0)
train_dummy$BsmtFinType1_GLQ<-ifelse(train_dummy$BsmtFinType1=="GLQ",1,0)
train_dummy$BsmtFinType1_Unf<-ifelse(train_dummy$BsmtFinType1=="Unf",1,0)
train_dummy <- train_dummy[ , ! names(train_dummy) %in% c("BsmtFinType1")]
train_dummy$house_story<-as.numeric(train_dummy$house_story)
#Neigh
train_dummy$Neigh <- ifelse(train_dummy$Neigh == 'College'| train_dummy$Neigh == 'CollegeTest', 1,
                    ifelse(train_dummy$Neigh == 'Edwards'|train_dummy$Neigh == 'EdwardsTest', 2, 0))

summary(train_dummy)

train_dummy <- train_dummy %>% relocate(uniqueID, .before = SalePrice)

#Correlation
cor(train_dummy[sapply(train_dummy, is.numeric)])

#Transform Data
train_numeric <- train_dummy[c("YearBuilt","YrSold","OpenPorchSF",
                              "BsmtFinSF1","WoodDeckSF","GrLivArea","LotArea")]
pp_zscore <- preProcess(train_numeric, method = c("center", "scale"))
train_numeric_zscore <- predict(pp_zscore, train_numeric)
train_numeric_zscore$ID <- c(1:num_rows3)
names(train_numeric_zscore)<-c("YrBuilt_T","YrSold_T","OpenPorchSF_T","BsmtFinSF1_T",
                               "WoodDeckSF_T","GrLivArea_T","LotArea_T","ID")

#Merge transformed into training data
df_list4<-list(train_dummy,train_numeric_zscore)
train_tran<-Reduce(function(x,y) merge(x,y,all=TRUE),df_list4)
View(train_tran)
train_tran <- train_tran[ , ! names(train_tran) %in% c("YearBuilt","YrSold",
                                                       "OpenPorchSF","BsmtFinSF1","WoodDeckSF",
                                                       "GrLivArea","LotArea")]

#Remove Test Data
test_tran<-train_tran[is.na(train_tran$SalePrice), ] 
test_dummy<-train_dummy[is.na(train_dummy$SalePrice), ] 
test_clean<-train_clean[is.na(train_clean$SalePrice), ] 

#Export Test Data
write.csv(test_tran,file ="C:/Users/eudoc/Documents/Statistics/STAT580/Project2/test_tran.csv", row.names = FALSE)
write.csv(test_dummy,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/test_dummy.csv", row.names = FALSE)
write.csv(test_clean,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/test_clean.csv", row.names = FALSE)

#Remove Rows for test data in training data
train_clean<-train_clean[!is.na(train_clean$SalePrice), ]
train_dummy<-train_dummy[!is.na(train_dummy$SalePrice), ]
train_tran<-train_tran[!is.na(train_tran$SalePrice), ]

#Export Training Data
write.csv(train_tran,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_tran.csv", row.names = FALSE)
write.csv(train_dummy,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_dummy.csv", row.names = FALSE)
write.csv(train_clean,"C:/Users/eudoc/Documents/Statistics/STAT580/Project2/train_clean.csv", row.names = FALSE)

#Create test and training data sets
set.seed(23)
sale_sample_vector <- createDataPartition(train_tran$SalePrice, p = 0.8, list = FALSE)
sale_test<-train_tran[-sale_sample_vector,]
sale_train<-train_tran[sale_sample_vector,]

#Remove ID for analysis
sale_train <- sale_train[ , ! names(sale_train) %in% c("ID")]
sale_test<-sale_test[, ! names(sale_test) %in% c("ID")]

#Fitting Regression Tree
tree_sale<-tree(formula=SalePrice~.,sale_train, na.action = "na.pass")
summary(tree_sale)
plot(tree_sale)
text(tree_sale, pretty=0)

#Create Training  and Test Data Sets
train_tree <- sale_train[c("OverallQual","KitchenQual_TA","GrLivArea_T","OverallCond",
                               "YrBuilt_T","Foundation_CBlock","BsmtFinSF1_T","TotRmsAbvGrd","SalePrice")]
test_tree<-sale_test[c("OverallQual","KitchenQual_TA","GrLivArea_T","OverallCond",
                       "YrBuilt_T","Foundation_CBlock","BsmtFinSF1_T","TotRmsAbvGrd","SalePrice")]

#PCA Analysis
sale_pca<-train_tran
sale_pca <- sale_pca[ , ! names(sale_pca) %in% c("SalePrice","ID")]
pp_pca <- preProcess(sale_pca,  method = c("BoxCox", "center", "scale", "pca"), pcaComp=4)
sale_tran_pca <- predict(pp_pca, sale_pca)
head(sale_tran_pca, n = 5)
options(digits = 2)
pp_pca$rotation
pp_pca_var <- apply(sale_tran_pca, 2, var)
sale_pca_var <- data.frame(Variance = round(100 * pp_pca_var / sum(pp_pca_var), 2), CumulativeVariance = round(100 * cumsum(pp_pca_var) / sum(pp_pca_var), 2))
sale_pca_var

plot(c(1:4),sale_pca_var$Variance, xlab="Principal Component",ylab="Variance Explained",
     main="Scree Plot")
lines(sale_pca_var$Variance, lty=1)

sale_train_pca <- sale_tran_pca[sale_sample_vector,]
sale_test_pca<-sale_tran_pca[-sale_sample_vector,]

#Factorial Data sets
train_factor <- sale_train[c("OverallQual","Fireplaces","KitchenQual_Ex","GrLivArea_T","OverallCond",
                           "YrBuilt_T","GarType_Att","FullBath","SalePrice",
                           "BldgType_Twnhs","LotShape_ir2","LotArea_T")]
test_factor<-sale_test[c("OverallQual","Fireplaces","KitchenQual_Ex","GrLivArea_T","OverallCond",
                       "YrBuilt_T","GarType_Att","FullBath","SalePrice",
                       "BldgType_Twnhs","LotShape_ir2","LotArea_T")]

#Export Datasets as csv
write.csv(train_tree,"Statistics/STAT580/Project2/train_tree.csv", row.names = FALSE)
write.csv(test_tree,"Statistics/STAT580/Project2/test_tree.csv", row.names = FALSE)
write.csv(train_tran,"Statistics/STAT580/Project2/train_tran.csv", row.names = FALSE)
write.csv(sale_train_pca,"Statistics/STAT580/Project2/sale_train_pca.csv", row.names = FALSE)
write.csv(sale_test_pca,"Statistics/STAT580/Project2/sale_test_pca.csv", row.names = FALSE)
write.csv(train_factor,"Statistics/STAT580/Project2/train_factor.csv", row.names = FALSE)
write.csv(test_factor,"Statistics/STAT580/Project2/test_factor.csv", row.names = FALSE)