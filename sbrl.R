library(RCurl)
library(sbrl)
library(ROCR)

#installing sbrl
# link: "https://cran.r-project.org/src/contrib/Archive/sbrl/sbrl_1.2.tar.gz"
# install.packages("~/Downloads/sbrl_1.2.tar.gz", repos = NULL, type = "source")
# lots of dependencies to install

# grabbing data from uci
census_income_text <- getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data')
census_income <- read.csv(textConnection(census_income_text), header=F, stringsAsFactors = F)

colnames(census_income) <- c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status',
                             'occupation', 'relationship', 'race', 'sex', 'capital-gain', 'capital-loss',
                             'hours-per-week', 'native-country', 'income')
# https://arxiv.org/pdf/1602.08610.pdf - SBLR paper

## Data Prep
df <- census_income
df <- replace(df,df == " ?",NA)  ##Creating Missings

#Trim leading spaces
df$workclass <- trimws(df$workclass)
df$education <- trimws(df$education)
df$`marital-status` <- trimws(df$`marital-status`)
df$occupation <- trimws(df$occupation)
df$relationship <- trimws(df$relationship)
df$race <- trimws(df$race)
df$sex <- trimws(df$sex)
df$`native-country` <- trimws(df$`native-country`)

##Create bins for numeric features
df$age <- paste(cut(df$age, 4))
df$fnlwgt <- paste(cut(df$fnlwgt, 4))
df$`education-num` <- paste(cut(df$`education-num`, 4))
df$`capital-gain` <- paste(cut(df$`capital-gain`, 4))
df$`capital-loss` <- paste(cut(df$`capital-loss`, 4))
df$`hours-per-week` <- paste(cut(df$`hours-per-week`, 4))

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)

#Create target
df$label <- ifelse(df$income==" >50K",1,0)
df$label <- as.integer(df$label)
df$income <- NULL

#Split data
set.seed(1) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
data_train <- df[sample, ]
data_test  <- df[-sample, ]

#data_train <- data_train %>% select(`capital-gain`,`marital-status`,education,label)
#data_train <- data_train %>% select(`capital-gain`,`marital-status`,label)
#data_train <- data_train %>% select(age,`marital-status`,label)

#Train Model
sbrl_model <- sbrl(data_train, iters=30000, pos_sign="1",
                   neg_sign="0", rule_minlen=1, rule_maxlen=1, 
                   minsupport_pos=0.003, minsupport_neg=0.003, 
                   lambda=1, eta=1, nchain=20)
print(sbrl_model)

#Predict on test
yhat <- predict(sbrl_model, data_test)
pred_ROCR <- prediction(yhat[2],as.factor(data_test$label))
auc_ROCR <- performance(pred_ROCR, measure = "auc")
print(auc_ROCR@y.values[[1]])