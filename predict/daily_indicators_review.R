

#ver : 2023-07-14

#pkg set
mypkg <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if(length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}


pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "pROC", "readxl", "MASS", "Epi", "caTools", "DAAG", "caret", "psych")

mypkg(pkg)


#data set

data_day <- read_excel("C:/Users/kangmo/Documents/github/r_windows/predict/actual_day.xlsx")

head(data_day)
dim(data_day)
str(data_day)

table(data_day$dlvry_rgn1_nm)

#�Ļ����� ���� : ����, �ֹ��� ��� ���̴���
data_day <- data_day %>%
    mutate(weekday = weekdays(as.Date(business_day)),
           ord_by_rider = ord_cnt/rider_cnt)



var <- c('weekday', 'dlvry_rgn1_nm')


data_day[, var] <- lapply(data_day[, var], factor)



rgn1_nm_values <- unique(data_day$dlvry_rgn1_nm)



results_dlvry <- data.frame(rgn1 = character(), r2 = numeric(), coef = numeric(), range = numeric(), stringsAsFactors = FALSE)
results_consign <- data.frame(rgn1 = character(), r2 = numeric(), coef = numeric(), range = numeric(), stringsAsFactors = FALSE)
results_over_20 <- data.frame(rgn1 = character(), r2 = numeric(), coef = numeric(), range = numeric(), stringsAsFactors = FALSE)


#�� ���� : ��޽ð�, �����ҿ�ð�, �����ȳ��ð�+20���ʰ���


#��޽ð� ��
get_dlvry_lm <- function(data){
    model_dlvry <- lm(avg_dlvry_diff ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
    r2 <- summary(model_dlvry)$adj.r.squared
    coef <- coefficients(model_dlvry)
    range <- 1.96 * summary(model_dlvry)$sigma
    result <- data.frame(r2, coef, range)
    result
}

#��޽ð�
for (rgn1 in rgn1_nm_values) {
    filtered_data <- data_day %>%
        filter(dlvry_rgn1_nm == rgn1)
    result_value <- get_dlvry_lm(filtered_data)
    result_row <- data.frame(rgn1 = rgn1, result_value)
    results_dlvry <- rbind(results_dlvry, result_row)
    
}






#�����ҿ�ð���
get_consign_lm <- function(data){
    model_consign <- lm(avg_consign_diff ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
    r2 <- summary(model_consign)$adj.r.squared
    coef <- coefficients(model_consign)
    range <- 1.96 * summary(model_consign)$sigma
    result <- data.frame(r2, coef, range)
    result
}

#�����ҿ�ð�
for (rgn1_nm in rgn1_nm_values) {
    filtered_data <- data_day %>%
        filter(rgn1_nm == rgn1_nm)
    result_value <- get_consign_lm(filtered_data)
    result_row <- data.frame(rgn1_nm = rgn1_nm, result_value)
    results_consign <- rbind(results_consign, result_row)
}


#�����ȳ��ð�+20���ʰ��� ��
get_over_20_lm <- function(data){
    model_over_20 <- lm(over_20 ~ rider_cnt + ord_cnt + weekday + mean_fee, data = data)
    r2 <- summary(model_over_20)$adj.r.squared
    coef <- coefficients(model_over_20)
    range <- 1.96 * summary(model_over_20)$sigma
    result <- data.frame(r2, coef, range)
    result
}


#���Ƚ�+20���ʰ�
for (rgn1_nm in rgn1_nm_values) {
    filtered_data <- data_day %>%
        filter(rgn1_nm == rgn1_nm)
    result_value <- get_over_20_lm(filtered_data)
    result_row <- data.frame(rgn1_nm = rgn1_nm, result_value)
    results_over_20 <- rbind(results_over_20, result_row)
}


results_dlvry
results_consign
results_over_20

file_results_dlvry <- write.csv(results_dlvry, "results_dlvry.csv", fileEncoding = "CP949")

file_results_consign <- write.csv(results_consign, "results_consign.csv", fileEncoding = "CP949")

file_results_over_20 <- write.csv(results_over_20, "results_over_20.csv", fileEncoding = "CP949")