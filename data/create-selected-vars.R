# filter variables.lu for select categories and return only variables that appeared on all surveys

t <- variables.lu[category %in% c('Travel', 'Worker', 'Reason for leaving previous residence', 'Household', 'Person', 'Transit Payment Method')][, count:= 1]

x <- dcast.data.table(t, category + variable + variable_name ~ survey_year, fun.aggregate = sum, value.var = 'count')

y <- x[`2017` == 1 & `2019` == 1 & `2021` == 1, .(category, variable)]

write.csv(y, 'data/vars.csv', row.names = FALSE)










