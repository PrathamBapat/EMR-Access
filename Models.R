####### ORDINAL TARGET
data_ord <- subset(data_final, select = -c(MUL_target, bin_target)) # Remove the Multinomial/Binary target columns
data_ord$ord_target <- as.ordered(data_ord$ord_target)
str(data_ord)

library(MASS)
ord <- polr(ord_target ~ .,  data = data_ord)

ord_ctable <- coef(summary(ord))
p <- pnorm(abs(ord_ctable[, 't value']), lower.tail = FALSE) * 2
ord_ctable <- cbind(ord_ctable, "p value" = p)


####### BINARY TARGET
data_bin <- subset(data_final, select = -c(MUL_target, ord_target)) # Remove the Multinomial/Binary target columns
str(data_bin)
logg <- glm(bin_target~., data = data_bin, family = "binomial")
summary(logg)
