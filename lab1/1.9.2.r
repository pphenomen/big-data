df <- data.frame(var1=c(11,21,31), var2=c(12,22,32),
var3=c(13,23,33), var4=c(14,24,34), row.names=c("case1",
"case2", "case3"))
print(df)

# 1.1
case1_values <- df["case1", c("var1", "var2", "var3")]
print(case1_values)

# 1.2
case2_values <- df["case2", ][df["case2", ] > 22]
print(case2_values)

# 1.3
case3_values <- colnames(df)[c(1,3)]
print(case3_values)

# 1.4
df$Y <- c(-1,0,1)
print(df)

# 1.5
df <- df[-which(rownames(df) == "case2"), ]
print(df)

# 1.6
df$var2 <- df$var2^3
print(df)


