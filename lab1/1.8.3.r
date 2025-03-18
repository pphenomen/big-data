S1 <- sum(1 / ((1:50) * (2:51)))
S2 <- sum(1 / (2^(0:20)))

n <- 0:9
S3 <- sum((1+3*n) / (3^n))
count_greater_05 <- sum(seq3 > 0.5)

print(S1)
print(S2)
print(S3)
print(count_greater_05)
