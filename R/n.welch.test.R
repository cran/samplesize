`n.welch.test` <-
function(power = 0.80, alpha = 0.95, mean.diff = 2, sd.est1 = 1, sd.est2 = 2.65)
{power = power
n.start = 4
alpha = alpha
mean.diff = mean.diff
sd.est1 = sd.est1
sd.est2 = sd.est2
n.start = n.start
k <- sd.est2/sd.est1
n1.pri <- n.start/(1 + k)
n2.pri <- (k * n.start)/(1 + k)
n1 <- max(n1.pri, 2)
n2 <- max(n2.pri, 2)
gamma <- sd.est1/(sd.est1 + sd.est2)
c <- mean.diff/(sd.est1 + sd.est2)
df_approx <- 1/ ((gamma)^2/ (n1 - 1) + (1 - gamma)^2/ (n2-1))
tkrit.alpha <- qt(alpha, df = df_approx)
tkrit.beta <- qt(power, df = df_approx)
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)
while(n.start <= n.temp)
{n.start <- n1 + n2 + 1
n1 <- n.start/(1 + k)
n2 <- (k * n.start)/(1 + k)
df_approx <- 1/ ((gamma)^2/ (n1 - 1) + (1 - gamma)^2/ (n2-1))
tkrit.alpha <- qt(alpha, df = df_approx)
tkrit.beta <- qt(power, df = df_approx)
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)
}
cat("", "\n", "\t")
cat("sample.size:", ceiling(n.start), "\n", "\t")

cat("sample.size n1:", ceiling(n1), "\n", "\t")

cat("sample.size n2:", ceiling(k * n1), "\n", "\t")
cat("", "\n")
}

