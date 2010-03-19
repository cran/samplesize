`n.indep.t.test.neq` <-
function(power = 0.80, alpha = 0.95, mean.diff = 0.80, sd.est = 0.83, k = 0.5)
{power = power
n.start = 3
alpha = alpha
mean.diff = mean.diff
sd.est = sd.est
n.start = n.start
k = k
c <- (mean.diff/sd.est)*(sqrt(k)/(1+k))
tkrit.alpha <- qt(alpha, df = n.start - 2)
tkrit.beta <- qt(power, df = n.start - 2)
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)

while(n.start <= n.temp)
{n.start <- n.start + 1
tkrit.alpha <- qt(alpha, df = n.start - 2)
tkrit.beta <- qt(power, df = n.start - 2)
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)
}
n1 <- n.start / (1 + k)
n2 = k * n1
return(c("sample.size:", n.start, "sample.size n.1:", round(n1,1), "sample.size n.2:", round(n2,1)))
}


