`n.paired.t.test` <-
function(power = 0.80, alpha = 0.95, mean.diff = 0.80, sd.est = 0.83)
{power = power
n.start = 2
alpha = alpha
mean.diff = mean.diff
sd.est = sd.est
n.start = n.start
c <- mean.diff / sd.est
tkrit.alpha <- qt(alpha, df = n.start - 1)
tkrit.beta <- qt(power, df = n.start - 1 )
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)

while(n.start <= n.temp)
{n.start <- n.start + 1
tkrit.alpha <- qt(alpha, df = n.start - 1)
tkrit.beta <- qt(power, df = n.start - 1 )
n.temp <- ((tkrit.alpha + tkrit.beta)^2)/(c^2)
}
return(c("sample.size:", n.start))
}

