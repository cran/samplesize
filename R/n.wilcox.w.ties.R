`n.wilcox.w.ties` <-
function(beta = 0.20, alpha = 0.05, t = 0.53, p = c(0.66, 0.15, 0.19), q = c(0.61, 0.23, 0.16))
{power = power
 alpha = alpha/ 2
 t = t
         p = p
         q = q
         
         Z1 <- qnorm(alpha)
         Z2 <- qnorm(beta)
     
pq1 <- function(p, q)
{
 D <- length(p)
if(length(q)!=D)
 {stop("p and q must be vectors of equal length")}
 PQ1 <- 0
for(i in 2:D)
 {
  PQ1<-PQ1+p[i]*sum(q[1:(i-1)])
 }
 return(PQ1)
}

         p.t <- (1 - t) * p
         q.t <- t * q
         pq.t <- p.t + q.t
         pq.t.3 <- pq.t ^3
         t.sum <- sum(pq.t.3) 
         
         pq <- cbind(p,q)
         pq.sum <- sum(apply(pq, 1, prod))
         
         N <- (((Z1 + Z2)^2) * (1 - t.sum)) / (12 * t * (1 - t)*(pq1(p = p, q = q) + 0.5 * pq.sum - 0.5)^2)

         cat("","\n","\t")
 cat("sample.size:",N, "\n")}

