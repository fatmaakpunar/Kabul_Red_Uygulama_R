#hedef dağılımı
f <- function(x){
  (1/sqrt(2*pi))*exp(-0.5*x^2)
}

#Aday Dağılım
g <- function(x){
  0.5 * exp(-abs(x))
}

#c sabitini belirleme
xd <- seq(-10, 10, length.out=10000)
ad <- f(x)/g(x)
c <- max(ad)

#simülasyon
n <- 100000
samples <- numeric(n)
i <- 0
k <- 0

while(i<n){
  k <- k+1
  u_aday <- runif(1)
  y <- (ifelse(u_aday < 0.5, #ters dönüşüm
                        log(2 * u_aday), # Senaryo 1 (u < 0.5)
                        -log(2 * (1 - u_aday))) # Senaryo 2 (u >= 0.5)
        u_test <- runif(1)
        oran <- f(y)/(c*g(y))
        if (u_test <= oran){ #x_i=y
          i <- i+1
          samples[i] <- y
          }
        }

#sonuç ve tablo
cat("Kabul Oranı:", n / k)
hist(samples, breaks = 70, prob = TRUE, col = "seagreen1", border = "white",
     main = "Laplace Önerili Kabul-Ret")
curve(f(x), add = TRUE, col = "darkred", lwd = 2)
        
  
