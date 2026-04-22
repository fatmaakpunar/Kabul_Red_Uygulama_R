#Hedef Fonksiyon (Standart Normal)
f <- function(x) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * x^2)
}

# Aday Fonksiyon (Cauchy Dağılımı)
g <- function(x) {
  1 / (pi * (1 + x^2))
}

# c Sabitini Belirleme
# f(x)/g(x) oranını maksimize eden c değeri
xd <- seq(-10, 10, length.out = 10000)
ad <- f(xd) / g(xd)
c <- max(ad) #1.520346 

#Simülasyon
n <- 100000
samples <- numeric(n) #yer aç
i <- 0
k <- 0

while(i < n) { #i=n ise dur
  k <- k + 1
  # Cauchy'den aday üretme (Ters Dönüşüm: tan(pi * (u - 0.5)))
  u_aday <- runif(1)
  x_candidate <- tan(pi * (u_aday - 0.5))
  
  # Kabul-Ret testi
  u_test <- runif(1)
  
  # Oran hesaplama (f(x) / (c * g(x)))
  oran <- f(x_candidate) / (c * g(x_candidate))
  
  if(u_test <= oran) {#Eğer U <= f(y) / (c*g(y)) ise
    i <- i + 1
    samples[i] <- x_candidate #x_i=y kaydet
  }
}
