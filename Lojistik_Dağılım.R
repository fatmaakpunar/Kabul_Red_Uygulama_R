#Algoritma:
#i=0, k=0, i=n ise dur
#1.	Adım=> y, g(y) den üret
#2.	Adım =>uniformdan bağımsız üret
#3.	Eğer U<=f(y)/c*g(y) xi=y ise i=i+1, k=k+1 değilse k=k+1 ve 0. Adıma git

# Hedef Fonksiyon (Normal Dağılım)
f <- function(x) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * x^2)
}

# Aday Fonksiyonu (Lojistik Dağılım)
g <- function(x) {
  exp(-x) / (1 + exp(-x))^2
}

# c Sabitini Belirleme
# f(x)/g(x) oranının maksimum olduğu noktayı buluyoruz
xd <- seq(-10, 10, length.out = 1000)
ad <- f(xd) / g(xd)
c <- max(ad) #1.595729

#Simülasyon
n <- 100000
samples <- numeric(n)
i <- 0    # i = 0 (Kabul edilenlerin sayısı)
k <- 0   # k = 0 (Toplam üretilen aday sayısı)

while(i < n) { # i = n ise dur (n'e ulaşana kadar dön)
  k <- k + 1
  # Lojistik dağılımdan aday üretme (y)
  u_aday <- runif(1)
  x_candidate <- log(u_aday / (1 - u_aday)) #y
  
  # Kabul-Ret testi için ikinci uniform
  u_test <- runif(1) #U
  
  # Oran hesaplama (f(y) / c*g(y))
  oran <- f(x_candidate) / (c * g(x_candidate))
  
  if(u_test <= oran) { #Eğer U <= f(y) / (c*g(y)) ise
    i <- i + 1
    samples[i] <- x_candidate # x_i = y (Adayı listeye kaydet)
  } #if sonu
} #Değilse k = k + 1 ve 0. Adıma git

# Sonuçları Kontrol Etme
hist(samples, breaks = 60, prob = TRUE, 
     main = "Manuel Lojistik Önerili Kabul-Ret",
     col = "lightblue", border = "white")
curve(f(x), add = TRUE, col = "red", lwd = 2)
