Orang.ke=c(seq(1:10))
BB.Sebelum=c(57,69,56,67,55,56,62,67,67,56)
BB.Sesudah=c(55,70,56,65,54,55,64,65,67)
data=data.frame(Orang.ke,BB.Sebelum,BB.Sesudah)
data

t.test(x=data$BB.Sebelum, y=data$BB.Sesudah,
       alternative = "greater",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

Nb = length(BB.Sesudah)
Na = length(BB.Sebelum)
Sa = sd(BB.Sebelum)
Sb = sd(BB.Sesudah)
db = Na+Nb-2

S2gab = ((Na-1)*(Sa^2) + (Nb-1)*(Sb^2))/db
S2gab

Xa = mean(BB.Sebelum)
Xb = mean(BB.Sesudah)

penyebut = sqrt(S2gab*((1/Na)+(1/Nb)))
penyebut

thit = (Xa-Xb)/penyebut
thit

ttable = qt(p=.05 , df = db, lower.tail=FALSE)
ttablemin = ttable*(-1)

if(thit>ttable || thit<ttablemin){
  print('tolak h0')
}else {
  print('terima h0')
}
