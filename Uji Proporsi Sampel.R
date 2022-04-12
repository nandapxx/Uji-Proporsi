#uji proporsi satu sample 
#uji chi kuadrat
prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)
prop.test(130, 210, 0.7, conf.level = 0.95)

#Uji Z
ztes.prop<-function(x,n,p,conf.level){
  prop=x/n
  alpha=1-conf.level
  zstar=qnorm(1-alpha/2)
  SE=(prop*(1-prop)/n)^0.5
  zhitung=(prop-p)/SE
  pvalue=1-pnorm(abs(zhitung))
  Ringkasan<-
    data.frame(n=c(n),sukses=c(x),proporsi=c(prop),StandarError=c(SE),
               zhitung=c(zhitung),pvalue=c(pvalue))
  Ringkasan$Kesimpulan<-ifelse(Ringkasan$pvalue>=0.05,c("H0 Diterima"),c("Ho
Ditolak"))
  print(Ringkasan)
}
ztes.prop(130, 210, 0.7, 0.95)

#uji proporsi dua sample
# uji ch kuadrat
prop.test(c(a,b),c(n1,n2))
prop.test(c(45,56),c(45+35,56+47))

#uji z
ztes.prop2<-function(x1,x2,n1,n2,conf.level){
  prop1=x1/n1;prop2=x2/n2
  alpha=1-conf.level
  zstar=qnorm(1-alpha/2)
  SE=(prop1*(1-prop1)/n1+prop2*(1-prop2)/n2)^0.5
  zhitung=(prop1-prop2)/SE
  pvalue=1-pnorm(abs(zhitung))
  BB=(prop1-prop2)-zstar*SE
  BA=(prop1-prop2)+zstar*SE
  Ringkasan<-
    data.frame(n=c(n1,n2),sukses=c(x1,x2),proporsi=c(prop1,prop2),StandarError=c(SE),
               zhitung=c(zhitung),pvalue=c(pvalue),BatasAtas=c(BA),BatasBawah=c(BB))
  Ringkasan$Kesimpulan<-ifelse(Ringkasan$pvalue>=0.05,c("H0 Diterima"),c("Ho Ditolak"))
  print(Ringkasan)
}
ztes.prop2(45, 56, 80, 103, 0.95)
           
