```{r, echo=FALSE, results='hide',message=FALSE}
#Limpar Memoria
rm(list=ls())
# Iniciar o Contador para medicao do tempo do calculo
ptm <- proc.time()
# Numero de Digitos 
options(digits=11)
# Pacotes utilizados:
library("lubridate")
library("reshape2")
library("sqldf")
# importacao das tabelas para o calculo #
base <- read.delim("base.csv")
JUROS <- read.delim("JUROS.csv")
CRESCSAL <- read.delim("CRESCSAL.csv")
PCT <- read.table("PCT.csv", header=T, quote="\"")
TABUAVALIDOS <- read.csv("TABUAVALIDOS.csv")
TABUAENTINVALIDEZ <- read.delim("TABUAENTINVALIDEZ.csv")
TABUAINVALIDOS <- read.delim("TABUAINVALIDOS.csv")
DATABASE <- read.table("DATABASE.csv", header=T, quote="\"")
# Categorizacao das Variaveis #
# PERCETUAL DE CASADOS #
PC<-data.frame(pc=numeric(1))
PC$pc<-0.8
# TABELA base #
class(base$NASC)
base$NASC<-as.POSIXct(as.character(base$NASC), format = "%d/%m/%Y")
class(base$NASC)
class(base$ADM)
base$ADM<-as.POSIXct(as.character(base$ADM), format = "%d/%m/%Y")
class(base$ADM)
class(base$ADES)
base$ADES<-as.POSIXct(as.character(base$ADES), format = "%d/%m/%Y")
class(base$ADES)
class(DATABASE$database)
DATABASE$database<-as.POSIXct(as.character(DATABASE$database), format = "%d/%m/%Y")
class(DATABASE$database)
base$IDADE <- floor(new_interval(base$NASC, DATABASE$database) / duration(num = 1, units = "years"))
#base$IDADE <- (new_interval(base$NASC, DATABASE$database) / duration(num = 1, units = "years"))
class(base$IDADE)
class(base$SEXO)
class(base$TVP)
# FIM TABELA base #
# INICIO TABELA JUROS #
class(JUROS$JUROS)
class(JUROS$ANO)
JUROS$ANO<-as.POSIXct(as.character(JUROS$ANO), format = "%d/%m/%Y")
class(JUROS$ANO)
# FIM TABELA JUROS #
# INICIO TABELA CRESCSAL #
class(CRESCSAL$CRES_SAL)
class(CRESCSAL$ANO)
CRESCSAL$ANO<-as.POSIXct(as.character(CRESCSAL$ANO), format = "%d/%m/%Y")
class(CRESCSAL$ANO)
# FIM TABELA CRESCSAL#
# INICIO TABELA PCT #
class(PCT$PCT)
# FIM TABELA PCT #
# TABUAS #
class(TABUAVALIDOS$qx_validos)
as.numeric()
is.numeric(TABUAVALIDOS$qx_validos)
class(TABUAENTINVALIDEZ$ix)
class(TABUAINVALIDOS$qx_invalidos)
# FIM TABUAS #
# DESCONTO #
j.f<-data.frame(V = numeric(nrow(JUROS)+1))
j.f$seq<-seq(1, 122, by = 1)
JUROS$seq<-seq(1, 121, by = 1)
for(i in 1:(nrow(j.f)))
{
  j.f$V[i]<-1/(1+JUROS$JUROS[i])**(JUROS$seq[i])
}
for(i in 1:nrow(JUROS))
{
  for(k in 1:nrow(JUROS)){
       j.f$COMP[1]<-(1+JUROS$JUROS[1])
        j.f$COMP[i+1]<-j.f$COMP[i]*(1+JUROS$JUROS[k]) 
  }
}
# FIM DESCONTO #
# PROBABILIDADES #
TABUAVALIDOS$px<-(1-TABUAVALIDOS$qx_validos)
for(i in 1:115)
{
  TABUAVALIDOS$lx[1]<-1000000
  TABUAVALIDOS$lx[i+1]<-TABUAVALIDOS$lx[i]*(TABUAVALIDOS$px[i+4]) 
}
TABUAOK<-subset(TABUAVALIDOS,Idade<112)
class(TABUAVALIDOS$lx)
for(i in 1:111)
{
  TABUAOK$ly[1]<-1000000
   TABUAOK$ly[i+1]<-TABUAOK$ly[i]*(TABUAOK$px[i]) 
}
class(TABUAOK$ly)
for(n in 1:111)
{
  TABUAOK$dx[n]<-TABUAOK$lx[n]-TABUAOK$lx[n+1]
}
class(TABUAOK$dx)
TABUAOK$Dx<-TABUAOK$lx[1:112]*j.f$V[1:112]
for(i in 1:111)
{
  TABUAOK$Nx[i]<-sum(TABUAOK$Dx[i:nrow(TABUAOK)])
}
TABUAOK$Cx<-TABUAOK$dx[1:112]*j.f$V[1:112]
for(o in 1:nrow(TABUAOK))
{
  TABUAOK$Mx[o]<-sum(TABUAOK$Cx[o:nrow(TABUAOK)])
}
TABUAOK$lxy<-TABUAOK$lx*TABUAOK$ly
TABUAOK$Dxy<-TABUAOK$lxy[1:112]*j.f$V[1:112]
for(i in 1:nrow(TABUAOK))
{
  TABUAOK$Nxy[i]<-sum(TABUAOK$Dxy[i:nrow(TABUAOK)])
}
base$DATAAPOS <- base$ADM + 1103760000  
base$numerador_puc <- new_interval(base$ADM, DATABASE$database) / duration(num = 1, units = "years")
base$denominador_puc <- new_interval(base$ADM,base$DATAAPOS) / duration(num = 1, units = "years")
base$num_puc <- floor(new_interval(base$ADM, DATABASE$database) / duration(num = 1, units = "years"))
base$idade_na_apos <- floor(new_interval(base$NASC, base$DATAAPOS) / duration(num = 1, units = "years"))
# FIM CALCULO PUC #
# INICIO CALCULO PROVISAO BENEFICIO A CONCEDER APOSENTADORIA NORMAL #
base$tempo_ate_apos<-base$idade_na_apos-base$IDADE
basef<-sqldf("select * from base where num_puc>0 AND idade_na_apos<115 AND tempo_ate_apos>0")
APOSNORM<-data.frame(saldo = numeric(nrow(basef)),ben_min = numeric(nrow(basef)),anuidade_titular = numeric(nrow(basef)),anuidade_conjuge = numeric(nrow(basef)),anuidade_conjunta = numeric(nrow(basef)),puc = numeric(nrow(basef)),nPx = numeric(nrow(basef)),Vx = numeric(nrow(basef)),provisao_casados = numeric(nrow(basef)),provisao_solteiros = numeric(nrow(basef)),provisao_total = numeric(nrow(basef)))
basef$IDADE_CONJUGE<-basef$IDADE-4
APOS<-merge(basef,TABUAOK,by.x="IDADE",by.y="Idade")
APOS2<-merge(basef,TABUAOK,by.x="IDADE_CONJUGE",by.y="Idade")
APOS3<-merge(basef,TABUAOK,by.x="idade_na_apos",by.y="Idade")
APOS4<-merge(basef,j.f,by.x="tempo_ate_apos",by.y="seq",all)
APOS5<-merge(basef,j.f,by.x="IDADE",by.y="seq",all)
APOSNORM$anuidade_titular<-APOS$Nx/APOS$Dx-(11/24)
APOSNORM$anuidade_conjuge<-APOS2$Nx/APOS2$Dx-(11/24)
APOSNORM$anuidade_conjunta<-APOS$Nxy/APOS$Dxy-(11/24)  
APOSNORM$nPx<-APOS3$lx/APOS$lx
APOSNORM$Vx<-APOS4$V
APOSNORM$saldo<-APOS4$SALDO*APOS4$COMP+APOS4$CONTRIB*13*APOS4$COMP
puc<-function(x,y){x/y}  
APOSNORM$puc<-mapply(puc,basef$numerador_puc,basef$denominador_puc)
ben_min<-function(x){0.9*x}
                            
APOSNORM$ben_min<-mapply(ben_min,basef$SALARIO)
# Tratamento para os que possuem rateio PUC maiores que 1 #
for (p in 1:nrow(APOSNORM)){
   if(APOSNORM$puc[p]>1){
       APOSNORM$puc[p]<-1
  }
  else{
    APOSNORM$puc[p]<-APOSNORM$puc[p]
  }
}
apos_cas<-function(x,y,z,k,w,t,u) {13*x*(y+PCT$PCT[1]*(z-k))*w*t*u*PC$pc[1]}
APOSNORM$provisao_casados<-mapply(apos_cas,APOSNORM$ben_min,APOSNORM$anuidade_titular,APOSNORM$anuidade_conjuge,APOSNORM$anuidade_conjunta,APOSNORM$puc,APOSNORM$nPx,APOSNORM$Vx)
apos_solt<-function(x,y,z,w,t) {13*x*y*z*w*t*(1-PC$pc[1])}
APOSNORM$provisao_solteiros<-mapply(apos_solt,APOSNORM$ben_min,APOSNORM$anuidade_titular,APOSNORM$puc,APOSNORM$nPx,APOSNORM$Vx)
APOSNORM$provisao_total<-APOSNORM$provisao_casados+APOSNORM$provisao_solteiros
sum(APOSNORM$provisao_total)
#FIM CALCULO PROVISAO BENEFICIO A CONCEDER APOSENTADORIA NORMAL #
PMBAC<-data.frame(aposentadoria_normal=numeric(1),invalidez=numeric(1),pensao=numeric(1),peculio=numeric(1),reserva_total_beneficio_a_conceder=numeric(1))
PMBAC$aposentadoria_normal<-sum(APOSNORM$provisao_total)
PMBAC$invalidez<-sum(APOSINV$provisao_inv_total)
PMBAC$pensao<-sum(PEN_ATIVOS$provisao_pen)
PMBAC$peculio<-sum(PEC_ATIVO$provisao_pec)
PMBAC$reserva_total_beneficio_a_conceder<-PMBAC$aposentadoria_normal+PMBAC$invalidez+PMBAC$pensao+PMBAC$peculio
# Parar o contador
proc.time() - ptm
# FIM SALVAR PROVISOES TOTAIS NA MEMORIA #
```
```{r, echo=FALSE,message=FALSE, results='asis', prompt=FALSE, dpi=600}
library("xtable")
print(xtable(PMBAC, caption="Provisao Matematica de Beneficio a Conceder", digits=3), 
      comment = FALSE,
      include.rownames = FALSE)
