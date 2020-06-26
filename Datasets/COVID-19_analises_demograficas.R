library(readxl)
library(ggplot2)
library(plotly)
COVID_SC2 = read_excel("boavista_covid_dados_abertos.xlsx")
attach(COVID_SC2)

#SELECAO DADOS SC

IDADE <- 0
SEXO <- 0
CIDADE <- 0
RECUPERADO <- 0
MORTO <- 0
M_IDADE <- 0
M_SEXO <- 0
j <- 0
k <- 0
i<- 0
for (i in 1:length(idade)){
  if(idade[i]!='NULL'){
  j = j+1
  IDADE[j] <- idade[i]
  SEXO[j] <- sexo[i]
  CIDADE[j] <- municipio[i]
  RECUPERADO[j] <- recuperados[i]
  MORTO[j] <- obito[i]
  if(obito[i]=="SIM"){
    k = k + 1
    M_IDADE[k] <- idade[i]
    M_SEXO[k] <- sexo[i]
  }
  }
}
DADOS_RELEVANTES <- data.frame(IDADE,SEXO,CIDADE,RECUPERADO,MORTO)
DADOS_MORTOS <- data.frame(M_IDADE,M_SEXO)

getdata <- function(DF1, nome_cidade, n){
  IDADE_C<- 0
  SEXO_C<- "0"
  CIDADE_C<- "0"
  RECUPERADO_C<- "0"
  MORTO_C<- "0"
  M_IDADE_C<- 0
  M_SEXO_C<- "0"
  j<-0
  k<-0
  for(i in 1:length(CIDADE)){
    if(CIDADE[i]==nome_cidade){
      j = j + 1
      IDADE_C[j] <- IDADE[i]
      SEXO_C[j] <- SEXO[i]
      CIDADE_C[j] <- CIDADE[i]
      RECUPERADO_C[j] <- RECUPERADO[i]
      MORTO_C[j] <-  MORTO[i]
      if(MORTO[i]=="SIM"){
        k = k + 1
        M_IDADE_C[k] <- IDADE[i]
        M_SEXO_C[k] <- SEXO[i]
      }
    }
  }
  if(n==1){
    return(DADOS_RELEVANTES_C <- data.frame(IDADE_C,SEXO_C,CIDADE_C,RECUPERADO_C,MORTO_C))
  }else if(n==2){
    return(DADOS_MORTOS_C <- data.frame(M_IDADE_C,M_SEXO_C))
  }
}

#SELECAO DADOS JOINVILLE
JLLE<-getdata(DADOS_RELEVANTES,"JOINVILLE", 1)
M_JLLE<-getdata(DADOS_RELEVANTES,"JOINVILLE", 2)

#SELECAO DADOS CHAPECO
CHP<-getdata(DADOS_RELEVANTES,"CHAPECO", 1)
M_CHP<-getdata(DADOS_RELEVANTES,"CHAPECO", 2)

#SELECAO DADOS CONCORDIA
CRD<-getdata(DADOS_RELEVANTES,"CONCORDIA", 1)
M_CRD<-getdata(DADOS_RELEVANTES,"CONCORDIA", 2)

#SELECAO DADOS FLOIANOPOLIS
FLP<-getdata(DADOS_RELEVANTES,"FLORIANOPOLIS", 1)
M_FLP<-getdata(DADOS_RELEVANTES,"FLORIANOPOLIS", 2)

#SELECAO DADOS ITAJAI
ITJ<-getdata(DADOS_RELEVANTES,"ITAJAI", 1)
M_ITJ<-getdata(DADOS_RELEVANTES,"ITAJAI", 2)

#SELECAO DADOS BLUMENAU
BLM<-getdata(DADOS_RELEVANTES,"BLUMENAU", 1)
M_BLM<-getdata(DADOS_RELEVANTES,"BLUMENAU", 2)

#SELECAO DADOS BALNEARIO CAMBORIU
BNC<-getdata(DADOS_RELEVANTES,"BALNEARIO CAMBORIU", 1)
M_BNC<-getdata(DADOS_RELEVANTES,"BALNEARIO CAMBORIU", 2)

#SELECAO DADOS CRICIUMA
CCM<-getdata(DADOS_RELEVANTES,"CRICIUMA", 1)
M_CCM<-getdata(DADOS_RELEVANTES,"CRICIUMA", 2)

#SELECAO DADOS PALHOCA
PLC<-getdata(DADOS_RELEVANTES,"PALHOCA", 1)
M_PLC<-getdata(DADOS_RELEVANTES,"PALHOCA", 2)

#SELECAO DADOS NAVEGANTES
NVG<-getdata(DADOS_RELEVANTES,"NAVEGANTES", 1)
M_NVG<-getdata(DADOS_RELEVANTES,"NAVEGANTES", 2)

#SELECAO DADOS JARAGUA DO SUL
JRG<-getdata(DADOS_RELEVANTES,"JARAGUA DO SUL", 1)
M_JRG<-getdata(DADOS_RELEVANTES,"JARAGUA DO SUL", 2)

cidcm <- matrix(nrow = 3,ncol = 11)
cidcm[1,]<- c("Chapec?","Conc?rdia","Florian?polis","Itaja?","Blumenau","Balne?rio Cambori?","Joinville","Crici?ma",
          "Palho?a","Navegantes","Jaragu? do Sul")
cidcm[2,]<- c(length(CHP$IDADE_C),length(CRD$IDADE_C),length(FLP$IDADE_C),length(ITJ$IDADE_C),length(BLM$IDADE_C),
              length(BNC$IDADE_C),length(JLLE$IDADE_C),length(CCM$IDADE_C),length(PLC$IDADE_C),length(NVG$IDADE_C),
              length(JRG$IDADE_C))
cidcm[3,]<- c(length(M_CHP$M_IDADE_C),length(M_CRD$M_IDADE_C),length(M_FLP$M_IDADE_C),length(M_ITJ$M_IDADE_C),
              length(M_BLM$M_IDADE_C),length(M_BNC$M_IDADE_C),length(M_JLLE$M_IDADE_C),length(M_CCM$M_IDADE_C),
              length(M_PLC$M_IDADE_C),length(M_NVG$M_IDADE_C),length(M_JRG$M_IDADE_C))

#FUNCAO PARA PLOTAR GRAFICOS SOBRE IDADE
lc_poridade <-function(Casos,lx,ly){
  conta <- matrix(0,nrow=7,ncol = 1)
  for(i in 1:length(Casos)){
  if (!is.na(Casos[i])){
    if(Casos[i]<16){
      conta[1] <- conta[1]+1
    }else if(Casos[i]<31){
      conta[2] <- conta[2]+1
    }else if(Casos[i]<46){
      conta[3] <- conta[3]+1
    }else if(Casos[i]<61){
      conta[4] <- conta[4]+1
    }else if(Casos[i]<76){
      conta[5] <- conta[5]+1
    }else if(Casos[i]<91){
      conta[6] <- conta[6]+1
    }else{
      conta[7] <- conta[7]+1
    }
  }
  }
  print(conta)
  conta2 <- c("até 15","16 a 30","31 a 45","46 a 60","61 a 75","76 a 90","mais de 90")
  conta2 <- factor(conta2,levels = conta2)
  
  CONTA <- data.frame(conta,conta2)
  cores <- c('#002A52','#4A6D7C','#6EA09E','#9E8FB2','#484041','#F6BD60','#FAE1DF')
  poridade <-ggplot(CONTA,aes(x=conta2,y=conta,text=conta))+
    geom_bar(stat="identity", width=0.7,color=cores,fill=cores) +
    theme_classic() + theme(axis.text.x = element_text(angle=0, vjust=0.5,size=10,face="bold")) +
    theme(axis.text.y = element_text(vjust=0.5,size=10,face="bold"))  +  
    geom_text(aes(label=paste(100*round(conta/length(Casos),digits=3),"%", sep = "")), hjust=-0.02, size=3)+
    labs(x=lx, y=ly,caption = "covidjoinville.com")+
    theme(axis.line.y=element_blank(),axis.line.x =element_blank(),plot.caption = element_text(size=10,face="bold")) +
    theme(axis.line.y=element_blank(),axis.line.x =element_blank()) + coord_flip()
  return(poridade)
}
lc_poridade(JLLE$IDADE_C,"Idade",paste("N?mero de casos por idade em", cidcm[1,7]))
lc_poridade(as.numeric(paste(FLP$IDADE_C)),"Idade","Número de mortes em Joinville")
lc_poridade(IDADE,"Idade","N?mero de casos")
lc_poridade(M_IDADE,"Idade","Número de mortos em Santa Catarina")
as.numeric(paste(IDADE))

#FUNCAO PARA PLOTAR GRAFICOS SOBRE GENERO
lc_porgenero <-function(Casos,tt){
  conta <- matrix(0,nrow=2,ncol = 1)
  for(i in 1:length(Casos)){
    if(Casos[i]=="FEMININO"){
      conta[1] <-  conta[1]+1
    }else if(Casos[i]=="MASCULINO"){
      conta[2] <-  conta[2]+1
    }
  }
  conta2 <- c("Feminino","Masculino")
  
  CONTA <- data.frame(conta,conta2)
  cores <- c('#9E8FB2','#6EA09E')
  
  fig <- plot_ly(CONTA, labels = ~conta2, values = ~conta, type = 'pie',
                 marker = list(colors = cores,
                               line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout(title = tt)%>% 
    layout(annotations=list(x=1.2, y = 0.15, text = "covidjoinville.com",showarrow = F, xref='paper',
                            yref='paper',xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
  
  return(fig)
}
lc_porgenero(JLLE$SEXO_C,"Casos por sexo em Joinville")
ggplotly(lc_porgenero(JLLE$SEXO_C,"Sexo","N?mero de casos em Joinville"))

px.pie(df, values='pop', names='country', title='Population of European continent')