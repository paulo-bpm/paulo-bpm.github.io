library(RCurl)
library(XML)
library(readxl)
library(openxlsx)

###########################

COVID_J = read_excel("JLLE_COVID_hoje.xlsx")
attach(COVID_J)

j <- length(COVID_J$DATA)

pmjpage <- readLines("https://www.joinville.sc.gov.br/publicacoes/dados-casos-coronavirus-municipio-de-joinville/")

grabnewdata <- function(pagedata,mypattern){
  newdata=grep(mypattern,pagedata,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,newdata)
  matches = mapply(getexpr,newdata,gg)
  result = gsub(mypattern,'\\1',matches)
  names(result) = NULL
  return(result)
}

dadoshoje <- matrix(0,nrow = 1, ncol = 11)
datahoje <- j+1
dadoshoje[1,1:11]<-c(grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[2],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[3],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[4],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[5],
                     grabnewdata(pmjpage[315:329],'<td><strong>([^<]*)</strong></td>')[1],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[6],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[7],
                     grabnewdata(pmjpage[315:329],'<td>([^<]*)</td>')[8],
                     grabnewdata(pmjpage[315:329],'<td><strong>([^<]*)</strong></td>')[2],
                     grabnewdata(pmjpage[315:329],'<td><strong>([^<]*)</strong></td>')[3],
                     grabnewdata(pmjpage[315:329],'<td><strong>([^<]*)</strong></td>')[4])
dadoshoje <- as.numeric(dadoshoje)
COVID_J2 <- 0
COVID_J2 <-COVID_J
COVID_J2[j+1,1] <- datahoje
COVID_J2[j+1,2:12] <- as.list(dadoshoje)
COVID_J2[j+1,2:18] <- as.list(c(as.numeric(COVID_J2[j+1,2])-0,as.numeric(COVID_J2[j+1,3])-0,as.numeric(COVID_J2[j+1,4])-0,
                        as.numeric(COVID_J2[j+1,5])-0,as.numeric(COVID_J2[j+1,6])-0,as.numeric(COVID_J2[j+1,7])-0,
                        as.numeric(COVID_J2[j+1,8])-0,as.numeric(COVID_J2[j+1,9])-0,as.numeric(COVID_J2[j+1,10])-0,
                        as.numeric(COVID_J2[j+1,11])-0,as.numeric(COVID_J2[j+1,12])-0,
                        as.numeric(COVID_J2[j+1,6])-as.numeric(COVID_J2[j,6]),
                        as.numeric(COVID_J2[j+1,10])-as.numeric(COVID_J2[j+1,12]), 
                        as.numeric(COVID_J2[j+1,5])-as.numeric(COVID_J2[j,5]), 
                        as.numeric(COVID_J2[j+1,2])-as.numeric(COVID_J2[j,2]),
                        as.numeric(COVID_J2[j+1,6])-as.numeric(COVID_J2[j+1,5])-as.numeric(COVID_J2[j+1,2]),
                        (as.numeric(COVID_J2[j+1,10])-as.numeric(COVID_J2[j+1,12]))-as.numeric(COVID_J2[j,14])))
                      


wb <- createWorkbook()
addWorksheet(wb, sheetName = 'COVIDJoinville')
writeData(wb,'COVIDJoinville',COVID_J2,na.string = NULL)
saveWorkbook(wb,"JLLE_COVID_hoje.xlsx",
             overwrite = TRUE)

###############################

UTI_J = read_excel("JLLE_UTICOVID_hoje.xlsx")
attach(UTI_J)

j <- length(UTI_J$CONTAD)+1
UTI_J2 <-UTI_J

linha <-as.numeric(grep('de leitos de UTI',pmjpage))
utidata <- grabnewdata(pmjpage[(linha[3]+16):(linha[3]+22)],'<td>([^<]*)</td>')
utidata2 <- grabnewdata(pmjpage[linha[3]:(linha[3]+22)],'<strong>([^<]*)</strong>')

utidata<-as.numeric(utidata)
utidata2<-as.numeric(utidata2)

UTI_J2[j,1:7] <- as.list(c(j,utidata,sum(utidata[1:3]),utidata2))


wb <- createWorkbook()
addWorksheet(wb, sheetName = 'UTIJoinville')
writeData(wb,'UTIJoinville',UTI_J2,na.string = NULL)
saveWorkbook(wb,"JLLE_UTICOVID_hoje.xlsx",
             overwrite = TRUE)

#################################################################


COVID_JRG = read_excel("JRG_COVID_hoje.xlsx")
attach(COVID_JRG)

j <- length(JRG_TOTAL)+1

jrgpage <- readLines("https://www.jaraguadosul.sc.gov.br/boletim-coronavirus")
linha <-as.numeric(grep('Boletim',jrgpage))
##linha 11056:1065



grabnewdata <- function(pagedata,mypattern){
  newdata=grep(mypattern,pagedata,ignore.case = TRUE,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,newdata)
  matches = mapply(getexpr,newdata,gg)
  result = gsub(mypattern,'\\1',ignore.case = TRUE,matches)
  names(result) = NULL
  return(result)
}
dadoshoje = matrix(nrow = 1, ncol = 7)
dadoshoje[1,1] <- grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) casos confirmados') #TOTAL
dadoshoje[1,2] <- grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) recuperados') #RECUPERADOS
#dadoshoje[1,3] <-  #grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) em ') #EM ISOLAMENTO
#dadoshoje[1,4] <-  #grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) internado') #INTERNADOS
dadoshoje[1,5] <- grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) Ã³bitos') #MORTES
dadoshoje[1,6] <- j
dadoshoje[1,7] <- grabnewdata(jrgpage[linha[2]:linha[3]-1],'\t\t\t([^<]*) em ')
dadoshoje <- as.numeric(dadoshoje)

COVID_JRG2<-COVID_JRG
COVID_JRG2[j,1:7]<-as.list(dadoshoje)
COVID_JRG2[j,8:9]<-as.list(c(COVID_JRG2[j,1]-COVID_JRG2[j-1,1],COVID_JRG2[j,5]-COVID_JRG2[j-1,5]))


wb <- createWorkbook()
addWorksheet(wb, sheetName = 'COVIDJaragua')
writeData(wb,'COVIDJaragua',COVID_JRG2)
saveWorkbook(wb,"JRG_COVID_hoje.xlsx",
             overwrite = TRUE)


