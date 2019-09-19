---

output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, comment=NA)

if ("dplyr" %in% rownames(installed.packages()) == FALSE){
  install.packages('dplyr')
}
library('dplyr')


if ("ISLR" %in% rownames(installed.packages()) == FALSE){
  install.packages('ISLR')
}
library('ISLR')

if ("knitr" %in% rownames(installed.packages()) == FALSE){
  install.packages('knitr')
}
library('knitr')

if ("stats" %in% rownames(installed.packages()) == FALSE){
  install.packages('stats')
}
library('stats')


if ("stargazer" %in% rownames(installed.packages()) == FALSE){
  install.packages('stargazer')
}
library('stargazer')

if ("forcats" %in% rownames(installed.packages()) == FALSE){
  install.packages('forcats')
}
library('forcats')

if ("tidyverse" %in% rownames(installed.packages()) == FALSE){
  install.packages("tidyverse",repos = "http://cran.us.r-project.org")
}
  library(tidyverse)

if ("MASS" %in% rownames(installed.packages()) == FALSE){
  install.packages("MASS",repos = "http://cran.us.r-project.org")
}
  library(MASS)



```


```{r downloadData, echo=T}

data <- read.csv("Datos para Regresion.csv")
data2 <- data %>% 
  mutate(Descripcion = ifelse(Descripcion==0,"No", "Si"),
         Descripcion= factor(Descripcion),
         Mes = ifelse(Mes=="NaN", NA, Mes),
         Mes = ifelse(is.na(Mes), round(mean(Mes,na.rm = T)), Mes),
         Edad = ifelse(is.na(Edad), round(mean(Edad,na.rm = T)), Edad),
         Estatura = ifelse(is.na(Estatura), round(mean(Estatura,na.rm = T)), Estatura))

data2$Complexion <- data2$Complexion %>% fct_collapse(
  `NO ESPECIFICADO`= c("No Especificado", "NO ESPECIFICADO"))

#sapply(data2, class)
#summary(data2)

```



```{r regresionLogistica, warning=FALSE, echo=T}

#Regresion Logistica
n <- nrow(data2)
set.seed(157869)
sampleNum <- sample(seq(1,n),n*.75,replace=F, prob=NULL)

trainData <- data2[sampleNum,]
testData <- data2[-sampleNum,]

logit <- glm(Aparecio~. , family = binomial, data = trainData )

#stargazer(logit, type = "html")

```
<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Variable dependiente:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Aparecio</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">ComplexionMediana</td><td>0.078</td></tr>
<tr><td style="text-align:left"></td><td>(0.098)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionNO ESPECIFICADO</td><td>-0.596<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.116)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionObesa</td><td>-0.873</td></tr>
<tr><td style="text-align:left"></td><td>(0.729)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionRobusta</td><td>-0.074</td></tr>
<tr><td style="text-align:left"></td><td>(0.115)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoBAJA CALIFORNIA</td><td>-3.472<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.776)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoBAJA CALIFORNIA SUR</td><td>-0.628</td></tr>
<tr><td style="text-align:left"></td><td>(1.070)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCAMPECHE</td><td>1.455<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.548)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCHIAPAS</td><td>-1.775<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.060)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCHIHUAHUA</td><td>-2.158<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.450)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCIUDAD DE MEXICO</td><td>-15.807</td></tr>
<tr><td style="text-align:left"></td><td>(271.612)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCOAHUILA DE ZARAGOZA</td><td>-2.583<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.550)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoCOLIMA</td><td>-0.458</td></tr>
<tr><td style="text-align:left"></td><td>(0.457)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoDURANGO</td><td>-2.118<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.779)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoESTADO DE MEXICO</td><td>-0.387</td></tr>
<tr><td style="text-align:left"></td><td>(0.327)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoGUANAJUATO</td><td>1.707<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.344)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoGUERRERO</td><td>-0.306</td></tr>
<tr><td style="text-align:left"></td><td>(0.354)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoHIDALGO</td><td>-0.075</td></tr>
<tr><td style="text-align:left"></td><td>(0.450)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoJALISCO</td><td>-3.249<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.552)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoMICHOACAN</td><td>-3.714<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.050)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoMORELOS</td><td>0.064</td></tr>
<tr><td style="text-align:left"></td><td>(0.413)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoNAYARIT</td><td>-15.559</td></tr>
<tr><td style="text-align:left"></td><td>(622.975)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoNO ESPECIFICADO</td><td>-15.761</td></tr>
<tr><td style="text-align:left"></td><td>(1,338.682)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoNUEVO LEON</td><td>-0.079</td></tr>
<tr><td style="text-align:left"></td><td>(0.343)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoOAXACA</td><td>-16.130</td></tr>
<tr><td style="text-align:left"></td><td>(686.067)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoPUEBLA</td><td>-0.600<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.347)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoQUERETARO</td><td>1.137<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.356)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoQUINTANA ROO</td><td>1.566<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.425)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoSAN LUIS POTOSI</td><td>-15.595</td></tr>
<tr><td style="text-align:left"></td><td>(779.820)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoSINALOA</td><td>0.936<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.326)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoSONORA</td><td>-2.922<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.661)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoTABASCO</td><td>-16.034</td></tr>
<tr><td style="text-align:left"></td><td>(893.128)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoTAMAULIPAS</td><td>-4.858<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.776)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoTLAXCALA</td><td>-0.375</td></tr>
<tr><td style="text-align:left"></td><td>(1.092)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoVERACRUZ</td><td>-15.876</td></tr>
<tr><td style="text-align:left"></td><td>(321.549)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoYUCATAN</td><td>-0.410</td></tr>
<tr><td style="text-align:left"></td><td>(0.676)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">EstadoZACATECAS</td><td>0.935<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.361)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Mes</td><td>-0.060<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">DescripcionSi</td><td>-0.201<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.084)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Edad</td><td>-0.015<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Estatura</td><td>-0.002</td></tr>
<tr><td style="text-align:left"></td><td>(0.002)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">SexoMUJER</td><td>0.698<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.077)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constante</td><td>-1.616<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.466)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observaciones</td><td>25,830</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-3,266.368</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>6,616.736</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


```{r regresionLogistica2, warning=FALSE, echo=T}

#Predicciones

logProbs <- logit%>% predict(testData, type="response")


data3 <- testData %>% 
  mutate(logitProbs = logProbs,
         predict = ifelse(logitProbs>max(logProbs)/2, 1, 0),
         error = ifelse( Aparecio != predict, 1,0))

errorRate <- sum(data3$error)/nrow(testData)

sprintf('La tasa de error de este modelo es %.2f %%' , errorRate*100 )

tabLogit <- table(data3$predict,data3$Aparecio)
kable(tabLogit, caption = "Clasificacion real contra predicción (Columnas es la real)")
kable(round(prop.table(tabLogit, margin=2)*100,2), caption = "Procentajes de Error por clasificacion real")



```



```{r regresionLogistica3, warning=FALSE, echo=T}

#Regresion Logistica sin estados
n <- nrow(data2)
set.seed(157869)
sampleNum2 <- sample(seq(1,n),n*.75,replace=F, prob=NULL)

trainData <- data2[sampleNum,]
testData <- data2[-sampleNum,]

logit2 <- glm(Aparecio ~ Complexion + Mes + Descripcion + Edad + Estatura + Sexo, family = binomial, data = trainData )
#stargazer(logit2, type = "html")

```
<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Variable dependiente:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Aparecio</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">ComplexionMediana</td><td>0.118</td></tr>
<tr><td style="text-align:left"></td><td>(0.093)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionNO ESPECIFICADO</td><td>-0.181<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.084)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionObesa</td><td>-1.126</td></tr>
<tr><td style="text-align:left"></td><td>(0.713)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">ComplexionRobusta</td><td>-0.400<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.110)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Mes</td><td>-0.046<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">DescripcionSi</td><td>-0.309<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.072)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Edad</td><td>-0.016<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Estatura</td><td>-0.004<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.002)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">SexoMUJER</td><td>0.694<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.071)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constante</td><td>-1.820<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.319)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observaciones</td><td>25,830</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-4,013.508</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>8,047.016</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


```{r regresionLogistica4, warning=FALSE, echo=T}

#Predicciones

logProbs2 <- logit2%>% predict(testData, type="response")


data4 <- testData %>% 
  mutate(logitProbs = logProbs2,
         predict = ifelse(logitProbs>max(logProbs2)/2, 1, 0),
         error = ifelse( Aparecio != predict, 1,0))

errorRate2 <- sum(data4$error)/nrow(testData)

sprintf('La tasa de error de este modelo es %.2f %%' , errorRate2*100 )

tabLogit2 <- table(data4$predict,data4$Aparecio)
kable(tabLogit2, caption = "Clasificacion real contra predicción (Columnas es la real) ")

kable(round(prop.table(tabLogit2, margin=2)*100,2), caption = "Procentajes de Error por clasificacion real")



```



##Conclusiones

Primero notemos que en ambos modelos las variables tienen efectos similares. Veamos primero la complexión, la única complexión con efecto positivo es para la complexión mediana, este comportamiento se repite para ambos modelos. Ahora notemos que tanto la edad como la estatura y el mes en el que desaparecen tienen una relación negativa con la posibilidad de que se encuentre al desaparecido. También, es importante notar que la presencia de una descripción al momento del reporte de la persona desapercibida, también tiene un efecto negativo sobre la posibilidad de aparición. Finalmente notemos que hay una relación positiva si el sexo del desaparecido es mujer. Esto, en el contexto del problema nos dice que en general es más dificíl encontrar al desparecido si este es muy alto, de alta edad, hombre y en general de complexión corporal alta y que se haya presentado con una descripción de la víctima. 
  Notemos que para este modelo, casi todos los coeficientes son significativos al nivelo 5%, lo cuál nos habla de una buena presencia de estos modelos cuando no hay ningun otro presente.

Ahora agreguemos la presencia de la entidad federativa, primero notemos que la tasa de error disminuye. También es importante notar que el efecto de la gran mayoría de las idaentidad federativa depende y sigue una tendencia, si muchos desaparenen en estas entidad el efecto será negativo, auqnue no necesariamente el efecto es creciente de acuerdo al porcentaje de desaparecidos por entidad.

Ambos modelos tienen una tasa pequeña de error, pero notaremos que es muy diferente como clasifican y el error que dan. 
Para el primer modelo podemos notar que ambos errores son mas bajos que en el segundo modelo en el que no incluimos las entidades federativas. Con esto en mente veamos que además el error de tipo I es mucho mayor, es decir, el modelo predice que la persona va a ser encontrada mientas que en realidad no ha sido encontrada. En el contexto del problema, este es un peligro ya que es extraordinariamente difícil que los encuentren y el error de tipo I donde se da un falso positivo resulta en un error costoso.





