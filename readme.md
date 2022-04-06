# Promedio electoral de las elecciones presidenciales colombianas de 2022

## Comenzando 🚀

El objetivo es dar una idea agregada de la situación de los distintos candidatos ante las elecciones presidenciales de Colombia de 2022.

El resultado se puede ver [aquí](https://elpais.com/internacional/2022-04-06/petro-domina-en-las-encuestas-seguido-por-un-fico-gutierrez-al-alza.html?utm_medium=Social&utm_source=Twitter&ssm=TW_CM_AME#Echobox=1649255860), por ejemplo.


### Pre-requisitos 📋

Tidyverse. Poco más, por ahora.


### Componentes ⚙️

1a_v.R <- es el script maestro para los análisis de las encuestas en primera vuelta. Está convenientemente comentado para que resulte legible (¡espero!). Absorbe los datos de recetas-electorales.com y los transforma en los outputs necesarios. Esos outputs son en muchos casos tablas de datos que después empleo para realizar gráficos en datawrapper. Dichas tablas quedan en el directorio raíz como archivos .csv con el sufijo "dw" y constituyen el punto de salida de todo el script, que no genera ningún gráfico por sí mismo.

ponderador_lsv <- Mantiene una tabla de referencia de las puntuaciones que asignó el [Semáforo de Encuestas de lasillavacia.com](http://lasillavacia.com/historias/silla…) a cada casa encuestadora para realizar la ponderación.

### Explicación metodológica del promedio

A día de hoy, el promedio de encuestas incluye las últimas publicadas de cada casa encuestadora registrada ante el Consejo Nacional Electoral hasta el 5 de abril, desde el 14 de marzo de 2022 (el día posterior a las consultas interpartidistas que terminaron por definir las candidaturas a la presidencia. La recopilación se recoge desde la plataforma recetas-electorales.com, y se contrasta con la publicación en diversos medios.

En el futuro es de esperar que el promedio recogerá varias versiones y dispondrá por tanto de una evolución. Este readme cambiará convenientemente.

Cada encuesta tiene un peso ligeramente distinto en el promedio que depende de la valoración numérica realizada por el Semáforo de Encuestadoras de lasillavacia.com, que valora a cada una de las casas de encuestas de acuerdo con una serie de parámetros técnicos, metodológicos y de acierto en el resultado final. Para evitar sesgar en exceso el promedio, el peso asignado solo varía en un 20%, de manera que la encuesta de la encuestadora mejor valorada recibe un 100% y la peor valorada recibe un 80% de peso sobre el promedio final. Cuando una encuestadora sí está registrada ante el CNE pero no está valorada por lasillavacia.com, el promedio le asigna un punto intermedio entre ambos extremos.


## Autores ✒️

**Jorge Galindo** - *Responsable del repo*

Pero este proyecto habría sido imposible sin el trabajo del [Semáforo de Encuestas de lasillavacia.com](http://lasillavacia.com/historias/silla…), encabezado por Rafael Unda; y sin el juicioso recopilado que realiza [recetas-electorales.com](http://recetas-electorales.com) habría sido sin duda mucho más costoso.

## Licencia 📄

Licencia libre, úsalo como consideres :)
