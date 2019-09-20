\name{SankeyTimeSeries}
\alias{SankeyTimeSeries}
\title{Analizar evolución de individuos por intervalos definidos}
\description{
Este paquete determina el cambio de los individuos a través del tiempo en función a una variable cuantitativa. Se utiliza un diagrama de barras con flujos comunmente conocido como diagrama de Sankey.
}
\usage{
SankeyTimeSeries(df_base, interv, titulo, ruta="")
}
\arguments{
  \item{df_base}{la base de datos para el análisis, constituida por las columnas:}
    \itemize{
      \item 1) CAMPANA: campañas de venta (ej. 201718..201909)
      \item 2) ID: identificadores de los individuos (ej. ASESORA, ZONA, SECTOR, DEPARTAMENTO)
      \item 3) VARIABLE: campo de análisis (ej. pedidos, ventas, PPP, PPU, UPP, faltantes)
      }
  \item{interv}{los valores que determinan los intervalos; se recomienda utilizar percentiles representativos (ej. \code{c(10,50,90)}}
  \item{titulo}{texto en la posición superior de la grafica}
  \item{exportar}{valor binario (1: para exportar, 0: no exportar)}
  \item{ruta}{el directorio donde se exportan los archivos, si se requiere}
}
\examples{
## Base de datos generada
DF_TOTAL <- data.frame(CAMPANA = rep(201901:201908, times=200),
                       ZONA = rep(1:200, each=8, len=1600),
                       VENTA = ceiling(rep(runif(200, 100000, 300000),
                                           each=8,
                                           len=1600) *
                                       rnorm(1600,0.9,0.12)))

## realizar analisis
SankeyTimeSeries(DF_TOTAL,
                 ceiling(quantile(DF_TOTAL$VENTA, c(0.1,0.5,0.9))/1000)*1000,
                 "Evolución de venta acumulada por zona")
}
\keyword{accuracy}
