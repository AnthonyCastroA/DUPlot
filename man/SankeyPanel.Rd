\name{SankeyPanel}
\alias{SankeyPanel}
\title{Analizar la distribución de la asertividad entre modelos}
\description{
Este paquete determina los cambios en los intervalos de asertividad que se dieron para cada producto según el modelo utilizado. Se muestra un diagrama de barras con flujos comunmente conocido como diagrama de Sankey.
}
\usage{
SankeyPanel(df_base, titulo, ruta="")
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
k <- 2
n <- 200
DF_TOTAL <- data.frame(CAMPANA = rep(201901:(201900+k), times=n),
                       COD_PROD = paste0("P",rep(1:n, each=k, len=n*k)),
                       MOD = ceiling(rep(runif(n, 100, 8000),
                                           each=k,
                                           len=n*k) *
                                     rnorm(n*k,1,0.1) *
                                     cbind(sapply(seq(from=0.28,
                                                      to=0.08,
                                                      by=-0.02),
                                                  function(x){rnorm(n*k,1,x)}),1)))

colnames(DF_TOTAL) <- c("CAMPANA", "COD_PROD",
                        "ANTI_MKT", "ANTI_MOD",
                        "PADV_RG3", "PADV_MOD",
                        "ADVC_RG3", "ADVC_MKT", "ADVC_MOD",
                        "PNAC_RG3", "PNAC_MOD",
                        "PEDI_RG3",	"PEDI_MOD",
                        "CANT_REA")

DF_TOTAL <- DF_TOTAL[DF_TOTAL == 201902,]

## realizar analisis
SankeyPanel(DF_TOTAL,
            "Comparativo de la asertividad entre modelos de pronóstico")
}
\keyword{accuracy}
