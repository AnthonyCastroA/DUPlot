#' Create high impact visualizations for direct selling companies
#'
#' This package gather several visualization designs which representation lets make quick analysis and consequently take better decisions in business.
#'
#' @param df_base
#' @param campana
#' @param lugar
#' @param ruta
#' @param interv
#' @param titulo
#' @param subtitulo
#' @param linea
#' @param label
#' @param qvar
#' @param freq
#' @param plot.qvar
#' @param show.lab
#' @param umbral
#' @param modset
#' @param actual
#' @param reference
#' @param df_test
#'
#' @export
#' @examples


calc_performance <- function(df_base, lugar, ruta=""){

  n_prod_total <- nrow(df_base)
  df_dlt <- df_base[(complete.cases(df_base)==F | df_base$REAL==0), ]
  df_base <- df_base[(complete.cases(df_base)==F | df_base$REAL==0)==F, ]
  n_prod_compl <- nrow(df_base)

  n_models <- ncol(df_base)-5

  INT_LAB <- data.frame(INT = levels(cut(0, breaks = c(0,
                                                       0.5,
                                                       0.8,
                                                       1.2,
                                                       1.8,
                                                       2.5,
                                                       Inf))),
                        LAB = c("a) 0 - 50%",
                                "b) 50 - 80%",
                                "c) 80 - 120%",
                                "d) 120 - 180%",
                                "e) 180 - 250%",
                                "f) > 250%"))

  DF1 <- data.frame(df_base,
                    ASERT = df_base$REAL / df_base[, 6:(5+n_models)])

  DF1 <- data.frame(DF1,
                    INTERV = apply(DF1[, (6+n_models):(5 + 2*n_models)],
                                   2,
                                   function(x){INT_LAB$LAB[match(cut(x, breaks=c(0,
                                                                                 0.5,
                                                                                 0.8,
                                                                                 1.2,
                                                                                 1.8,
                                                                                 2.5,
                                                                                 Inf)),
                                                                 INT_LAB$INT)]}))

  DF1 <- data.frame(DF1,
                    DIF_CANT = DF1[, 6:(5 + n_models)] - DF1$REAL)

  DF1X <- DF1

  DF1X$DIF_CANT.RG3 <- DF1X$DIF_CANT.RG3
  DF1X$DIF_CANT.MOD <- DF1X$DIF_CANT.MOD

  DFA <- DF1[, c(1:2, (6 + 2 * n_models):(5 + 3 * n_models))]

  DFA <- reshape(DFA,
                 v.name=c("INTERV.ASERT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5+n_models)],
                 direction = "long",sep = ".")[,-(5)]
  colnames(DFA) <- c("CAMPANA",
                     "LINEA",
                     "MODELO",
                     "ASERTIVIDAD")

  DFAN <- aggregate((1:nrow(DFA)) ~  CAMPANA + LINEA + ASERTIVIDAD + MODELO ,
                    DFA,
                    FUN = length)


  colnames(DFAN) <- c("CAMPANA",
                      "LINEA",
                      "ASERTIVIDAD",
                      "MODELO",
                      "CODI_VENT")

  DFAN$ASERTIVIDAD <- factor(DFAN$ASERTIVIDAD, levels = c("a) 0 - 50%",
                                                          "b) 50 - 80%",
                                                          "c) 80 - 120%",
                                                          "d) 120 - 180%",
                                                          "e) 180 - 250%",
                                                          "f) > 250%"))
  library(dplyr)

  DFAP1 <- group_by(DFAN, paste(CAMPANA, LINEA, MODELO)) %>% mutate(PRODUCTOS=CODI_VENT/sum(CODI_VENT))

  DFAP1 <- data.frame(DFAP1)

  DFAP <- DFAP1[, c(1, 2, 4, 3, ncol(DFAP1))]

  INT_LAB <- data.frame(INT = levels(cut(0, breaks = c(0,
                                                       0.5,
                                                       0.8,
                                                       1,
                                                       1.2,
                                                       1.8,
                                                       2.5,
                                                       Inf))),
                        LAB = c("a) 0 - 50%",
                                "b) 50 - 80%",
                                "c) 80 - 100%",
                                "d) 100 - 120%",
                                "e) 120 - 180%",
                                "f) 180 - 250%",
                                "g) > 250%"))

  DF1 <- data.frame(df_base,
                    ASERT = df_base$REAL / df_base[, 6:(5+n_models)])

  DF1 <- data.frame(DF1,
                    INTERV = apply(DF1[, (6+n_models):(5 + 2*n_models)],
                                   2,
                                   function(x){INT_LAB$LAB[match(cut(x,
                                                                     breaks=c(0,
                                                                              0.5,
                                                                              0.8,
                                                                              1,
                                                                              1.2,
                                                                              1.8,
                                                                              2.5,
                                                                              Inf)),
                                                                 INT_LAB$INT)]}))

  DF1 <- data.frame(DF1,
                    DIF_CANT = DF1[, 6:(5 + n_models)] - DF1$REAL)


  DFQ <- DF1[,c(1, 2, (6 + 2 * n_models):(5 + 3 * n_models),
                (6 + 3 * n_models):(5 + 4 * n_models))]

  DFQ <- reshape(DFQ,
                 v.name = c("INTERV.ASERT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5 + n_models)],
                 direction = "long", sep = ".") ###REVISAR EN EL OTRO CODIGO

  DFQ <- DFQ[,-(ncol(DFQ))] ###REVISAR EN EL OTRO CODIGO

  colnames(DFQ) <- c("CAMPANA",
                     "LINEA",
                     paste("DIF_CANT",
                           colnames(DF1)[6:(5+n_models)],
                           sep="."),
                     "MODELO",
                     "ASERTIVIDAD")

  DFQ <- reshape(DFQ,
                 v.name = c("DIF_CANT"),
                 varying = 3:(2 + n_models),
                 times = colnames(DF1)[6:(5 + n_models)],
                 direction = "long",
                 sep = ".") ###REVISAR EN EL OTRO CODIGO

  DFQ <- DFQ[,-(ncol(DFQ))]###REVISAR EN EL OTRO CODIGO

  colnames(DFQ) <- c("CAMPANA",
                     "LINEA",
                     "MODELO",
                     "ASERTIVIDAD",
                     "TEST",
                     "DIFERENCIA")

  DFQ <- DFQ[DFQ$MODELO == DFQ$TEST, c(1:4, 6)]

  DFQ <- aggregate(DIFERENCIA ~ ASERTIVIDAD+MODELO+LINEA+CAMPANA, DFQ, FUN=sum)

  DFQ$ASERTIVIDAD <- factor(DFQ$ASERTIVIDAD, levels = c("a) 0 - 50%",
                                                        "b) 50 - 80%",
                                                        "c) 80 - 100%",
                                                        "d) 100 - 120%",
                                                        "e) 120 - 180%",
                                                        "f) 180 - 250%",
                                                        "g) > 250%"))

  X_RESULTADO <- list()
  X_RESULTADO[["DF_TOTAL"]] <- df_base
  X_RESULTADO[["DF_DTL"]] <- df_dlt
  X_RESULTADO[["ASERT_NOM"]] <- DFAN
  X_RESULTADO[["ASERT_PCT"]] <- DFAP
  X_RESULTADO[["CANT_SYF"]] <- DFQ
  X_RESULTADO[["CONS_ASERT"]] <- DF1X

  print(paste("Productos totales ingresados:", nrow(df_base), sep=" "))
  print(paste("Productos incompletos eliminados:", nrow(df_dlt), sep=" "))

  library(openxlsx)

  print(DFAP)
  print(DFQ)


  if(ruta!=""){

    write.xlsx(x = DF1X, file = paste(ruta, "/", lugar, "_", "CONS_ASERT.xlsx", sep=""), sheetName = "DATOS", col.names = T, row.names = F)

    if (nrow(df_dlt)!=0){
      write.xlsx(x = df_dlt, file = paste(ruta, "/", lugar, "_", "DATA_ELIMI.xlsx", sep=""), sheetName = "DATOS", col.names = T, row.names = F)
      print(df_dlt)

    }

  }

  return(X_RESULTADO)
}




plot_performance <- function(df_base, campana, linea, lugar, ruta=""){

  DFAP <- df_base[["ASERT_PCT"]]
  DFAP <- DFAP[DFAP$LINEA == linea,]

  library(ggplot2)

  col_asertividad = c("#E41A1C",
                      "#377EB8",
                      "#4DAF4A",
                      "#984EA3",
                      "#FF7F00",
                      "#999999")

  plot_DFAP <- ggplot(DFAP[DFAP$CAMPANA == campana,],
                      aes(x = ASERTIVIDAD,
                          y = PRODUCTOS,
                          group = MODELO,
                          label = paste0(format(PRODUCTOS * 100,
                                                digits = 0),"%"),
                          fill = ASERTIVIDAD)) +
    geom_bar(position = "dodge",
             stat = "identity") +
    scale_fill_manual(values = col_asertividad, drop=F) +
    scale_y_continuous(labels = scales::percent_format(scale = 100,
                                                       accuracy = 1)) +
    scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
                                    #,margin = margin(b = 12)
    ),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom") +
    ggtitle(paste("Distribución de productos según asertividad \n",
                  linea,
                  " ",
                  campana,
                  " - ",
                  lugar,
                  sep = "")) +
    xlab("Asertividad") +
    ylab("Productos") +
    geom_text(position = position_dodge(width = 0.7),
              color = "black",
              size = 4.2,
              fontface = "bold",
              vjust = -0.25) +
    facet_grid(~MODELO) +
    guides(fill=guide_legend(nrow=2, byrow=T))

  DFQ <- df_base[["CANT_SYF"]]
  DFQ <- DFQ[DFQ$LINEA==linea,]

  col_asertividad2 = c("#E41A1C",
                       "#377EB8",
                       "#4DAF4A",
                       "#4DAF4A",
                       "#984EA3",
                       "#FF7F00",
                       "#999999")

  options(scipen=100000)

  plot_DFQ <- ggplot(DFQ[DFQ$CAMPANA == campana,], aes(x = ASERTIVIDAD,
                                                       y = DIFERENCIA,
                                                       fill = ASERTIVIDAD,
                                                       label = DIFERENCIA)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = col_asertividad2, drop=F) +
    scale_x_discrete(drop=F) +
    theme(plot.title = element_text(color = "#636363",
                                    size = 18,
                                    face = "bold.italic"
                                    #,margin = margin(b = 12)
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12,
                               face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14,
                                face = "bold",
                                colour = "#252525"),
    axis.ticks.x = element_blank(),
    text = element_text(face = "bold"),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom") +
    ggtitle(paste("Distribución de sobrantes / faltantes según asertividad \n",
                  linea,
                  " ",
                  campana,
                  " - ",
                  lugar,
                  sep = "")) +
    xlab("Asertividad") +
    ylab("Diferencia (unidades)") +
    geom_text(aes(label = format(ceiling(abs(DIFERENCIA)/10)*10,
                                 digits=0)),
              size = 4.1,
              position = position_dodge(width = 0.95),
              vjust = -0.25,
              color = "black",
              fontface = "bold")+
    facet_grid(~ MODELO) +
    guides(fill=guide_legend(nrow=2, byrow=T))


  if(ruta!=""){

    png(paste(ruta, "/", lugar, "_", substr(linea, 1,2), "_ASERT_", campana, ".png", sep=""),
        width=1700, height=1200, res=220)
    plot(plot_DFAP)
    dev.off()

    png(paste(ruta, "/", lugar, "_", substr(linea, 1,2), "_DIFER_", campana, ".png", sep=""),
        width=1700, height=1200, res=220)
    plot(plot_DFQ)
    dev.off()

  }

  print(plot_DFAP)
  print(plot_DFQ)

}



AccyAnalysis <- function(df_base, campana, lugar, ruta=""){

  #df_camp <- df_base[df_base$CODI_CAMP == campana,]
  df_camp <- df_base

  consolidado_res <- calc_performance(df_camp, lugar, ruta)

  nomb_lines <- levels(unique(df_camp$NOMB_LINE))

  for (i in nomb_lines){

    plot_performance(consolidado_res, campana, i, lugar, ruta)

  }

  return(consolidado_res)

}

# DATA_ASERT <- RESULTS$CONS_ASERT
#
# if(nrow(RESULTS$df_dlt)>0){
#
#   DATA_ELIMI <- RESULTS$df_dlt
#
# }

#library(knitr)

#purl("1. Performance_Modelo_General.Rmd")

SankeyTimeSeries <- function(df_base, interv, titulo, linea="",ruta=""){

  library(ggalluvial)
  library(RColorBrewer)
  library(dplyr)

  df_base[,1] <- as.character(df_base[,1])

  n_int <- length(interv) + 1
  interv <- c(0, interv, Inf)
  df_base$INTERVAL <- cut(df_base[,3], b = interv)

  LABELS <- data.frame(INTERVAL = sort(unique(df_base$INTERVAL)),
                       LABEL = c(paste0("<", interv[2]/1000, "K"),
                                 mapply(function(x,y) paste0(x/1000, "K-\n", y/1000, "K"), interv[2:(n_int-1)], interv[3:n_int]),
                                 paste0(">", interv[n_int]/1000,"K")))

  LABELS <- data.frame(INTERVAL = sort(unique(df_base$INTERVAL)),
                       LABEL = c(paste0("<",interv[2]*100, "%"),
                                 mapply(function(x,y) paste0(x*100, "% - ", y*100, "%"), interv[2:(n_int-1)], interv[3:n_int]),
                                 paste0(">",interv[n_int]*100,"%")))

  df_base$otro <- df_base$INTERVAL
  df_base$INTERVAL <- LABELS$LABEL[match(df_base$INTERVAL, LABELS$INTERVAL)]
  df_base$INTERVAL <- factor(df_base$INTERVAL, levels = as.character(LABELS$LABEL)[n_int:1])



  DB_1 <- data.frame(id = df_base[,2],
                     round = df_base[,1],
                     episode = df_base[,4],
                     interv = df_base[,5])
  colortable<- data.frame(episode = unique(DB_1$episode)[order(unique(DB_1$interv))],
                          color = c(c("#E41A1C","#377EB8")[min(2, 7-n_int):2], c("#4DAF4A","#984EA3")[0:min(2,n_int-2)], c("#FF7F00", "#999999")[min(2, max(1,6-n_int)):2]),
                          stringsAsFactors = FALSE)

  DB_FINAL <- DB_1 %>% left_join(colortable, by="episode")

  plot_sankey <- ggplot(DB_FINAL, aes(x = round, stratum = episode, alluvium = id,
                                      fill = color, label = episode)) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 40),
          plot.subtitle = element_text(hjust = 0.5, size = 27),
          strip.text.x = element_text(size = 15),
          legend.text = element_text(size = 20),
          legend.title = element_text(size=24),
          legend.position = "left")+
    # theme(panel.background = element_blank(),
    #       axis.ticks = element_blank(),
    #       axis.text.x = element_text(size=20),
    #       axis.text.y = element_blank(),
    #       axis.title.x = element_blank(),
    #       text = element_text(face = "bold"),
    #       plot.title = element_text(hjust = 0.5, size = 42),
    #       plot.subtitle = element_text(hjust = 0.5, size = 28)) +
    geom_flow(width=0.45) +
    geom_stratum(color = NA, width=0.45) +
    scale_fill_identity("ASERTIVIDAD",guide="legend", labels=colortable$episode[n_int:0], breaks=colortable$color[n_int:0]) +
    ggtitle(titulo) +
    labs(subtitle = paste0("\n", linea," (CAMPAÑA ", min(as.character(df_base$CAMPANA)),
                           " - CAMPAÑA ", max(as.character(df_base$CAMPANA)),")")) +
    guides(fill=guide_legend(
      keywidth=0.6,
      keyheight=0.8,
      default.unit="inch")
    )
  #+
  #geom_text(stat = "stratum", fontface = "bold", color = "black", size=5)

  if(ruta!=""){

    png(ruta, width=5000, height=3000, res=300)
    plot(plot_sankey)
    dev.off()

  }

  print(plot_sankey)

  #ggsave(paste(cwd,"/PER_EVOL_SALES_201909.png", sep=""), height = 10, width = 15)
}



SankeyPanel <- function(df_base, titulo, ruta="", label=c(ANTI = "ANTICIPO",
                                                                 PADV = "PREPEDIDO ADV",
                                                                 ADVC = "ADVANCE - NACIONAL",
                                                                 PNAC = "PREPEDIDO NAC",
                                                                 PEDI = "PEDIDO NAC")){
  library(ggalluvial)
  library(RColorBrewer)
  library(dplyr)

  LABEL_MODELS <- colnames(df_base)[3:(ncol(df_base)-1)]

  df_base <- data.frame(df_base[,-(3:ncol(df_base))],
                         ASERT = df_base[,ncol(df_base)] / df_base[, 3:(ncol(df_base)-1)])

  df_base <- reshape(df_base,
                      v.name = c("ASERT"),
                      varying = 3:ncol(df_base),
                      times = LABEL_MODELS,
                      direction = "long",sep = ".")

  df_base <- df_base[,-ncol(df_base)]

  df_base <- data.frame(MODELO = df_base$time,
                         ID = df_base$COD_PROD,
                         ASERTIVIDAD = df_base$ASERT)

  INTERV <- c(0, 0.5, 0.8, 1.2, 1.8, 2.5, Inf)
  n_int <- length(INTERV) - 1
  df_base$INTERVAL <- cut(df_base[,3], b = INTERV)

  LAB_INTERV <- levels(cut(0, b = INTERV))

  LABELS <- data.frame(INTERVAL = sort(LAB_INTERV),
                       LABEL = c(paste0("0 - ", INTERV[2]*100, "%"),
                                 mapply(function(x,y) paste0(x*100, "% - ", y*100, "%"), INTERV[2:(n_int-1)], INTERV[3:n_int]),
                                 paste0("> ", INTERV[n_int]*100,"%")))

  df_base$INTERVAL <- LABELS$LABEL[match(df_base$INTERVAL, LABELS$INTERVAL)]
  df_base$INTERVAL <- factor(df_base$INTERVAL, levels = as.character(LABELS$LABEL)[n_int:1])
  df_base$GROUP <- factor(substr(df_base$MODELO,1,4), levels = unique(substr(df_base$MODELO,1,4)))
  df_base$MODELO <- factor(substr(df_base$MODELO,6,8), levels = c("RG3", "MOD", "MKT"))


  DB_1 <- data.frame(id = df_base[,2],
                     round = df_base[,1],
                     episode = df_base[,4],
                     group = df_base[,5])

  plot_sankey <- ggplot(DB_1, aes(x = round, stratum = episode, alluvium = id,
                                  fill = episode, label = episode)) +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=16),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 36),
          plot.subtitle = element_text(hjust = 0.5, size = 28),
          strip.text.x = element_text(size = 15),
          legend.text = element_text(size = 20),
          legend.title = element_text(size=20),
          legend.position = "left") +
    geom_flow(width=0.45) +
    #geom_alluvium(width=0.45)+
    geom_stratum(color = NA, width=0.45) +
    #scale_fill_identity()+
    #scale_fill_identity("ASERTIVIDAD",guide="legend", labels=colortable$episode, breaks=colortable$color, drop=TRUE) +
    scale_fill_manual("ASERTIVIDAD",values=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#999999")[n_int:1], drop=F)+
    ggtitle(titulo)+
    guides(fill=guide_legend(
      keywidth=0.5,
      keyheight=0.6,
      default.unit="inch")
    )+
    facet_grid(~group, scales="free", space="free"
               ,
               labeller = as_labeller(label)
    )
  #+geom_text(stat = "stratum", fontface = "bold", color = "black", size=5)

  if(ruta!=""){

    png(ruta, width=4500, height=2500, res=300)
    plot(plot_sankey)
    dev.off()

  }

  print(plot_sankey)
  #ggsave(paste(cwd,"/PER_EVOL_SALES_201909.png", sep=""), height = 10, width = 15)
}




SankeyTimeSeriesQ <- function(df_base,
                              interv,
                              titulo,
                              linea = "",
                              ruta = "",
                              qvar = "",
                              freq = F,
                              plot.qvar = (qvar!="")[1],
                              show.lab = T){

  library(ggrepel)
  library(ggalluvial)
  library(RColorBrewer)
  library(dplyr)

  df_base[,1] <- as.character(df_base[,1])
  df_base <- df_base[df_base[,3]!=0,]

  n_int <- length(interv) + 1
  interv <- c(0, interv, Inf)
  df_base$INTERVAL <- cut(df_base[,3], b = interv)

  LABELS <- data.frame(INTERVAL = sort(levels(df_base$INTERVAL)),
                       LABEL = c(paste0("<", interv[2]/1000, "K"),
                                 mapply(function(x,y) paste0(x/1000, "K-\n", y/1000, "K"), interv[2:(n_int-1)], interv[3:n_int]),
                                 paste0(">", interv[n_int]/1000,"K")))

  LABELS <- data.frame(INTERVAL = sort(levels(df_base$INTERVAL)),
                       LABEL = c(paste0("<",interv[2]*100, "%"),
                                 mapply(function(x,y) paste0(x*100, "% - ", y*100, "%"), interv[2:(n_int-1)], interv[3:n_int]),
                                 paste0(">",interv[n_int]*100,"%")))

  df_base$otro <- df_base$INTERVAL
  df_base$INTERVAL <- LABELS$LABEL[match(df_base$INTERVAL, LABELS$INTERVAL)]
  df_base$INTERVAL <- factor(df_base$INTERVAL, levels = as.character(LABELS$LABEL)[n_int:1])



  DB_1 <- data.frame(id = df_base[,2],
                     round = df_base[,1],
                     episode = df_base[,4],
                     interv = df_base[,5],
                     quantity = qvar)
  colortable<- data.frame(episode = factor(unique(LABELS$LABEL), levels = as.character(LABELS$LABEL)[n_int:1]),
                          color = c(c("#E41A1C","#377EB8")[min(2, 7-n_int):2], c("#4DAF4A","#3F8A38")[0:max(1,n_int-5)], c("#984EA3")[0:min(1,n_int-3)], c("#FF7F00", "#999999")[min(2, max(1,6-n_int)):2]))

  DB_FINAL <- DB_1 %>% left_join(colortable, by="episode")


  if(plot.qvar){
    p <- ggplot(DB_FINAL, aes(x = round, y=quantity, stratum = episode, alluvium = id,
                              fill = color, label=episode))
  }
  else{
    p <- ggplot(DB_FINAL, aes(x = round, stratum = episode, alluvium = id,
                              fill = color, label=episode))
  }


  plot_sankey <- p +
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size=20),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          text = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 40),
          plot.subtitle = element_text(hjust = 0.5, size = 27),
          strip.text.x = element_text(size = 15),
          legend.text = element_text(size = 20),
          legend.title = element_text(size=24),
          legend.position = "left")+
    geom_flow(width=0.45) +
    geom_stratum(color = NA, width=0.45) +
    scale_fill_identity("ASERTIVIDAD",
                        guide="legend",
                        labels=colortable$episode[n_int:0],
                        breaks=colortable$color[n_int:0],
                        drop=F) +
    ggtitle(titulo) +
    labs(subtitle = paste0("\n", linea," (CAMPAÑA ", min(as.character(df_base$CAMPANA)),
                           " - CAMPAÑA ", max(as.character(df_base$CAMPANA)),")")) +
    guides(fill=guide_legend(
      keywidth=0.6,
      keyheight=0.8,
      default.unit="inch")
    )

  if(show.lab){


    plot_sankey <- plot_sankey + geom_label_repel(aes(label=sapply(1:nrow(df_base),function(x){ifelse((qvar!="")[1] & !freq, qvar[x], 1)})),
                                                  stat = "stratum",
                                                  fontface = "bold",
                                                  color = "black",
                                                  size = 5,
                                                  nudge_y = 0,
                                                  direction = "both",
                                                  #hjust        = 1,
                                                  vjust = -0.25,
                                                  segment.size = 0.25,
                                                  force = 1,
                                                  show.legend = FALSE)
  }

  if(ruta != ""){

    png(ruta, width = 5000, height = 3000, res = 300)
    plot(plot_sankey)
    dev.off()

  }

  print(plot_sankey)

  #ggsave(paste(cwd,"/PER_EVOL_SALES_201909.png", sep=""), height = 10, width = 15)
}



ServiceLevel <- function(df_base,
                         titulo,
                         subtitulo="",
                         ruta = "",
                         show.lab = T,
                         umbral = 0.95){
  library(ggrepel)

  labels <- colnames(df_base)[2:ncol(df_base)]

  titulo_final <- paste0(titulo," (", unique(substr(df_base$CAMP,1,4)),")")

  df_base$CAMP <- factor(paste0("C",substr(df_base$CAMP, 5, 6)),
                         levels = paste0("C", sprintf("%02d", 1:18)) )

  df_base <- reshape(df_base,
                     direction="long",
                     varying = list(2:ncol(df_base)),
                     timevar = "ZONAS",
                     v.names = "NIV_SERV")
  df_base <- data.frame(CAMP = df_base$CAMP,
                        ZONAS = labels[df_base$ZONAS],
                        NIV_SERV = df_base$NIV_SERV)
  df_base <- cbind(df_base,
                   LAB = paste0(format(df_base[, ncol(df_base)]*100, digits=3), "%"))

  plot_serlev <- ggplot(df_base, aes(x = CAMP, y = NIV_SERV, color= ZONAS, labels=LAB)) +
    theme(panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_text(size=22, color = "gray28"),
          panel.grid.major = element_line(color = "black", size=1),
          text = element_text(face = "bold"),
          plot.title = element_text(hjust = 0, size = 27, face = "bold.italic", color = "#636363"),
          plot.subtitle = element_text(hjust = 1, size = 22, face = "bold.italic", color = "#636363"),
          strip.text.x = element_text(size = 15),
          axis.line = element_line(colour = "black", size=1.5),
          legend.position = c(0.8845, 0.3),
          legend.title = element_blank(),
          legend.text = element_text(size=16),
          legend.background = element_rect(fill="white",
                                           size=1,
                                           linetype="solid",
                                           color="black"))+
    geom_rect(xmin = 0, xmax = length(levels(df_base$CAMP))+1, ymin = 0.5, ymax = umbral, fill="#fac8bf",col="black", alpha=0.1) +
    geom_rect(xmin = 0, xmax = length(levels(df_base$CAMP))+1, ymin = umbral, ymax = 2, fill="#99ffba",col="black", alpha=0.1) +
    geom_point(aes(group = ZONAS), size=3.5) +
    ggtitle(titulo_final) +
    geom_line(aes(group = ZONAS), size=2) +
    scale_x_discrete(drop=F) +
    scale_y_continuous(limit=c(0.65, 1), expand = c(0,0), breaks=seq(0.65,1,0.05), labels = scales::percent_format(accuracy = 1L))+
    scale_color_manual(values = c("#404040", "#0000b3")) +
    xlab("\nCampañas") +
    ylab("Nivel de Servicio") +
    guides(color=guide_legend(
      keywidth=0.6,
      keyheight=0.5,
      default.unit="inch"))

  if(show.lab){

    plot_serlev <- plot_serlev + geom_label_repel(aes(label=ifelse(CAMP == df_base$CAMP[length(df_base$CAMP)], as.character(LAB),""), size=5),
                                                  direction = "y",
                                                  hjust = 0,
                                                  nudge_x = 0.2,
                                                  show.legend = F)
  }

  if(subtitulo!=""){
    plot_serlev <- plot_serlev + labs(subtitle = paste0(subtitulo))
  }

  if(ruta != ""){
    png(ruta, width = 4200, height = 1800, res = 300)
    plot(plot_serlev)
    dev.off()
  }

  print(plot_serlev)
}



validate_models <- function(modset=c("",""), actual, reference = matrix(rep("",4),nr=2,nc=2), df_test=""){

  library(DescTools)
  library(relaimpo)

  if(modset[1]!=""){

    model_type <- sapply(modset, function(x){class(x)[[1]]})
    y_type <- sapply(modset, function(x){substr(deparse(x$terms[[2]]), 1, 3)})

    values <- sapply(modset, function(x){sapply(1:nrow(df_test),
                                                function(i)
                                                  tryCatch(predict(x, df_test[i,]),
                                                           error=function(e) NA))})
    if(is.null(nrow(values))){
      values <- data.frame(t(values))
    }

    colnames(values) <- names(modset)

    if(sum(y_type == "log") > 0){
      values[,y_type=="log"] <- sapply(values[,y_type=="log"], FUN = function(x){exp(x)})
    }

    if (reference[1,1] != ""){
      values <- cbind(values, reference)
      for (i in 1:ncol(reference)){
        model_type <- c(model_type, "lm")
        y_type <- c(y_type, "nor")
      }
    }

  }
  else{
    values <- reference

    if(is.null(nrow(values))){
      values <- data.frame(t(values))
    }

    model_type <- rep("lm", ncol(values))
    y_type <- rep("nor", ncol(values))
  }



  cons_var <- data.frame(MODEL = ifelse(model_type=="lm", "Linear Model",
                                        ifelse(model_type=="glm",
                                               "G. Linear Model",
                                               "Random Forest")))
  cons_var$TRANS <- ifelse(y_type == "log", "Logarithm", "Normal")
  cons_var$MAPE <- apply(values, 2, function(x){MAPE(x,actual, na.rm = T)})
  cons_var$MAE_S <- apply(values, 2, function(x){sum(sapply(x-actual,function(y){max(0,y)}), na.rm = T)})
  cons_var$MAE_S_C <- apply(values, 2, function(x){mean((actual/x <= 1), na.rm=T)})
  cons_var$MAE_F <- apply(values, 2, function(x){sum(sapply(actual-x,function(y){max(0,y)}), na.rm = T)})
  cons_var$MAE_suma <- apply(values, 2, function(x){sum(abs(actual-x), na.rm = T)})
  cons_var$MAE <- apply(values, 2, function(x){MAE(x, actual, na.rm = T)})
  cons_var$RMSE <- apply(values, 2, function(x){RMSE(x, actual, na.rm = T)})
  #cons_var$LNE <- apply(values, 2, function(x){sum(log(x/actual)^2)})
  cons_var$BRLE <- apply(values, 2, function(x){mean((actual/x <= 0.5), na.rm = T)})
  cons_var$BRRE <- apply(values, 2, function(x){mean((actual/x >= 1.8), na.rm = T)})
  cons_var$BRTE <- apply(values, 2, function(x){mean((actual/x <= 0.5) + (actual/x >= 1.8), na.rm = T)})
  row.names(cons_var) <- if(is.null(nrow(values))){rownames(values)}else{colnames(values)}
  #values$rank <-  rownames(data.frame(mod_lor1.1[7]))[order(data.frame(mod_lor1.1[7]), decreasing = T)]
  #ranking_lm <- names(calc.relimp(mod_lor1.3)$lmg)[order(calc.relimp(mod_lor1.3)$lmg, decreasing = T)]

  return(list(error_analysis = cons_var, forecast_values = values))

}
