#------------------------------------------------------------------------------#
# Proyecto:                   La muerte materna en México y la importancia de 
#                             las estimaciones confiables
# Objetivo:                   Analizar los datos de defunciones maternas de 
#                             INEGI y Secretaría de Salud
#
# Encargadas:                 Katia Guzmán Martínez; Irékani Alarcón
# Correos:                    katia.guzmart@gmail.com
# 
# Última actualización:       05 de octubre de 2022
#------------------------------------------------------------------------------#
# Fuentes:
# - MORTALIDAD 2021:         https://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/mortalidad/MortalidadGeneral.asp
# - NATALIDAD 2021:         https://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/natalidad/nacimientos.asp

# devtools::install_github("mxvscorrupcion/mccistyleR",auth_token = 
#                            "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")


Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(scipen=999)
options(survey.lonely.psu="adjust")
pacman::p_load(lubridate,hot.deck,zoo,stringi,gridExtra,ggthemes,tidyverse,glue,ggrepel,
               hrbrthemes,magick,scales,RColorBrewer,foreign,srvyr,openxlsx)
#colors <- c("#880e7a","grey","#e2d3ff" )
colors <- c("#a8c586","grey", "#308525")
`%+%` <- paste0
`%notin%` <- Negate(`%in%`)

# Directorios ----
paste_inp       <- function(x){paste0("01_datos_crudos/" , x)}
paste_out       <- function(x){paste0("02_datos_limpios/", x)}
paste_plot      <- function(x){paste0("03_plots/", x)}
catalogo_ent <- readRDS("02_datos_limpios/entidades.rds") 

# Procesamiento de datos ----
# Esta base se procesó y consolidó por motivos de eficiencia
mortalidad_materna <- readRDS("02_datos_limpios/mortalidad_materna2_2000_2020.rds") %>% 
    bind_rows(readRDS("02_datos_limpios/mortalidad_materna2_1990_1990.rds")%>% 
                  mutate(across(c(anio_ocur, anio_regis), ~ifelse(.<1998,1900+.,.))) ) %>% 
    mutate(cross_sitio_ocur = case_when(
        sitio_ocur == 1 ~ "Secretaría de Salud",
        sitio_ocur == 2 ~ "IMSS PROSPERA",
        sitio_ocur == 3 ~ "IMSS",
        sitio_ocur == 4 ~ "ISSSTE",
        sitio_ocur == 5 ~ "PEMEX",
        sitio_ocur == 6 ~ "SEDENA",#"Secretaría de la Defensa Nacional (SEDENA) ",
        sitio_ocur == 7 ~ "SEMAR",#"Secretaría de Marina (SEMAR)",
        sitio_ocur == 8 ~ "Otro",#"Otra unidad pública",
        sitio_ocur == 9 ~ "Unidad médica privada",
        sitio_ocur == 10 ~ "Vía pública",
        sitio_ocur == 11 ~ "Hogar",
        sitio_ocur == 12 ~ "Otro",#"Otro lugar",
        #sitio_ocur == 99 ~ "No especificado"
        T ~ NA_character_
    ))

# Esta base se procesó y consolidó por motivos de eficiencia
natalidad_mes  <- readRDS("02_datos_limpios/natalidad.rds") %>% 
    mutate(ent_ocurr = str_pad(ent_ocurr,width = 2,"left","0"))

## Corrección por subregistro ----
anio <- str_pad(20,width = 2,side = "left",pad = "0")

# LA SIGUIENTE BASE DE DATOS NO SE ENCUENTRA EN ESTE REPOSITORIO 
# SE PUEDE DESCARGAR EN: 
# https://www.inegi.org.mx/contenidos/programas/natalidad/microdatos/2020/natalidad_base_datos_2020_dbf.zip
natalidad_temp <- read.dbf("01_datos_crudos/NACIM20.DBF") %>% 
    janitor::clean_names() %>% 
    rename_all(~stri_replace_all(.,fixed ="_nacim", "_nac")) %>% 
    mutate(across(where(~is.factor(.)), ~as.character(.)))
names(natalidad_temp)
natalidad_temp2 <- natalidad_temp %>%
    count(ent_ocurr, ano_nac, mes_nac, dia_nac, fue_prese) %>% 
    mutate(fue_prese = case_when(
        fue_prese == 1 ~ "vivo",
        fue_prese == 2 ~ "muerto",
        T ~ "noespecificada"
    )) %>% 
    pivot_wider(names_from = fue_prese,values_from = n,values_fill = 0)
if("noespecificada"%in%names(natalidad_temp2)){
    natalidad_temp2 <- natalidad_temp2 %>%
        mutate(total = vivo + muerto + noespecificada)
} else {
    natalidad_temp2 <- natalidad_temp2 %>%
        mutate(total = vivo + muerto)
}

nueva_natalidad <- natalidad_mes %>%
    group_by(mes_ocurr=mes_nac, anio_ocur=ano_nac) %>% 
    summarise(vivo = sum(vivo, na.rm = T)) %>% 
    left_join(
        natalidad_temp2%>%
            group_by(mes_ocurr=mes_nac, anio_ocur=ano_nac) %>% 
            summarise(vivo2 = sum(vivo, na.rm = T)) %>% 
            filter(anio_ocur ==2020)
    ) %>% 
    filter(anio_ocur ==2020) %>% 
    group_by(mes_ocurr, anio_ocur) %>% 
    transmute(diferencia = vivo-vivo2) %>% 
    mutate(anio_ocur = anio_ocur+1) %>% 
    right_join( natalidad_mes %>%
                    group_by(mes_ocurr=mes_nac, anio_ocur=ano_nac) %>% 
                    summarise(vivo = sum(vivo, na.rm = T)) ) %>% 
    mutate(diferencia = ifelse(is.na(diferencia), 0, diferencia),
           vivo = vivo+diferencia
    )


# Gráficas -----
d_ssa_inegi <- mortalidad_materna %>% 
    filter(anio_ocur != 9999) %>% 
    filter(anio_ocur >= 2012) %>% 
    #filter(!is.na(razon_m)) %>% 
    count(razon_m, anio_ocur) %>% 
    left_join(
        natalidad_mes %>%
            #filter(ano_nac > 2018) %>% 
            group_by(ano_nac) %>% 
            summarise(vivo = sum(vivo, na.rm = T)) %>% 
            rename(anio_ocur =ano_nac)
    ) %>% 
    mutate(
        razon = n/vivo,
        razon = razon *100000,
        tipo = "INEGI"
    ) %>% 
    drop_na() %>% 
    select(
        anio = anio_ocur, tot = n, rmm = razon, tipo
    ) %>% 
    bind_rows(
        # Captura manual de datos de defunciones y RMM publicados por la SSalud 
        tribble(
            ~anio,	~tot,	~rmm, ~tipo,
            2012,	940,    41.2, "Secretaría de Salud",
            2013,	894,    39.3, "Secretaría de Salud",
            2014,	960,    42.6, "Secretaría de Salud",
            2015,	769,    34.6,   "Secretaría de Salud",
            2016,	774,    35.5, "Secretaría de Salud",
            2017,	749,    34.5,   "Secretaría de Salud",
            2018,	697,    33.9, "Secretaría de Salud",
            2019,	685,    33.3, "Secretaría de Salud",
            2020,	963,   44.8, "Secretaría de Salud",
            2021,	1036,   53.1, "Secretaría de Salud"
        )
    ) %>%
    bind_rows(
        mortalidad_materna %>% 
            filter(anio_ocur != 9999) %>% 
            filter(anio_ocur >= 2000) %>% 
            #filter(!is.na(razon_m)) %>% 
            count(razon_m, 
                  anio_ocur) %>% 
            left_join(
                nueva_natalidad %>%
                    #filter(ano_nac > 2018) %>% 
                    group_by(anio_ocur) %>% 
                    summarise(vivo = sum(vivo, na.rm = T)) 
            ) %>% 
            mutate(
                razon = n/vivo,
                razon = razon *100000,
                tipo = "INEGI (corregido por subregistro)"
            ) %>% 
            select(anio = anio_ocur, tot = n, rmm = razon, tipo) %>% 
            filter(anio == 2021)
    ) %>% 
    glimpse

## Defunciones maternas (INEGI vs SSalud) ----
ggplot(
    d_ssa_inegi %>% 
        filter(tipo != "INEGI (corregido por subregistro)"),
    aes(
        x = as.factor(anio),
        y = tot,
        group = tipo,
        col = tipo,
        label = scales::number(tot, big.mark = ",")
    )
) + 
    geom_line(size = 1.5) + geom_point(size = 1.8) +
    # scale_x_continuous(breaks = seq(2000,2022,3)) +
    scale_y_continuous(limits =  c(500,1200), labels = scales::number_format(big.mark = ","))+
    labs(
        title= str_wrap("Defunciones maternas", 40),
        subtitle = "Por año de ocurrencia",
        caption = "Elaboración por @guzmart_ y @irekanialarcon con información del @INEGI_informa",
        x= "",
        y= "",
        fill = "",
        color = "",
    ) + 
    geom_label(
        size = 5, family = "Gotham Narrow Medium", #nudge_y = 5,
        show.legend = F, 
        aes(vjust = case_when(
            tipo == "INEGI" & anio != 2013 & anio != 2014 ~ -0.4, 
            tipo == "INEGI" & (anio == 2013 | anio == 2014) ~ 1.8,
            tipo != "INEGI" & (anio == 2013 | anio == 2014) ~ -0.4,
            T ~ 1.5
        ))
    )+
    scale_color_manual(values = rev((colors[-2])))+
    theme_bw() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.caption = element_text(size = 24),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 20),
        text = element_text(family = "Ubuntu"),
        legend.position = "top"
    )
ggsave(filename = paste_plot("01_defunciones_maternas.png"), 
       width = 15, height = 15, dpi = 200, bg= "transparent")
## Razón de mortalidad materna (INEGI vs SSalud) ----
ggplot(
    d_ssa_inegi%>% 
        filter(tipo != "INEGI (corregido por subregistro)"),
    aes(
        x = as.factor(anio),
        y = rmm,
        group = tipo,
        col = tipo,
        label = scales::number(round(rmm,2), big.mark = ",")
    )
) + 
    geom_line(size = 1.5) + geom_point(size = 1.8) +
    scale_y_continuous(limits =  c(20,100), labels = scales::number_format(big.mark = ","))+
    labs(
        title= str_wrap("Razón de mortalidad materna", 40),
        subtitle = "Por año de ocurrencia",
        caption = "Elaboración por @guzmart_ y @irekanialarcon con información del @INEGI_informa",
        x= "",
        y= "",
        fill = "",
        color = "",
    ) + 
    geom_label(
        size = 5, family = "Gotham Narrow Medium", #nudge_y = 5,
        show.legend = F, 
        aes(vjust = case_when(
            tipo == "INEGI" & anio != 2013 & anio != 2014 ~ -0.4, 
            tipo == "INEGI" & (anio == 2013 | anio == 2014) ~ 1.8,
            tipo != "INEGI" & (anio == 2013 | anio == 2014) ~ -0.4,
            T ~ 1.5
        ))
    )+
    scale_color_manual(values = rev((colors[-2])))+
    theme_bw() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.caption = element_text(size = 24),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 20),
        text = element_text(family = "Ubuntu"),
        legend.position = "top"
    )
ggsave(filename = paste_plot("02_rmm.png"), 
       width = 15, height = 15, dpi = 200, bg= "transparent")

## Razón de mortalidad materna (corregida por subregistro) ----
ggplot(
    d_ssa_inegi,
    aes(
        x = as.factor(anio),
        y = rmm,
        group = tipo,
        col = tipo,
        label = scales::number(round(rmm,2), big.mark = ",")
    )
) + 
    geom_segment(aes(x = "2020", y = 60.96, xend = "2021", yend = 66.54), linetype = "dashed",
                 size = 1.5) +
    geom_line(size = 1.5) + geom_point(size = 1.8) +
    # scale_x_continuous(breaks = seq(2000,2022,3)) +
    scale_y_continuous(limits =  c(20,100), labels = scales::number_format(big.mark = ","))+
    labs(
        title= str_wrap("Razón de mortalidad materna", 40),
        subtitle = "Por año de ocurrencia",
        caption = "Elaboración por @guzmart_ y @irekanialarcon con información del @INEGI_informa",
        x= "",
        y= "",
        fill = "",
        color = "",
    ) + 
    geom_label(
        size = 5, family = "Gotham Narrow Medium", #nudge_y = 5,
        show.legend = F, 
        aes(vjust = case_when(
            tipo == "INEGI" & anio != 2013 & anio != 2014 ~ -0.4, 
            tipo == "INEGI" & (anio == 2013 | anio == 2014) ~ 1.8,
            tipo != "INEGI" & (anio == 2013 | anio == 2014) ~ -0.4,
            T ~ 1.5
        ))
    )+
    scale_color_manual(values = c(colors[1], "#ff6260", colors[3]))+
    theme_bw() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.caption = element_text(size = 24),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 20),
        text = element_text(family = "Ubuntu"),
        legend.position = "top"
    )
ggsave(filename = paste_plot("03_rmm_corregida.png"), 
       width = 15, height = 15, dpi = 200, bg= "transparent")


## Razón de mortalidad materna (por mes durante la pandemia) ----
ggplot(
    mortalidad_materna %>% 
        filter(anio_ocur > 2019) %>% 
        mutate(fecha_ocur = as.Date(sprintf("01 %s %s", mes_ocurr, anio_ocur),
                                    format = "%d %m %Y")
        ) %>% 
        #filter(!is.na(razon_m)) %>% 
        count(razon_m, 
              fecha_ocur) %>% 
        left_join(
            nueva_natalidad %>%
                filter(anio_ocur > 2018) %>% 
                
                mutate(fecha_ocur = as.Date(sprintf("01 %s %s", mes_ocurr, anio_ocur),
                                            format = "%d %m %Y")
                ) %>% 
                group_by(fecha_ocur) %>% 
                summarise(vivo = sum(vivo, na.rm = T))
        ) %>% 
        mutate(razon = n/vivo,
               razon = razon *100000) %>% 
        drop_na() ,
    aes(x = fecha_ocur,
        y = razon,            
        color = "a",
    )
    
) +
    geom_line(size = 1.5) + geom_point(size = 1.8) +
    geom_vline(xintercept = as.Date("15 05 2021", format = "%d %m %Y"), color = "red", linetype ="dashed")+
    #scale_x_continuous(breaks = 2000:2022) +
    scale_x_date(date_labels = "%b-%y",date_breaks = "3 months")+
    scale_y_continuous(limits =  c(0,150),breaks = seq(0,150,25),expand = c(0,0))+
    labs(
        title= "Razón de mortalidad materna\n(por cada 100,000 nacidos vivos)",
        subtitle = "Por mes de ocurrencia durante la pandemia por COVID-19\nEn rojo la fecha de inicio de vacunación de mujeres embarazadas\n(15 de mayo de 2021)",
        caption = "Elaboración por @guzmart_ y @irekanialarcon con información del @INEGI_informa",
    ) + 
    geom_text_repel(
        aes(
            label = case_when(
                fecha_ocur == "2020-07-01" ~ paste0(as.yearmon(fecha_ocur), "\n", round(razon,2)),
                fecha_ocur == "2021-01-01" ~ paste0(as.yearmon(fecha_ocur), "\n", round(razon,2)),
                fecha_ocur == "2021-08-01" ~ paste0(as.yearmon(fecha_ocur), "\n", round(razon,2)),
                T ~ as.character(round(razon,2))
            ), 
        ),
        color = "black",size = 5, family = "Gotham Narrow Medium", 
        nudge_y = 5, show.legend = F
    )+
    scale_color_manual(values = (colors[-2]))+
    theme_bw() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.caption = element_text(size = 24),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 20),
        text = element_text(family = "Ubuntu"),
        legend.position = "none"
    )
ggsave(filename = paste_plot("04_rmm_pandemia.png"), 
       width = 15, height = 15, dpi = 200, bg= "transparent")
## Razón de mortalidad materna (por mes y por año) ----
ggplot(
    mortalidad_materna %>% 
        filter(anio_ocur != 9999) %>% 
        filter(anio_ocur >= 2000) %>% 
        count(razon_m, mes_ocurr, anio_ocur) %>% 
        left_join(
            nueva_natalidad %>%
                # filter(ano_nac > 2018) %>% 
                group_by(mes_ocurr, anio_ocur) %>% 
                summarise(vivo = sum(vivo, na.rm = T))
        ) %>% 
        mutate(razon = n/vivo,
               razon = razon *100000) %>% 
        #filter(!(anio_ocur == 2021 & mes_ocurr==12)) %>% 
        mutate(color = case_when(
            anio_ocur == 2020 ~ "2020",
            anio_ocur == 2021 ~ "2021",
            T ~ "2000-2019"
        ),
        labb = format(as.Date("01 "%+%mes_ocurr%+%" 2020", format = "%d %m %Y"), format = "%b")
        ) %>% 
        drop_na(labb) ,
    aes(x = mes_ocurr,
        y = razon,            
        label = ifelse(anio_ocur >2019,round(razon,2),NA), 
        group = anio_ocur,
        color = color,
    )
    
) +
    geom_line(size = 1.5) + geom_point(size = 1.8) +
    geom_text_repel(color = "black",size = 5, family = "Gotham Narrow Medium", nudge_y = 5,
                    show.legend = F)+
    scale_x_continuous(breaks = 1:12,labels = stri_trans_totitle(format(as.Date("01 "%+%1:12%+%" 2020", format = "%d %m %Y"), format = "%b"))) +
    #scale_x_date(date_labels = "%b-%y",date_breaks = "3 months")+
    scale_y_continuous(limits =  c(0,150),breaks = seq(0,150,25),expand = c(0,0))+
    labs(
        title= "Razón de mortalidad materna\n(por cada 100,000 nacidos vivos)",
        #subtitle = "Por mes de ocurrencia durante la pandemia por COVID-19",
        subtitle = "Por mes de ocurrencia durante la pandemia por COVID-19",
        caption = "Elaboración por @guzmart_ y @irekanialarcon con información del @INEGI_informa",
        x= "",
        y= "",
        fill = "",
        color = "",
    ) + 
    scale_color_manual(breaks = c("2020","2021","2000-2019"), values = c("#a8c586" ,"#308525","grey")) +
    theme_bw() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.caption = element_text(size = 24),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 20),
        text = element_text(family = "Ubuntu"),
        legend.position = "top"
    )
ggsave(filename = paste_plot("05_rmm_mes_anio.png"), 
       width = 15, height = 15, dpi = 200, bg= "transparent")
