#Analizar el estado de cuenta
library(tidyverse)
library(pdftools)
library(stringr)
library(stringi)
library(utils)
library(rebus)
library(shiny)
library(data.table)
library(lubridate)
options(scipen = 999)


# Primero resolvemos el caso ----------------------------------------------
nombre_cuenta <- "my_account_balance.pdf"
contrasena <- '****'


cuenta <- pdf_text(nombre_cuenta,upw = contrasena)

#Hay tantos objetos como paginas de pdf
paginas_pdf <- length(cuenta)
#Vamos a quedarnos con los objetos que tienen valores de transacciones
#Viendo el pdf parece que la ultima pagina tiende a no tener nada mas que terminos y condiciones

cuenta_analizar <- cuenta[1:(paginas_pdf-1)]

#Vamos a analizar cada pagina por separado
pag <- cuenta_analizar

extrae_estado_cuenta <- function(pagina_estado_cuenta){
    
  
    # pagina_estado_cuenta <- pag[2] 
  
  #Como se ve este objeto?
   # View(pagina_estado_cuenta)
  #Vemos un objeto con una sola observacion, tenemos que llevarlo a algo mas facil de trabajar
  
  
   #Separamos la pagina en lineas y convertimos el objeto en una observacion de texto por linea
   lineas_pag <- stri_split_lines1(pagina_estado_cuenta)
   
   #es dificil hacer match de texto con el lenguaje en castellano por las tildes diacriticas  
   #traducimos tildes y caracteres especiales del castellano a un lenguaje generico
   lineas_trad <- stri_trans_general(lineas_pag, id = 'Latin-ASCII')
   
   
   #Busquemos algunas observaciones para trabajarlas
   # ejemplo_lineas <- lineas_trad[29:41]
   
   #Comparemos el resultado con la anterior vista
   # View(ejemplo_lineas)
   
   #Busquemos unos patrones para detectar
   
   #Valores monetarios
   #Posibles casos: 990, 1.990, 10.990, 100.000
   valor_monetario <- one_or_more(DGT) %R% DOT %R% digit(3)
   #Como se lee esta expresion: Uno o mas digitos y luego un punto y luego 3 digitos
   
   # #Veamos como se ve este match en el texto buscado
   # str_view(string = ejemplo_lineas, pattern = valor_monetario)
   # str_view_all(string = ejemplo_lineas, pattern = valor_monetario)
   
   #Opciones 
   patron_cuotas <- or('COMPRA SIMPLE', 'COMPRA EN CUOTA')
   
   # str_view(ejemplo_lineas, patron_cuotas)
   
   
   #Podemos llegar a escribir patrones muy complejos de manera intuitiva
   ubicacion <- START %R% one_or_more(WRD) %R% optional(zero_or_more(SPC)) %R% one_or_more(WRD) %R% optional(zero_or_more(SPC)) %R% one_or_more(WRD)
   
   
   # str_view(ejemplo_lineas, ubicacion)
   
   #Intentemos llevar esta data a un formato mas "usable"
   #Usamos el operador capture()
   
   #la captura nos permite "Marcar" la data que queremos obtener
   #con la ayuda de una funcion, podemos llevar las capturas a una matriz
   # patron_ubicacion_fecha <- capture(ubicacion) %R% one_or_more(SPC) %R% capture(patron_fechas) %R% SPC
   # 
   # str_view_all(ejemplo_lineas, patron_ubicacion_fecha)
   # 
   # str_match(ejemplo_lineas, patron_ubicacion_fecha)
   #Retorna la primera columna con la fila completa y las demas columnas con los match correspondientes
   
   #Nos quedamos con la info que queremos y la trabajamos en el esquema que tanto amamos (<3 tidyverse)
   # str_match(ejemplo_lineas, patron_ubicacion_fecha)[, -c(1)] %>% 
   #   data.table() %>% 
   #   set_names(c("lugar", "fecha"))
   
   
   # Patrones a detectar -----------------------------------------------------
   patron_inicio <- or('2.1. TOTAL OPERACIONES', "LUGAR DE OPERACION")
   patron_fin <- or("3. INFORMACION DE PAGO", "2 de 3", "3 de 3")
   ubicacion <- START %R% one_or_more(WRD) %R% optional(zero_or_more(SPC)) %R% one_or_more(WRD) %R% optional(zero_or_more(SPC)) %R% one_or_more(WRD)
   patron_fechas <- one_or_more(DGT) %R% '/' %R% one_or_more(WRD) %R% '/' %R% one_or_more(DGT)
   patron_ubicacion_fecha <- capture(ubicacion) %R% one_or_more(SPC) %R% capture(patron_fechas) %R% SPC
   sin_cuotas <- 'COMPRA SIMPLE'
   cuotas <- 'COMPRA EN CUOTA'
   patron_cuotas <- or(sin_cuotas, cuotas)
   patron_compra <- or('RIPLEY', patron_cuotas)
   patron_montos <-  or(SPC %R%
     capture(one_or_more(DGT) %R%
               DOT %R%
               one_or_more(DGT)) %R%
     one_or_more(SPC) %R%
     capture(one_or_more(DGT) %R%
               DOT %R%
               one_or_more(DGT)) %R%
     one_or_more(SPC) %R%
     capture(zero_or_more(WRD) %R%
               '/' %R%
               zero_or_more(WRD)) %R%
     capture(one_or_more(SPC) %R%
               one_or_more(DGT) %R%
               DOT %R%
               one_or_more(DGT)), 
     SPC %R% capture(digit(3)) %R% 
       one_or_more(SPC) %R%
       capture(digit(3)) %R%
       one_or_more(SPC) %R%
       capture(zero_or_more(WRD) %R%
                         '/' %R%
                         zero_or_more(WRD)) %R%
       one_or_more(SPC) %R%
       capture(digit(3)) %R% END)
   
   
   espacios_de_sobra <- SPC %R% one_or_more(SPC)
   # patron_concepto <- ubicacion %R% one_or_more(SPC) %R% patron_fechas %R% SPC %R% one_or_more(one_or_more(WRD %R% SPC))
   
   
   # " 999 999 S/C 999" %>% 
   #   str_view(SPC %R% capture(digit(3)) %R% 
   #              SPC %R% capture(digit(3)) %R%
   #              SPC %R% capture(zero_or_more(WRD) %R%
   #                                '/' %R%
   #                                zero_or_more(WRD)) %R%
   #              SPC %R% capture(digit(3)) %R% END)
   # 
   # " 999 999 S/C 999" %>% 
   #   str_view(patron_montos)
   #Hasta poder identificar bien el patron del concepto, vamos a definir el concepto como todo aquello que no es ubicacion, fecha ni monto
   
   #Las transacciones aparecen 2 lineas despues de un caracter que dice '2.1. TOTAL OPERACIONES' y 1 linea antes de "3. INFORMACION DE PAGO"
   # str_detect(lineas_trad, pattern = patron_inicio)
   # sum(str_detect(lineas_trad, pattern = patron_inicio))
   # sum(str_detect(lineas_trad, pattern = patron_fin))
   
   
   linea_inicio <- which(str_detect(lineas_trad, patron_inicio)) + 2
   linea_fin <- which(str_detect(lineas_trad, patron_fin)) - 2
   
   #Buscamos las lineas con una transaccion. Esas son aquellas que tienen un patron de numeros y las que estan entre el inicio y el fin
   # transacciones <- lineas_trad[linea_inicio:linea_fin] %>% 
   #                  str_subset(pattern = patron_montos)
   
   transacciones <- lineas_trad %>% 
                    str_subset(pattern = or(sin_cuotas, cuotas, 'S/C'))
   
   #Vamos a reemplazar las palabras que sobran como compra en cuotas o compra simple
   transacciones_clean <- str_remove(transacciones, patron_cuotas)
   
   
   #Nos quedamos con los conceptos de las transacciones
   conceptos <- transacciones_clean %>% #tail(6) %>% 
     str_replace(pattern = patron_ubicacion_fecha, "") %>% 
     str_remove_all(pattern = patron_montos) %>% 
     str_trim() %>% 
     str_remove_all(pattern = espacios_de_sobra) %>% 
     str_remove(pattern = "S/C$")
   
   
   #Vamos a realizar una operacion recursiva que nos permita obtener toda la informacion del texto 
   patrones_cuenta <- list(patron_ubicacion_fecha,
                           patron_montos)
   
   
   #Como podemos 
   
   datos_cuenta <- map(patrones_cuenta, function(patrones_cuenta, .df = transacciones_clean){
     
     # print(paste("Trabajando el patron", ""))
     # .df <- transacciones_clean
     # patron <- patrones_cuenta %>% pluck(2)
     
     patron <- unique(patrones_cuenta)
     
     #Imprimimos el patron como referencia
     print(patron)
     
     #Guardamos los patrones capturados por las expresiones regulares
     df_patrones <- str_match(.df, pattern =  patron)[,-c(1)]
     # df_patrones <- str_match(.df, pattern =  patron)
     
     
     #Imprimimos el df
     df_patrones
   } ) %>% 
     reduce(cbind) %>% 
     cbind(., conceptos) %>% 
     data.table() 
   
   
   #Renombramos las columnas de nuestros datos
   datos_cuenta <- datos_cuenta %>% 
     set_names(c('lugar', 'fecha', "pago_operacion_miles", "monto_total_pagar_miles", "cant_cuotas_miles", "valor_cuota_miles", "pago_operacion_cientos", "monto_total_pagar_cientos", "cant_cuotas_cientos", "valor_cuota_cientos", "concepto")) %>% 
     # Nos quedamos con una sola columna
     mutate(pago_operacion = ifelse(!is.na(pago_operacion_miles), pago_operacion_miles, pago_operacion_cientos),
            monto_total_pagar = ifelse(!is.na(monto_total_pagar_miles), monto_total_pagar_miles, monto_total_pagar_cientos),
            cant_cuotas = ifelse(!is.na(cant_cuotas_miles), cant_cuotas_miles, cant_cuotas_cientos),
            valor_cuota = ifelse(!is.na(valor_cuota_miles), valor_cuota_miles, valor_cuota_cientos)) %>% 
     select(-matches("_miles|_cientos"))
   
   
   #Damos formato a las columnas para que se puedan usar
   # glimpse(datos_cuenta)
   
   datos_cuenta_clean <- datos_cuenta %>% 
     mutate(pago_operacion = as.numeric(str_replace(pago_operacion, "\\.", "")),
            monto_total_pagar = as.numeric(str_replace(monto_total_pagar, "\\.", "")),
            valor_cuota = as.numeric(str_replace(valor_cuota, "\\.", "")),
            cuota_actual = as.numeric(ifelse(cant_cuotas == "S/C", 0, str_sub(cant_cuotas, 1, 2))),
            cuotas_totales = as.numeric(ifelse(cant_cuotas == "S/C", 0, str_sub(cant_cuotas, 4, 5))),
            dia = str_sub(fecha, 1, 2),
            mes = str_sub(fecha, 4, 6),
            mes = case_when(mes == 'ENE' ~'01',
                            mes == 'FEB' ~ '02',
                            mes == 'MAR' ~ '03',
                            mes == "ABR" ~ '04',
                            mes == "MAY" ~ "05",
                            mes == "JUN" ~ "06",
                            mes == "JUL" ~ "07",
                            mes == "AGO" ~ "08",
                            mes == "SEP" ~ "09",
                            mes == "OCT" ~ "10",
                            mes == "NOV" ~ "11",
                            mes == "Dic" ~ "12",
                            T ~ "Revisar"),
            yr = paste0(20, str_sub(fecha, 8, 9)),
            fecha2 = as.Date(paste(yr,mes,dia, sep ="-"))) %>% 
     select(lugar, fecha = fecha2, concepto,  pago_operacion, monto_total_pagar, valor_cuota, cuota_actual, cuotas_totales)
   
   
   
 } 

datos_estado_cuenta <- pag %>% 
                       map_dfr(extrae_estado_cuenta)
  


