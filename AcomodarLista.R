


# Paquetes
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# 1) Importa (ajusta la ruta/encoding si hace falta)
df <- read_csv("rsvp (2).csv", locale = locale(encoding = "UTF-8"))

# 2) Pivotea a long tomando solo columnas asiste_*
#    y preserva Codigo y Comentario si existen (si el nombre varía, cámbialo aquí)
clean <- df %>%
  # Si tus nombres son 'Código' o 'comentarios', ajusta abajo:
  rename(Codigo = any_of(c("Codigo","Código","codigo")),
         Comentario = any_of(c("Comentario","comentario","comentarios"))) %>%
  pivot_longer(
    cols = matches("^asiste(_|)[0-9]*$|^asiste_[0-9]+$|^asiste\\d+$|^asiste_\\d+$"),
    names_to = "col_asiste",
    values_to = "raw_asiste",
    values_drop_na = TRUE
  ) %>%
  # 3) Separar "Nombre | SI/NO" en dos columnas
  separate_wider_delim(
    raw_asiste, delim = "|",
    names = c("Asistentes", "Asistencia"),
    too_few = "align_start", cols_remove = TRUE
  ) %>%
  # 4) Limpieza fina
  mutate(
    Asistentes = Asistentes %>% str_trim(),
    Asistencia = Asistencia %>% str_trim() %>% str_to_upper()
  ) %>%
  # 5) Filtrar filas vacías (por si hay celdas sin nombre)
  filter(!is.na(Asistentes), Asistentes != "") %>%
  # 6) Quedarse solo con las columnas pedidas
  select(Codigo, Asistentes, Asistencia, Comentario) %>%
  # 7) Orden opcional
  arrange(Codigo, Asistentes)



View(clean)


# 8) (Opcional) guardar la base limpia
write_csv(clean, "rsvp_limpio.csv")
