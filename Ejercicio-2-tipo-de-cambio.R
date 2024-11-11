#Ejercicio 2

#Agregar las curvas bono cero cupon

curvas_cero_cupon <- curvas_cero_cupon %>% 
  mutate(curva_bono_cero_cupon_bccr_colones = 1 / (1 + curva_cero_cupon_bccr_colones / 100) ^ (vencimiento_anos)) %>% 
  mutate(curva_bono_cero_cupon_fed_dolares = 1 / (1 + curva_cero_cupon_fed_dolares / 100) ^ (vencimiento_anos))
