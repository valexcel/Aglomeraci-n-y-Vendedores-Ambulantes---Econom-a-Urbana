# Aglomeración y trabajo Informal: explorando los efectos socioeconómicos de la concentración de vendedores ambulantes en Bogotá, Colombia

**Por Kelly Cadena-Guerrero, Alejandra González-Beltrán y Valentina González-Beltrán**

Universidad de los Andes – Facultad de Economía  
Economía Urbana  
Ignacio Sarmiento Barbieri

## Resumen del proyecto
En Colombia, hay 481.655 micronegocios de vendedores ambulantes (DANE, 2024), que representan el 4,2% del empleo informal y ocupan a 556.501 trabajadores. En Bogotá, la cifra alcanza los 170.000, consolidando un sector clave de la economía urbana. A pesar de ello, los estudios sobre los efectos de la aglomeración en el sector informal son escasos, generalmente centrados en beneficios económicos como salarios y productividad. Sin embargo, en contextos donde los vendedores ambulantes enfrentan una alta inseguridad e instituciones débiles en una economía con altos niveles de informalidad, surge una pregunta crucial: **¿existen economías de aglomeración en estos entornos? Si las hay, ¿existen externalidades positivas de esta aglomeración sobre la seguridad percibida por parte de estos micronegocios ambulantes?** Este estudio, mediante un experimento RCT en Bogotá, busca responder estas preguntas y abordar una brecha significativa en la literatura, particularmente en regiones como América Latina, donde la informalidad es predominante.

📄 Este repositorio contiene el código utilizado para el proyecto titulado ***Aglomeración y trabajo Informal: explorando los efectos socioeconómicos de la concentración de vendedores ambulantes en Bogotá, Colombia***. 


## Entendiendo el código 💻
El código permite realizar una simulación para el research design. Esto nos permite conocer cuáles son los resultados clave de nuestro modelo para entender su efectividad (sesgo, poder de la prueba y error cuadrático medio). 
A continuación se detallan los pasos necesarios para ejecutar el código en este repositorio:

1. **Definir el modelo**: 
   Se enuncia el modelo utilizando los datos de **ingresos promedio de vendedores ambulantes** (1), **percepción de seguridad** (2) y **número de conexiones laborales** como variables principales.

2. **Determinamos la elegibilidad al tratamiento y los resultados potenciales**:
   Se distribuye la probabilidad de asignación según UPZ en la localidad de Kennedy. Los resultados potenciales se establecen de acuerdo a la la revisión de literatura.

3. **Simulación de resultados**:
Modelo DiD con y sin variable instrumental estimado en dos etapas.

4. **Diagnóstico del modelo**:
Se evalua el poder estadístico, sesgo y error cuadrático medio para cada uno de los modelos estimados (ingresos, percepción de seguridad y conexiones laborales). 



🏳️ Los tres scritps de R siguen el mismo procedimiento con pequenas variaciones realizadas con el fin de comprobar la robustez de la estimación. En particular: 

***Design.R:*** ejecuta la simulacion del modelo original.

***RobustnessChecks_1.R:*** incorpora una variación en el porcentaje de compliance.

***RobustnessChecks_2.R:*** incorpora una variación en los resultados potenciales. 


## Resultados y análisis ✅
El documento titulado **resultados** contiene el documento final del proyecto. Incluye la revisión de literatura, hipótesis, modelo, resultados y bibliografía. 



***Notas al pie***

(1) DANE (2024). Encuesta de Micronegocios (EMICRON) - Vendedores ambulantes 2019-2023.  
(2) Pinto (2024). *Colombia está en octavo puesto de América en el índice de percepción de seguridad*. La Republica.co.
   
