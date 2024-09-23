# Manual De Usuario

En esta ocasión se realizó un proyecto con el propósito de crear un estilo de editor de texto para poder determinar el mejor país para establecer un comercio basado en la saturación de los paises propuestos en el archivo .ORG que se debe de cargar.

Para la utilización del mismo, se requiere que se pueda descargar el repositorio completo. Por lo regular, se descarga el .zip del realese propuesto, por lo que deberá de descomprimirlo. Ahora bien, habra una consola e ingrese este comando `cd Project\ 1`, luego introduzca `cd Python` y por último `py Main.py`. Esto, básicamente sirve para acceder al archivo .py en donde se encuentra el main de Python y permite ejecutarlo. Habiendo introducido esto, se le desplegará la aplicación la cual luce así:

![Home](./DocumentationImages/cap1.PNG)

Dentro del mismo, usted contará con un lugar para editar texto o bien, si desea puede subir un archivo .ORG para que sea analizado posteriormente. En la sección de menú, usted contará con 3 opciones, las cuales son las clásicas, de guardar, guardar como y abrir.
Luego de poner en el input lo que desea usted que se analice, se verá más o menos así:

![Editor de texto lleno](./DocumentationImages/cap2.PNG)

Cuando haya puesto todo lo que desea analizar, apache el botón que está abajo del editor de texto y espere a los resultados. Cuando ya se haya calculado todo, se mostrará en patalla los datos obtenidos por el analizador.

![Resultados del análisis](./DocumentationImages/cap3.PNG)

Si todo estaba correcto, también se generará un html indicando todos los lexemas y tokens encontrados de lo que usted introdujo.

![Tabla de Lexemas](./DocumentationImages/cap4.PNG)

De igual forma, si al momento de usar el editor de texto presenta algún problema léxico, el programa no le devolverá el resultado, sino que solamente generará un html con una lista de error encontrados en el área de texto.

![Error en el editor](./DocumentationImages/cap5.PNG)

Y más o menos así se vería la tabla en donde se indican los errores encontrados:

![Error en el editor](./DocumentationImages/cap6.PNG)

Por último, cabe destacar el hecho que usted puede cerrar el programa y guardar todo lo que está en el input del editor de texto, no el resultado del análisis.
