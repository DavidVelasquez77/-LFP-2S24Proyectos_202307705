# Manual Técnico

**Descripción Breve:** este proyecto consiste en la implementación de un analizador léxico y análisis de un input para determinar un país en específico el cual es la mejor opción de colocar un negocio. La interfaz gráfica está realizado Tkinter de Python, mientras que todo lo demás fue llevado a cabo por el lenguaje de programacion Fortran.

### Autómata

Para la realización del mismo, primero se procedió a calcularlo como tal, a través del método del árbol. A continuación se presenta:

![Expresion regular](./DocumentationImages/c1.jpg)

Posteriormente, se procedió a aplicar cada paso del método del árbol, es decir, agregando los first, last, follows, entre otras cosas por cada nodo y determinar también todo lo que se requería por cada hoja. Por lo que el árbol finalizado quedó de dicha manera:

![Árbol autómata](./DocumentationImages/c2.jpg)

Con la información del árbol realizado, se pudo determinar la tabla del Follow y la tabla de transiciones. Ya con esta última tabla, se logró determinar de manera precisa el autómata requerido para tal proyecto. Gracias a la aplicación de este método, se obtuve el autómata con menor número de estados y transiciones disponibles.

![Tablas](./DocumentationImages/c3.jpg)

Para que se viera un tanto más estético y entendible, se usó una página web para graficarlo, la cual es llamada "Finite State Machine Designer", del cual, se puede mencionar que su uso es bastante sencillo y funcional, por lo tanto, el autómata quedó así:

![Autómata](./DocumentationImages/c4.JPG)

Yo con el autómata hecho, ya solo tocaba codificarlo. Por lo tanto, en el módulo utils.f90 de la carpeta Fortran, existe un método llamado

```Fortran
subroutine analyze(content)
```

Es en este método en donde ocurre toda la magia del proyecto, ya que en este casao, dentro de la subrutina, se puso un `select case` el cual se utilizaría para movilizarnos entre los distintos estados que puede llegar a tener. Como se vio en la imagen anterior, se manejaron 5 estados distintos que literalmente se codificó lo que se calculó con el método del árbol. En cada estado, se determinaba algo distnto. Por ejemplo, cualquier caracter entreaba en el estado 0, si se determinaba que era una letra, se iba hacia el estado uno. Una vez en ese estado, se miraba si lo que seguí a continuación era otra letra u otro caracter. Si seguía siendo letra, se iba almacenando en un buffer hasta que resultara siendo otro caracter. Cuando llega al fin de la palabra, se analizaba si pertenecía a alguna palabra clave definida previamente y si sí era, se iba a la tabla de lexemas encontrados, de lo contrario, se iba hacia una tabla donde se presentan los error encontrados por el analizador.

De la misma manera, se manejaron los demás estados, para los números, para los símbolos especiales y cadenas de texto. Ahora bien, para los estados de error, no se pusieron gráficamente, pero de igual forma se manejaron. En este caso, se manejaban al momento que se terminaba la cadena de texto, digitos, caracteres especiales, etc. Simplemente se validaba si pertenecía o no al lenguaje según su categoría y en base a ello, agarraba para un lado o hacia al otro. Dirigiendolos hacia la tabla de lexemas válidos o de errores.

### Reporte HTML
Para la realización del mismo, se utilizaorn dos arrays bastante importantes para toda la aplicación en sí, la cual es en donde se iban guardando los lexémas válidos y errores encontrados en el análisis. Con dichos arreglos, se procedió a extraer sus valores almacenados y ya con ellos se iba extrayendo la información útil para generar los reportes. Si la longitud de la lista de errores era mayor o igual a 2, entonces solo se generaba el reporte de erroes, de lo contario, se generaba el reporte de lexemas válidos. 

La subrutina responsable de tales acciones es la siguiente:
```Fortran
subroutine writeHTML(list, isError)
```
La cual pide 2 parámetros, el primero es la lista de lexemas ya sea buenos o erróneos y el otro es un booleano ya sea si va a ser una tabla para errores o no. Este último atributo es necesario ya que, el encabezado de ambas tablas es un tanto distinto.

### Reporte Graphviz
El módulo correspondiente para todo esto es el llamado "handleGraphiz.f90" ubicado en la carpeta de Fortran. Del mismo, se puede mencionar que para antes de generarlo, se tuvieron que haber capturado los valores de cada continente y país, además de su color y saturación de cada uno. Para ello, se trabajó con objetos. Un objeto tipo `Continent` podía contener una lista de objetos tipo `Country`, el cual, constaba con información primordial para generar el reporte con graphviz. Media vez obtenido los valores requeridos para su elaboración, con el siguiente método se mandaba a realizar el archivo .dot y la imagen .png para luego poder visualizarlo de una manera agradable en la interfaz de Tkinter.

```Fortran
subroutine createGraphiz()
```

Este método llamaba a otro métodos y se generaba el .dot correspondiente. Media vez generado ese archivo, se llamaba a otro método para generar la imágen.

### Ganador
Para encontrar el ganador del análisis requerido, siemplemente se tuvo que calcular el país con menor saturación obtenida, a lo cual se usó el método intrínseco que trae Fortran por defecto llamado `minVal()`, el cual solo se le pasa como parámetro una lista con los valores que se quiere calcular el mínimo y este mismo lo devuelve. Luego con ello, solo se iba a traer al páis con dicha saturación. Si existía un empate entre dos paises con la misma saturación, entonces si debía verificar qué continente tenía menos saturación y en base a ello ya se sacaba el resultado. Para ello se creó la subrutina que llamaba a otras subrutinas para determinar el ganador y enviarle a python la información que deberá de presentar en su GUI.

```Fortran
subroutine sendTheWinner()
```


### Conexión Fortran - Python
Para realizar dicha conexión, se utilizó la librería `subprocess` de Python, el cual permite ejecutar un subproceso. Lo que se hizo es ejecutar el .exe del Main de Fortran y enviarle todo lo capturando en el input de la GUI de Tkinter. El método responsable de dichas acciones es el `def sendContent()`, del cual, la línea más crucial es la siguiente:

```Python
result = subprocess.run(["../Fortran/Main.exe"], input=content, stdout=subprocess.PIPE, text=True,stderr=subprocess.PIPE)
```

Cuando se enviaba el contenido, se esperaba una respuesta del mismo envío. De la forma en que lo recibía era por medio de consola. Es por eso que las funciones de enviar al ganador de Fortran, su finalidad era simplemente imprimir en consola los resultados del análisis. Por lo tanto, fortran enviaba el nombre, población y ruta de la bandera del país ganador. Por lo tanto, python solo se enfocaba en capturar dicho resultado y ponerlo en su interfaz.

