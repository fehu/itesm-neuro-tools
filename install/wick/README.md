Instalación automatizada en Ubuntu
==================================

### Requisitos

* Establecer la variable del entorno `WEKA_HOME`. Debe de apuntar a la carpeta de Instalación de Weka,
  donde se encuentra `weka.jar`.


### Ejecución

`bash install-ubuntu.sh`

Importante: usa `bash` en lugar de `sh`, porque en Ubuntu `sh` apunta a `dash`.

#### Interfaz Gráfica

La interfaz de _classificación_ puede no funcionar totalmente correcto con el tema (colores, etc.) de Ubuntu.
Se nescesitará cambiar el tema (Configuraciones > Apariencia) a 'Adwaita' (se encuentra en el paquete de sistema
`gnome-themes-standard`) o tal vez también cualquier otra.

Se provee un script para hacerlo:
```bash
source change-gtk-theme.sh
source change-gtk-theme.sh restore
```



### Efectos

El script hace lo siguiente:

1. Instala _GHC_ y _cabal_.
2. Instala _Oracle JDK_ __8__ y lo hace __version por defecto__.
3. Instala: `git`, `zlib1g-dev`, `libglib2.0`, `libcairo2-dev`, `libpango1.0-dev`, `libgtk-3-dev`.
4. Descarga el proyecto en cuestion `itesm-neuro-tools` y sus dependencias, que no están publicadas:
   `EitherProjections`, `CommandArgs`, `WekaData`, `Nat` y `java-bridge`.
5. Instala los proyectos del punto 4.
6. Genera código para _JNI_ (usando `java-bridge` fork) en `itesm-neuro-tools/image-characteristics/bindings`.
7. Instala el resto de dependencias con _cabal_.
8. Instala `itesm-neuro-tools/image-characteristics`.
9. Demuesta el mensaje de ayuda de la ejecutable instalada: `wick -h`.

#### "Basura"

* _GHC_ y _cabal_ se instalan en `/opt`.
* Todos los paquetes, descargados por _cabal_, se encuentran en `~/.cabal`.
  También se usa la carpeta `~/.ghc`.

### Problemas

Los problemas principales que se encuentran son las diferencias en el código generado por `java-bridge`.
La razon, la desconozco, pero los cambios se introducen por la instalción de `java-bridge`.

Hay dos maneras de resolverlo:

1. Reinstalar `java-bridge` con `cabal install --force-reinstalls`, recorrer `makeWekaInterface.sh`, que se encuentra
   en `itesm-neuro-tools/image-characteristics` y esperar a que el código generado corresponda a las llamadas a java,
   hechas en el proyecto.

2. Modificar los lugares problematicos: el problema es la diferencia en el orden en el cual se generan los nombres para
   los métodos de java sobrecargados. Haskell no permite sobrecargar, por esto se genera un nombre diferente para
   cada interfaz del método. Tales nombres reciben `'` en fin, por ejemplo `Evaluation.toSummaryString''`.
   Las descripciones de la interfaz de Weka pueden ser encontradas en
   `itesm-neuro-tools/image-characteristics/bindings/dist/doc/html/weka-bindings/index.html`.

Para instalar un proyecto paso por paso, ejecuta

```bash
cabal configure
cabal build
cabal copy
cabal register
```

