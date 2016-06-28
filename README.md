# Programación Funcional Básica en Scala

Ejercicios básicos de programación funcional en Scala

## [Slides](https://slides.com/miguelvila/basic-fp-in-scala)

## Para generar archivos que permitan abrir el proyecto en distintos IDEs

Idealmente tener instalada la misma versión de SBT que se encuentra en el archivo `project/build.properties`

### Eclipse

```bash
$ sbt eclipse
```

### Intellij

```bash
$ sbt gen-idea
```
## Ejecutar tests

Todos los tests:

```bash
$ sbt test
```

Un test en particular:

```bash
$ sbt testOnly exercises.<NombreModulo>
```
