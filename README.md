<p align="center">
  <img src="https://img.shields.io/badge/Python-3.x-blue?style=for-the-badge&logo=python" alt="Python">
  <img src="https://img.shields.io/badge/Fortran-90-purple?style=for-the-badge&logo=fortran" alt="Fortran">
  <img src="https://img.shields.io/badge/License-MIT-green?style=for-the-badge" alt="License">
</p>

---

🌎 **Read this in:** [English](#english-version) | [Español](#versión-en-español)

---

## English Version

### 🧩 Overview

This project is a **desktop-based visual UI compiler** that transforms a **custom declarative language** into fully functional **HTML and CSS**.  
It combines a **Python (Tkinter) frontend** with a **high-performance Fortran compiler backend**, providing lexical and syntactic analysis without relying on regular expressions or external parser generators.

The tool is designed for users who want to **define web interfaces structurally**, validate them through a formal language, and automatically generate production-ready web files.

---

### ⚙️ How It Works

1. The user writes UI definitions using a structured `.LFP`-like language.
2. Python (Tkinter) provides:
   - Code editor
   - Error visualization
   - Token inspection
3. The source is passed to a **Fortran compiler** that:
   - Performs **lexical analysis using a DFA**
   - Applies **syntactic analysis via recursive descent parsing**
   - Builds an internal representation of the UI
4. If no errors are found:
   - HTML and CSS files are generated
   - Layout and styles reflect the original specification

---

### 🚀 Features

- **Custom UI Definition Language**  
  Declaratively define controls, properties, and layout.

- **Formal Compiler Backend (Fortran)**  
  Lexical and syntactic analysis implemented.

- **Error Reporting**  
  Detailed lexical and syntactic errors with line and column tracking.

- **Token Inspection**  
  Visual table showing recognized tokens and lexemes.

- **HTML & CSS Generation**  
  Produces clean, structured, and maintainable output files.

- **Desktop GUI**  
  Interactive editor built with Tkinter.

---

### 🛠 Technologies Used

- **Languages:** Python 3, Fortran 90
- **GUI:** Tkinter
- **Visualization:** Graphviz
- **Toolchain:** gfortran, Git/GitHub

---

## Versión en Español

### 🧩 Descripción General

Esta aplicación es un **compilador visual de interfaces gráficas** que transforma un **lenguaje declarativo personalizado** en archivos **HTML y CSS funcionales**.

El sistema integra una **interfaz gráfica en Python (Tkinter)** con un **backend de compilación en Fortran**, encargado del análisis léxico y sintáctico sin uso de expresiones regulares ni librerías externas de parsing.

Está orientado a usuarios que desean **definir interfaces web de forma estructurada**, validarlas mediante lenguajes formales y generar automáticamente su representación web.

---

### ⚙️ Funcionamiento

1. El usuario define la interfaz usando un archivo de texto estructurado.
2. La interfaz gráfica permite:
   - Editar código
   - Visualizar errores
   - Inspeccionar tokens
3. El backend en Fortran:
   - Ejecuta un **AFD para análisis léxico**
   - Aplica **parser descendente recursivo**
4. Si el código es válido:
   - Se generan archivos `.html` y `.css`
   - El diseño respeta posiciones, estilos y jerarquía

---

### 🚀 Características

- **Lenguaje Declarativo de Interfaces**
- **Compilador Formal en Fortran**
- **Reporte completo de errores léxicos y sintácticos**
- **Tabla de tokens reconocidos**
- **Generación automática de HTML y CSS**
- **Interfaz gráfica interactiva**

---

## ▶️ How to Run / Cómo ejecutar

### Prerequisites

- Python 3.x
- gfortran
- Graphviz

### Installation

```bash
git clone https://github.com/DPaniagua5/
analyzer-html-generator.git
cd fortran-python-analyzer
```

## 📄 Input file / Estructura de entrada

```bash
<!--Controles
Contenedor contlogin;
Contenedor contFondo; 
Boton cmdIngresar; 
Clave pswClave;
Etiqueta passw; 
Etiqueta Nombre; 
Texto Texto0;
Contenedor contlogo2;
Contenedor ContLogo1;
Contenedor ContBody;
Controles -->
<!--propiedades
/*
Definicion de propiedades
*/
//#$inicio de contlogin contlogin.setAncho(190);
contlogin.setAlto(150);
contlogin.setColorFondo(47,79,79);
//#$fin de contlogin
//#$inicio de contFondo contFondo.setAncho(800);
contFondo.setAlto(100);
contFondo.setColorFondo(64,64,64);
//#$fin de contFondo
//#$inicio de cmdIngresar
cmdIngresar.setTexto("Ingresar");
contlogin.add(cmdIngresar);
//#$fin de cmdIngresar
//#$inicio de pswClave pswClave.setTexto("");
//#$fin de pswClave
//#$inicio de etiqueta passw
passw.setAncho(53); passw.setAlto(13 );
passw.setColorLetra(128,128,128);
passw.setTexto("Password");
//#$fin de passw
//#$inicio de Nombre
Nombre.setAncho(44);
Nombre.setAlto(13);
Nombre.setColorLetra(128,128,128);
Nombre.setTexto("Nombre");
//#$fin de Nombre
//#$inicio de JTextField0
JTextField0.setTexto("");
//#$fin de JTextField0
//#$inicio de contlogo2
contlogo2.setAncho(150);
contlogo2.setAlto( 50);
contlogo2.setColorFondo(0,128,128);
//#$fin de contlogo2
//#$inicio de ContLogo1
ContLogo1.setAncho(50);
ContLogo1.setAlto( 50);
ContLogo1.setColorFondo(64,64,64);
//#$fin de ContLogo1
//#$inicio de ContBody
ContBody.setAncho(800);
ContBody.setAlto(300);
ContBody.setColorFondo(64,224,208);
//#$fin de ContBody
propiedades -->
<!--Colocacion
/*
Posicionamiento de los controles
*/
contFondo.setPosicion(25,330);
this.add(contFondo);
contlogin.setPosicion(586,110);
ContBody.add(contlogin);
passw.setPosicion(11,54);
contlogin.add(passw);
cmdIngresar.setPosicion(40,100);
pswClave.setPosicion(67,48);
contlogin.add(pswClave);
Nombre.setPosicion(8,21);
contlogin.add(Nombre);
JTextField0.setPosicion(65,20);
contlogin.add(JTextField0);
contlogo2.setPosicion(88,25);
ContBody.add(contlogo2);
ContLogo1.setPosicion(36,25);
ContBody.add(ContLogo1);
ContBody.setPosicion(23,21);
this.add(ContBody);
Colocacion -->
```

Para mayor información ver [Manuales](./Manuales/)
