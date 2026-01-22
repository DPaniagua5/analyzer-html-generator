from tkinter import *
from tkinter import messagebox, filedialog
from tkinter import ttk

import subprocess
import webbrowser



global archivo_actual
archivo_actual = None

def Analizar():
    # Obtener el contenido del área de texto 
    data = TxtBox_E.get("1.0", END)
    resultado = subprocess.run(
        ["..\Backend\main.exe"],  # Ejecutable de Fortran
        input=data,  # la data que se manda a Fortran
        stdout=subprocess.PIPE,  # la data que viene de Fortran   
        text=True  # la salida se maneja como texto
    )
    output = resultado.stdout.strip()         
    datos = output.split(' ')
    
    Tabla_E.delete(*Tabla_E.get_children())
    filas_unicas = set()
    if len(datos) == 1:
         messagebox.showinfo("Información", "No se encontraron errores")
         return
    else:
# Insertar datos en la tabla y verificar duplicados
        for i in range(0, len(datos), 5):
            fila = (datos[i], datos[i+1], datos[i+2], datos[i+3], datos[i+4])
            if fila not in filas_unicas:
                filas_unicas.add(fila)
                Tabla_E.insert("", "end", values=fila)    

def Tokens():
    try:
        webbrowser.open(r"..\Out\tokens.html")
    except Exception as e:
        messagebox.showerror("Error: ", f"Error al abrir el archivo: {e}")
def Salida():
    try:
        webbrowser.open(r"..\Out\Salida.html")
    except Exception as e:
        messagebox.showerror("Error: ", f"Error al abrir el archivo: {e}")

Principal = Tk()
Principal.title("Lenguajes Formales y de Programación B-: PROYECTO 2")
Principal.iconbitmap("icono.ico")
Principal.resizable(0,0)    
Principal.config(bg="grey")

Frame_F = Frame(Principal)
Frame_F.grid()                                

TxtBox_E = Text(Frame_F, wrap=NONE)
TxtBox_E.grid(row=0, column=0, sticky="nsew")

scroll = Scrollbar(Frame_F)
scroll.grid(row=0, column=1, sticky="ns")
scroll.config(command=TxtBox_E.yview)
TxtBox_E.config(yscrollcommand=scroll.set)    

def Posicion_mouse(event):
    x = event.x
    index = TxtBox_E.index(INSERT)
    texto_hasta_cursor = TxtBox_E.get("1.0", index)
    lineas = texto_hasta_cursor.splitlines()
    y = len(lineas)
    
    Frame_M.delete("position")  
    Frame_M.create_text(
        10, 10, text=f"Posicion Y:{y} -- Posicion X:{x}", anchor="nw", tag="position"
    )
Frame_M = Canvas(Frame_F, bg="lightblue")
Frame_M.grid(row=0, column=2, sticky="nsew")
# Funcion para mostrar la posicion del mouse
Principal.bind("<Motion>", Posicion_mouse)
columns = ('Tipo', 'Linea', 'Columna', 'Token', 'Descripción')
Tabla_E = ttk.Treeview(Frame_F, columns=columns, show='headings')
for col in columns:
    Tabla_E.heading(col, text=col)
    Tabla_E.column(col, width=100)
    Tabla_E.grid(row=1, column=0, columnspan=3, sticky="nsew")

#Creacion de barra de menu
menu_bar = Menu(Principal)
Principal.config(menu=menu_bar)
#Funciones del menu
def salir():
    resp = messagebox.askquestion("Salir", "¿Está seguro que desea salir?")
    if resp == "yes":
        Principal.quit()
def abrir():
   Tabla_E.delete(*Tabla_E.get_children())
   global archivo_actual
   file = filedialog.askopenfilename(
        title = "Seleccione un archivo",
        filetypes = (("LFP files", "*.LFP"), ("Todos los archivos", "*.*"))
    )
   if file:
       archivo_actual = file
   with open(file, "r") as f:
        messagebox.showinfo("Información", "Archivo cargado con éxito")
        contenido = f.read()
        TxtBox_E.delete(1.0, END)
        TxtBox_E.insert(1.0, contenido)

def guardar():
    global archivo_actual
    if archivo_actual:
        with open(archivo_actual, "w") as f:
            contenido = TxtBox_E.get(1.0, END)
            f.write(contenido)
            archivo_actual = archivo_actual
    else:
        Guardar_Como()
        archivo_actual = archivo_actual
            

def Guardar_Como():
    global archivo_actual
    G_file = filedialog.asksaveasfilename(
        defaultextension=".LFP",
        filetypes = (("LFP files", "*.LFP"), ("Todos los archivos", "*.*")))
    archivo_actual = G_file
    if G_file is None:
        return
    else:
        G_file = open(G_file, "w")
        G_contenido = TxtBox_E.get(1.0, END)
        G_file.write(G_contenido)
        G_file.close()
        
    
    
def nuevo():
    global archivo_actual
    if archivo_actual != None:
        resp = messagebox.askquestion("Nuevo", "¿Desea guardar el archivo actual?")
        if resp == "yes":
            Guardar_Como()
            TxtBox_E.delete(1.0, END)
            Tabla_E.delete(*Tabla_E.get_children())
            archivo_actual = None
        else:
            TxtBox_E.delete(1.0, END)
            Tabla_E.delete(*Tabla_E.get_children())
            archivo_actual = None
    else :
        TxtBox_E.delete(1.0, END)
        Tabla_E.delete(*Tabla_E.get_children())
        archivo_actual = None 
   
    #Opciones del menu en cascada
menu_opciones = Menu(menu_bar, tearoff=0)
menu_bar.add_cascade(label="Archivo", menu=menu_opciones)
menu_opciones.add_command(label="Nuevo", command = nuevo)
menu_opciones.add_command(label="Abrir", command = abrir)
menu_opciones.add_separator()
menu_opciones.add_command(label="Guardar", command = guardar)
menu_opciones.add_command(label="Guardar como", command = Guardar_Como)
menu_opciones.add_separator()
menu_opciones.add_command(label="Salir", command = salir)
    #Demás opciones del menu
menu_bar.add_command(label="Análisis", command = Analizar)
menu_bar.add_command(label="Tokens", command= Tokens)
menu_bar.add_command(label="Salida", command= Salida)

Principal.mainloop()