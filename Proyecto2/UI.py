import tkinter as tk
from tkinter import ttk
from tkinter import filedialog
from tkinter import messagebox
import subprocess
import os
import webbrowser

class main:
    def __init__(self, root):
        self.root = root
        self.configurar_ventana()

        # Define color scheme
        self.colors = self.definir_colores()

        # Apply a theme and configure styles
        self.configurar_estilo()

        # Main frame to organize elements
        self.main_frame = ttk.Frame(self.root)
        self.main_frame.pack(expand=True, fill='both')

        # Create interface components
        self.crear_banner_titulo()
        self.crear_botones_archivo()
        self.crear_boton_datos_estudiante()
        self.crear_paned_window()
        self.crear_barra_estado()

    def configurar_ventana(self):
        self.root.title("interfaz")
        self.root.geometry("1200x800")

    def definir_colores(self):
        return {
            'bg': '#001A00',
            'fg': '#33FF33',
            'accent': '#00CC00',
            'button': '#FFFFFF',
            'button_text': '#001A00',
            'text_bg': '#000A00',
            'text_fg': '#66FF66',
            'highlight': '#00FF00'
        }

    def configurar_estilo(self):
        self.style = ttk.Style(self.root)
        self.style.theme_use('clam')

        # Configure general styles
        self.configurar_colores_base()
        self.configurar_colores_botones()

    def configurar_colores_base(self):
        self.style.configure('TFrame', background=self.colors['bg'])
        self.style.configure('TLabel', background=self.colors['bg'], foreground=self.colors['fg'])

    def configurar_colores_botones(self):
        self.style.configure('TButton',
                             background=self.colors['button'],
                             foreground=self.colors['button_text'],
                             borderwidth=0)
        self.style.map('TButton', background=[('active', self.colors['accent'])])
        self.style.configure('Accent.TButton',
                             background=self.colors['accent'],
                             foreground=self.colors['button'],
                             borderwidth=0)
        self.style.map('Accent.TButton', background=[('active', self.colors['highlight'])])

    def crear_banner_titulo(self):
        banner_frame = self.crear_frame(self.main_frame, estilo='Banner.TFrame', pady=(0, 10))
        self.configurar_estilo_banner()
        ttk.Label(banner_frame, text="PROYECTO2", style='Banner.TLabel').pack(pady=10)

    def configurar_estilo_banner(self):
        self.style.configure('Banner.TFrame', background=self.colors['accent'])
        self.style.configure('Banner.TLabel', background=self.colors['accent'],
                             foreground=self.colors['button'], font=('Helvetica', 16, 'bold'))

    def crear_frame(self, parent, estilo='TFrame', pady=5, padx=10):
        frame = ttk.Frame(parent, style=estilo)
        frame.pack(fill='x', padx=padx, pady=pady)
        return frame

    def crear_botones_archivo(self):
        file_button_frame = self.crear_frame(self.main_frame)
        botones = [
            ("Nuevo", self.nuevo_archivo),
            ("Abrir", self.abrir_archivo),
            ("Guardar", self.guardar_archivo),
            ("Guardar como", self.guardar_como),
            ("Analizar Código", self.analizar),
            ("Limpiar", self.limpiar_datos)
        ]
        self.agregar_botones(file_button_frame, botones)

    def agregar_botones(self, frame, botones):
        for text, command in botones:
            estilo = 'Accent.TButton' if text == "Analizar Código" else 'TButton'
            self.crear_boton(frame, text, command, estilo)

    def crear_boton(self, parent, text, command, estilo):
        btn = ttk.Button(parent, text=text, command=command, style=estilo)
        btn.pack(side=tk.LEFT, padx=5)
        btn.bind('<Enter>', lambda e, b=btn: self.on_button_hover(e, b))
        btn.bind('<Leave>', lambda e, b=btn: self.on_button_leave(e, b))

    def crear_boton_datos_estudiante(self):
        ttk.Button(self.main_frame, text="Datos del Estudiante",
                   command=self.mostrar_datos_estudiante, style='TButton').pack(pady=10)

    def mostrar_datos_estudiante(self):
        datos = [
            "Nombre: Josué David Velásquez Ixchop",
            "Carnet: 202307705",
            "Carrera: Ingeniería en Ciencias y Sistemas",
            "Curso: Lenguajes Formales y de Programación",
        ]
        self.mostrar_en_ventana(datos, "Datos del Estudiante")

    def mostrar_en_ventana(self, datos, titulo):
        datos_window = tk.Toplevel(self.root)
        datos_window.title(titulo)
        datos_window.geometry("400x200")
        datos_window.configure(bg=self.colors['bg'])

        for dato in datos:
            ttk.Label(datos_window, text=dato, background=self.colors['bg'], foreground=self.colors['fg']).pack(anchor="w", padx=10, pady=5)

    def crear_paned_window(self):
        paned_window = ttk.PanedWindow(self.main_frame, orient=tk.HORIZONTAL)
        paned_window.pack(expand=True, fill='both', padx=10, pady=5)

        self.crear_area_texto(paned_window)
        self.crear_notebook(paned_window)

    def crear_area_texto(self, paned_window):
        left_frame = ttk.Frame(paned_window)
        paned_window.add(left_frame, weight=1)

        text_container = ttk.LabelFrame(left_frame, text="Editor de código")
        text_container.pack(expand=True, fill='both')

        self.area_texto, scroll_y, scroll_x = self.configurar_area_texto(text_container)
        scroll_y.config(command=self.area_texto.yview)
        scroll_x.config(command=self.area_texto.xview)

    def configurar_area_texto(self, container):
        scroll_y = ttk.Scrollbar(container)
        scroll_y.pack(side=tk.RIGHT, fill=tk.Y)
        scroll_x = ttk.Scrollbar(container, orient=tk.HORIZONTAL)
        scroll_x.pack(side=tk.BOTTOM, fill=tk.X)
        
        area_texto = tk.Text(container, wrap=tk.NONE, undo=True,
                             yscrollcommand=scroll_y.set,
                             xscrollcommand=scroll_x.set,
                             bg=self.colors['text_bg'],
                             fg=self.colors['text_fg'],
                             insertbackground=self.colors['highlight'],
                             selectbackground=self.colors['accent'],
                             selectforeground=self.colors['button_text'])
        area_texto.pack(expand=True, fill='both')
        return area_texto, scroll_y, scroll_x

    def crear_notebook(self, paned_window):
        right_frame = ttk.Frame(paned_window)
        paned_window.add(right_frame, weight=1)

        notebook = ttk.Notebook(right_frame)
        notebook.pack(expand=True, fill='both', padx=10, pady=5)

        self.crear_tabla_tokens(notebook)
        self.crear_tabla_errores(notebook)

    def crear_tabla(self, parent, titulo, columnas):
        table_container = ttk.LabelFrame(parent, text=titulo)
        table_container.pack(expand=True, fill='both')

        scroll_y = ttk.Scrollbar(table_container)
        scroll_y.pack(side=tk.RIGHT, fill=tk.Y)

        tabla = ttk.Treeview(table_container, columns=columnas, show="headings", yscrollcommand=scroll_y.set)
        tabla.pack(expand=True, fill='both')
        scroll_y.config(command=tabla.yview)
        return tabla

    def crear_tabla_tokens(self, parent):
        columnas = ("Lexema", "Tipo", "Fila", "Columna")
        self.tabla_tokens = self.crear_tabla(parent, "Tabla de Tokens", columnas)

    def crear_tabla_errores(self, parent):
        columnas = ("Tipo", "Linea", "Columna", "Token", "Descripcion")
        self.tabla_errores = self.crear_tabla(parent, "Tabla de Errores", columnas)

    def crear_barra_estado(self):
        self.barra_estado = ttk.Label(self.root, text="Posición: 1:0")
        self.barra_estado.pack(side=tk.BOTTOM, fill=tk.X)
        self.area_texto.bind("<KeyRelease>", self.actualizar_posicion)

    def on_button_hover(self, event, button):
        button.configure(cursor="hand2")

    def on_button_leave(self, event, button):
        button.configure(cursor="")

    def limpiar_datos(self):
        self.area_texto.delete(1.0, tk.END)
        for tabla in [self.tabla_tokens, self.tabla_errores]:
            tabla.delete(*tabla.get_children())
        self.actualizar_posicion(None)

    def actualizar_posicion(self, event):
        cursor_pos = self.area_texto.index(tk.INSERT)
        row, col = cursor_pos.split(".")
        self.barra_estado.config(text=f"Posición: {row}:{col}")

    # Archivos
    def nuevo_archivo(self):
        if messagebox.askokcancel("Nuevo", "¿Quieres crear un nuevo archivo? Se borrarán los cambios no guardados."):
            self.limpiar_datos()

    def abrir_archivo(self):
        ruta_archivo = filedialog.askopenfilename(defaultextension=".txt",
                                                  filetypes=[("Archivos de texto", "*.txt"),
                                                             ("Todos los archivos", "*.*")])
        if ruta_archivo:
            with open(ruta_archivo, 'r') as archivo:
                contenido = archivo.read()
            self.area_texto.delete(1.0, tk.END)
            self.area_texto.insert(tk.END, contenido)

    def guardar_archivo(self):
        contenido = self.area_texto.get(1.0, tk.END)
        if not hasattr(self, 'ruta_archivo') or not self.ruta_archivo:
            self.guardar_como()
        else:
            with open(self.ruta_archivo, 'w') as archivo:
                archivo.write(contenido)

    def guardar_como(self):
        archivo = filedialog.asksaveasfilename(defaultextension=".txt",
                                               filetypes=[("Archivos de texto", "*.txt"),
                                                          ("Todos los archivos", "*.*")])
        if archivo:
            self.ruta_archivo = archivo
            self.guardar_archivo()

    def analizar(self):
        pass  # Aquí iría el código de análisis

# Inicializar la aplicación
if __name__ == "__main__":
    root = tk.Tk()
    app = main(root)
    root.mainloop()
