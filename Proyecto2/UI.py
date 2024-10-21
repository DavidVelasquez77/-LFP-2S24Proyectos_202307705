import tkinter as gui
from tkinter import ttk, filedialog, messagebox
import subprocess, os, webbrowser

class AnalizadorFortran:
    def __init__(self, ventana):
        ventana.title("Proyecto 2 - Analizador de Fortran")
        ventana.geometry("900x650")

        # Colores personalizados
        self.paleta = {
            'fondo': '#1C1C1C',
            'texto_boton': '#FFFFFF',
            'fondo_boton': '#4CAF50',
            'fondo_editor': '#0B3D91',
            'texto_editor': '#D3D3D3',
        }

        ventana.configure(bg=self.paleta['fondo'])

        # Contenedor principal
        self.contenedor = ttk.Frame(ventana)
        self.contenedor.pack(expand=True, fill='both')

        # Crear botones
        self.configurar_botones()

        # Crear área de edición de texto
        self.configurar_editor_texto()

        # Contador de líneas y caracteres
        self.configurar_contador()

    def configurar_botones(self):
        marco_botones = ttk.Frame(self.contenedor)
        marco_botones.pack(fill='x')

        # Información de los botones
        lista_botones = [
            ("Nuevo Archivo", self.crear_archivo),
            ("Cargar Archivo", self.cargar_archivo),
            ("Guardar Archivo", self.almacenar_archivo),
            ("Ejecutar Análisis", self.realizar_analisis),
            ("Datos del Estudiante", self.mostrar_datos),
            ("Buscar Texto", self.buscar)
        ]

        for texto, accion in lista_botones:
            boton = ttk.Button(marco_botones, text=texto, command=accion, style='TButton')
            boton.pack(side=gui.LEFT, padx=5, pady=5)

    def configurar_editor_texto(self):
        marco_editor = ttk.Frame(self.contenedor)
        marco_editor.pack(fill='both', expand=True)

        scroll_vertical = gui.Scrollbar(marco_editor)
        scroll_vertical.pack(side=gui.RIGHT, fill=gui.Y)

        # Editor de texto
        self.campo_texto = gui.Text(marco_editor, wrap=gui.NONE, 
                                    bg=self.paleta['fondo_editor'], 
                                    fg=self.paleta['texto_editor'],
                                    yscrollcommand=scroll_vertical.set)
        self.campo_texto.pack(expand=True, fill='both')
        scroll_vertical.config(command=self.campo_texto.yview)

        # Resaltado de sintaxis Fortran
        self.campo_texto.tag_configure('keyword', foreground='yellow')

    def configurar_contador(self):
        self.marco_contador = ttk.Frame(self.contenedor)
        self.marco_contador.pack(fill='x')
        self.contador_lineas = ttk.Label(self.marco_contador, text="Líneas: 0 | Caracteres: 0")
        self.contador_lineas.pack(anchor='e', padx=10, pady=5)

        # Actualizar contador cuando se modifique el contenido
        self.campo_texto.bind('<KeyRelease>', self.actualizar_contador)

    def actualizar_contador(self, event=None):
        lineas = int(self.campo_texto.index('end-1c').split('.')[0])
        caracteres = len(self.campo_texto.get(1.0, gui.END)) - 1
        self.contador_lineas.config(text=f"Líneas: {lineas} | Caracteres: {caracteres}")

    def crear_archivo(self):
        # Limpiar el área de texto
        self.campo_texto.delete(1.0, gui.END)

    def cargar_archivo(self):
        archivo = filedialog.askopenfilename(
            filetypes=[("Archivos de Código", "*.f95 *.txt"), ("Todos los archivos", "*.*")]
        )
        if archivo:
            with open(archivo, 'r') as f:
                contenido = f.read()
                self.campo_texto.delete(1.0, gui.END)
                self.campo_texto.insert(gui.END, contenido)
            self.actualizar_contador()

    def realizar_analisis(self):
        codigo = self.campo_texto.get(1.0, gui.END)
        if codigo.strip():
            resultado = self.ejecutar_comando_fortran(codigo)
            messagebox.showinfo("Resultado del Análisis", resultado)
        else:
            messagebox.showerror("Error", "El editor está vacío")

    def ejecutar_comando_fortran(self, codigo_fuente):
        try:
            directorio = os.path.dirname(os.path.abspath(__file__))
            ruta_ejecucion = os.path.join(directorio, "analizador_fortran")

            if os.name == 'nt':
                ruta_ejecucion += ".exe"

            if not os.path.exists(ruta_ejecucion):
                return "Error: No se encontró el ejecutable de análisis."

            proceso = subprocess.Popen(
                [ruta_ejecucion], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
            )
            salida, errores = proceso.communicate(input=codigo_fuente)

            if proceso.returncode != 0:
                return f"Errores durante la ejecución: {errores}"

            return salida if salida else "Análisis completado sin errores."

        except Exception as e:
            return f"Error al intentar ejecutar el análisis: {str(e)}"


    def mostrar_datos(self):
        datos = [
            "Nombre: Josué David Velásquez Ixchop",
            "Carnet: 202307705",
            "Carrera: Ingeniería en Ciencias y Sistemas",
            "Curso: Lenguajes Formales y de Programación",
        ]

        datos_window = gui.Toplevel()
        datos_window.title("Datos del Estudiante")
        datos_window.geometry("400x200")
        datos_window.configure(bg=self.paleta['fondo'])

        for dato in datos:
            label = ttk.Label(datos_window, text=dato, background=self.paleta['fondo'], foreground=self.paleta['texto_editor'])
            label.pack(anchor="w", padx=10, pady=5)

    def buscar(self):
        buscar_window = gui.Toplevel()
        buscar_window.title("Buscar Texto")
        buscar_window.geometry("300x100")
        buscar_window.configure(bg=self.paleta['fondo'])

        label_buscar = ttk.Label(buscar_window, text="Texto a buscar:")
        label_buscar.pack(padx=10, pady=5)

        entrada_buscar = ttk.Entry(buscar_window)
        entrada_buscar.pack(padx=10, pady=5)

        boton_buscar = ttk.Button(buscar_window, text="Buscar", command=lambda: self.identificador(entrada_buscar.get()))
        boton_buscar.pack(padx=10, pady=5)

    def identificador(self, texto):
        self.campo_texto.tag_remove('found', '1.0', gui.END)
        if texto:
            idx = '1.0'
            while True:
                idx = self.campo_texto.search(texto, idx, nocase=1, stopindex=gui.END)
                if not idx:
                    break
                end_idx = f"{idx}+{len(texto)}c"
                self.campo_texto.tag_add('found', idx, end_idx)
                idx = end_idx
            self.campo_texto.tag_config('found', background='yellow', foreground='black')

if __name__ == "__main__":
    root = gui.Tk()
    app = AnalizadorFortran(root)
    root.mainloop()
