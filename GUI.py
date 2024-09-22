import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import os
import webbrowser
import customtkinter as ctk
from PIL import Image, ImageTk

class OrgEditor:
    def __init__(self):
        self.setup_ui()
        self.ruta_archivo = ctk.StringVar()

    def setup_ui(self):
        ctk.set_appearance_mode("dark")
        ctk.set_default_color_theme("blue")

        self.ventana = ctk.CTk()
        self.ventana.title("Editor de Archivos ORG")
        self.ventana.geometry("1250x1000")
        self.ventana.configure(fg_color="black")

        self.create_header()
        self.create_main_frame()
        self.create_text_area()
        self.create_info_frame()
        self.create_buttons()  # Moved this to the end

    def create_header(self):
        header = ctk.CTkFrame(self.ventana, corner_radius=0, fg_color="black", height=100)
        header.pack(fill="x")

        gradient = tk.Canvas(header, height=100, bg="black", highlightthickness=0)
        gradient.pack(fill="x")

        for i in range(1250):
            r, g, b = 0, 0, int((1 - i / 1250) * 255)
            color = f'#{r:02x}{g:02x}{b:02x}'
            gradient.create_line(i, 0, i, 100, fill=color)

        title = ctk.CTkLabel(gradient, text="PROYECTO 1", font=("Roboto", 32, "bold"), text_color="white")
        title.place(relx=0.5, rely=0.5, anchor="center")

    def create_main_frame(self):
        self.frame_principal = ctk.CTkFrame(self.ventana, fg_color="black")
        self.frame_principal.pack(padx=20, pady=20, fill=tk.BOTH, expand=True)

    def create_text_area(self):
        frame_texto = ctk.CTkFrame(self.frame_principal, fg_color="black", width=600)
        frame_texto.pack(side=tk.LEFT, padx=10, pady=10, fill=tk.BOTH, expand=True)

        self.texto = ctk.CTkTextbox(frame_texto, wrap='word', height=300, width=600)
        self.texto.pack(padx=10, pady=10, fill=tk.BOTH, expand=True)

    def create_buttons(self):
        frame_botones = ctk.CTkFrame(self.frame_principal, fg_color="black")
        frame_botones.pack(side=tk.BOTTOM, padx=10, pady=10, fill=tk.X)

        buttons = [
            ("Guardar", self.guardar_archivo),
            ("Guardar como", self.guardar_archivo_como),
            ("Abrir", self.cargar_archivo),
            ("Acerca de", self.mostrar_acerca_de),
            ("Salir", self.ventana.quit),
            ("Análisis", self.analizar)
        ]

        for text, command in buttons:
            if text == "Análisis":
                ctk.CTkButton(frame_botones, text=text, command=command).pack(side=tk.BOTTOM, pady=(5, 0), padx=5, fill=tk.X)
            else:
                ctk.CTkButton(frame_botones, text=text, command=command).pack(side=tk.BOTTOM, pady=2, padx=5, fill=tk.X)
                
    def create_info_frame(self):
        frame_info = ctk.CTkFrame(self.frame_principal, fg_color="black", width=600)
        frame_info.pack(side=tk.RIGHT, padx=10, pady=10, fill=tk.BOTH, expand=True)

        self.create_graph_canvas(frame_info)
        self.create_country_info(frame_info)

    def create_graph_canvas(self, parent):
        frame_grafico = tk.Frame(parent)
        frame_grafico.grid(row=0, column=0, columnspan=2, pady=10, sticky="nsew")
        parent.grid_rowconfigure(0, weight=1)
        parent.grid_columnconfigure(0, weight=1)

        self.grafico_placeholder = tk.Canvas(frame_grafico, bg="white", width=600, height=600)
        self.grafico_placeholder.grid(row=0, column=0, sticky="nsew")

        scrollbar_v = tk.Scrollbar(frame_grafico, orient="vertical", command=self.grafico_placeholder.yview)
        scrollbar_v.grid(row=0, column=1, sticky="ns")

        scrollbar_h = tk.Scrollbar(frame_grafico, orient="horizontal", command=self.grafico_placeholder.xview)
        scrollbar_h.grid(row=1, column=0, sticky="ew")

        self.grafico_placeholder.configure(yscrollcommand=scrollbar_v.set, xscrollcommand=scrollbar_h.set)
        frame_grafico.grid_rowconfigure(0, weight=1)
        frame_grafico.grid_columnconfigure(0, weight=1)

        self.grafico_placeholder.bind("<Configure>", self.on_configure)

        ctk.CTkLabel(parent, text="Gráfico generado por Graphviz").grid(row=1, column=0, columnspan=2, pady=5)

    def create_country_info(self, parent):
        self.etiqueta_pais = ctk.CTkLabel(parent, text="País seleccionado: ")
        self.etiqueta_pais.grid(row=2, column=0, sticky="w", padx=5, pady=2)

        self.etiqueta_saturacion = ctk.CTkLabel(parent, text="Saturación: ")
        self.etiqueta_saturacion.grid(row=3, column=0, sticky="w", padx=5, pady=2)

        self.etiqueta_poblacion = ctk.CTkLabel(parent, text="Población: ")
        self.etiqueta_poblacion.grid(row=4, column=0, sticky="w", padx=5, pady=2)

        self.etiqueta_imagen = ctk.CTkLabel(parent, text="")
        self.etiqueta_imagen.grid(row=2, column=1, rowspan=3, padx=5, pady=2)

    def on_configure(self, event):
        self.grafico_placeholder.config(scrollregion=self.grafico_placeholder.bbox("all"))

    def ejecutar_fortran(self, contenido):
        proceso = subprocess.Popen(["./main"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        stdout, stderr = proceso.communicate(input=contenido)

        if proceso.returncode != 0:
            return f"Error en la ejecución: {stderr}"
        
        lines = stdout.strip().split('\n')
        for line in reversed(lines):
            if line.startswith("PAIS_MENOR_SATURACION:"):
                _, info = line.split(":", 1)
                info = info.strip()
                partes = info.split("|")
                
                if len(partes) == 4:
                    nombre = partes[0].strip().strip('"')
                    saturacion = int(partes[1].strip())
                    bandera = partes[2].strip().strip('"')
                    poblacion = int(partes[3].strip())
                    return nombre, saturacion, bandera, poblacion
        
        return None

    def cargar_archivo(self):
        archivo = filedialog.askopenfilename(filetypes=[("Archivos ORG", "*.org")])
        if archivo:
            with open(archivo, 'r') as file:
                self.texto.delete('1.0', ctk.END)
                self.texto.insert(ctk.END, file.read())
            self.ruta_archivo.set(archivo)

    def guardar_archivo(self):
        archivo = self.ruta_archivo.get()
        if archivo:
            with open(archivo, 'w') as file:
                file.write(self.texto.get('1.0', ctk.END))
        else:
            self.guardar_archivo_como()

    def guardar_archivo_como(self):
        archivo = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos ORG", "*.org")])
        if archivo:
            with open(archivo, 'w') as file:
                file.write(self.texto.get('1.0', ctk.END))
            self.ruta_archivo.set(archivo)

    def abrir_html(self, nombre_archivo):
        ruta_actual = os.path.dirname(os.path.abspath(__file__))
        ruta_archivo = os.path.join(ruta_actual, nombre_archivo)
        if os.path.exists(ruta_archivo):
            webbrowser.open('file://' + ruta_archivo)
        else:
            messagebox.showerror("Error", f"No se encontró el archivo {nombre_archivo}")

    def analizar(self):
        contenido = self.texto.get('1.0', ctk.END)
        if contenido.strip():
            resultado = self.ejecutar_fortran(contenido)
            self.abrir_html("tokens.html")
            self.abrir_html("errores.html")

            if resultado:
                nombre_pais, saturacion, ruta_bandera, poblacion = resultado
                self.etiqueta_pais.configure(text=f"País seleccionado: {nombre_pais}")
                self.etiqueta_saturacion.configure(text=f"Saturación: {saturacion}%")
                self.etiqueta_poblacion.configure(text=f"Población: {poblacion}")
                self.cargar_imagen_pais(ruta_bandera)
                self.cargar_grafico()
            else:
                messagebox.showinfo("Análisis", "Se encontraron errores en los tokens. Revisa los archivos HTML generados.")
                self.etiqueta_pais.configure(text="País seleccionado: ")
                self.etiqueta_saturacion.configure(text="Saturación: ")
                self.etiqueta_poblacion.configure(text="Población: ")
                self.etiqueta_imagen.configure(image='')
                self.etiqueta_imagen.image = None
                self.grafico_placeholder.delete("all")
        else:
            messagebox.showerror("Error", "El editor está vacío")

    def cargar_imagen_pais(self, ruta_imagen):
        try:
            img = Image.open(ruta_imagen)
            img = img.resize((150, 100), Image.Resampling.LANCZOS)
            ctk_img = ctk.CTkImage(light_image=img, dark_image=img, size=(150, 100))
            self.etiqueta_imagen.configure(image=ctk_img)
            self.etiqueta_imagen.image = ctk_img
        except FileNotFoundError:
            self.etiqueta_imagen.configure(image='')
            self.etiqueta_imagen.image = None

    def cargar_grafico(self):
        try:
            img = Image.open("grafica.png")
            img_tk = ImageTk.PhotoImage(img)
            self.grafico_placeholder.delete("all")
            self.grafico_placeholder.create_image(0, 0, anchor="nw", image=img_tk)
            self.grafico_placeholder.image = img_tk
            self.grafico_placeholder.config(scrollregion=self.grafico_placeholder.bbox("all"))
        except FileNotFoundError:
            messagebox.showerror("Error", "No se encontró el archivo grafica.png")

    def mostrar_acerca_de(self):
        ventana_acerca_de = ctk.CTkToplevel(self.ventana)
        ventana_acerca_de.title("Acerca de")
        ventana_acerca_de.geometry("600x300")
        ventana_acerca_de.configure(fg_color="black")

        frame_principal = ctk.CTkFrame(ventana_acerca_de, fg_color="black")
        frame_principal.pack(fill=tk.BOTH, expand=True, padx=20, pady=20)

        frame_info = ctk.CTkFrame(frame_principal, fg_color="black")
        frame_info.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=(0, 10))

        info = ctk.CTkLabel(frame_info, text="Información del Desarrollador", font=("Arial", 16, "bold"), text_color="white")
        info.pack(pady=(0, 10), anchor="w")

        datos = [
            "Nombre: Josué David Velásquez Ixchop",
            "Carnet: 202307705",
            "Carrera: Ingeniería en Ciencias y Sistemas",
            "Curso: Lenguajes Formales y de Programación",
        ]

        for dato in datos:
            label = ctk.CTkLabel(frame_info, text=dato, font=("Arial", 12), text_color="white", anchor="w")
            label.pack(pady=2, anchor="w")

        frame_imagen = ctk.CTkFrame(frame_principal, fg_color="black")
        frame_imagen.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

        try:
            img = Image.open("aux.png")
            ctk_img = ctk.CTkImage(light_image=img, dark_image=img, size=(200, 250))
            imagen_label = ctk.CTkLabel(frame_imagen, image=ctk_img, text="")
            imagen_label.pack(expand=True)
        except FileNotFoundError:
            error_label = ctk.CTkLabel(frame_imagen, text="Imagen no encontrada", font=("Arial", 12), text_color="red")
            error_label.pack(expand=True)

    def run(self):
        self.ventana.mainloop()

if __name__ == "__main__":
    editor = OrgEditor()
    editor.run()