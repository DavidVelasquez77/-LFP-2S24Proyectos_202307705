import customtkinter as ctk
import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess

class EditorApp(ctk.CTk):
    def __init__(self):
        super().__init__()

        self.title("Editor de Archivos")
        self.geometry("800x600")

        # Configurar el tema
        ctk.set_appearance_mode("system")
        ctk.set_default_color_theme("green")

        # Crear la barra de menú
        self.menu_bar = tk.Menu(self)
        self.config(menu=self.menu_bar)

        # Menú Archivo
        self.file_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Archivo", menu=self.file_menu)
        self.file_menu.add_command(label="Nuevo", command=self.nuevo_archivo)
        self.file_menu.add_command(label="Abrir", command=self.abrir_archivo)
        self.file_menu.add_command(label="Guardar", command=self.guardar_archivo)
        self.file_menu.add_command(label="Guardar como", command=self.guardar_como)
        self.file_menu.add_separator()
        self.file_menu.add_command(label="Salir", command=self.quit)

        # Menú Análisis
        self.analysis_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Análisis", menu=self.analysis_menu)    
        self.analysis_menu.add_command(label="Analizar", command=self.enviar_datos)    

        # Menú Tokens
        self.tokens_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Tokens", menu=self.tokens_menu)
        self.tokens_menu.add_command(label="Mostrar Tokens", command=self.mostrar_tokens)


        # Área de texto
        self.texto = ctk.CTkTextbox(self)
        self.texto.pack(expand=True, fill="both", padx=10, pady=10)
        self.texto.configure(font=("Consolas", 14))

        # Área de información
        self.info_frame = ctk.CTkFrame(self)
        self.info_frame.pack(fill="x")

        self.posicion_label = ctk.CTkLabel(self.info_frame, text="Posición: 1, 0")
        self.posicion_label.pack(side="left", padx=5)

        # Tabla de errores
        self.tabla_errores = ctk.CTkTextbox(self, height=100)
        self.tabla_errores.pack(fill="x", padx=10, pady=10)
        self.tabla_errores.insert("1.0", "Tipo\tLínea\tColumna\tToken\tDescripción\n")
        self.tabla_errores.configure(state="disabled")

        # Binding para actualizar la posición del cursor
        self.texto.bind("<<CursorChange>>", self.actualizar_posicion)
        self.texto.bind("<KeyRelease>", self.actualizar_posicion)
        self.texto.bind("<ButtonRelease-1>", self.actualizar_posicion)

    def nuevo_archivo(self):
        if self.texto.get("1.0", "end-1c"):
            if self.preguntar_guardar():
                self.texto.delete("1.0", "end")
                if hasattr(self, 'current_file'):
                    del self.current_file

    def abrir_archivo(self):
        if self.texto.get("1.0", "end-1c"):
            if not self.preguntar_guardar():
                return
        
        file_path = filedialog.askopenfilename(filetypes=[("LFP files", "*.lfp")])
        if file_path:
            with open(file_path, 'r') as file:
                content = file.read()
                self.texto.delete("1.0", "end")
                self.texto.insert("1.0", content)
            self.current_file = file_path

    def guardar_archivo(self):
        if hasattr(self, 'current_file'):
            with open(self.current_file, 'w') as file:
                content = self.texto.get("1.0", "end-1c")
                file.write(content)
        else:
            self.guardar_como()

    def guardar_como(self):
        file_path = filedialog.asksaveasfilename()
        if file_path:
            with open(file_path, 'w') as file:
                content = self.texto.get("1.0", "end-1c")
                file.write(content)
            self.current_file = file_path

    def preguntar_guardar(self):
        respuesta = messagebox.askyesnocancel("Guardar cambios", "¿Desea guardar los cambios?")
        if respuesta is True:
            self.guardar_archivo()
            return True
        elif respuesta is False:
            return True
        else:
            return False

    def actualizar_posicion(self, event=None):
        index = self.texto.index(tk.INSERT)
        line, column = index.split('.')
        self.posicion_label.configure(text=f"Posición: {line}, {int(column)+1}")
        
    def enviar_datos(self):
        dato = self.texto.get("1.0", "end-1c")
        
        # Ejecutar el programa Fortran y enviar el dato
        
        resultado = subprocess.run(
            ["./main.exe"],  # Ejecutable compilado
            input=dato,  # Enviar el dato como cadena de texto
            stdout=subprocess.PIPE,  # Capturar la salida del programa
            text=True  # Asegurarse de que la salida se maneje como texto
        )
        
        salida = resultado.stdout.strip()
        self.tabla_errores.configure(state="normal")
        self.tabla_errores.delete("1.0", "end")
        self.tabla_errores.insert("1.0", salida)
        self.tabla_errores.configure(state="disabled")
        
    
    def mostrar_tokens(self):
        pass
        



if __name__ == "__main__":
    app = EditorApp()
    app.mainloop()