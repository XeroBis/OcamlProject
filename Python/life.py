import tkinter as tk
from tkinter import ttk
import random
import threading


# Barrière de synchronisation
class Barriere:
    def __init__(self, n):
        self.n = n
        self.count = 0
        self.cv = threading.Condition()

    def wait(self):
        with self.cv:
            self.count += 1
            if self.count == self.n:
                self.cv.notify_all()
                self.count = 0
            else:
                self.cv.wait()


class ConwayGameLife:
    def __init__(self, master, size):
        self.master = master
        self.size = size
        master.title("Conway's Game of Life")
        self.current_frame = None
        self.create_menu_frame()

    def create_menu_frame(self):
        if self.current_frame:
            self.current_frame.destroy()

        self.current_frame = tk.Frame(self.master)
        self.current_frame.pack()

        # Choix de la version
        menu_label = ttk.Label(self.current_frame, text="Choose a version:")
        menu_label.pack(pady=10)
        version_var = tk.StringVar()
        version_var.set("Sequential")
        version_combobox = ttk.Combobox(self.current_frame, textvariable=version_var, values=[
                                        "Sequential", "Parallel", "With Barrier"])
        version_combobox.pack(pady=10)

        # Choix de la taille de la grid
        n_label = ttk.Label(self.current_frame,
                            text="Enter the dimension (25 <= n <= 100) :")
        n_label.pack()
        self.n_var = tk.StringVar(value="25")
        n_entry = ttk.Entry(self.current_frame, textvariable=self.n_var)
        n_entry.pack(pady=10)

        # Choix d'un pattern
        pattern_label = ttk.Label(self.current_frame, text="Choose a pattern:")
        pattern_label.pack()
        self.pattern_var = tk.StringVar(value="Random")
        pattern_combobox = ttk.Combobox(self.current_frame, textvariable=self.pattern_var, values=[
                                        "Random", "Glider", "Glider Gun", "Blinker", "Toad", "Beacon", "Loafer Synth"])
        pattern_combobox.pack(pady=10)

        # Bouton de lancement
        start_button = ttk.Button(self.current_frame, text="Start",
                                  command=lambda: self.start_simulation(version_var.get()))
        start_button.pack(pady=15)

    def start_simulation(self, version):
        """
        Fonction qui sert aux différents lancements de simulations
        """
        if version == "Sequential":
            self.start_sequential_simulation()
        elif version == "Parallel":
            self.start_parallel_simulation()
        elif version == "With Barrier":
            self.start_barrier_simulation()

    def init_simulation(self):
        """
        Fonction pour initialiser la simulation, 
        est lancée par les 3 différentes simulations
        """
        if self.current_frame:
            self.current_frame.destroy()

        self.current_frame = tk.Frame(self.master)
        self.current_frame.pack()

        # Ici on récupère la valeur de la variable n initialisée dans le menu
        if self.n_var.get().isdigit():  # Si la string obtenu est un nombre
            self.n = max(min(int(self.n_var.get()), 100), 25)
        else:  # sinon (si il y a des char)
            self.n = 25
        # ici on change la valeur de n si un des 2 pattern est choisi car ils sont grands
        # et on besoin de place
        if self.pattern_var.get() in ["Glider Gun", "Loafer Synth"]:
            self.n = 50 if self.n < 50 else self.n
        self.cell_size = self.size / self.n

        # Initialisation de la population en utilisant le pattern choisi dans le menu
        self.population = self.initialize_population(
            self.n, self.pattern_var.get())

        # Initialisation du tableau des voisins
        self.voisins = [[0] * self.n for _ in range(self.n)]

        # Canvas pour afficher la grille
        self.canvas = tk.Canvas(self.current_frame,
                                width=self.size, height=self.size)
        self.canvas.pack()

        # Bouton pour revenir au menu
        back_button = ttk.Button(
            self.current_frame, text="Back", command=self.create_menu_frame)
        back_button.pack(pady=10)

    def display_population(self):
        """
        Affiche la grid avec les cellules vivantes/mortes
        """

        self.canvas.delete("all")
        for i in range(self.n):
            for j in range(self.n):
                color = "black" if self.population[i][j] == 1 else "white"
                self.canvas.create_rectangle(j * self.cell_size, i * self.cell_size, (j + 1)
                                             * self.cell_size, (i + 1) * self.cell_size, fill=color, outline="gray")

    def update_neighbors(self, x, y):
        """
        Calcule le nombre de voisins de la case en position (x, y)
        Change la valeur du tableau de voisins en position (x, y)
        """
        count = 0
        for i in range(-1, 2):
            for j in range(-1, 2):
                if 0 <= x + i < self.n and 0 <= y + j < self.n:
                    count += self.population[x + i][y + j]
        count -= self.population[x][y]
        self.voisins[x][y] = count

    def update_neighbors_barriere(self, barrier, x, y):
        """
        Calcule le nombre de voisins de la case en position (x, y)
        Change la valeur du tableau de voisins en position (x, y)
        Boucle à l'infini en attendant la barrière
        """
        while True:
            count = 0
            for i in range(-1, 2):
                for j in range(-1, 2):
                    if 0 <= x + i < self.n and 0 <= y + j < self.n:
                        count += self.population[x + i][y + j]
            count -= self.population[x][y]
            self.voisins[x][y] = count
            barrier.wait()

    def start_sequential_simulation(self):
        """
        Commence le jeu de la vie de façon séquentielle
        """
        self.init_simulation()

        # Fonction pour évoluer la population d'une génération
        def evolve_population():
            for i in range(self.n):
                for j in range(self.n):
                    self.update_neighbors(i, j)
            self.population = [[1 if (self.population[x][y] == 1 and self.voisins[x][y] in [2, 3]) or
                                (self.population[x][y] == 0 and self.voisins[x][y] == 3) else 0
                                for y in range(len(self.population[x]))] for x in range(len(self.population))]

            self.display_population()
            # Répéter après x millisecondes
            self.canvas.after(100, evolve_population)

        # Lancement de la simulation graphique
        evolve_population()

    def start_parallel_simulation(self):
        """
        Fonction qui lance la simulation avec n * n thread à chaque génération
        """
        self.init_simulation()

        def evolve_population():
            threads = []
            # pour chaque case on lance un thread qui calcule le nombre de voisins d'une case
            for i in range(self.n):
                for j in range(self.n):
                    thread = threading.Thread(
                        target=self.update_neighbors, args=(i, j))
                    threads.append(thread)
                    thread.start()
            for t in threads:
                t.join()

            # calcul de la population à chaque génération
            self.population = [[1 if (self.population[x][y] == 1 and self.voisins[x][y] in [2, 3]) or
                                (self.population[x][y] == 0 and self.voisins[x][y] == 3) else 0
                                for y in range(len(self.population[x]))] for x in range(len(self.population))]

            self.display_population()
            # Répéter après x millisecondes
            self.canvas.after(100, evolve_population)

        evolve_population()

    def start_barrier_simulation(self):
        """
        Fonction qui lance la simulation avec n * n thread en continu
        """
        self.init_simulation()
        # +1 pour inclure le thread principal
        barrier = Barriere(self.n * self.n + 1)

        # on initialize les threads une seule fois
        threads = []
        for i in range(self.n):
            for j in range(self.n):
                thread = threading.Thread(
                    target=self.update_neighbors_barriere, args=(barrier, i, j))
                threads.append(thread)
                thread.start()

        def evolve_population():
            barrier.wait()

            # calcul de la population à chaque génération
            self.population = [[1 if (self.population[x][y] == 1 and self.voisins[x][y] in [2, 3]) or
                                (self.population[x][y] == 0 and self.voisins[x][y] == 3) else 0
                                for y in range(len(self.population[x]))] for x in range(len(self.population))]

            self.display_population()
            self.canvas.after(1000, evolve_population)

        evolve_population()

    def initialize_population(self, n, pattern):
        """
        Fonction permettant d'initialiser le tableau de la population 
        avec un des pattern implémenté.
        """
        if pattern == "Random":
            return [[random.choice([0, 1]) for _ in range(n)] for _ in range(n)]

        pattern_dict = {
            "Glider": [[0, 0, 0], [0, 1, 0], [0, 0, 1], [1, 1, 1]],
            "Blinker": [[0, 0, 0], [1, 1, 1]],
            "Toad": [[0, 0, 0, 0], [0, 1, 1, 1], [1, 1, 1, 0]],
            "Beacon": [[0, 0, 0, 0], [1, 1, 0, 0], [1, 1, 0, 0], [0, 0, 1, 1], [0, 0, 1, 1]],
            "Loafer Synth": [
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ],
            "Glider Gun": [
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            ]
        }

        if pattern in pattern_dict:
            pattern_matrix = pattern_dict[pattern]

            # Redimensionner les pattern en adéquation avec la taille de la grid (n x n)
            resized_pattern = [[0] * n for _ in range(n)]

            for i in range(min(n, len(pattern_matrix))):
                for j in range(min(n, len(pattern_matrix[i]))):
                    resized_pattern[i][j] = pattern_matrix[i][j]

            return resized_pattern
        else:
            return [[0] * n for _ in range(n)]


def main():
    root = tk.Tk()
    ConwayGameLife(root, 800)
    root.mainloop()


if __name__ == "__main__":
    main()
