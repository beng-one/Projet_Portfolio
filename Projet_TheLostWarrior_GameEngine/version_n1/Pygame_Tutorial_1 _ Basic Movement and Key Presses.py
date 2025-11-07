# importer pygame
import pygame

# initialiser pygame
pygame.init()

# créer un écran de jeu vidéo
screen_width = 500
screen_height = 500
win = pygame.display.set_mode((screen_width, screen_height))

# Afficher la pharase d'entête
pygame.display.set_caption('First Game')

# Créer un caractère avec des attributs (taille, poids, vélocité, etc)
x = 0
y = 0
width = 4
height = 30
velocity = 1000

# boucle de jeu
running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Déplacer le caractère
    keys = pygame.key.get_pressed()
    if keys[pygame.K_LEFT]:
        x -= velocity
    if keys[pygame.K_RIGHT]:
        x += velocity
    if keys[pygame.K_UP]:
        y += velocity
    if keys[pygame.K_DOWN]:
        y += velocity

    # Afiicher l'arrière plan blanc
    win.fill('black')

    # Créer un caractère à partir des attributs
    pygame.draw.rect(win, (255, 0, 0), (x, y, width, height))
    pygame.display.update()

# quitter pygame
pygame.quit()
