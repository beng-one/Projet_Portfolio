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
x = 50
y = 425
width = 40
height = 60
velocity = 5
isJump = False
jumpCount = 10

# boucle de jeu
running = True
while running:
    pygame.time.delay(50)
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Déplacer le caractère
    keys = pygame.key.get_pressed()
    if keys[pygame.K_LEFT] and x > velocity:
        x -= velocity
    if keys[pygame.K_RIGHT] and x < screen_width - width - velocity:
        x += velocity
    if not (isJump):
        if keys[pygame.K_UP] and y > velocity:
            y -= velocity
        if keys[pygame.K_DOWN] and y < screen_height - height - velocity:
            y += velocity
        if keys[pygame.K_SPACE]:
            isJump = True
    else:
        if jumpCount >= -10:
            neg = 1
            if jumpCount < 0:
                neg = -1
            y -= (jumpCount ** 2) * 0.25 * neg
            jumpCount -= 1
        else:
            isJump = False
            jumpCount = 10

    # Afiicher l'arrière plan blanc
    win.fill('black')

    # Créer un caractère à partir des attributs
    pygame.draw.rect(win, (255, 0, 0), (x, y, width, height))
    pygame.display.update()

# quitter pygame
pygame.quit()
