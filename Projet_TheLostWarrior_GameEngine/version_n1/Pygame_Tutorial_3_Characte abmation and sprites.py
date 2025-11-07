# importer pygame
import pygame

# initialiser pygame
pygame.init()

# créer un écran de jeu vidéo
screen_width = 500
screen_height = 500
win = pygame.display.set_mode((screen_width, screen_height))

# Afficher la pharase d'entête
pygame.display.set_caption('The Lost Warrior')

# CharaDesign

# liste contenant des images du personnage se déplaçant vers la droite
walkRight = [pygame.image.load('R1.png'), pygame.image.load('R2.png'), pygame.image.load('R3.png'),
             pygame.image.load('R4.png'), pygame.image.load('R5.png'), pygame.image.load('R6.png'),
             pygame.image.load('R7.png'), pygame.image.load('R8.png'), pygame.image.load('R9.png')]

# liste contenant des images du personnage se déplaçant vers la gauche
walkLeft = [pygame.image.load('L1.png'), pygame.image.load('L2.png'), pygame.image.load('L3.png'),
            pygame.image.load('L4.png'), pygame.image.load('L5.png'), pygame.image.load('L6.png'),
            pygame.image.load('L7.png'), pygame.image.load('L8.png'), pygame.image.load('L9.png')]

# image de fonds
bg = pygame.image.load('bg.png')

# image du persoannge mobile
char = pygame.image.load('standing.png')

# Créer un caractère avec des attributs (taille, poids, vélocité, etc)
x = 50
y = 425
width = 64
height = 64
velocity = 10

# Attributs pour se déplacer
left = False
right = False
walkCount = 0

# Attribut pour sauter
isJump = False
jumpCount = 10

# image par seconde (FPS)
clock = pygame.time.Clock()

# Fonction pour changer d'image lorsque le personnage se déplace
def redrawGameWindow():
    global walkCount

    win.blit(bg, (0, 0))

    if walkCount + 1 >= 27:
        walkCount = 0
    if left:
        win.blit(walkLeft[walkCount // 3], (x, y))
        walkCount += 1
    elif right:
        win.blit(walkRight[walkCount // 3], (x, y))
        walkCount += 1
    else:
        win.blit(char, (x, y))
        walkCount = 0
    pygame.display.update()

# boucle principal du jeu
running = True
while running:

    # Nombre d'image par seconde
    clock.tick(27)

    # Boucle pour traiter les évènements du jeu
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # Déplacer le caractère
    keys = pygame.key.get_pressed()
    if keys[pygame.K_LEFT] and x > velocity:
        x -= velocity
        left = True
        right = False
    elif keys[pygame.K_RIGHT] and x < screen_width - width - velocity:
        x += velocity
        left = False
        right = True
    else:
        right = False
        left = False
        walkCount = 0
    if not (isJump):
        if keys[pygame.K_SPACE]:
            isJump = True
            left = False
            right = False
            walkCount = 0
    else:
        if jumpCount >= -10:
            neg = 1
            if jumpCount < 0:
                neg = -1
            y -= (jumpCount ** 2) * 0.5 * neg
            jumpCount -= 1
        else:
            isJump = False
            jumpCount = 10

    redrawGameWindow()

# quitter pygame
pygame.quit()
