

; Constants
PROJECTILECOOLDOWN = 30
PROJECTILEMAX = 2
FRICTION = 10
PLAYERACCELLERATION = 20
PLAYERFALLSPEED = 20

OPTIONSNUMBER = 3

TOWERBANK1 = 16
TOWERBANKBODY1=20
TOWERBANKBODY2=21
TOWERBANKBODY3=22
TOWERBANKBODY4=23

CLOUDSBANK1=24
CLOUDSBANK2=25
CLOUDSBANK3=26
CLOUDSBANK4=27
CLOUDSBANK5=28
CLOUDSBANK6=29
CLOUDSBANK7=30
CLOUDSBANK8=31


.scope EntityType ; NB this should match the order in the process entity list in order for the jump table to hit the right address
    NoEntity = 0
    Player = 1
    Player2 = 2
    Player3 = 3
    Player4 = 4
    Slider = 5
    ProjectileSpell = 6
    Explosion = 7
    LightningEmitter = 8 
    Sparks = 9 
    Lightning = 10
    Fireball = 11
    Respawner = 12
    RespawnerPortal = 13
    RespawnerBroomStick = 14
    IceBeam = 15
    Teleporter = 16
    TeleporterP2 = 17
    TeleporterP3 = 18
    TeleporterP4 = 19
    Hat = 20
    Scoretextp1 = 21
    Scoretextp2 = 22
    Scoretextp3 = 23
    Scoretextp4 = 24
    Scorenumberp1 = 25
    Scorenumberp2 = 26
    Scorenumberp3 = 27
    Scorenumberp4 = 28
    Scorenumberseconddigitp1 = 29
    Scorenumberseconddigitp2 = 30
    Scorenumberseconddigitp3 = 31
    Scorenumberseconddigitp4 = 32
    VerticalLaser = 33
    Crystal = 34
    OptionsScreenSelector = 35
.endscope

.scope GState
    GameStateTitleScreen = 0
    GameStateMapSelect = 1
    GameStateGame = 2
    GameStateVictory = 3

.endscope

.scope ButtonReturn
    NoPress = 0
    Press = 1
    Release = 2
.endscope

.struct Entity ;  base entity structure
    type .byte ; each entity has its own type assigned, if you somehow make more than 255 entities than idk what to do
    xpos .byte ; x position
    ypos .byte ; y position
    xpossub .byte 
    ypossub .byte 
    attributes .byte ;
    collisionlayer .byte ; which of the 4 active palettes this sprite should use
    generalpurpose .byte ; this has no specific use, it can be defined on a per entity basis
    animationframe .byte
    animationtimer .byte
    flags .byte 
.endstruct

.struct MapHeader 
    mapdata .word 
    palettedata .word 
    metatiledata .word 
.endstruct

.scope Options 
    OptionPlayerCount = 0
    OptionMap = 1 
    OptionPalette = 2
.endscope


.scope EntityFlags
    NoDraw = 1
    NoProcess = 2
    FlipH = 4 
    FlipV = 8
    ; a = 16
    ; b = 32 
    ; c = 64 
    ; d = 128
.endscope

.scope DFlags
    NoDraw = 0
    ClearVertTiles1 = 1
    ClearVertTiles2 = 2
    WriteVertTiles1 = 3
    FillTileBuffer = 3
    SwapPallette = 4
    ClearScreenForVictory = 5
    WriteVictoryText = 6
    WriteVictoryPlayer = 7
.endscope

