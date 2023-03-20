; This is a template for a basic NES rom
; it uses ines 1.0, not 2.0

; Constants
PROJECTILECOOLDOWN = 30
PROJECTILEMAX = 2
FRICTION = 10
PLAYERACCELLERATION = 5
PLAYERFALLSPEED = 20

; INes 1.0
.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $02 ; 2 * 8KB CHR ROM
.byte %01000011 ; mapper and mirroring
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte "<"
.byte "3Matt" ; filler bytes to fill out the end of the header

.scope Numbers
    Zero = 240
    One = 241 
    Two = 242 
    Three = 243
    Four = 244
    Five = 245
    Six = 246
    Seven = 247
    Eight = 248 
    Nine = 249
.endscope

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

.segment "ZEROPAGE" ; 0-FF. One page of ram that is faster access than rest of the ram. Use for values most frequently used
    MAXENTITIES =15; max allowed number of entities. Each entity takes a number of bytes in the zero page equal to the entity struct
    ; this CANNOT run over the end of the zero page or it will not work. If you want more entities, you will need to dedicate a non zero
    ; page ram segment to it
    entities: .res .sizeof(Entity) * MAXENTITIES ; 6 * 30 = 180/256 bytes
    entity_mem = .sizeof(Entity) * MAXENTITIES ; mem used

    vxhigh: .res 1
    vxlow: .res 1
    CurrentColumnHigh: .res 1
    CurrentColumnLow: .res 1
    world: .res 2 ; this is a pointer to the address of the current screen we want to fetch tiles on
    metatilepointer: .res 2
    pallettepointer: .res 2
    screenheaderpointer: .res 2
    seed: .res 2 ; the seed for the pseudo rng. This will be inited to anything by 0 later
    jumppointer: .res 2 ; used to jump to specic places in memory with jump tables
    MetaspritePointer: .res 2
    nmidone: .res 1 ; value to check to see if the nmi is done
    scrollx: .res 1 ; how far the screen is scrolled in x dir
    scrolly: .res 1 ; how far the screen is scrolled in y dir
    buttons: .res 1 ; this holds the state of player input for controller 1
    buttonsp2: .res 1 ; this holds the state of player input for controller 1
    buttonsp3: .res 1
    buttonsp4: .res 1
    buttonflag: .res 1
    buttonflagp2: .res 1
    buttonflagp3: .res 1 
    buttonflagp4: .res 1 
    currenttable: .res 2
    currentrowodd: .res 1 ; holds the currrent row when drawing tiles
    currentroweven: .res 1
    currentcollisionaddress: .res 2
    tilebufferindex: .res 1
    framecount: .res 1
    oambufferoffset: .res 1
    animationtrack: .res 1
    animationtracktimer: .res 1
    vyhigh: .res 1 
    vylow: .res 1
    playerjumptrack: .res 1
    playerflags: .res 1
    projectilecountp1: .res 1
    projectilecooldownp1: .res 1
    vxlowp2: .res 1
    vxhighp2: .res 1
    vylowp2: .res 1
    vyhighp2: .res 1
    projectilecountp2: .res 1 
    projectilecooldownp2: .res 1
    vxhighp3: .res 1
    vxlowp3: .res 1
    vyhighp3: .res 1
    vylowp3: .res 1
    vxhighp4: .res 1
    vxlowp4: .res 1
    vyhighp4: .res 1
    vylowp4: .res 1
    spritebufferposition: .res 1
    temp: .res 1
    temp2: .res 1
    temp3: .res 1
    rng: .res 1 ; rng is stored here once a frame
    rectangle1: .res 4 
    rectangle2: .res 4
    DrawFlags: .res 1 ;
    animateflags: .res 1 
    AttributesAddressHigh: .res 1
    AttributesAddressLow: .res 1

    PPUControl = $2000 
    PPUMask= $2001 
    PPUStatus = $2002
    OAMAddress =$2003
    OAMData = $2004
    PPUScroll = $2005
    PPUAddress = $2006 
    PPUData = $2007 
    OAMDMA = $4014

    IRQLATCH = $C000
    IRQRELOAD = $C001
    IRQDISABLE = $E000
    IRQENABLE = $E001

    BANKSELECT = $8000
    BANKDATA = $8001
    MIRRORING = $A000
    PRGRAMPROTECT = $A001

.segment "OAM"
SpriteBuffer: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "RAM"
    CurrentBackgroundPalette: .res 16
    CurrentSpritePallette: .res 16
    GameTimer: .res 2
    GameTimerConverted: .res 2
    GameTimerAddress: .res 2
    CollisionMap: .res 240
    PalletteFadeIndex: .res 1
    PalletteFadeTrigger: .res 1
    PalletteFlags: .res 1
    TileBuffer: .res 32
    TileBuffer2: .res 32
    AttributeBuffer: .res 16
    SpawnerIndex: .res 1
    SpawnerStack: .res 16
    DestructionIndex: .res 1
    DestructionStack: .res 5 
    PlayerSpawnIndex: .res 1
    PlayerSpawnStack: .res 4
    ScoreP1: .res 3
    ScoreP1Converted: .res 2
    ScoreP2: .res 3
    ScoreP2Converted: .res 2
    ScoreP3: .res 3
    ScoreP3Converted: .res 2
    ScoreP4: .res 3
    ScoreP4Converted: .res 2
    ScorePickerState: .res 1
    GameState: .res 1
    InvincibilityTimerP1: .res 1
    InvincibilityTimerP2: .res 1
    InvincibilityTimerP3: .res 1
    InvincibilityTimerP4: .res 1
    WeaponTypeP1: .res 1
    WeaponTypeP2: .res 1
    WeaponTypeP3: .res 1
    WeaponTypeP4: .res 1
    ConveyorBeltAddress1: .res 2
    ConveyorBeltBuffer1: .res 8 
    ConveyorBeltAddress2: .res 2
    ConveyorBeltBuffer2: .res 8
    ConveyorBeltAddress3: .res 2
    ConveyorBeltBuffer3: .res 8 
    ConveyorBeltAddress4: .res 2
    ConveyorBeltBuffer4: .res 8 
    WaterfallBuffer1: .res 8
    WaterfallBuffer2: .res 8

    WaterfallAddress1: .res 2
    WaterfallAddress2: .res 2
    WaterfallAddress3: .res 2
    WaterfallAddress4: .res 2


.segment "STARTUP" ; this is called when the console starts. Init a few things here, otherwise little here
    Reset:
        SEI ; Disables all interrupts
        CLD ; disable decimal mode (nes 6502 does not support decimals)

    

        ; Disable sound IRQ
        LDA #$40
        STA $4017

        ; Initialize the stack register
        LDX #$FF
        TXS

        INX ; #$FF + 1 => #$00

        ; Zero out the PPU registers
        STX PPUControl
        STX PPUMask

        STX $4010

    :
        BIT PPUStatus ; this waits for a vblank
        BPL :-

        TXA

;; This clears out the memory when we start up
CLEARMEM:
    STA $0000, X ; Zero page memory
    STA $0100, X 
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    LDA #$FF
    STA $0200, X ; this is a buffer Sprite/entity data, which can can be sent to the ppu later during nmi
    LDA #$00
    INX
    BNE CLEARMEM    ; Keep looping until all the memory is cleared

; wait for vblank. We want to wait for the system to do one scan of the screen before we do anthing else
:
    BIT PPUStatus
    BPL :-

    LDA #$02
    STA OAMDMA
    NOP

    ; $3F00 == palette address
    LDA #$3F
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    LDX #$00

.segment "CODE"

LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #$23
STA AttributesAddressHigh
LDA #$C0
STA AttributesAddressLow

LDA #<MetaTileListTitle
STA metatilepointer
LDA #>MetaTileListTitle
STA metatilepointer+1

; JSR SwapLeftBankToTitle
; JSR SwapRightBankToTitle

;Load Pallete
LDA #<PaletteData
STA pallettepointer
LDA #>PaletteData
STA pallettepointer+1
JSR LoadPalletteToRam
; LoadPalettes:
;     FillPaletteRam:
;         LDA PaletteData, X
;         STA CurrentBackgroundPalette, X 
;         INX 
;         CPX #$20 
;         BNE FillPaletteRam
;         LDX #$00
LDX #$00 
    LoadtoPPU:
        LDA CurrentBackgroundPalette, X 
        STA PPUData ; $3F00, $3F01, $3F02 => $3F1F
        INX
        CPX #$20
        BNE LoadtoPPU 

; For the prng to work, it needs a seed of any nonzero number to start at
InitSeed: 
    LDA #$08
    STA seed
    STA seed+1




InitWorld:
    LDA #< ScreenDefault ; take the low byte
    STA world ; store low byte in z page
    LDA #> ScreenDefault ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

; setup address in PPU for nametable data
    BIT PPUStatus ; reading PPUStatus sets the latch to access scrolling and writing to the PPU 
    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    LDX #$00
    LDY #$00
                
SetAttributes:
    LDX #$00
    LDA #$23
    STA PPUAddress
    LDA #$C0
    STA PPUAddress
    AttributeLoop:
    LDA AttributesDefault, X 
    STA $2007
    INX
    CPX #64
    BNE AttributeLoop


LDA #<ScreenDefault
STA world
LDA #>ScreenDefault
STA world+1
JSR LoadCollisionData


    BIT PPUControl
    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUAddress
    LDA #$00
    STA PPUAddress


JSR InitTitleScreen

ldx #<music_data_untitled
ldy #>music_data_untitled
        
; lda #$01 ; NTSC
; jsr famistudio_init
; lda #$00
; jsr famistudio_music_play

LDA #$02
STA playerflags


LDA #$BF 
STA IRQLATCH
STA IRQRELOAD
STA IRQDISABLE
STA IRQENABLE





; LDA #$00
; LDA #DFlags::ClearVertTiles1
; STA DrawFlags

; Enable Rendering
LDA #%10010000
STA PPUControl
LDA #%00111110
STA PPUMask
CLI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    ; JSR ManageGameState
    JSR SelectGameStatePath
    JSR Setrng
    JSR FillTileBuffer
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR OAMBuffer   ; Sprite data is written to the buffer here
    ; JSR famistudio_update
    
; Once the game logic loop is done, we hover here and wait for a vblank
; After a return from Vblank, we jump back to the logic loop    
IsVBlankDone:
    LDA nmidone
    CMP #$01
    BNE IsVBlankDone
    LDA #$00
    STA nmidone
    JMP Loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; NMI Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Main loop that exectutes when the NMI interrupts. Here is where we want to do our drawing. All drawing must be done before the beam resets
MAINLOOP: 
    ; load the current registers onto the stack. This doesn't matter if we finish our logic before the nmi is called, 
    ;but if it is, we need to preserve the registers so that we can carry on where we left off 
    PHA ; 2
    TXA ; 2
    PHA ; 2
    TYA ;2
    PHA  ;2

    ;10

    LDA #$00 ;2
    STA PPUMask ;4 ; disable rendering before we access the ppu for safety

    ; Check if we're drawing new tiles via jump table
    LDA DrawFlags ;3
    BEQ :+ ;2/3
    ASL ;2
    TAY ;2
    LDA NMIDrawCommands, Y ;5 
    STA jumppointer ;3
    LDA NMIDrawCommands+1, Y ;5 
    STA jumppointer+1;3
    JMP (jumppointer);5
    :
    NMIDrawCommandReturnPoint:

    LDA #$BF ;2
    STA IRQLATCH ;4
    STA IRQRELOAD ;4
    STA IRQENABLE ;4

    ; read sprites
    LDA #$00
    STA $2003 
    LDA #$02 
    STA OAMDMA

    BIT PPUStatus;4
    LDA #$00;2
    STA PPUScroll;4
    STA PPUScroll;4

    LDA #%10011110;2
    STA PPUMask;4 ; reenable rendering for the ppu
    INC nmidone;5



    ; Bring the stack values back into the registers
    PLA;2
    TAY ;2
    PLA ;2
    TAX ;2
    PLA ;2
    
    RTI ;6

NMIDrawCommands:
    .word NMINoCommand ; 0 
    .word NMIClearVerticalColumn1
    .word NMIClearVerticalColumn2
    .word NMIWriteVertTiles1 ; 3
    .word NMISwapPallette ; 4
    .word NMIClearVictoryScreen ;5
    .word NMIDrawVictoryScreen ; 6
    .word NMIDrawVictoryPlayer ; 7
    .word NMIUpdateAnimatedTiles ; 8

NMIAnimateCommands:
    .word NMINoCommand
    .word NMIUpdateAnimatedTiles

NMISwapPallette:

TriggerVerticalNMIDraw:
LDA DrawFlags
EOR #DFlags::ClearVertTiles2
STA DrawFlags
lda #$01 
sta CurrentColumnLow
RTS

NMINoCommand:
    JMP NMIDrawCommandReturnPoint

NMIClearVerticalColumn1:
    ;Set up the next address to write to
    BIT PPUStatus
    LDA CurrentColumnHigh
    STA PPUAddress
    LDA CurrentColumnLow
    STA PPUAddress
    LDA #%10010100
    STA PPUControl
    LDX #30
    LDA #$EC
    :
    STA PPUData 
    DEX 
    BNE :- 
    LDA CurrentColumnLow
    CLC 
    ADC #$02
    STA CurrentColumnLow
    CMP #32
    BNE :+
    LDA #$01 
    STA CurrentColumnLow
    ; LDA #DFlags::ClearVertTiles2
    LDA #$02
    STA DrawFlags
    :
JMP NMIDrawCommandReturnPoint

NMIClearVerticalColumn2:
    BIT PPUStatus
    LDA CurrentColumnHigh
    STA PPUAddress
    LDA CurrentColumnLow
    STA PPUAddress
    LDA #%10010100
    STA PPUControl
    LDX #30
    LDA #$EC
    :
    STA PPUData 
    DEX 
    BNE :- 
    LDA CurrentColumnLow
    CLC 
    ADC #$02
    STA CurrentColumnLow
    CMP #33
    BNE :+
    LDA #$20
    STA CurrentColumnHigh
    LDA #$00
    STA CurrentColumnLow
    LDA #$00
    STA currentroweven
    ; LDA #DFlags::WriteVertTiles1
    LDA #$03
    STA DrawFlags
    ; LDA DrawFlags
    ; EOR #DFlags::FillTileBuffer
    ; STA DrawFlags
    :
JMP NMIDrawCommandReturnPoint

NMIWriteVertTiles1:
    LDA #%10010000
    STA PPUControl
    BIT PPUStatus
    LDA CurrentColumnHigh
    STA PPUAddress
    LDA CurrentColumnLow
    STA PPUAddress
    
    LDY #$00
    :
    LDA TileBuffer, Y
    STA PPUData
    INY
    LDA TileBuffer, Y
    STA PPUData
    INY 
    CPY #32
    BNE :-

    LDY #$00
    :
    LDA TileBuffer2, Y
    STA PPUData
    INY
    LDA TileBuffer2, Y
    STA PPUData
    INY 
    CPY #32 
    BNE :-

    LDA CurrentColumnLow
    CLC 
    ADC #$40
    sta CurrentColumnLow
    LDA CurrentColumnHigh
    ADC #$00
    sta CurrentColumnHigh
    LDA currentroweven
    CLC 
    ADC #$10
    STA currentroweven

    ;check if we're at the end
    LDA CurrentColumnLow
    CMP #$C0 
    BNE :+
    LDA CurrentColumnHigh
    CMP #$23
    BNE :+
    LDA #$00
    STA DrawFlags
    :
JMP NMIDrawCommandReturnPoint


NMIClearVictoryScreen:
    LDA #%10010000
    STA PPUControl
    BIT PPUStatus
    LDA CurrentColumnHigh
    STA PPUAddress
    LDA CurrentColumnLow
    STA PPUAddress
    LDA #$FF
    LDX #64
    :
    STA PPUData
    DEX
    bne :-
; LDA #%00010000
; STA PPUControl
; BIT PPUStatus
; LDA CurrentColumnHigh
; STA PPUAddress
; LDA CurrentColumnLow
; STA PPUAddress

; LDX #31
; LDA #$00
; ClearVictoryScreenLoop:
;     STA PPUData
;     DEX
;     BNE :-
LDA CurrentColumnLow
CLC 
ADC #$40
STA CurrentColumnLow
LDA CurrentColumnHigh
ADC #$00
STA CurrentColumnHigh

LDA CurrentColumnLow
CMP #$C0
BNE :+
LDA CurrentColumnHigh
CMP #$23
BNE :+
LDA #$03
STA DrawFlags
:
JMP NMIDrawCommandReturnPoint

NMIDrawVictoryScreen:
jmp NMIDrawCommandReturnPoint


NMIDrawVictoryPlayer:
JMP NMIDrawCommandReturnPoint

NMIUpdateAnimatedTiles: ;180
; JMP NMIDrawCommandReturnPoint ;

    LDA #%10010000 ;2
    STA PPUControl ;4
    BIT PPUStatus ;4
    LDA ConveyorBeltAddress1 ;4
    STA PPUAddress ;4
    LDA ConveyorBeltAddress1+1
    STA PPUAddress
    LDA ConveyorBeltBuffer1
    STA PPUData
    LDA ConveyorBeltBuffer1+1
    STA PPUData
    LDA ConveyorBeltBuffer1+2
    STA PPUData
    LDA ConveyorBeltBuffer1+3
    STA PPUData
    LDA ConveyorBeltBuffer1+4
    STA PPUData
    LDA ConveyorBeltBuffer1+5
    STA PPUData
    LDA ConveyorBeltBuffer1+6
    STA PPUData
    LDA ConveyorBeltBuffer1+7
    STA PPUData

    BIT PPUStatus ;4
    LDA ConveyorBeltAddress2 ;4
    STA PPUAddress ;4
    LDA ConveyorBeltAddress2+1 ;4
    STA PPUAddress ;4

    LDA ConveyorBeltBuffer2
    STA PPUData
    LDA ConveyorBeltBuffer2+1
    STA PPUData
    LDA ConveyorBeltBuffer2+2
    STA PPUData
    LDA ConveyorBeltBuffer2+3
    STA PPUData
    LDA ConveyorBeltBuffer2+4
    STA PPUData
    LDA ConveyorBeltBuffer2+5
    STA PPUData
    LDA ConveyorBeltBuffer2+6
    STA PPUData
    LDA ConveyorBeltBuffer2+7
    STA PPUData

    LDA #%10010100
    STA PPUControl
    BIT PPUStatus
    LDA WaterfallAddress1
    STA PPUAddress
    LDA WaterfallAddress1+1
    STA PPUAddress
    LDA WaterfallBuffer1
    STA PPUData
    LDA WaterfallBuffer1+1
    STA PPUData
    LDA WaterfallBuffer1+2
    STA PPUData
    LDA WaterfallBuffer1+3
    STA PPUData
    LDA WaterfallBuffer1+4
    STA PPUData
    LDA WaterfallBuffer1+5
    STA PPUData
    LDA WaterfallBuffer1+6
    STA PPUData
    LDA WaterfallBuffer1+7
    STA PPUData

    BIT PPUStatus
    LDA WaterfallAddress2
    STA PPUAddress
    LDA WaterfallAddress2+1
    STA PPUAddress
    lda WaterfallBuffer2
    sta PPUData
    LDA WaterfallBuffer2+1
    STA PPUData
    LDA WaterfallBuffer2+2
    STA PPUData
    LDA WaterfallBuffer2+3
    STA PPUData
    LDA WaterfallBuffer2+4
    STA PPUData
    LDA WaterfallBuffer2+5
    STA PPUData
    LDA WaterfallBuffer2+6
    STA PPUData
    LDA WaterfallBuffer2+7
    STA PPUData

    BIT PPUStatus
    LDA WaterfallAddress3
    STA PPUAddress
    LDA WaterfallAddress3+1
    STA PPUAddress
    LDA WaterfallBuffer1
    STA PPUData
    LDA WaterfallBuffer1+1
    STA PPUData
    LDA WaterfallBuffer1+2
    STA PPUData
    LDA WaterfallBuffer1+3
    STA PPUData
    LDA WaterfallBuffer1+4
    STA PPUData
    LDA WaterfallBuffer1+5
    STA PPUData
    LDA WaterfallBuffer1+6
    STA PPUData
    LDA WaterfallBuffer1+7
    STA PPUData

    BIT PPUStatus
    LDA WaterfallAddress4
    STA PPUAddress
    LDA WaterfallAddress4+1
    STA PPUAddress
    LDA WaterfallBuffer2
    STA PPUData
    LDA WaterfallBuffer2+1
    STA PPUData
    LDA WaterfallBuffer2+2
    STA PPUData
    LDA WaterfallBuffer2+3
    STA PPUData
    LDA WaterfallBuffer2+4
    STA PPUData
    LDA WaterfallBuffer2+5
    STA PPUData
    LDA WaterfallBuffer2+6
    STA PPUData
    LDA WaterfallBuffer2+7
    STA PPUData



JMP NMIDrawCommandReturnPoint ;

IRQ:
    PHA 
    BIT PPUStatus
    LDA #$00
    STA PPUScroll
    STA PPUScroll
    STA IRQDISABLE
    PLA 
RTI

;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

LoadPalletteToRam:
    LDY #$00
    LoadPalletteToRamLoop:
    LDA (pallettepointer), Y
    STA CurrentBackgroundPalette, Y
    INY 
    CPY #$20
    BNE LoadPalletteToRamLoop
RTS

BrightenPallette:
    LDX #$00
    BrightenPalletteLoop:
    LDA CurrentBackgroundPalette, X 
    CLC 
    ADC #$10
    CMP #$40
    BCC :+
    STA CurrentBackgroundPalette, X
    :
    INX
    CPX #$20
    BNE BrightenPalletteLoop
RTS 

FadePallette:
    LDX #$00
    FadePalletteLoop:
    LDA CurrentBackgroundPalette, X 
    SEC 
    SBC #$10
    BCC :+
    LDA #$0f
    :
    STA CurrentBackgroundPalette, X
    INX
    CPX #$20
    BNE FadePalletteLoop
RTS

;;;
; Initialisation
;;;
InitTitleScreen:
;Clear all sprite entities
LDX #$00
LDA #$00
:
STA entities, X
INX 
CPX #entity_mem
BNE :-

LDA #<MetaTileListTitle
STA metatilepointer
LDA #>MetaTileListTitle
sta metatilepointer+1

LDA #<TitleScreen
STA world
LDA #>TitleScreen
STA world+1

jsr SwapLeftBankToTitle
JSR SwapRightBankToTitle
LDA #$03
STA DrawFlags

LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

 


LDA #$01
STA PalletteFlags


LDA #GState::GameStateTitleScreen
STA GameState

RTS 


InitGameHat:
;Clear all sprite entities
LDX #$00
LDA #$00
:
STA entities, X
INX 
CPX #entity_mem
BNE :-

; LDX #$a0
; LDY #$e0
; LDA #EntityType::LightningEmitter
; JSR SpawnEntity





LDX #$C0
LDY #$10
LDA #EntityType::Hat
jsr SpawnEntity
LDX #$D8
LDY #$c8
LDA #EntityType::Player 
JSR SpawnEntity
; LDX #$C0
; LDY #$10
; LDA #EntityType::Player2
; JSR SpawnEntity

LDX #$10
LDY #$40
LDA #EntityType::VerticalLaser
JSR SpawnEntity

LDX #$80
LDY #$a0
LDA #EntityType::LightningEmitter
jsr SpawnEntity


LDX #$10
LDY #$10
LDA #EntityType::Scoretextp1
jsr SpawnEntity

LDX #$20
LDY #$10
LDA #EntityType::Scorenumberp1
jsr SpawnEntity

LDX #$24
LDY #$10 
LDA #EntityType::Scorenumberseconddigitp1
JSR SpawnEntity

LDX #$10
LDY #$18
LDA #EntityType::Scoretextp2
jsr SpawnEntity

LDX #$20
LDY #$18
LDA #EntityType::Scorenumberp2
jsr SpawnEntity

LDX #$24
LDY #$18 
LDA #EntityType::Scorenumberseconddigitp2
JSR SpawnEntity


LDX #$10
LDY #$20
LDA #EntityType::Scoretextp3
jsr SpawnEntity

LDX #$10
LDY #$28
LDA #EntityType::Scoretextp4
jsr SpawnEntity

LDA #$00
STA ScoreP1
STA ScoreP2
STA ScoreP3
STA ScoreP4

LDA #$09
STA ScoreP1+1
STA ScoreP2+1
STA ScoreP3+1
STA ScoreP4+1

LDA #$00
STA WeaponTypeP1
ADC #$01
STA WeaponTypeP2
ADC #$01
STA WeaponTypeP3
ADC #$01
STA WeaponTypeP4

LDA #$03
STA DrawFlags
LDA #$04
STA GameState

LDA #$23
STA ConveyorBeltAddress1
LDA #$44
STA ConveyorBeltAddress1+1

LDA #$23
STA ConveyorBeltAddress2
LDA #$54
STA ConveyorBeltAddress2+1

LDA #$22
STA WaterfallAddress1
LDA #$D2
STA WaterfallAddress1+1

LDA #$22
STA WaterfallAddress2
LDA #$D3
STA WaterfallAddress2+1


LDA #$22
STA WaterfallAddress3
LDA #$CC
STA WaterfallAddress3+1
LDA #$22
STA WaterfallAddress4
LDA #$CD
STA WaterfallAddress4+1

LDA #$AC 
STA WaterfallBuffer1
LDA #$BC 
STA WaterfallBuffer1+1
LDA #$CC 
STA WaterfallBuffer1+2
LDA #$DC 
STA WaterfallBuffer1+3
LDA #$AE 
STA WaterfallBuffer1+4
LDA #$BE 
STA WaterfallBuffer1+5
LDA #$CE 
STA WaterfallBuffer1+6
LDA #$DE 
STA WaterfallBuffer1+7

LDA #$AD 
STA WaterfallBuffer2
LDA #$BD 
STA WaterfallBuffer2+1
LDA #$CD 
STA WaterfallBuffer2+2
LDA #$DD 
STA WaterfallBuffer2+3
LDA #$AF 
STA WaterfallBuffer2+4
LDA #$BF 
STA WaterfallBuffer2+5
LDA #$CF 
STA WaterfallBuffer2+6
LDA #$DF 
STA WaterfallBuffer2+7


LDA #$E0
LDX #$00
:
STA ConveyorBeltBuffer1, x
sta ConveyorBeltBuffer2, x
INX
CPX #$08
BNE :-

LDA #<MetaTileList
STA metatilepointer
LDA #>MetaTileList
sta metatilepointer+1

LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #9
STA GameTimer+1
LDA #0
STA GameTimer 

LDA #$20
STA GameTimerAddress
LDA #$0F
STA GameTimerAddress+1

LDA #<ScreenDefault
STA world
LDA #>ScreenDefault
STA world+1

JSR SwapLeftBankToGame
JSR SwapRightBankToGame

LDA #<PaletteData
STA pallettepointer
LDA #>PaletteData
STA pallettepointer+1
JSR LoadPalletteToRam

RTS

InitVictoryScreen:
LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #GState::GameStateVictory
STA GameState

LDA #<MetaTileListTitle
STA metatilepointer
LDA #>MetaTileListTitle
sta metatilepointer+1

LDA #<VictoryScreen
STA world
LDA #>VictoryScreen
STA world+1


JSR SwapRightBankToTitle
JSR SwapLeftBankToTitle

LDA #DFlags::ClearScreenForVictory
STA DrawFlags


RTS



ManageGameState:
    JSR DecrementTimers
RTS

SelectGameStatePath:
    LDA GameState
    ASL 
    TAY 
    LDA GameStatePath, Y 
    STA jumppointer
    LDA GameStatePath+1, Y 
    STA jumppointer+1
    JMP (jumppointer)

DoTitleLogic:
    JSR ReadButtons
    JSR ProcessEntities
    JSR ProcessTitleInput

DoMapSelectLogic:
    JSR ReadButtons
    JSR ProcessEntities
    JSR ProcessMapSelectInput

DoGameLogic:
    JSR ProcessSpawnStack
    JSR ReadButtons
    JSR DecrementTimers
    JSR ProcessEntities
    JSR UpdateConveyorBelts
    JSR UpdateWaterfalls
    JSR ProcessDestructionStack
    JSR ProcessPlayerSpawnStack
    JSR CheckForGameEnd
    RTS 

WaitForGameLoad:
    LDA DrawFlags
    BNE :+
    LDA #$08 
    STA DrawFlags
    LDA #$02 
    STA GameState
    :
RTS

DoVictoryScreenLogic:
RTS

ProcessMapSelectInput:
RTS

ProcessTitleInput:
    ; JSR CheckStart
    ; CMP #ButtonReturn::Release
    LDA rng
    CMP #$FF
    BNE :+
        JSR InitGameHat
    :
    RTS

; a = jump table destination/2 
; x = amount to change by
AddScoreP1:
    INC ScoreP1+1
    LDA ScoreP1+1
    CMP #$0A
    BCC :+
    LDA #$00 
    STA ScoreP1+1
    INC ScoreP1
    LDA ScoreP1
    CMP #$0A 
    BCC :++
    LDA #$00
    STA ScoreP1
    :
    STA ScoreP1+1
    RTS
    :
    STA ScoreP1
RTS

SubScoreP1:
    DEC ScoreP1+1
    LDA ScoreP1+1
    BPL :+
    LDA #$09 
    STA ScoreP1+1
    DEC ScoreP1
    LDA ScoreP1
    BPL :++
    LDA #$09
    STA ScoreP1
    :
    STA ScoreP1+1
    RTS
    :
    STA ScoreP1
RTS

AddScoreP2:
    INC ScoreP2+1
    LDA ScoreP2+1
    CMP #$0A
    BCC :+
    LDA #$00 
    STA ScoreP2+1
    INC ScoreP2
    LDA ScoreP2
    CMP #$0A 
    BCC :++
    LDA #$00
    STA ScoreP2
    :
    STA ScoreP2+1
    RTS
    :
    STA ScoreP2
RTS

SubScoreP2:
    DEC ScoreP2+1
    LDA ScoreP2+1
    BPL :+
    LDA #$09 
    STA ScoreP2+1
    DEC ScoreP2
    LDA ScoreP2
    BPL :++
    LDA #$09
    STA ScoreP2
    :
    STA ScoreP2+1
    RTS
    :
    STA ScoreP2
RTS

AddScoreP3:
    INC ScoreP3+1
    LDA ScoreP3+1
    CMP #$0A
    BCC :+
    LDA #$00 
    STA ScoreP3+1
    INC ScoreP3
    LDA ScoreP3
    CMP #$0A 
    BCC :++
    LDA #$00
    STA ScoreP3
    :
    STA ScoreP3+1
    RTS
    :
    STA ScoreP3
RTS

SubScoreP3:
    DEC ScoreP3+1
    LDA ScoreP3+1
    BPL :+
    LDA #$09 
    STA ScoreP3+1
    DEC ScoreP3
    LDA ScoreP3
    BPL :++
    LDA #$09
    STA ScoreP3
    :
    STA ScoreP3+1
    RTS
    :
    STA ScoreP3
RTS

AddScoreP4:
    INC ScoreP4+1
    LDA ScoreP4+1
    CMP #$0A
    BCC :+
    LDA #$00 
    STA ScoreP4+1
    INC ScoreP4
    LDA ScoreP4
    CMP #$0A 
    BCC :++
    LDA #$00
    STA ScoreP4
    :
    STA ScoreP4+1
    RTS
    :
    STA ScoreP4
RTS

SubScoreP4:
    DEC ScoreP4+1
    LDA ScoreP4+1
    BPL :+
    LDA #$09 
    STA ScoreP4+1
    DEC ScoreP4
    LDA ScoreP4
    BPL :++
    LDA #$09
    STA ScoreP4
    :
    STA ScoreP4+1
    RTS
    :
    STA ScoreP4
RTS



DecrementTimers:
    LDA framecount
    BNE :+
    LDA GameTimer+1
    SEC
    SBC #$01
    STA GameTimer+1
    LDA GameTimer
    SBC #$00
    STA GameTimer
    :
    LDA GameTimer+1
    CMP #10
    BCC :+
    LDA #9
    STA GameTimer+1
    :

    ;convert timer
    LDA GameTimer+1 
    CLC 
    ADC #$F0 
    STA GameTimerConverted+1
    LDA GameTimer
    CLC 
    ADC #$F0 
    STA GameTimerConverted

    LDA projectilecooldownp1
    BEQ :+
    DEC projectilecooldownp1
    :
    ; lda #$00
    ; STA projectilecooldownp1

RTS

CheckForGameEnd:
    LDA GameState
    ASL 
    TAY 
    LDA GameEndCheckTable, Y 
    STA jumppointer
    LDA GameEndCheckTable+1, Y 
    STA jumppointer+1
    JMP (jumppointer)

GameEndCheckTable:
    .word CheckForGameEndTitle
    .word CheckForGameEndHat
    .word CheckForGameEndHat

CheckForGameEndBattle:
RTS

CheckForGameEndTitle:
RTS

CheckForGameEndHat:
    LDA ScoreP1+1
    BNE :+
    LDA ScoreP1
    BNE :+
    JSR InitVictoryScreen
    :
    LDA ScoreP2+1
    BNE :+
    LDA ScoreP2
    BNE :+
    JSR InitVictoryScreen
    :

RTS


ProcessSpawnStack:
    ; LDY SpawnerIndex
    ; BEQ EndProcessSpawnStack
    ; LDA SpawnerIndex, Y
    ; PHA  
    ; DEC SpawnerIndex
    ; LDA SpawnerIndex, Y
    ; PHA  
    ; DEC SpawnerIndex
    ; LDA SpawnerIndex, Y
    ; PHA  
    ; DEC SpawnerIndex
    ; PLA 
    ; TAY
    ; PLA 
    ; TAX 
    ; PLA
    ; JSR SpawnEntity

EndProcessSpawnStack:
    RTS 

AddEntityToSpawnStack:
    LDY SpawnerIndex
    CPY MAXENTITIES
    BNE :+
    STA SpawnerStack, Y 
    INC SpawnerIndex
    TYA 
    STA SpawnerStack, Y
    INC SpawnerIndex
    TXA 
    STA SpawnerStack, Y
    INC SpawnerIndex 
    :
    RTS 


ProcessDestructionStack:
    LDY DestructionIndex ; index for the stack
    BEQ EndProcessDestructionStack; if 0, stack is empty
    
    DestructionStackLoop:
    LDX DestructionIndex, Y ; load the entity number 
    LDA entities+Entity::type, X ; store the type for jump later
    PHA
    STA temp
    LDA entities+Entity::ypos, X 
    PHA 
    LDA entities+Entity::xpos, X 
    PHA 
    LDA #$00 
    STA entities+Entity::type, X ;clear that entity type
    STA entities+Entity::xpos, X 
    STA entities+Entity::ypos, X
    STA entities+Entity::attributes, X
    STA entities+Entity::generalpurpose, X
    STA entities+Entity::animationframe, X
    LDA #$05
    STA entities+Entity::animationtimer, X
    DEC DestructionIndex ; dec the stack
    ; JMP ProcessDestructionStack
    LDA temp 
    ASL 
    TAX
    LDA DestroyEntityList, X 
    STA jumppointer
    LDA DestroyEntityList+1, X 
    STA jumppointer+1
    JMP (jumppointer) 
; jump away to creating a death effect for whatever we destroyed

    EndProcessDestructionStack:
        RTS 

AddEntityToDestructionStack:
    LDY DestructionIndex
    STA DestructionStack, Y 
    INC DestructionIndex
    RTS 

AddEntityToPlayerSpawnStack:
    LDY PlayerSpawnIndex
    STA PlayerSpawnStack, Y 
    INC PlayerSpawnIndex
    RTS 

ProcessPlayerSpawnStack:
    LDA framecount
    BEQ :+
    RTS
    :
    LDY PlayerSpawnIndex
    BEQ EndProcessPlayerSpawnStack
    PlayerSpawnStackLoop:
    LDA PlayerSpawnIndex, Y  
    ASL 
    TAY
    LDA PlayerSpawnTable, Y
    STA jumppointer
    LDA PlayerSpawnTable+1, Y
    STA jumppointer+1
    DEC PlayerSpawnIndex
    JMP (jumppointer)

EndProcessPlayerSpawnStack:
    RTS

NoSpawn:
    RTS
; we never go here

SpawnPlayerPort1:
    LDX #$20
    LDY #$20
    LDA #EntityType::Teleporter
    JSR SpawnEntity
    RTS

SpawnPlayerPort2:
    LDX #$D0
    LDY #$20
    LDA #EntityType::TeleporterP2
    JSR SpawnEntity
    RTS

SpawnPlayerPort3:
    LDX #$30
    LDY #$A0
    LDA #EntityType::TeleporterP3
    JSR SpawnEntity
    RTS

SpawnPlayerPort4:
    LDX #$D0
    LDY #$B0
    LDA #EntityType::TeleporterP4
    JSR SpawnEntity
    RTS

; memory selection in x
; new bank in y
SwapBank:
    STX BANKSELECT
    STY BANKDATA
RTS

SwapLeftBankToGame:
LDX #%00000000
LDY #$08
STX BANKSELECT
STY BANKDATA

LDX #%00000001
LDY #$0A
STX BANKSELECT
STY BANKDATA
RTS

SwapRightBankToGame:
LDX #%00000010
LDY #$0C
STX BANKSELECT
STY BANKDATA

LDX #%00000011
LDY #$0D
STX BANKSELECT
STY BANKDATA

LDX #%00000100
LDY #$0E
STX BANKSELECT
STY BANKDATA

LDX #%00000101
LDY #$0F
STX BANKSELECT
STY BANKDATA

RTS

SwapLeftBankToTitle:
LDX #%00000000
LDY #$00
STX BANKSELECT
STY BANKDATA

LDX #%00000001
LDY #$02
STX BANKSELECT
STY BANKDATA
RTS

SwapRightBankToTitle:
LDX #%00000010
LDY #$04
STX BANKSELECT
STY BANKDATA

LDX #%00000011
LDY #$05
STX BANKSELECT
STY BANKDATA

LDX #%00000100
LDY #$06
STX BANKSELECT
STY BANKDATA

LDX #%00000101
LDY #$07
STX BANKSELECT
STY BANKDATA

RTS



SetMirroring:

RTS

;;MATHS
;; Entities

; value to add in A 
EntityAddSubX:
    CLC 
    ADC entities+Entity::xpossub, X 
    STA entities+Entity::xpossub, X 
    LDA entities+Entity::xpos, X 
    ADC #$00
    STA entities+Entity::xpos, X
RTS
EntityMinusSubX:
    SEC 
    SBC entities+Entity::ypossub, X 
    STA entities+Entity::ypossub, X 
    LDA entities+Entity::xpos, X 
    SBC #$00
    STA entities+Entity::xpos, X
RTS
EntityAddSubY:
    CLC 
    ADC entities+Entity::ypossub, X 
    STA entities+Entity::ypossub, X 
    LDA entities+Entity::ypos, X 
    ADC #$00
    STA entities+Entity::ypos, X
RTS
EntityMinusSubY:
    SEC 
    SBC entities+Entity::ypossub, X 
    STA entities+Entity::ypossub, X 
    LDA entities+Entity::ypos, X 
    SBC #$00
    STA entities+Entity::ypos, X
RTS

ProcessEntities:
    LDX #$00
    ProcessEntitiesLoop:

        ; LDA entities+Entity::type, X
        ; ASL ; transfer current entity type to y
        ; TAY  
        ; LDA ProcessEntityList, Y ; use y as a jump pointer to jump to the process function for that entity!
        ; STA jumppointer
        ; LDA ProcessEntityList+1, Y 
        ; STA jumppointer+1 
        ; JMP (jumppointer)

        LDA entities+Entity::type, X
        BNE :+
        JMP EntityComplete
        :
        ASL ; transfer current entity type to y
        TAY  
        LDA StateMachineList, Y ; use y as a jump pointer to jump to the process function for that entity!
        STA jumppointer
        LDA StateMachineList+1, Y 
        STA jumppointer+1 

        LDA entities+Entity::generalpurpose, X 
        ASL 
        TAY 
        LDA (jumppointer), Y
        PHA 
        INY 
        LDA (jumppointer), Y 
        STA jumppointer+1
        PLA 
        STA jumppointer
        JMP (jumppointer)

    SkipEntity:
        JMP EntityComplete



    ; End step of processing an entity
    ; We shift the current x offset back into A, add the size of the entity struct, then put it back in A
    ; If we now process another entity, it will be offset by the size of the struct in X, giving us the address of the next entity
    EntityComplete:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #entity_mem  ; If we have reached the same amount as the mem taken by entities, we have looped over every entity
    BEQ :+
    JMP ProcessEntitiesLoop
    :
RTS

StateMachineList:
    .word PlayerStateMachine ; dummy
    .word PlayerStateMachine
    .word Player2StateMachine
    .word Player3StateMachine
    .word Player4StateMachine
    .word SliderStateMachine
    .word ProjectileSpellStateMachine
    .word ExplosionStateMachine
    .word LightningEmitterStateMachine
    .word SparkStateMachine ;;;
    .word LightningStateMachine
    .word FireBallStateMachine
    .word RespawnerStateMachine
    .word RespawnerPortalStateMachine
    .word RespawnerBroomStickStateMachine
    .word IceBeamStateMachine
    .word TeleportStateMachine
    .word TeleportStateMachineP2
    .word TeleportStateMachineP3
    .word TeleportStateMachineP4
    .word HatStateMachine
    .word ScoreTextStateMachine
    .word ScoreTextP2StateMachine
    .word ScoreTextP3StateMachine
    .word ScoreTextP4StateMachine
    .word ScoreNumberStateMachine
    .word ScoreNumberP2StateMachine
    .word ScoreNumberP3StateMachine
    .word ScoreNumberP4StateMachine
    .word ScoreNumberDigit2StateMachine
    .word ScoreNumberDigit2StateMachineP2
    .word ScoreNumberDigit2StateMachine
    .word ScoreNumberDigit2StateMachineP2

    .word VerticalLaserStateMachine

PlayerStateMachine:
    .word PlayerInit;0
    .word PlayerOnFloor ;1
    .word PlayerMoving ; 2
    .word PlayerJumping ; 3
    .word PlayerJumpReleased ;4
    .word PlayerHeadbonk ;5
    .word PlayerFalling ;6
    .word PlayerFallingThroughPlatform;7
    .word PlayerDisabled ;8

Player2StateMachine:
    .word Player2Init 
    .word Player2OnFloor
    .word Player2Jumping
    .word Player2Headbonk
    .word Player2JumpReleased
    .word Player2Disabled


Player3StateMachine:
    .word Player3Init 
    .word Player3OnFloor
    .word Player3Jumping
    .word Player3Headbonk
    .word Player3JumpReleased
    .word Player3Disabled

Player4StateMachine:
    .word Player4Init 
    .word Player4OnFloor
    .word Player4Jumping
    .word Player4Headbonk
    .word Player4JumpReleased
    .word Player4Disabled

ProjectileSpellStateMachine:
    .word ProjectileSpellMovingLeft 
    .word ProjectileSpellMovingRight
    .word ProjectileSpellDissipating

LightningEmitterStateMachine:
    .word LightningEmitterInit
    .word LightningEmitterWaitingForSparks  
    .word LightningEmitterWaitingForLightning

LightningStateMachine:
    .word LightningInit
    .word LightningExtending
    .word LightningFlashing

SparkStateMachine:
    .word SparkInit
    .word SparkSpark


SliderStateMachine:
    .word SliderInit
    .word SliderRight
    .word SliderLeft

ExplosionStateMachine:
    .word ExplosionInit
    .word ExplosionExplode

FireBallStateMachine:
    .word FireballInit
    .word FireballMoveRight
    .word FireballMoveLeft

RespawnerStateMachine:
    .word RespawnerInit 
    .word RespawnerMoveToSpawn
    .word RespawnerMoveLeft
    .word RespawnerMoveRight

RespawnerPortalStateMachine:
    .word PortalInit 
    .word PortalExpand
    .word PortalWait
    .word PortalContract

RespawnerBroomStickStateMachine:
    .word BroomStickInit
    .word BroomStickMoveToSpawn
    .word BroomStickLeaveScreen

IceBeamStateMachine:
    .word IceBeamInit
    .word IceBeamRight
    .word IceBeamLeft 
    .word IceBeamFormCrystal

TeleportStateMachine:
    .word TeleportInit
    .word TeleportRaise
    .word TeleportSpawnP1
    .word TeleportLower

TeleportStateMachineP2:
    .word TeleportInitP2
    .word TeleportRaise
    .word TeleportSpawnP2
    .word TeleportLower

TeleportStateMachineP3:
    .word TeleportInitP3
    .word TeleportRaise
    .word TeleportSpawnP3
    .word TeleportLower

TeleportStateMachineP4:
    .word TeleportInitP4
    .word TeleportRaise
    .word TeleportSpawnP4
    .word TeleportLower

HatStateMachine:
    .word HatInit
    .word HatWaiting
    .word HatAttachedToP1
    .word HatAttachedToP2
    .word HatAttachedToP3
    .word HatAttachedToP4
    ; .word HatfollowingP1
    ; .word HatfollowingP2
    ; .word HatfollowingP3
    ; .word HatfollowingP4
    ; .word HatKnockedOff

ScoreTextStateMachine:
    .word ScoreTextp1Init
ScoreTextP2StateMachine:
    .word ScoreTextp1Init
ScoreTextP3StateMachine:
    .word ScoreTextp1Init
ScoreTextP4StateMachine:
    .word ScoreTextp1Init
     
ScoreNumberStateMachine:
    .word ScoreNumberp1Init
    .word ScoreNumberp1SetScore
ScoreNumberDigit2StateMachine:
    .word ScoreNumberDigit2p1Init
    .word ScoreNumberDigit2p1SetScore


ScoreNumberP2StateMachine:
    .word ScoreNumberp1Init
    .word ScoreNumberp2SetScore
ScoreNumberDigit2StateMachineP2:
    .word ScoreNumberDigit2p2Init
    .word ScoreNumberDigit2p2SetScore



ScoreNumberP3StateMachine:
    .word ScoreNumberp1Init
    .word ScoreNumberp1SetScore
ScoreNumberDigit2StateMachineP3:
    .word ScoreNumberDigit2p1Init
    .word ScoreNumberDigit2p1SetScore

ScoreNumberP4StateMachine:
    .word ScoreNumberp1Init
    .word ScoreNumberp1SetScore
ScoreNumberDigit2StateMachineP4:
    .word ScoreNumberDigit2p1Init
    .word ScoreNumberDigit2p1SetScore

VerticalLaserStateMachine:
    .word VerticalLaserInit
    .word VerticalLaserNormal
; Entity Behaviours

PlayerInit:
    LDA #%00000000
    STA entities+Entity::attributes, X
    ; STA animationtrack
    LDA #$00
    JSR InitAnimation
    LDA #$10 
    STA entities+Entity::animationtimer, X  
    LDA #$01
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

Player2Init:
    LDA #%00000001
    STA entities+Entity::attributes, X 
    LDA #%00000010 
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

Player3Init:
    LDA #%00000010
    STA entities+Entity::attributes, X 
    LDA #$04
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

Player4Init:
    LDA #%00000011
    STA entities+Entity::attributes, X 
    LDA #$08
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

PlayerOnFloor:
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; apply velocity then collide
    JSR PlayerApplyVelocityX
    JSR EjectFromRightWall
    JSR EjectFromLeftWall

    ; check if we are on floor
    INC entities+Entity::ypos, X
    JSR CollideDown2
    BNE :+ ; if on floor, skip ahead
        LDA #$06 ; set state to falling
        STA entities+Entity::generalpurpose, X
    :
    ; LDA #$03
    ASL 
    TAY 
    LDA CollisionEffectTable, Y 
    STA jumppointer
    LDA CollisionEffectTable+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine

    DEC entities+Entity::ypos, X
    ; Check for jump
    INC entities+Entity::ypos, X
    JSR CheckA
    BEQ :+
        JSR CollideDown
        BEQ :+
        LDA #$00
        STA playerjumptrack
        LDA #$03 ; set state to jumping
        STA entities+Entity::generalpurpose, X
    :
    DEC entities+Entity::ypos, X
    JMP EntityComplete 
PlayerMoving:
    JMP EntityComplete 
PlayerJumping:
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; jump velocity handling
    LDY playerjumptrack
    LDA JumpStrength, Y
    CMP #$01
    BEQ :+
    INC playerjumptrack
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X
    JMP :+++
    :
    ; Check if we're inside a jumpable platform
    JSR CollideDown2
    CMP #$04
    BEQ :+
    LDA #$06
    STA entities+Entity::generalpurpose, X
    JMP :++
    :
    LDA #$07
    STA entities+Entity::generalpurpose, X

    :
    ;check for headbonk
    JSR CollideUp2
    BEQ :+
        LDA #$05
    STA entities+Entity::generalpurpose, X
    :
    ; apply movement
    JSR PlayerApplyVelocityX
    JSR EjectFromLeftWall
    JSR EjectFromRightWall
    JSR EjectFromTopWall

    JMP EntityComplete
PlayerJumpReleased:
    JMP EntityComplete
PlayerHeadbonk:
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ;jump velocity handling
    LDY playerjumptrack
    LDA HeadbonkStrength, Y
    CMP #$01
    BEQ :+
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X
    INC playerjumptrack
    JMP :++
    :
    LDA #$06
    STA entities+Entity::generalpurpose, X
    :
    JSR PlayerApplyVelocityX
    JSR EjectFromRightWall
    JSR EjectFromLeftWall
    JSR EjectFromTopWall
    JMP EntityComplete
PlayerFalling:
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    LDA vylow
    ADC #PLAYERFALLSPEED
    STA vylow
    LDA vyhigh
    ADC #$00
    STA vyhigh
    JSR PlayerApplyVelocityY
    ; check if we're on floor
    ; INC entities+Entity::ypos, X 
    JSR CollideDown2
    BEQ :+
        LDA #$01 
        STA entities+Entity::generalpurpose, X
        LDA #$00
        STA vylow
        STA vyhigh
        JSR EjectFromBottomWall
        ; dec entities+Entity::ypos, X 
        jmp EntityComplete
    :
    JSR PlayerApplyVelocityX
    JSR EjectFromRightWall
    JSR EjectFromLeftWall

    JMP EntityComplete
PlayerFallingThroughPlatform:
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; fall
    LDA vylow
    ADC #PLAYERFALLSPEED
    STA vylow
    LDA vyhigh
    ADC #$00
    STA vyhigh
    JSR PlayerApplyVelocityY
    JSR CollideDown2
    CMP #$04
    BEQ :+
        LDA #$06
        sta entities+Entity::generalpurpose, X
    :
    JSR PlayerApplyVelocityX
    JSR EjectFromRightWall
    JSR EjectFromLeftWall
    jmp EntityComplete
PlayerDisabled:
    JMP EntityComplete


PlayerApplyFriction:
    LDA vxhigh
    BEQ PlayerApplyFrictionLessThanZero
    LDA vxhigh
    BMI ApplyFrictionPositive

    ApplyFrictionNegative:
    LDA vxlow
    SEC 
    SBC #FRICTION
    STA vxlow
    BCC :+ ; if minus, add the carry and reset vx to 128 
    LDA vxhigh
    SBC #$00 
    STA vxhigh
    LDA #127 
    STA vxlow
    :
RTS 

    ApplyFrictionPositive: ;
    LDA vxlow
    CLC 
    ADC #FRICTION
    STA vxlow
    BCC :+ ; if we go over 0, add the carry and reset vx to -128
    LDA vxhigh
    ADC #$00
    STA vxhigh
    LDA #128
    STA vxlow
    :
RTS

PlayerApplyFrictionLessThanZero:
    LDA vxlow
    BPL :+
    ; if number is negative, increase it
    CLC 
    ADC #$10
    STA vxlow
    RTS 
    :
    ; If postive, reduce number
    SEC
    SBC #$10
    STA vxlow
    BCS :+ 
    LDA #$00 
    STA vxlow
    :
RTS 

PlayerApplyVelocityX:
    LDA vxlow
    CLC 
    ADC entities+Entity::xpossub, X 
    STA entities+Entity::xpossub, X 
    LDA entities+Entity::xpos, X 
    ADC vxhigh
    STA entities+Entity::xpos, X
RTS

PlayerApplyVelocityY:
    LDA vylow
    CLC 
    ADC entities+Entity::ypossub, X 
    STA entities+Entity::ypossub, X 
    LDA entities+Entity::ypos, X 
    ADC vyhigh
    STA entities+Entity::ypos, X
RTS


PlayerXMovement:
    LDA vxhigh
    ; BEQ EndXMovement 
    BIT vxhigh
    BMI LimitXLeft

    LimitXRight:
        JSR ApplyFriction
        LDA vxhigh
        CMP #$01
        BCC :+ 
        LDA #$01
        STA vxhigh
        LDA #$00
        STA vxlow
        :
        JMP AddVx
    LimitXLeft:
        JSR ApplyFriction
        BCS AddVx
        LDA vxhigh
        CMP #$FF
        BCS :+ 
        LDA #$FF 
        STA vxhigh
        LDA #$00 
        STA vxlow
        :
    AddVx:
        LDA entities+Entity::xpossub, X
        CLC 
        ADC vxlow
        STA entities+Entity::xpossub, X
        LDA entities+Entity::xpos, X 
        ADC #$00
        ADC vxhigh
        STA entities+Entity::xpos, X 
EndXMovement:
RTS

ApplyFriction:
    LDA vxhigh
    BEQ FrictionLessThanZero
    BMI FrictionP

    FrictionN:
    LDA vxlow
    SEC 
    SBC #$20 
    STA vxlow
    LDA vxhigh
    SBC #$00 
    STA vxhigh
RTS 

    FrictionP:
    LDA vxlow
    CLC 
    ADC #$20 
    STA vxlow
    LDA vxhigh
    ADC #$00
    STA vxhigh
RTS

FrictionLessThanZero: 
    LDA vxlow
    SEC 
    SBC #$20
    STA vxlow
    BCS :+ 
    LDA #$00 
    STA vxlow
    :
    RTS 


PlayerApplyFrictionq:
    LDA temp2
    BNE :+
    RTS
    :
    LDA vxhigh
    BEQ FrictionReduceSubOnly
    BIT vxhigh
    BMI ApplyFrictionLeft

    ApplyFrictionRight:
        LDA vxlow
        SEC 
        SBC #$40
        STA vxlow
        LDA vxhigh
        SBC #$00
        STA vxhigh
        JMP EndPlayerApplyFriction
    ApplyFrictionLeft:
        LDA vxlow
        CLC 
        ADC #$40
        STA vxlow
        LDA vxhigh
        ADC #$00
        STA vxhigh
    EndPlayerApplyFriction:
    RTS
    FrictionReduceSubOnly:
    LDA temp2
    BEQ :+
    LDA vxlow
    SEC
    SBC #$40
    BCC :+
    LDA #$00
    STA vxlow
    :
    RTS
InitAnimation:
    STA animationtrack
 ; animation number in A 
    ASL 
    TAY 
    LDA AnimationStringsPlayer, Y 
    STA jumppointer
    LDA AnimationStringsPlayer+1,Y 
    STA jumppointer+1
    LDY #$00
    LDA (jumppointer), Y
    STA entities+Entity::animationframe, X 
    INY 
    LDA (jumppointer), Y 
    STA entities+Entity::animationtimer, X
    LDA #$00
    STA animationtracktimer

RTS 

AdvancePlayerAnimation:
    DEC entities+Entity::animationtimer, X 
    BEQ :+
    RTS 
    :

    LDA animationtrack
    ASL 
    TAY 
    LDA AnimationStringsPlayer, Y 
    STA jumppointer
    LDA AnimationStringsPlayer+1,Y 
    STA jumppointer+1


    LDA animationtracktimer
    TAY 
    LDA (jumppointer), Y
    CMP #$FF 
    BNE :+
        LDA #$00
        TAY 
        LDA (jumppointer), Y 
        STA entities+Entity::animationframe, X
        INY 
        LDA (jumppointer), Y 
        STA entities+Entity::animationtimer, X
        INY 
        TYA
        STA animationtracktimer
        RTS
    :
    CMP #$FE 
    BNE :+
        INY 
        LDA (jumppointer), Y 
        JSR InitAnimation
        RTS
    :
    STA entities+Entity::animationframe, X 
    INY 
    LDA (jumppointer), Y 
    STA entities+Entity::animationtimer, X
    INC animationtracktimer
    INC animationtracktimer
RTS 

PlayerAddSpeedList:
    .word DummyEntry
    .word AddSpeedP1
    .word AddSpeedP2
    .word AddSpeedP3
    .word AddSpeedP4

PlayerSubSpeedList:
    .word DummyEntry
    .word SubSpeedP1
    .word SubSpeedP2
    .word SubSpeedP3
    .word SubSpeedP4

PlayerAddSpeed:
    LDA entities+Entity::type, X 
    ASL 
    TAY 
    LDA PlayerAddSpeedList, Y 
    STA jumppointer
    LDA PlayerAddSpeedList+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine
    RTS

PlayerSubSpeed:
    LDA entities+Entity::type, X 
    ASL 
    TAY 
    LDA PlayerSubSpeedList, Y 
    STA jumppointer
    LDA PlayerSubSpeedList+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine
    RTS

AddSpeedP1:
    ; add to low vel and carry to high
    LDA #PLAYERACCELLERATION
    CLC 
    ADC vxlow
    STA vxlow
    BPL :+
    INC vxhigh
    LDA #00
    STA vxlow
    :
    ; cap speed
    LDA vxhigh
    CMP #$01
    BCC :+
    LDA #$01 
    STA vxhigh
    LDA #00
    STA vxlow
    :
RTS

AddSpeedP2:
    RTS
AddSpeedP3:
    RTS
AddSpeedP4:
    RTS

SubSpeedP1:
    ; add to low vel and carry to high
    LDA #PLAYERACCELLERATION
    SEC 
    SBC vxlow
    STA vxlow
    BPL :+
    DEC vxhigh
    LDA #00
    STA vxlow
    :
    ; cap speed
    LDA vxhigh
    CMP #$FF
    BCS :+
    LDA #$FF 
    STA vxhigh
    LDA #00
    STA vxlow
    :
RTS

SubSpeedP2:
    RTS
SubSpeedP3:
    RTS
SubSpeedP4:
    RTS

DummyEntry:
RTS

Player2OnFloor:
    LDA #$00
    STA temp2 ; use temp 2 to see if we've pressed l/r
    ; Check if we're shooting this frame 
    JSR CheckBP2
    ; if b, spawn fireball 
    CMP #ButtonReturn::Release
    BNE :+
        JSR PlayerAttemptSpawnFireball
    :
    ; Move down and eject from floor and eject if needed
    LDA vylowp2 ;load velocity subpixel
    CLC 
    ADC #$20 ; move subpixeldown
    STA vylowp2
    LDA vyhighp2
    ; Do NOT set carry, retain it to seeif subpixeloverflowered
    ADC #$00 ; if carry is set,this will add 1
    STA vyhighp2

    LDA vylowp2
    CLC 
    ; ADC yposlowp2
    ; STA yposlowp2
    LDA entities+Entity::ypos, X 
    ADC #$00
    ADC vyhighp2
    STA entities+Entity::ypos, X 

    JSR CollideDown2 ; eject fro floor
    BEQ :+ ; Skip ahead if we didn't hit the floor
    ; Kill velocity
        LDA #$00
        STA vyhighp2
        STA vylowp2
    ; Check for jump
        JSR CheckAP2
        CMP #ButtonReturn::Press
        BNE :+
            LDA #$02
            STA entities+Entity::generalpurpose, X
            LDA #$00
            STA entities+Entity::generalpurpose, X
            LDA #$05
            STA entities+Entity::animationframe, X   
    :

    ; check if right is pressed
    JSR CheckRightP2
    ; move right if pressed
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlowp2
        CLC 
        ADC #$50
        STA vxlowp2
        LDA vxhighp2
        ADC #$00
        STA vxhighp2
        ; Clamp veloctiy
        CMP #$01
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlowp2
        CMP #$30
        BCC :+
        LDA #$30
        STA vxlowp2
    :

    ;Check Left Cl
    JSR CheckLeftP2
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlowp2
        SEC 
        SBC #$70
        STA vxlowp2
        LDA vxhighp2
        SBC #$00
        STA vxhighp2
        ; Clamp veloctiy
        CMP #$FF
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlowp2
        CMP #$30
        BCS :+
        LDA #$30
        STA vxlowp2
    :

        ; Add to position
    ; JSR PlayerXMovement
        ; Eject from wall
    JSR CollideLeft2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhighp2
        STA vxlowp2
    :    
    JSR CollideRight2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhighp2
        STA vxlowp2
    :    



    Player2OnFloorAnimate:
    ; pick animation frame!
    ; default frame
    LDA #$00
    STA entities+Entity::animationframe, X 
    LDA vyhighp2
    BEQ :+
        LDA #$04
        STA entities+Entity::animationframe, X
        JMP EntityComplete
    :

    JMP EntityComplete 
Player2Jumping:
    JMP EntityComplete
Player2Headbonk:
    JMP EntityComplete

Player2JumpReleased:
    JMP EntityComplete

Player2Disabled:
    JMP EntityComplete
Player2Idle:
    JMP EntityComplete

;;;; PLAYER 3 
Player3OnFloor:
    JMP EntityComplete 
Player3Jumping:
    JMP EntityComplete
Player3Headbonk:
    JMP EntityComplete

Player3JumpReleased:
    JMP EntityComplete

Player3Disabled:
    JMP EntityComplete
Player3Idle:
    JMP EntityComplete

;;;; PLAYER 4 
Player4OnFloor:
    JMP EntityComplete 
Player4Jumping:
    JMP EntityComplete
Player4Headbonk:
    JMP EntityComplete
Player4JumpReleased:
    JMP EntityComplete

Player4Disabled:
    JMP EntityComplete
Player4Idle:
    JMP EntityComplete


PlayerAttack:
    LDA entities+Entity::type, X 
    ASL 
    TAY 
    LDA PlayerProjectileCheck, Y 
    STA jumppointer
    LDA PlayerProjectileCheck+1, Y 
    STA jumppointer+1 
    JMP (jumppointer)
    ; reenter at this point
    Endplayerprojectilecheck:
    ; firing allowed
    TXA  ; store entity pos
    PHA
    LDA entities+Entity::ypos, X 
    TAY  
    ; check player facing
    LDA entities+Entity::attributes, X
    STA temp
    LDA entities+Entity::collisionlayer, X
    STA temp2
    LDA temp 
    AND #%01000000
    BPL :+
    ; going left 
    LDA entities+Entity::xpos, X
    TAX 
    JMP :++
    :
    ;going right
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07
    TAX
    :
    LDA temp3
    JSR SpawnEntity
    PLA 
    TAX
    RTS 


PlayerProjectileCheck:
    .word PlayerAttack
    .word CheckP1ProjectileCount
    .word CheckP2ProjectileCount
    .word CheckP3ProjectileCount
    .word CheckP4ProjectileCount

CheckP1ProjectileCount:
    LDA projectilecooldownp1
    BEQ :+
        RTS 
    :
    LDA projectilecountp1
    CMP #PROJECTILEMAX
    BCC :+
        RTS
    :
    LDA WeaponTypeP1
    LDY #$00
    LDA PlayerWeapons, Y 
    STA temp3
    LDA #PROJECTILECOOLDOWN
    STA projectilecooldownp1
    INC projectilecountp1
    JMP Endplayerprojectilecheck 

CheckP2ProjectileCount:
    JMP Endplayerprojectilecheck 

CheckP3ProjectileCount:
    JMP Endplayerprojectilecheck 
CheckP4ProjectileCount:
    JMP Endplayerprojectilecheck 

PlayerWeapons:
    .byte $0B
    .byte $0B
    .byte $0B
    .byte $0B

PlayerAttemptSpawnFireball:
    LDA projectilecooldownp1 ; load cooldown
    BEQ :+ ; if timer is zero, continue, else return
        RTS 
    :
    LDA projectilecountp1 ; are there fewer projectiles than the max
    CMP #$03
    BCS PlayerEndSpawnFireball
    TXA 
    PHA 
    LDA entities+Entity::attributes, X 
    STA temp 
    LDA entities+Entity::collisionlayer, x
    STA temp2
    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01
    TAY 
    LDA entities+Entity::attributes, X
    AND #%01000000
    BPL :+
        LDA entities+Entity::xpos, X 
        TAX
        JMP :++
    : 
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07
    TAX 
    :
    LDA #EntityType::Fireball
    JSR SpawnEntity
    LDA #$50
    STA projectilecooldownp1
    PLA 
    TAX 
    INC projectilecountp1
    PlayerEndSpawnFireball:
    RTS 
    
ProjectileSpellMovingLeft:
    JSR CollideLeft2
    BEQ :+
    ; LDA #$02
    ; STA entities+Entity::generalpurpose, X
    ; LDA #$03
    ; STA entities+Entity::animationframe, X
    ; LDA #$10
    ; STA entities+Entity::animationtimer, X
    ; JMP EntityComplete
    ; :
    ; DEC entities+Entity::animationtimer, X
    ; LDA entities+Entity::animationtimer, X
    ; BNE :+
    ; LDA #$0A
    ; STA entities+Entity::animationtimer, X
    ; INC entities+Entity::animationframe, X
    ; LDA entities+Entity::animationframe, X
    ; CMP #$03
    ; BNE :+
    ; LDA #$00 
    ; STA entities+Entity::animationframe, X
    ; :
    LDA entities+Entity::xpos, X 
    SEC 
    SBC #$01
    STA entities+Entity::xpos, X
    JMP EntityComplete
ProjectileSpellMovingRight:
    JSR SpriteCollide
 
    BEQ :+
    ; If !0, we hit another sprite
    ; JSR AddEntityToDestructionStack
    ; TXA 
    ; JSR AddEntityToDestructionStack

    ; JSR CollideRight2
    ; BEQ :+
    ; LDA #$02
    ; STA entities+Entity::generalpurpose, X 
    ; JMP EntityComplete
    ; :
    ; DEC entities+Entity::animationtimer
    ; LDA entities+Entity::animationtimer
    ; BNE :+
    ; LDA #$10
    ; STA entities+Entity::animationtimer
    ; INC entities+Entity::animationframe
    ; LDA entities+Entity::animationframe
    ; CMP #$03
    ; BNE :+
    ; LDA #$00 
    ; STA entities+Entity::animationframe
    :
    LDA entities+Entity::xpos, X 
    CLC  
    ADC #$01
    STA entities+Entity::xpos, X
    LDA entities+Entity::xpos, X
    CMP #$FE 
    BCC :+
    LDA entities+Entity::ypos, X
    CLC 
    ADC #$04
    STA entities+Entity::ypos, X
    :
    
    JMP EntityComplete
ProjectileSpellDissipating:
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X
    CMP #$06
    BNE :+
    TXA
    JSR AddEntityToDestructionStack
    :
    JMP EntityComplete

LightningEmitterInit:
    LDA #%00000011 
    STA entities+Entity::attributes, X 
    LDA #$78
    STA entities+Entity::animationtimer, X
    LDA #$01 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

LightningEmitterWaitingForSparks: ; TODO rewrite following non branching path (1 cycle each)
    DEC entities+Entity::animationtimer, X  
    BNE :+
    TXA 
    PHA 
    LDA #$02 
    STA entities+Entity::generalpurpose, X 
    LDA #$78
    STA entities+Entity::animationtimer, X 
    LDA entities+Entity::ypos, X
    CLC 
    ADC #$08
    TAY 
    LDA entities+Entity::xpos, X
    TAX  
    LDA #EntityType::Sparks
    JSR SpawnEntity
    PLA 
    TAX 

    :
    JMP EntityComplete

LightningEmitterWaitingForLightning:
    DEC entities+Entity::animationtimer, X 
    BNE :+
    TXA 
    PHA 
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    LDA #$78
    STA entities+Entity::animationtimer, X 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$08
    TAY 
    LDA entities+Entity::xpos, X
    TAX
    LDA #EntityType::Lightning
    JSR SpawnEntity
    PLA 
    TAX 
    :
    JMP EntityComplete

LightningInit:
    LDA #%00000011 
    STA entities+Entity::attributes, X 
    LDA #$0A
    STA entities+Entity::animationtimer, X
    LDA #%00000001
    STA entities+Entity::generalpurpose, X
    STA entities+Entity::collisionlayer, X
    LDA #$00
    STA entities+Entity::animationframe, X  
    JMP EntityComplete

LightningExtending:
    DEC entities+Entity::animationtimer, X
    BMI :+ 
    JMP EntityComplete
    : 
    INC entities+Entity::animationframe, X 
    LDA entities+Entity::animationframe, x
    CMP #$03 
    BEQ :+
    LDA #$04
    STA entities+Entity::animationtimer, X 
    JMP EntityComplete
    :
    LDA #$03 
    STA entities+Entity::animationframe, X
    STA entities+Entity::animationtimer, X
    LDA #$02 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

LightningFlashing:
    JSR SpriteCollide
    CMP #$FF
    BEQ :+
    JSR AddEntityToDestructionStack
    :
    DEC entities+Entity::animationtimer, X
    BPL EndLightningFlashing
    LDA #$0A 
    STA entities+Entity::animationtimer, X
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X
    CMP #$06 
    BNE EndLightningFlashing
    TXA 
    JSR AddEntityToDestructionStack
EndLightningFlashing:
    JMP EntityComplete

SparkInit:
    LDA #$01 
    STA entities+Entity::generalpurpose, X
SparkSpark:
        DEC entities+Entity::animationtimer, X
        LDA entities+Entity::animationtimer, X
        BPL :+
        LDA #$02
        STA entities+Entity::animationtimer, X
        LDA #$01 
        EOR entities+Entity::animationframe, X
        STA entities+Entity::animationframe, X
        LDA framecount
        BNE :+
        TXA 
        JSR AddEntityToDestructionStack
        :
    JMP EntityComplete




SliderInit:
    LDA #%00000001
    STA entities+Entity::attributes, X 
    LDA #$08
    STA entities+Entity::animationtimer, X
    LDA #$01 
    LDA #%00000010
    STA entities+Entity::collisionlayer, X 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

SliderRight: 
    ; Animate
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01
    EOR entities+ Entity::animationframe, X
    STA entities+ Entity::animationframe, X
    LDA #$0A 
    STA entities+ Entity::animationtimer, X
    :
    ; Move 
    INC entities+Entity::xpos, X
    JSR CollideRight2
    BEQ :+ 
    LDA #$02
    STA entities+Entity::generalpurpose, X
    LDA entities+Entity::attributes, X
    EOR #%01000000
    STA entities+Entity::attributes, X 
    JMP EntityComplete
    : 
    INC entities+Entity::ypos, X
    JSR CollideDown2
    JMP EntityComplete
SliderLeft:
    ;Animate 
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01
    EOR entities+ Entity::animationframe, X
    STA entities+ Entity::animationframe, X
    LDA #$0A 
    STA entities+ Entity::animationtimer, X
    :
    DEC entities+Entity::xpos, X
    JSR CollideLeft2
    BEQ :+ 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    LDA entities+Entity::attributes, X
    EOR #%01000000
    STA entities+Entity::attributes, X 
    JMP EntityComplete
    : 
    INC entities+Entity::ypos, X
    JSR CollideDown2
    ; BNE :+
    ; INC entities+Entity::ypos, X 
    ; :
    JMP EntityComplete

ExplosionInit:
    LDA #%00000000
    STA entities+Entity::attributes, X 
    LDA #$08
    STA entities+Entity::animationtimer, X
    LDA #$01 
    LDA #%00000000
    STA entities+Entity::collisionlayer
    LDA #$01 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
ExplosionExplode:
    DEC entities+Entity::animationtimer, X
    LDA entities+Entity::animationtimer, X
    BNE :+
    LDA #$05 
    STA entities+Entity::animationtimer, X
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X
    CMP #$04 
    BNE :+
    TXA 
    JSR AddEntityToDestructionStack
    :
    JMP EntityComplete

FireballInit:
    LDA #$08 
    STA entities+Entity::animationtimer, X
    LDA entities+Entity::collisionlayer,X
    EOR #$FF 
    AND #%00001111
    STA entities+Entity::collisionlayer, X
    LDA entities+Entity::attributes, X
    STA temp 
    LDA #%01000000 
    BIT temp
    BNE :+
    ; if 0, it is facing left and needs to go left
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
    :
    LDA #$02 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
FireballMoveLeft:
    JSR CollideLeft2
    BEQ :+
    TXA  
    JSR AddEntityToDestructionStack
    DEC projectilecountp1
    JMP EntityComplete
    :
    LDA entities+Entity::xpos, X
    SEC
    SBC #$02
    STA entities+Entity::xpos, X
    BCC :+
    TXA
    JSR AddEntityToDestructionStack
    DEC projectilecountp1
    : 
    JMP EntityComplete
FireballMoveRight:
    ; animate 
    LDA #$01
    BIT framecount
    BNE :++
    LDA entities+Entity::animationframe, X
    CLC 
    ADC #$01 
    CMP #$04
    BNE :+
    LDA #$00
    :
    STA entities+Entity::animationframe, x
    :
    ; ;Check for other sprites
    JSR SpriteCollide
    CMP #$FF
    BEQ :+
    JSR AddEntityToDestructionStack
    TXA 
    JSR AddEntityToDestructionStack
    DEC projectilecountp1
    JMP EntityComplete
    :

    ;Check for tiles
    JSR CollideRight2
    BEQ :+
    TXA  
    JSR AddEntityToDestructionStack
    DEC projectilecountp1
    JMP EntityComplete
    :
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01
    STA entities+Entity::xpos, X
    ; Destroy if off edge
    BCC :+
    TXA
    JSR AddEntityToDestructionStack
    DEC projectilecountp1
    : 
    JMP EntityComplete

RespawnerInit:
    LDA #$16
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    STA entities+Entity::attributes, X
    STA entities+Entity::animationframe
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

RespawnerMoveToSpawn:
    ; animate 
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01 
    EOR entities+Entity::animationframe, X 
    STA entities+Entity::animationframe, X 
    LDA #$16 
    STA entities+Entity::animationtimer, X 

    :
    ;move
    LDA entities+Entity::xpos, X 
    ; CMP #Spawnlocation::Spawn1X
    BEQ RespawnerEndXMove
    BCS :+
    INC entities+Entity::xpos, X 
    JMP RespawnerEndXMove
    :
    DEC entities+Entity::xpos, X 
    RespawnerEndXMove:

    LDA entities+Entity::ypos, X 
    ; CMP #Spawnlocation::Spawn1Y
    BEQ RespawnerEndYMove
    BCS :+
    INC entities+Entity::ypos, X 
    JMP RespawnerEndYMove
    :
    DEC entities+Entity::ypos, X 
    RespawnerEndYMove:
    
    LDA entities+Entity::ypos, X
    ; CMP #Spawnlocation::Spawn1Y
    BNE EndRespawnerMoveToSpawn 
    LDA entities+Entity::xpos, X 
    ; CMP #Spawnlocation::Spawn1X
    BNE EndRespawnerMoveToSpawn
    ; At spawnpoint, create player 
    TXA 
    PHA 
    LDY entities+Entity::ypos, X 
    LDA entities+Entity::xpos, X 
    TAX 
    LDA #EntityType::Player
    JSR SpawnEntity
    PLA 
    TAX 

    ; check which direction to leave the screen in
    CMP #$40
    BCC :+    
    LDA #$02
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete
    : 
    LDA #$03
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete
    EndRespawnerMoveToSpawn:
    JMP EntityComplete

RespawnerMoveLeft:
    ; animate 
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01 
    EOR entities+Entity::animationframe, X 
    STA entities+Entity::animationframe, X 
    LDA #$16 
    STA entities+Entity::animationtimer, X 
    :
    ;move
    LDA entities+Entity::xpos, X 
    SEC  
    SBC #$02
    STA entities+Entity::xpos, X 
    BCS :+
    TXA 
    JSR AddEntityToDestructionStack
    :
    JMP EntityComplete

RespawnerMoveRight:
    ; animate 
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01 
    EOR entities+Entity::animationframe, X 
    STA entities+Entity::animationframe, X 
    LDA #$16 
    STA entities+Entity::animationtimer, X 
    :
    ;move
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$03 
    STA entities+Entity::xpos, X 
    BCC :+
    TXA 
    JSR AddEntityToDestructionStack
    :
    JMP EntityComplete

PortalInit: ; spawns player 2 
    ; LDA #Spawnlocation::Spawn2X
    STA entities+Entity::xpos, X 
    ; LDA #Spawnlocation::Spawn2Y
    STA entities+Entity::ypos, X
    LDA #$00
    STA entities+Entity::animationtimer, X 
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA entities+Entity::attributes, X
    LDA #$00
    STA entities+Entity::animationframe, X 
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
PortalExpand:
    LDY entities+Entity::animationtimer, X 
    LDA AnimationPortalExpand, Y
    BMI :+
    STA entities+Entity::animationframe, X 
    INC entities+Entity::animationtimer, X
    JMP EntityComplete
    :
    LDA #$00
    STA entities+Entity::animationtimer, X 
    LDA #$02 
    STA entities+Entity::generalpurpose, X
    ; spawn player 3
    TXA 
    PHA 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$04
    TAY 
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$04
    TAX 
    LDA #EntityType::Player2 
    JSR SpawnEntity
    PLA 
    TAX 

    JMP EntityComplete
PortalWait:
    LDY entities+Entity::animationtimer, X 
    LDA AnimationPortalWait, Y
    BMI :+
    STA entities+Entity::animationframe, X 
    INC entities+Entity::animationtimer, X
    JMP EntityComplete
    :
    LDA #$03 
    STA entities+Entity::generalpurpose, X 
    LDA #$00
    STA entities+Entity::animationtimer, X
    JMP EntityComplete
PortalContract:
    LDY entities+Entity::animationtimer, X 
    LDA AnimationPortalContract, Y
    BMI :+
    STA entities+Entity::animationframe, X 
    INC entities+Entity::animationtimer, X
    JMP EntityComplete
    :
DestroyPortal:
    TXA 
    JSR AddEntityToDestructionStack
    JMP EntityComplete

BroomStickInit:; spawns player 3
    LDA #$16
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$02
    STA entities+Entity::attributes, X
    LDA #$00
    STA entities+Entity::animationframe
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
BroomStickMoveToSpawn:
    LDA #$02
    STA temp
    LDA entities+Entity::xpos, X 
    ; CMP #Spawnlocation::Spawn3X
    BEQ :+
    CLC 
    ADC #$02 
    STA entities+Entity::xpos, X 
    INC temp
    :
    DEC temp
    LDA entities+ Entity::ypos,x
    ; CMP #Spawnlocation::Spawn3Y
    BEQ :+
    CLC 
    ADC #$02 
    STA entities+Entity::ypos, X 
    INC temp
    :
    DEC temp
    ;check for spawn point reached
    LDA temp
    BEQ :+
    JMP EntityComplete
    :
    LDA #$02 
    STA entities+Entity::generalpurpose, X 
    LDA #$00 
    STA entities+Entity::animationtimer

    JMP EntityComplete
BroomStickLeaveScreen:
    LDA entities+Entity::animationtimer, X
    CMP #$0A
    BNE :+
    ;spawn player 
    TXA 
    PHA 
    LDY entities+Entity::ypos, X
    LDA entities+Entity::xpos, X 
    ADC #$04
    TAX 
    LDA #EntityType::Player3
    JSR SpawnEntity
    PLA
    TAX 
    :
    LDY entities+Entity::animationtimer, X
    LDA SpawnerSpeedRamp, Y
    CMP #$FF
    BEQ :+
    CLC 
    ADC entities+Entity::xpos,X
    STA entities+Entity::xpos,X
    INC entities+Entity::animationtimer, X
    JMP EntityComplete
    :
    LDA #$02 
    CLC 
    ADC entities+Entity::xpos, X
    CMP #$F0 
    BEQ :+
    STA entities+Entity::xpos, X
    JMP EntityComplete
    :
    TXA 
    JSR AddEntityToDestructionStack
    JMP EntityComplete

IceBeamInit:; spawns player 3
    LDA #$08
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$00
    STA entities+Entity::animationframe
    LDA entities+Entity::attributes, X
    STA temp
    LDA #%01000000
    BIT temp
    BNE :+
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
    :
    LDA #$02
    STA entities+Entity::generalpurpose, X  
    JMP EntityComplete
IceBeamLeft:
    DEC entities+Entity::xpos, X 
    JSR CollideLeft2
    BEQ :+
    LDA #$03
    STA entities+Entity::generalpurpose
    :
    ;animate 
    DEC entities+Entity::animationtimer
    BEQ :+
        JMP EntityComplete
    :
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X
    CMP #$92
    BNE :+
        LDA #$90
        STA entities+Entity::animationframe, X
    :
    LDA #$10
    STA entities+Entity::animationtimer, X
    JMP EntityComplete
IceBeamRight:
    ;animate 
    DEC entities+Entity::animationtimer, X
    BEQ :+
        JMP EndIceBeamRightAnimation
    :
    LDA #$08
    STA entities+Entity::animationtimer, x
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X
    CMP #$02
    BNE :+
        LDA #$00
        STA entities+Entity::animationframe, X
    :
    EndIceBeamRightAnimation:
    INC entities+Entity::xpos, X 
    JSR CollideRight2
    BEQ :+
    LDA #$03
    STA entities+Entity::generalpurpose, X
    LDA #$08
    STA entities+Entity::animationtimer, X
    LDA #$02
    STA entities+Entity::animationframe,X
    :
    JMP EntityComplete
IceBeamFormCrystal:
    DEC entities+Entity::animationtimer, X 
    BEQ :+
        JMP EntityComplete
    :
    LDA #10
    STA entities+Entity::animationtimer, X 
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X 
    CMP #$05
    BNE :+
    TXA 
    JSR AddEntityToDestructionStack
    :
    JMP EntityComplete

TeleportInit:
    LDA #%00000000
    STA entities+Entity::attributes, X
    LDA #$02
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$00
    STA entities+Entity::animationframe, X
    LDA #$01
    STA entities+Entity::generalpurpose, X
    ; Eject from floor/wall, just in case.
    JSR CollideLeft2 
    JMP EntityComplete
TeleportInitP2:
    LDA #%00000001
    STA entities+Entity::attributes, X
    LDA #$02
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$00
    STA entities+Entity::animationframe, X
    LDA #$01
    STA entities+Entity::generalpurpose, X
    ; Eject from floor/wall, just in case.
    JSR CollideDown2
    JSR CollideLeft2 
    JMP EntityComplete
TeleportInitP3:
    LDA #%00000010
    STA entities+Entity::attributes, X
    LDA #$02
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$00
    STA entities+Entity::animationframe, X
    LDA #$01
    STA entities+Entity::generalpurpose, X
    ; Eject from floor/wall, just in case.
    JSR CollideDown2
    JSR CollideLeft2 
    JMP EntityComplete
TeleportInitP4:
    LDA #%00000011
    STA entities+Entity::attributes, X
    LDA #$02
    STA entities+Entity::animationtimer, X 
    LDA #$00
    STA entities+Entity::collisionlayer, X 
    LDA #$00
    STA entities+Entity::animationframe, X
    LDA #$01
    STA entities+Entity::generalpurpose, X
    ; Eject from floor/wall, just in case.
    JSR CollideDown2
    JSR CollideLeft2 
    JMP EntityComplete
TeleportRaise:
    DEC entities+Entity::animationtimer, X
    BEQ :+
    JMP EntityComplete
    :
    LDA #$02
    STA entities+Entity::animationtimer, X
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X 
    CMP #$09
    BEQ :+
    JMP EntityComplete
    :
    DEC entities+Entity::animationframe,X
    LDA #$02 
    STA entities+Entity::generalpurpose, X
    LDA #$20
    STA entities+Entity::animationtimer, X
    JMP EntityComplete
TeleportLower:
    DEC entities+Entity::animationtimer, X 
    BEQ :+
    JMP EntityComplete
    :
    LDA #$02
    STA entities+Entity::animationtimer, X
    DEC entities+Entity::animationframe, X 
    BMI :+
    JMP EntityComplete
    :
    TXA 
    JSR AddEntityToDestructionStack
    JMP EntityComplete
TeleportSpawnP1:
    TXA 
    PHA
    LDY entities+Entity::ypos, X
    LDA entities+Entity::xpos, X 
    TAX 
    LDA #EntityType::Player
    JSR SpawnEntity
    PLA 
    TAX
    LDA #$03 
    STA entities+Entity::generalpurpose,X
    JMP EntityComplete
TeleportSpawnP2:
    TXA 
    PHA
    LDY entities+Entity::ypos, X
    LDA entities+Entity::xpos, X 
    TAX 
    LDA #EntityType::Player2
    JSR SpawnEntity
    PLA 
    TAX
    LDA #$03 
    STA entities+Entity::generalpurpose,X
    JMP EntityComplete
TeleportSpawnP3:
    TXA 
    PHA
    LDY entities+Entity::ypos, X
    LDA entities+Entity::xpos, X 
    TAX 
    LDA #EntityType::Player3
    JSR SpawnEntity
    PLA 
    TAX
    LDA #$03 
    STA entities+Entity::generalpurpose,X
    JMP EntityComplete
TeleportSpawnP4:
    TXA 
    PHA
    LDY entities+Entity::ypos, X
    LDA entities+Entity::xpos, X 
    SEC 
    SBC #$10
    TAX 
    LDA #EntityType::Player4
    JSR SpawnEntity
    PLA 
    TAX
    LDA #$03 
    STA entities+Entity::generalpurpose,X
    JMP EntityComplete

HatInit:
    LDA #$00
    STA entities+Entity::attributes, X
    LDA #$01
    STA entities+Entity::generalpurpose, X
    LDA #%00001111
    sta entities+Entity::collisionlayer, X
    JMP EntityComplete
HatWaiting:
    ;check for player collision
    JSR SpriteCollide
    CMP #$FF
    BEQ :+++
    STX temp
    TAX 
    LDA entities+Entity::type, X 
    CMP #EntityType::Player
    BNE :+
    LDX temp 
    LDA #$02
    STA entities+Entity::generalpurpose, X
    :
    CMP #EntityType::Player2 
    BNE :+
    LDX temp 
    LDA #$03
    STA entities+Entity::generalpurpose, X
    LDA #$00
    STA entities+Entity::collisionlayer, X
    JMP EntityComplete
    :
    STA entities+Entity::generalpurpose, X 
    :
    JMP EntityComplete

HatAttachedToP1:
    ; FIND p1 and move to them
    TXA 
    TAY
    LDA #EntityType::Player
    JSR EntityFindEntity
    CMP #$FF
    BNE :+
    LDA #$01 
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete 
    :
    LDA entities+Entity::xpos, Y 
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y
    SEC 
    SBC #$05 
    STA entities+Entity::ypos, X
    ;add a point to p1 once a second
    LDA framecount
    BNE :+
    JSR SubScoreP1
    :
    JMP EntityComplete

HatAttachedToP2:
    ; FIND p2 and move to them
    TXA 
    TAY
    LDA #EntityType::Player2
    JSR EntityFindEntity
    CMP #$FF
    BNE :+
    LDA #$01 
    STA entities+Entity::generalpurpose, X
    LDA rng 
    STA entities+Entity::xpos, X 
    STA entities+Entity::ypos, X 
        JMP EntityComplete 
    :
    LDA entities+Entity::xpos, Y 
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos, Y
    SEC 
    SBC #$06
    STA entities+Entity::ypos, X
    ;add a point to p1 once a second
    LDA framecount
    BNE :+
    JSR SubScoreP2
    :
    JMP EntityComplete
HatAttachedToP3:
    JMP EntityComplete
HatAttachedToP4:
    jmp EntityComplete


HatKnockedOff:
    JMP EntityComplete

ScoreTextp1Init:
    LDA #$00
    STA entities+Entity::attributes, X 
    JMP EntityComplete

ScoreNumberp1Init:
    LDA #$00
    STA entities+Entity::attributes, X
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

ScoreNumberp1SetScore:
    LDA ScoreP1 
    STA entities+Entity::animationframe, X 
    JMP EntityComplete

ScoreNumberp2SetScore:
    LDA ScoreP2 
    STA entities+Entity::animationframe, X 
    JMP EntityComplete


ScoreNumberDigit2p1Init:
    LDA #$00
    STA entities+Entity::attributes, X
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
ScoreNumberDigit2p1SetScore:
    LDA ScoreP1+1 
    STA entities+Entity::animationframe, X 
    JMP EntityComplete

ScoreNumberDigit2p2Init:
    LDA #$00
    STA entities+Entity::attributes, X
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
ScoreNumberDigit2p2SetScore:
    LDA ScoreP2+1 
    STA entities+Entity::animationframe, X 
    JMP EntityComplete

VerticalLaserInit:
    LDA #$01
    STA entities+Entity::generalpurpose, x
    LDA #10
    STA entities+Entity::animationtimer,X
    JMP EntityComplete

VerticalLaserNormal:
    DEC entities+Entity::animationtimer, X
    BNE :++
    INC entities+Entity::animationframe, X
    LDA entities+Entity::animationframe, X 
    CMP #3
    BCC :+
    LDA #$00
    STA entities+Entity::animationframe, X
    :
    LDA #5
    STA entities+Entity::animationtimer, X
    :
JMP EntityComplete

ClearEntity:
    ; wasteful but safer to clear all and not just type
    LDA #$00 
    STA entities+Entity::type, X
    STA entities+Entity::xpos, X
    STA entities+Entity::ypos, X
    STA entities+Entity::attributes, X
    STA entities+Entity::collisionlayer, X
    STA entities+Entity::generalpurpose, X
    STA entities+Entity::animationframe, X
    STA entities+Entity::animationtimer, X
    JMP EntityComplete

; finds the first entity of any type
; 
; a = type of entity you want to find
; y = your own index value to avoid finding yourself
; destroys a
; returns index in Y
EntityFindEntity:
    STA temp
    TXA 
    PHA
    STY temp2 
    LDX #$00
    EntityFindEntityLoop:
        CPX temp2
        BEQ :+
        LDA entities+Entity::type, X 
        CMP temp
        BEQ :+++
    :
    TXA
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #entity_mem
    BEQ :+
    JMP EntityFindEntityLoop
    :
    PLA 
    TAX
    LDA #$FF 
    RTS
    :
    TXA 
    TAY
    PLA 
    TAX 
RTS 
    
UpdateConveyorBelts:
    LDA #$01
    BIT framecount
    BNE :+
    RTS
    :
    LDX #$00
    ConveyorLoop:
    INC ConveyorBeltBuffer1, X 
    LDA ConveyorBeltBuffer1, X
    CMP #$E8 
    BCC :+
    LDA #$E0
    :
    STA ConveyorBeltBuffer1, X 
    INX 
    CPX #$08 
    BNE ConveyorLoop  

    LDX #$00
    ConveyorLoop2:
    DEC ConveyorBeltBuffer2, X 
    LDA ConveyorBeltBuffer2, X
    CMP #$E0 
    BCS :+
    LDA #$E7
    :
    STA ConveyorBeltBuffer2, X 
    INX
    CPX #$08 
    BNE ConveyorLoop2  

RTS

UpdateWaterfalls:
    ; LDA #%00000001
    ; BIT framecount
    ; BNE :+
    ; RTS
    ; :
    LDX #$00
    WaterfallLoop:
    LDA WaterfallBuffer1, X
    CLC 
    ADC #$10
    CMP #$EC 
    BNE :+
    LDA #$AE 
    :
    CMP #$EE 
    BNE :+
    LDA #$AC
    :
    STA WaterfallBuffer1, X
    INX 
    CPX #$08
    BNE WaterfallLoop

    LDX #$00
    WaterfallLoop2:
    LDA WaterfallBuffer2, X
    CLC 
    ADC #$10
    CMP #$ED
    BNE :+
    LDA #$AF 
    :
    CMP #$EF 
    BNE :+
    LDA #$AD
    :
    STA WaterfallBuffer2, X
    INX 
    CPX #$08
    BNE WaterfallLoop2
RTS

; BEFORE CALL:
;  - load screen data address into 'world'
;  - load collision data addess into currentcollisionaddress 
LoadSingleScreen:
    SEI ; disable all interuppts 
    LDA #%00010000
    STA PPUControl

    LDA #$00000000 ; disable all rendering
    STA PPUControl


    LDA #$00
    STA currentrowodd
    STA currentroweven

    LDA #$00 
    LDY #$00
    LDX #$00

    LoadScreenLoop:
        LDX #$10
        LoadOddRow:
            LDY currentrowodd
            LDA (world), Y ; get a meta tile
            ASL
            TAY ; move metatile ref to Y
            LDA  MetaTileList, Y
            STA jumppointer
            LDA MetaTileList+1, Y ; get the whole address of the data for that tile
            STA jumppointer+1
            LDY #$00 
            ; the first two bytes are uploaded directly to the ppu, as they are on the same row
            LDA (jumppointer), Y 
            STA PPUData
            INY 
            LDA (jumppointer), Y 
            STA PPUData
            INC currentrowodd
            DEX 
            BNE LoadOddRow ; break this loop when we've done one row

        LDY #$00

        LoadEvenRow:
            LDX #$00 
            LoadEvenRowLoop:
                LDY currentroweven
                LDA (world), Y
                ASL
                TAY 
                LDA MetaTileList, Y
                STA jumppointer
                LDA MetaTileList+1, Y
                STA jumppointer+1 
                LDY #$02

                LDA (jumppointer), Y 
                STA PPUData
                INY 
                LDA (jumppointer), Y 
                STA PPUData
                INC currentroweven

                INY ; 5th byte is collision data 
                LDA (jumppointer), Y
                PHA 
                TXA 
                TAY
                PLA 
                STA (currentcollisionaddress), Y

                INX 
                CPX #$10
                BNE LoadEvenRowLoop

        LDA currentcollisionaddress
        CLC 
        ADC #$10
        STA currentcollisionaddress
        LDA currentcollisionaddress+1
        ADC #$00 
        STA currentcollisionaddress+1

        LDA currentroweven
        CLC 
        ADC $10
        STA currentrowodd
        STA currentroweven
        CMP #$F0 
        BNE LoadScreenLoop

    FillCollisionMap:
        LDY #$00 
        FillCollisionMapLoop:
        LDA (world), Y 
        STA CollisionMap, Y 
        INY 
        TYA 
        CMP #$F0 
        BEQ :+
        JMP FillCollisionMapLoop
        :
 

EndScreenLoad:
LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
STA PPUControl
; Enabling sprites and background for left-most 8 pixels
; Enable sprites and background
LDA #%00111110
STA PPUMask
CLI
RTS

;Screen data in world/world+1
LoadCollisionData:
    LDY #$00
    LoadCollisionLoop:
        LDA (world), Y
        ; LDA #$01
        STA CollisionMap, Y
        INY
        TYA
        CMP #$F0
        BEQ :+
        JMP LoadCollisionLoop
        :
RTS

; X, Y and type in X, Y , A
AddEntityToStack:
    PHA 
    TXA 
    PHA 
    TYA 
    PHA 
    LDY SpawnerIndex
    CMP #$3C 
    BEQ SpawnImmediately
    PLA 
    STA SpawnerStack, Y
    INY 
    PLA 
    STA SpawnerStack, Y 
    INY
    STY SpawnerIndex
    RTS  


SpawnImmediately:
    PLA 
    TAY 
    PLA 
    TAX 
    PLA 
    JSR SpawnEntity
    RTS 

; X, Y and type in X,Y,A 
; temp = attributes
; temp2 = collision
SpawnEntity:
    PHA 
    TXA 
    PHA 
    TYA 
    PHA ; put ent type, x, y on stack
    LDX #$00
    EurydiceLoop:
        CPX #entity_mem
        BEQ EndEurydiceSpawn

        LDA entities+Entity::type, X ; grab the next entity 
        CMP #EntityType::NoEntity ; is the slot empty?
        BEQ AddEurydice ; if empty, branch
        TXA ; else add an offset then check for the next entity slot
        CLC
        ADC #.sizeof(Entity)
        TAX 
        JMP EurydiceLoop
    AddEurydice: 
        PLA; grab y pos we set before jumping here
        STA entities+Entity::ypos, X
        PLA ; grab x pos 
        STA entities+Entity::xpos, X
        PLA 
        STA entities+Entity::type, X
        LDA temp 
        STA entities+Entity::attributes, X 
        LDA temp2
        STA entities+Entity::collisionlayer, X
        ; some entities will override temp in their init, but there are circumstances where you want e.g. the attributes of the entity that spawned this one
        LDA #$00
        STA entities+Entity::generalpurpose, X 
        STA entities+Entity::animationtimer, X 
        ; STA entities+Entity::collisionlayer, X 
        RTS 
EndEurydiceSpawn:
    PLA 
    PLA 
    PLA
    RTS

IncFrameCount:
    INC framecount
    LDA framecount
    CMP #$3C
    BNE :+
    LDA #$00
    :
    STA framecount
RTS 

SetBank:
RTS 


; List of entity data0
PlayerData:
    .byte EntityType::Player ; entity type 
    .byte $00 ; sprite number (on chr bank sheet) 
    .byte $00 ; palette to use 0/1/2/3
SliderData:
    .byte EntityType::Slider
    .byte $01
    .byte $01
;;;;;;;;;;;;;
;Input 
;;;;;;;;;;;;;
ReadButtons:
    ; P  ing $4016 with a 1 to get it ready to send buttons 
    LDA #$01
    STA $4016
    STA buttons        ; Put 1 into buttons so we can use it to manipulate the carry flag in 8 loops time
    LSR A ; sets the carry
    STA $4016

    ;TODO make this a loop by slotting a one back into the carry flag after the 8th loop
    ReadButtonLoop:
        LDA $4016
        ROR 
        ROL buttons ; ror + rol moves the 
    
        BCC ReadButtonLoop ; after 8 loops, the 1 will shift back into the carry and end the loop

    LDA #$01
    LSR 
    ; ReadButtonLoopP3:
    ;     LDA $4016
    ;     ROR 
    ;     ROL buttonsp3
    ;     BCC ReadButtonLoopP3

    ; LDA #$01
    ; STA $4017
    ; STA buttonsp2
    ; LSR
    ; STA $4017

    ReadButtonLoopP2:
        LDA $4017
        ROR 
        ROL buttonsp2
        BCC ReadButtonLoopP2
    LDA #$01
    LSR 
    ; ReadButtonLoopP4:
    ;     LDA $4017 
    ;     ROR 
    ;     ROL buttonsp4
    ;     BCC ReadButtonLoopP4
RTS


CheckA:
    LDA buttons 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckARelease
    LDA buttonflag ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflag
    LDA #ButtonReturn::Press 
    RTS

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflag
        AND #$01
        BEQ :+
        LDA buttonflag
        EOR #$01 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #$00
RTS 

CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA buttonflag
    ORA #$02
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS

    CheckBRelease:
        LDA buttonflag
        AND #$02
        BEQ :+
        LDA buttonflag
        EOR #$02 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckSelectRelease 
    LDA buttonflag
    ORA #$04 
    STA buttonflag    
    LDA ButtonReturn::Press
    RTS
    CheckSelectRelease:
        LDA buttonflag
        AND #$04
        BEQ :+
        LDA buttonflag
        EOR #$04 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA buttonflag
    ORA #$08
    STA buttonflag
    LDA ButtonReturn::Press
    RTS
    CheckStartRelease:
        LDA buttonflag
        AND #$08
        BEQ :+
        LDA buttonflag
        EOR #$08 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA buttonflag
    ORA #$10
    STA buttonflag
    LDA ButtonReturn::Press
    RTS
    CheckUpRelease:
        LDA buttonflag
        AND #$10
        BEQ :+
        LDA buttonflag
        EOR #$10 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA buttonflag 
    ORA #$20 
    STA buttonflag 
    LDA ButtonReturn::Press
    RTS
    CheckDownRelease:
        LDA buttonflag
        AND #$20
        BEQ :+
        LDA buttonflag
        EOR #$20 
        STA buttonflag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckLeftRelease
    LDA buttonflag
    ORA #$40 
    STA buttonflag 
    LDA #$01
    RTS
    CheckLeftRelease:
        LDA buttonflag
        AND #$04
        BEQ :+
        LDA buttonflag
        EOR #$04 
        STA buttonflag
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRight:
    LDA buttons
    AND #%00000001
    BEQ CheckRightRelease
    LDA buttonflag 
    ORA #$80 
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS
    CheckRightRelease:
        LDA buttonflag
        AND #$80
        BEQ :+
        LDA buttonflag
        EOR #$80 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckAP2:
    LDA buttonsp2 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckAReleaseP2
    LDA buttonflagp2 ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflagp2
    LDA #$01 
    RTS

    CheckAReleaseP2: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflagp2
        AND #$01
        BEQ :+
        LDA buttonflagp2
        EOR #$01 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckBP2:

    LDA buttonsp2 
    AND #%01000000
    BEQ CheckBReleaseP2
    LDA buttonflagp2
    ORA #$02
    STA buttonflagp2
    LDA #ButtonReturn::Press
    RTS

    CheckBReleaseP2:
        LDA buttonflagp2
        AND #$02
        BEQ :+
        LDA buttonflagp2
        EOR #$02 
        STA buttonflagp2
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelectP2:
    LDA buttonsp2
    AND #%00100000
    BEQ CheckSelectReleaseP2 
    LDA buttonflagp2
    ORA #$04 
    STA buttonflagp2    
    LDA ButtonReturn::Press
    RTS
    CheckSelectReleaseP2:
        LDA buttonflagp2
        AND #$04
        BEQ :+
        LDA buttonflagp2
        EOR #$04 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStartP2:
    LDA buttonsp2
    AND #%00010000
    BEQ CheckStartReleaseP2
    LDA buttonflagp2
    ORA #$08
    STA buttonflagp2
    LDA ButtonReturn::Press
    RTS
    CheckStartReleaseP2:
        LDA buttonflagp2
        AND #$08
        BEQ :+
        LDA buttonflagp2
        EOR #$08 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUpP2:  
    LDA buttonsp2
    AND #%00001000
    BEQ  CheckUpReleaseP2
    LDA buttonflagp2
    ORA #$10
    STA buttonflagp2
    LDA ButtonReturn::Press
    RTS
    CheckUpReleaseP2:
        LDA buttonflagp2
        AND #$10
        BEQ :+
        LDA buttonflagp2
        EOR #$10 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDownP2:
    LDA buttonsp2
    AND #%00000100
    BEQ CheckDownReleaseP2 
    LDA buttonflagp2 
    ORA #$20 
    STA buttonflagp2 
    LDA ButtonReturn::Press
    RTS
    CheckDownReleaseP2:
        LDA buttonflagp2
        AND #$20
        BEQ :+
        LDA buttonflagp2
        EOR #$20 
        STA buttonflagp2
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeftP2:
    LDA buttonsp2
    AND #%00000010
    BEQ CheckLeftReleaseP2
    LDA buttonflagp2
    ORA #$40 
    STA buttonflagp2 
    LDA #$01
    RTS
    CheckLeftReleaseP2:
        LDA buttonflagp2
        AND #$04
        BEQ :+
        LDA buttonflagp2
        EOR #$04 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRightP2:

    LDA buttonsp2
    AND #%00000001
    BEQ CheckRightReleaseP2
    LDA buttonflagp2 
    ORA #$80 
    STA buttonflagp2
    LDA #$01
    RTS
    CheckRightReleaseP2:
        LDA buttonflagp2
        AND #$80
        BEQ :+
        LDA buttonflagp2
        EOR #$80 
        STA buttonflagp2
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckAP3:
    LDA buttonsp3 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckAReleaseP3
    LDA buttonflagp3 ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflagp3
    LDA #ButtonReturn::Press 
    RTS

    CheckAReleaseP3: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflagp3
        AND #$01
        BEQ :+
        LDA buttonflagp3
        EOR #$01 
        STA buttonflagp3
        LDA #ButtonReturn::Release
        RTS 
:
LDA #$00
RTS 

CheckBP3:

    LDA buttonsp3
    AND #%01000000
    BEQ CheckBReleaseP3
    LDA buttonflagp3
    ORA #$02
    STA buttonflagp3
    LDA #ButtonReturn::Press
    RTS

    CheckBReleaseP3:
        LDA buttonflagp3
        AND #$02
        BEQ :+
        LDA buttonflagp3
        EOR #$02 
        STA buttonflagp3
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelectP3:
    LDA buttonsp3
    AND #%00100000
    BEQ CheckSelectReleaseP3 
    LDA buttonflagp3
    ORA #$04 
    STA buttonflagp3    
    LDA ButtonReturn::Press
    RTS
    CheckSelectReleaseP3:
        LDA buttonflagp3
        AND #$04
        BEQ :+
        LDA buttonflagp3
        EOR #$04 
        STA buttonflagp3
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStartP3:
    LDA buttonsp3
    AND #%00010000
    BEQ CheckStartReleaseP3
    LDA buttonflagp3
    ORA #$08
    STA buttonflagp3
    LDA ButtonReturn::Press
    RTS
    CheckStartReleaseP3:
        LDA buttonflagp3
        AND #$08
        BEQ :+
        LDA buttonflagp3
        EOR #$08 
        STA buttonflagp3
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUpP3:  
    LDA buttonsp3
    AND #%00001000
    BEQ  CheckUpReleaseP3
    LDA buttonflagp3
    ORA #$10
    STA buttonflagp3
    LDA ButtonReturn::Press
    RTS
    CheckUpReleaseP3:
        LDA buttonflagp3
        AND #$10
        BEQ :+
        LDA buttonflagp3
        EOR #$10 
        STA buttonflagp3
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDownP3:
    LDA buttonsp3
    AND #%00000100
    BEQ CheckDownReleaseP3 
    LDA buttonflagp3 
    ORA #$20 
    STA buttonflagp3 
    LDA ButtonReturn::Press
    RTS
    CheckDownReleaseP3:
        LDA buttonflagp3
        AND #$20
        BEQ :+
        LDA buttonflagp3
        EOR #$20 
        STA buttonflagp3
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeftP3:
    LDA buttonsp3
    AND #%00000010
    BEQ CheckLeftReleaseP3
    LDA buttonflagp3
    ORA #$40 
    STA buttonflagp3 
    LDA #$01
    RTS
    CheckLeftReleaseP3:
        LDA buttonflagp3
        AND #$04
        BEQ :+
        LDA buttonflagp3
        EOR #$04 
        STA buttonflagp3
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRightP3:

    LDA buttonsp3
    AND #%00000001
    BEQ CheckRightReleaseP3
    LDA buttonflagp3 
    ORA #$80 
    STA buttonflagp3
    LDA #$01
    RTS
    CheckRightReleaseP3:
        LDA buttonflagp3
        AND #$80
        BEQ :+
        LDA buttonflagp3
        EOR #$80 
        STA buttonflagp3
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckAP4:
    LDA buttonsp4 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckAReleaseP4
    LDA buttonflagp4 ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA buttonflagp4
    LDA #ButtonReturn::Press 
    RTS

    CheckAReleaseP4: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA buttonflagp4
        AND #$01
        BEQ :+
        LDA buttonflagp4
        EOR #$01 
        STA buttonflagp4
        LDA #ButtonReturn::Release
        RTS 
:
LDA #$00
RTS 

CheckBP4:

    LDA buttonsp4 
    AND #%01000000
    BEQ CheckBReleaseP4
    LDA buttonflagp4
    ORA #$02
    STA buttonflagp4
    LDA #ButtonReturn::Press
    RTS

    CheckBReleaseP4:
        LDA buttonflagp4
        AND #$02
        BEQ :+
        LDA buttonflagp4
        EOR #$02 
        STA buttonflagp4
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckSelectP4:
    LDA buttonsp4
    AND #%00100000
    BEQ CheckSelectReleaseP4 
    LDA buttonflagp4
    ORA #$04 
    STA buttonflagp4    
    LDA ButtonReturn::Press
    RTS
    CheckSelectReleaseP4:
        LDA buttonflagp4
        AND #$04
        BEQ :+
        LDA buttonflagp4
        EOR #$04 
        STA buttonflagp4
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStartP4:
    LDA buttonsp4
    AND #%00010000
    BEQ CheckStartReleaseP4
    LDA buttonflagp4
    ORA #$08
    STA buttonflagp4
    LDA ButtonReturn::Press
    RTS
    CheckStartReleaseP4:
        LDA buttonflagp4
        AND #$08
        BEQ :+
        LDA buttonflagp4
        EOR #$08 
        STA buttonflagp4
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUpP4:  
    LDA buttonsp4
    AND #%00001000
    BEQ  CheckUpReleaseP4
    LDA buttonflagp4
    ORA #$10
    STA buttonflagp4
    LDA ButtonReturn::Press
    RTS
    CheckUpReleaseP4:
        LDA buttonflagp4
        AND #$10
        BEQ :+
        LDA buttonflagp4
        EOR #$10 
        STA buttonflagp4
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDownP4:
    LDA buttonsp4
    AND #%00000100
    BEQ CheckDownReleaseP4 
    LDA buttonflagp4 
    ORA #$20 
    STA buttonflagp4
    LDA ButtonReturn::Press
    RTS
    CheckDownReleaseP4:
        LDA buttonflagp4
        AND #$20
        BEQ :+
        LDA buttonflagp4
        EOR #$20 
        STA buttonflagp4
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeftP4:
    LDA buttonsp4
    AND #%00000010
    BEQ CheckLeftReleaseP4
    LDA buttonflagp4
    ORA #$40 
    STA buttonflagp4
    LDA #$01
    RTS
    CheckLeftReleaseP4:
        LDA buttonflagp4
        AND #$04
        BEQ :+
        LDA buttonflagp4
        EOR #$04 
        STA buttonflagp4
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRightP4:

    LDA buttonsp4
    AND #%00000001
    BEQ CheckRightReleaseP4
    LDA buttonflagp4
    ORA #$80 
    STA buttonflagp4
    LDA #$01
    RTS
    CheckRightReleaseP4:
        LDA buttonflagp4
        AND #$80
        BEQ :+
        LDA buttonflagp4
        EOR #$80 
        STA buttonflagp4
        LDA #$02
        RTS 
:
LDA #$00
RTS 



;;;;;;;;
; Collision
;;;;;;;;

EjectFromRightWall:
    JSR CollideRight2 ; check initial position
    BEQ :++
    CMP #$04
    BEQ :++
    :
    DEC entities+Entity::xpos, X 
    JSR CollideRight2
    BNE :-
    :
RTS

EjectFromLeftWall:
    JSR CollideLeft2 ; check initial position
    BEQ :++
    CMP #$04
    BEQ :++
    :
    INC entities+Entity::xpos, X 
    JSR CollideLeft2
    BNE :-
    :
RTS

EjectFromBottomWall:
    JSR CollideDown2 ; check initial position
    BEQ :++
    CMP #$04
    BEQ :++
    :
    DEC entities+Entity::ypos, X 
    JSR CollideDown2
    BNE :-
    :
RTS

EjectFromTopWall:
    JSR CollideUp2 ; check initial position
    BEQ :++
    CMP #$04
    BEQ :++
    :
    INC entities+Entity::ypos, X 
    JSR CollideUp2
    BNE :-
    :
RTS



CollideLeft2:
; LDA #$3f
;     STA $2001
    
    LDA #$00
    STA temp3
    CollideLeft2Loop:
    LDA entities+Entity::xpos, X ;4  
    LSR ;2
    LSR ;2
    LSR ;2
    LSR;2

    STA temp ; 3 

    LDA entities+Entity::ypos, X ;
    ; CLC 
    ; ADC #$07
    LSR ;2
    LSR ;2
    LSR ;2
    LSR ;2

    ASL ;2
    ASL ;2
    ASL 
    ASL 

    STA temp2 ;3

    CLC ; 2
    ADC temp ;2 
    TAY ; 2
    LDA CollisionMap, Y ; 4/5
    ASL ;2 
    TAY  ;2
    LDA MetaTileList, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileList+1, Y ; 4/5 
    STA jumppointer+1 ;3

    LDY #$04 ; 2 
    LDA temp ; 3
    ASL ;2 
    ASL ;2
    ASL ;2
    ASL ; X16 to return it to world scale 
    STA temp ;3  
    LDA entities+Entity::xpos, X ;4/5
    SEC ; 2
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09 ; 2
    BCC :+ ; 2/3
    INY
    :
    LDA temp2 ;3  
    SEC ;2
    SBC entities+Entity::ypos, X ; 4/5 
    CMP #$09 ;2
    BCC :+ ;2/3
    INY
    INY
    :
    LDA (jumppointer), Y ;5/6
    STA rectangle1 ;3

    ; get collision of second point 
    LDA entities+Entity::xpos, X 
    LSR 
    LSR 
    LSR 
    LSR

    STA temp 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2

    CLC
    ADC temp 
    TAY 
    LDA CollisionMap, Y
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer 
    LDA MetaTileList+1, Y 
    STA jumppointer+1

    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X
    SEC 
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY 
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y 
    ORA rectangle1

    ; BEQ :+
    ; CMP #$04
    ; BEQ :+
    ;     STA temp3
    ;     INC entities+Entity::xpos, X 
    ;     JMP CollideLeft2Loop
    ; :

    ; LDA #$1E
    ; STA $2001
    ; LDA temp3
    RTS 

CollideLeft:
    LDA entities+Entity::xpos, X
    SEC
    SBC #$01 ; add 8 pixels for a single sprite
    LSR 
    LSR 
    LSR 
    LSR 

    STA temp

    LDA entities+Entity::ypos, X 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2

    RTS 

CollideRight2:

    LDA #$00
    STA temp3

    CollideRight2Loop:

    LDA entities+Entity::xpos, X ;4  
    CLC ; 2
    ADC #$07 ; 2
    LSR ;2
    LSR ;2
    LSR ;2
    LSR;2

    STA temp ; 3 

    LDA entities+Entity::ypos, X ;
    ; CLC 
    ; ADC #$07
    LSR ;2
    LSR ;2
    LSR ;2
    LSR ;2

    ASL ;2
    ASL ;2
    ASL 
    ASL 

    STA temp2 ;3

    CLC ; 2
    ADC temp ;2 
    TAY ; 2
    LDA CollisionMap, Y ; 4/5
    ASL ;2 
    TAY  ;2
    LDA MetaTileList, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileList+1, Y ; 4/5 
    STA jumppointer+1 ;3

    LDY #$04 ; 2 
    LDA temp ; 3
    ASL ;2 
    ASL ;2
    ASL ;2
    ASL ; X16 to return it to world scale 
    STA temp ;3  
    LDA entities+Entity::xpos, X ;4/5
    SEC ; 2
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09 ; 2
    BCC :+ ; 2/3
    INY
    :
    LDA temp2 ;3  
    SEC ;2
    SBC entities+Entity::ypos, X ; 4/5 
    CMP #$09 ;2
    BCC :+ ;2/3
    INY
    INY
    :
    LDA (jumppointer), Y ;5/6
    STA rectangle1 ;3

    ; get collision of second point 
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR

    STA temp 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2

    CLC
    ADC temp 
    TAY 
    LDA CollisionMap, Y
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer 
    LDA MetaTileList+1, Y 
    STA jumppointer+1

    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::ypos, X
    SEC 
    SBC temp
    ; SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY 
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y 
    ORA rectangle1

    ; BEQ :+
    ; CMP #$04
    ; BEQ :+
    ;     STA temp3
    ;     DEC entities+Entity::xpos, X 
    ;     JMP CollideRight2Loop
    ; :
    ; LDA temp3
    RTS 

CollideRight: ; only to be called dfrom process entities
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$08 ; add 8 pixels for a single sprite 
    LSR 
    LSR 
    LSR 
    LSR 

    STA temp

    LDA entities+Entity::ypos, X 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2

    RTS 

CollideDown2:

    ; LDA #$3f
    ; STA $2001
    
    LDA #$00
    STA temp3

    CollideDown2Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY
    :
    LDA (jumppointer), Y
    STA rectangle1



    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$06 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    LDY #$04
    LDA (jumppointer), Y 

    ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY
    INY
    :
    LDA (jumppointer), Y
    ORA rectangle1 ; if the left OR right point returns a collision, we collide

    ; BEQ :+
    ; CMP #$04
    ; BEQ :+
    ;     STA temp3
    ;     DEC entities+Entity::ypos, X 
    ;     JMP CollideDown2Loop
    ; :
    ; LDA #$1E
    ; STA $2001
    
    ; LDA temp3
    ; lda #$04
    RTS 

CollideDown3:

    ; LDA #$3f
    ; STA $2001
    
    LDA #$00
    STA temp3

    CollideDown3Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY
    :
    LDA (jumppointer), Y
    STA rectangle1



    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$06 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X 
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    LDY #$04
    LDA (jumppointer), Y 

    ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp 
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2
    SEC 
    SBC entities+Entity::ypos, X
    CMP #$09
    BCC :+
    INY
    INY
    :
    LDA (jumppointer), Y
    ORA rectangle1

    RTS 

CollideDown:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    ; AND #%00011111 ; mask any bits over 16 
    STA temp

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$07
    LSR 
    LSR 
    LSR 
    LSR 
    ; AND #%00011111 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    STA temp2

    LDA entities+Entity::xpos, X
    CLC 
    ADC #$07 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    CLC 
    ADC #$08
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL 
    ASL 
    ASL 

    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    ORA temp2
    RTS

CollideUp2:
    LDA #$00
    STA temp3
    CollideUp2Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X  
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp  
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2  
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY
    INY 
    :
    LDA (jumppointer), Y
    STA rectangle1

    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X  
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp
    LDA entities+Entity::xpos, X  
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCS :+
    INY
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y
    ORA rectangle1
    
    BEQ :+

        CMP #$04
        BEQ :+

        STA temp3 
        INC entities+Entity::ypos, X 
        JMP CollideUp2Loop
    :
    LDA temp3
    RTS 

CollideUp3:
    LDA #$00
    STA temp3
    CollideUp3Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X  
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp  
    LDA entities+Entity::xpos, X 
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCC :+
    INY
    :
    LDA temp2  
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY
    INY 
    :
    LDA (jumppointer), Y
    STA rectangle1

    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$07 ; move 1 pixel in fromthe centre 
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
    ; AND #%00011111 ; mask bits over 16
    STA temp

    ; Do the same for Y pos 
    LDA entities+Entity::ypos, X  
    LSR 
    LSR 
    LSR 
    LSR ; we now have the y pos on the grid 
    ; Index into 2d array: (y * width) + X 
    ; Width = 16, so asl 4 times, add X to get index into array 
    ASL 
    ASL 
    ASL 
    ASL 

    STA temp2 ; store the y portion

    CLC 
    ADC temp ; add the x portion
    TAY 
    LDA CollisionMap, Y ; load the meta tile id
    ASL 
    TAY  
    LDA MetaTileList, Y 
    STA jumppointer
    LDA MetaTileList+1, Y 
    STA jumppointer+1
    ; LDY #$04
    ; LDA (jumppointer), Y 
    ; RTS

    ; ; work out the tile of the metatile the point we're checking is in 
    LDY #$04 
    LDA temp
    ASL 
    ASL 
    ASL 
    ASL ; X16 to return it to world scale 
    STA temp
    LDA entities+Entity::xpos, X  
    SEC 
    SBC temp ; get difference between two 
    CMP #$09
    BCS :+
    INY
    :
    LDA temp2 
    SEC 
    SBC entities+Entity::ypos, X 
    CMP #$09 
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y
    ORA rectangle1
    
    ; BEQ :+ 
    ;     STA temp3 
    ;     INC entities+Entity::ypos, X 
    ;     JMP CollideUp3Loop
    ; :
    ; LDA temp3
RTS 



CollideUp:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL
    ASL
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    STA temp2 

    LDA entities+Entity::xpos, X
    CLC 
    ADC #$07 ; get the centre of the sprite(ish)
    LSR 
    LSR 
    LSR 
    LSR 
    STA temp

    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01 
    LSR 
    LSR 
    LSR 
    LSR 

    ASL 
    ASL
    ASL
    ASL 

    CLC 
    ADC temp 
    TAY 
    LDA CollisionMap, Y 
    ORA temp2
    RTS 

SpriteCollide: 
    TXA 
    PHA 
    STA temp ; save your own position so it can be skipped
    ; first store your own collisionbit
    LDA entities+Entity::collisionlayer, X
    STA temp2 

    LDA entities+Entity::xpos, X
    STA rectangle1
    CLC 
    ADC #$07
    STA rectangle1+1
    LDA entities+Entity::ypos, X
    STA rectangle1+2 
    CLC 
    ADC #$07
    STA rectangle1+3
 
    ; compare collision bits
    LDX #$00
    LDA temp2
    BEQ CollideSpriteComplete
    SpriteCollideLoop:
    LDA entities+Entity::collisionlayer, X
    BEQ CollideSpriteComplete
    AND temp2
    BNE :+
    JMP CollideSpriteComplete
    :
    CPX temp 
    BEQ CollideSpriteComplete
    LDA entities+Entity::type, X 
    BEQ CollideSpriteComplete
    CLC ;???
    LDA entities+Entity::xpos, X
    STA rectangle2
    CLC 
    ADC #$07
    STA rectangle2+1
    LDA entities+Entity::ypos, X
    STA rectangle2+2 
    CLC 
    ADC #$07
    STA rectangle2+3 

    LDA rectangle1
    CMP rectangle2+1
    BCS CollideSpriteComplete
    LDA rectangle1+1 
    CMP rectangle2
    BCC CollideSpriteComplete
    LDA rectangle1+2
    CMP rectangle2+3 
    BCS CollideSpriteComplete
    LDA rectangle1+3 
    CMP rectangle2+2 
    BCC CollideSpriteComplete
    TXA ; X = the entity we have successfuly collided
    TAY  ; y = ditto
    PLA  ; pull X (the current entiy being processed)
    TAX  ; put it back in x ready to go back to the entity process loop
    TYA         
    RTS ; return with the collided object ID in A

    CollideSpriteComplete:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #entity_mem
    BEQ :+
    JMP SpriteCollideLoop
    : 

EndSpriteCollide:
    PLA 
    TAX 
    LDA #$FF
    RTS

NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

CollisionEffectTable:
    .word NoCollision ;0 
    .word Collision ;1
    .word CollisionConveyorLeft ;2
    .word CollisionConveyorRight ;3
    .word CollisionJumpablePlatform ;4

CollisionEffectTableHorizontal:
    .word NoCollisionHorizontal ;0 
    .word CollisionHorizontal ;1
    .word CollisionConveyorLeftHorizontal ;2
    .word CollisionConveyorRightHorizontal ;3
    .word CollisionJumpablePlatformHorizontal ;4


NoCollision:
    RTS 

Collision:
    RTS 

CollisionConveyorLeft:
    LDA entities+Entity::xpossub, X 
    SEC 
    SBC #$80
    STA entities+Entity::xpossub, X
    LDA entities+Entity::xpos, X
    SbC #$00 
    STA entities+Entity::xpos, X
RTS 

CollisionConveyorRight:
    LDA entities+Entity::xpossub, X 
    CLC 
    ADC #$80
    STA entities+Entity::xpossub, X
    LDA #$00
    ADC entities+Entity::xpos, X 
    STA entities+Entity::xpos, X
RTS 

CollisionJumpablePlatform:
    ; LDA #$00
    ; STA temp3
    ; RTS
    ; if we are going up, set temp to zero to disable collision
    ; if already falling, temp = 1 to set collision
    LDA entities+Entity::generalpurpose, X 
    ; if jumping, do not collide
    CMP #$02
    BNE :+
    LDA #$00
    STA temp3
    RTS
    :
    ; if moving along floor, collide as norm
    CMP #$06
    BNE :+
    LDA #$01 
    STA temp3
    RTS 
    :
    ; if on floor but not moving, do not collide
    CMP #$01 
    BNE :+
    LDA #$01
    STA temp3
    rts
    :
    ; if 5, we are falling and do NOT collide
    CMP #$05
    BNE :+
    LDA #$00
    STA temp3
    RTS
    :
RTS

NoCollisionHorizontal: ;0 
RTS

CollisionHorizontal: ;1
RTS

CollisionConveyorLeftHorizontal: ;2
RTS

CollisionConveyorRightHorizontal: ;3
RTS

CollisionJumpablePlatformHorizontal: ;4
    LDA #$00
    STA temp
RTS

JumpToPointerRoutine:
    JMP (jumppointer)
;;;;;
;PRNG GENERATOR 
;https://www.nesdev.org/wiki/Random_number_generator
;;;;;;
;Generates a pseudo random number inthe A register
;133-147 cycles per call
Prng: 
    LDY #08     ; iteration count (generates 8 bits)
	LDA seed+0
:
	ASL        ; shift the register
	ROL seed+1
	BCC :+
	EOR #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
	DEY
	BNE :--
	STA seed+0
	CMP #00     ; reload flags
	RTS

Setrng:
    JSR Prng
    STA rng
    RTS

;;;
;Tilebuffers
;;;
FillTileBuffer:
    ; LDA #<TitleScreen
    ; STA world
    ; LDA #>TitleScreen
    ; STA world+1

    LDX #$00
    LDY #$00
    LDA #DFlags::WriteVertTiles1
    BIT DrawFlags
    BNE :+
    LDA #$00
    STA tilebufferindex
    RTS
    :
    LDY tilebufferindex
    LDA (world), Y
    ASL 
    TAY 
    LDA (metatilepointer), Y 
    STA jumppointer
    INY
    LDA (metatilepointer), Y
    STA jumppointer+1

    ; fill first strip
    LDY #$00

    LDA (jumppointer), Y 
    STA TileBuffer, X
    INY
    INX 
    LDA (jumppointer), Y
    STA TileBuffer, X
    INY 
    INC tilebufferindex
    INX
    CPX #32
    BNE :-
    LDA tilebufferindex
    SEC 
    SBC #$10
    STA tilebufferindex
    ; fill second strip
    LDX #$00
    :
    LDY tilebufferindex
    LDA (world), Y
    ASL 
    TAY 
    LDA (metatilepointer), Y 
    STA jumppointer
    INY
    LDA (metatilepointer), Y
    STA jumppointer+1

    LDY #$02

    LDA (jumppointer), Y 
    STA TileBuffer2, X
    INY
    INX 
    LDA (jumppointer), Y
    STA TileBuffer2, X
    INY
    INC tilebufferindex
    INX
    CPX #32
    BNE :-
    RTS

FillAttributeBuffer:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OAM Buffer Handling ; All sprite data for the frame must be written into the OAM so that it can be sent to the PPU in vblank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OAMBuffer:
    ; Clear all sprite data from last frame 
    ClearSpriteBuffer:
        LDA #$00 
        STA spritebufferposition
        LDX #$00
        LDA #$FF 
        ClearBufferLoop:
            STA $0200, X
            INX 
            CPX #$FF
            BNE ClearBufferLoop

            LDX #$00
            LDY #$00
            LDA #$00
    DrawSprites:
        LDA entities+Entity::type, X 
        BNE :+
        JMP CheckEndSpriteDraw
        :
        ASL 
        TAY 
        LDA DrawSpriteList, Y 
        STA jumppointer
        LDA DrawSpriteList+1, Y 
        STA jumppointer+1

        LDA entities+Entity::animationframe, X
        ASL 
        TAY 
        LDA (jumppointer), Y 
        PHA 
        INY 
        LDA (jumppointer), Y 
        STA jumppointer+1
        PLA 
        STA jumppointer 


    DrawSpriteInit:
        LDY #$00
    DrawSpriteLoop:  
        LDA (jumppointer), Y
        ; get y offset 
        CLC 
        ADC entities+Entity::ypos, X
        SEC 
        SBC #$01 ; Ypos scanline is off by one and needs correcting 
        STY temp
        LDY spritebufferposition
        STA SpriteBuffer, Y
        INC spritebufferposition    
        LDY temp 
        INY 

        ; Tile
        LDA (jumppointer), Y 
        ; ORA entities+Entity::attributes
        ; EOR #%11000000
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp
        INC spritebufferposition
        INY
        ; Attributes
        LDA (jumppointer), Y
        ORA entities+Entity::attributes, X 
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp 
        INC spritebufferposition
        INY 

        LDA (jumppointer), Y 
        CLC 
        ADC entities+Entity::xpos, X 
        STY temp 
        LDY spritebufferposition
        STA SpriteBuffer, Y 
        LDY temp 
        INY 
        INC spritebufferposition

        LDA (jumppointer), Y 
        CMP #$FF 
        BNE DrawSpriteLoop  
    
    CheckEndSpriteDraw:
        TXA 
        CLC 
        ADC #.sizeof(Entity)
        TAX 
        CPX #entity_mem
        BEQ EndSpriteDraw
        JMP DrawSprites

    EndSpriteDraw:
        RTS 

DrawSpriteList:
    .word MetaSpriteListPlayer ; just a dummy to fill the entry. This will never be chosen
    .word MetaSpriteListPlayer
    .word MetaSpriteListPlayer2
    .word MetaSpriteListPlayer3
    .word MetaSpriteListPlayer4
    .word MetaSpriteListSlider
    .word MetaSpriteListProjectileSpell
    .word MetaSpriteListExplosion
    .word MetaSpriteListLightningEmitter
    .word MetaSpriteListSparks
    .word MetaSpriteListLightning
    .word MetaSpriteListFireball
    .word MetaSpriteListRespawner
    .word MetaSpriteListPortal
    .word MetaSpriteListBroomStick
    .word MetaSpriteListIceBeam
    .word MetaSpriteListTeleporter
    .word MetaSpriteListTeleporter
    .word MetaSpriteListTeleporter
    .word MetaSpriteListTeleporter
    .word MetaSpriteListHat
    .word MetaSpriteListScoreTextPlayerOne
    .word MetaSpriteListScoreTextPlayerTwo
    .word MetaSpriteListScoreTextPlayerThree
    .word MetaSpriteListScoreTextPlayerFour
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListScoreNumber
    .word MetaSpriteListVerticalLaser
;;;;;;;;
;; DEATH FUNCTIONS
;; These are all called from ProcessDestructionStack and MUST return there when they finish
;;;;;;;'

NoDeathAction:
    PLA 
    PLA 
    PLA
    JMP ProcessDestructionStack

Player1Death:
    PLA 
    PLA 
    PLA 
    JMP ProcessDestructionStack

Player2Death:
    PLA 
    PLA 
    PLA 
    JMP ProcessDestructionStack

Player3Death:
    PLA 
    PLA 
    PLA 
    JMP ProcessDestructionStack

Player4Death:
    PLA 
    PLA 
    PLA 
    JMP ProcessDestructionStack


RespawnPlayer:
    PLA 
    PLA 
    PLA
    JSR SpawnPlayerPort1
    JMP ProcessDestructionStack
RespawnPlayer2:
    PLA 
    TAX
    PLA
    TAY 
    PLA
    LDA #EntityType::Explosion
    JSR SpawnEntity

    LDA #$00
    STA entities+Entity::generalpurpose, X
    LDA #$02
    JSR AddEntityToPlayerSpawnStack
    JMP ProcessDestructionStack
RespawnPlayer3:
    PLA 
    PLA 
    PLA
    LDY #$50
    LDX #$50 
    LDA EntityType::Player
    JSR SpawnEntity
    JMP ProcessDestructionStack
RespawnPlayer4:
    PLA 
    PLA 
    PLA
    LDY #$50
    LDX #$50 
    LDA EntityType::Player
    JSR SpawnEntity
    JMP ProcessDestructionStack
DeathExplosion:
    PLA 
    TAX
    PLA
    TAY 
    PLA
    LDA #EntityType::Explosion
    JSR SpawnEntity
    JMP ProcessDestructionStack
RespawnSelf:
    PLA 
    PLA 
    PLA
    LDY rng
    LDX rng
    LDA temp
    JSR SpawnEntity
    JMP ProcessDestructionStack
ExplodeAndRespawn:
; this spawns first, if entity limit is reached no death effect
    LDY rng
    LDX rng
    LDA temp
    JSR SpawnEntity
    ; JMP ProcessDestructionStack
    PLA 
    TAX 
    PLA 
    TAY 
    PLA
    ; JMP ProcessDestructionStack
    LDA #EntityType::Explosion
    JSR SpawnEntity
    JMP ProcessDestructionStack




;;;;;;;;;;;;


; .segment "DATA"

; Meta tile definitions. The first 4 bytes refer to tiles in chr rom
; The 5th byte is the collision for the block. 0=no collide 1 = collide
brick:
    .byte $00,$01,$10,$11 ; chr rom reference bytes
    .byte $00,$00,$00,$00 ; collision bytes 
brick_hole:
    .byte $02,$03,$12,$13
    .byte $00,$00,$00,$00
brick_dark_left:
    .byte $04,$05,$14,$15
    .byte $00,$00,$00,$00
brick_dark_right:
    .byte $06,$07
    .byte $16,$17
    .byte $00,$00
    .byte $00,$00
brick_bright_left:
    .byte $08,$09
    .byte $18,$19    
    .byte $00,$00
    .byte $00,$00

brick_bright_right:
    .byte $0A,$0B
    .byte $1A,$1B
    .byte $00,$00
    .byte $00,$00
earth:
    .byte $20,$21
    .byte $30,$31
    .byte $01,$01
    .byte $01,$01
earth_edge_l:
    .byte $20,$25
    .byte $30,$35
    .byte $01,$01
    .byte $01,$01
earth_edge_r:
    .byte $24,$21
    .byte $33,$31
    .byte $01,$01
    .byte $01,$01

earth_top:
    .byte $22,$23
    .byte $32,$33
    .byte $01,$01
    .byte $01,$01
arch_tl:
    .byte $0C,$0D
    .byte $1C,$1D
    .byte $00,$00
    .byte $00,$00
arch_tr:
    .byte $0E,$0F
    .byte $1E,$1F
    .byte $00,$00
    .byte $00,$00
arch_bl:
    .byte $2C,$2D
    .byte $3C,$3D
    .byte $00,$00
    .byte $00,$00
arch_br:    
    .byte $2E,$2F
    .byte $3E,$3F
    .byte $00,$00
    .byte $00,$00
moon_tl:
    .byte $40,$41
    .byte $50,$51
    .byte $00,$00
    .byte $00,$00
moon_tr:
    .byte $42,$43
    .byte $52,$53
    .byte $00,$00
    .byte $00,$00
moon_bl:
    .byte $60,$61
    .byte $70,$71
    .byte $00,$00
    .byte $00,$00
moon_br:
    .byte $62,$63
    .byte $72,$73
    .byte $00,$00
    .byte $00,$00
crack_v:
    .byte $44,$45
    .byte $54,$55
    .byte $00,$00
    .byte $00,$00
crack_h:
    .byte $64,$65 
    .byte $74,$75
    .byte $00,$00
    .byte $00,$00
crack:
    .byte $46,$47
    .byte $56,$57
    .byte $00,$00
    .byte $00,$00
brick_lip_l:
    .byte $28,$29
    .byte $00,$01
    .byte $00,$00
    .byte $00,$00
brick_lip_r:
    .byte $2A,$2B 
    .byte $10,$11
    .byte $00,$00
    .byte $00,$00
brick_bulge_l:
    .byte $28,$29
    .byte $38,$39
    .byte $00,$00
    .byte $00,$00
brick_bulge_r:
    .byte $2A,$2B
    .BYTE $3A,$3B
    .byte $00,$00
    .byte $00,$00
water_l:
    .byte $4C,$4D
    .BYTE $5C,$5D
    .byte $01,$01
    .byte $01,$01
 water_r:
    .byte $4E,$4F
    .BYTE $5E,$5F
    .byte $01,$01
    .byte $01,$01 
window_l:
    .byte $6C,$6D
    .BYTE $7C,$7D
    .byte $00,$00
    .byte $00,$00    
window_r:
    .byte $6E,$6F
    .BYTE $7E,$7F 
    .byte $00,$00
    .byte $00,$00
bars_l:
    .byte $8C,$8D
    .BYTE $9C,$9D
    .byte $00,$00
    .byte $00,$00
bars_r:
    .byte $8E,$8F
    .BYTE $9E,$9F
    .byte $00,$00
    .byte $00,$00
floor:
    .byte $8A,$8B
    .byte $9A,$9B
    .byte $01,$01
    .byte $01,$01     
floor_no_collide:
    .byte $88,$89 
    .byte $98,$99 
    .byte $00,$00
    .byte $00,$00
floor_l:
    .byte $68,$69
    .byte $78,$79
    .byte $01,$01
    .byte $01,$01
floor_r:
    .byte $6A,$6B
    .byte $7A,$7B
    .byte $01,$01
    .byte $01,$01
floor_corner_l:
    .byte $A8,$A9 
    .byte $B8,$B9
    .byte $01,$01
    .byte $01,$01 
floor_corner_r:
    .byte $AA,$AB 
    .byte $BA,$BB
    .byte $01,$01
    .byte $01,$01 
floor_side_l:
    .byte $C8,$C9 
    .byte $D8,$D9 
    .byte $01,$01
    .byte $01,$01
floor_side_r:
    .byte $CA,$CB 
    .byte $DA,$DB
    .byte $01,$01
    .byte $01,$01
floor_corner_bot_l:
    .byte $E8,$E9 
    .byte $F8,$F9
    .byte $01,$01
    .byte $01,$01
floor_corner_bot_r:
    .byte $EA,$EB 
    .byte $FA,$FB
    .byte $01,$01
    .byte $01,$01 
floor_corner_bot_mid:
    .byte $E9,$EA 
    .byte $F9,$FA
    .byte $01,$01
    .byte $01,$01 
floor_corner_bot:
    .byte $E6,$E7 
    .byte $F6,$F7
    .byte $01,$01
    .byte $01,$01  
candles:
    .BYTE $A6,$A7 
    .byte $B6,$B7
    .byte $00,$00
    .byte $00,$00 
brazier:
    .BYTE $A4,$A5 
    .byte $B4,$B5
    .byte $00,$00
    .byte $00,$00
pal1:
    .byte $FC,$FC
    .byte $FC,$FC
    .byte $00,$00
    .byte $00,$00
pal2:
    .byte $FD,$FD
    .byte $FD,$FD
    .byte $00,$00
    .byte $00,$00
pal3:
    .byte $FF,$FF
    .byte $FE,$FE
    .byte $00,$00
    .byte $00,$00
pal4:
    .byte $FF,$FF
    .byte $FF,$FF
    .byte $00,$00
    .byte $00,$00
brick_divided:
    .byte $26, $27 
    .byte $36, $37
    .byte $00,$00
    .byte $00,$00
floor_middle:
    .byte $A9,$AA
    .byte $B9,$BA
    .byte $01,$01
    .byte $01,$01
floor_half:
    .byte $96,$97
    .byte $68,$69
    .byte $00,$00
    .byte $01,$01
floor_half_reversed:
    .byte $68,$69
    .byte $A2,$A3
    .byte $04,$04
    .byte $00,$00
laseremittertop:
    .byte $ea,$ea
    .byte $ec,$ec
    .byte $01,$01
    .byte $00,$00

laseremitterbottom:
    .byte $ec,$ec
    .byte $fd,$fe
    .byte $00,$00
    .byte $01,$01

conveyorBelt1:
    .byte $E0,$E1
    .BYTE $FE,$FD
    .byte $03,$03
    .byte $01,$01
conveyorBelt2:
    .byte $E2,$E3
    .BYTE $FE,$FD
    .byte $03,$03
    .byte $01,$01
conveyorBelt3:
    .byte $E4,$E5
    .BYTE $FE,$FD
    .byte $03,$03
    .byte $01,$01
conveyorBelt4:
    .byte $E6,$E7
    .BYTE $FE,$FD
    .byte $03,$03
    .byte $01,$01
conveyorBelt5:
    .byte $E8,$E9
    .BYTE $FE,$FD
    .byte $03,$03
    .byte $01,$01
conveyorbeltrl1:
    .byte $E0,$E1 
    .BYTE $FE,$FD
    .byte $02,$02
    .byte $02,$02
conveyorbeltrl2:
    .byte $E2,$E3
    .BYTE $FE,$FD
    .byte $02,$02
    .byte $02,$02

conveyorbeltrl3:
    .byte $E4,$E5
    .BYTE $FE,$FD
    .byte $02,$02
    .byte $02,$02

conveyorbeltrl4:
    .byte $E6,$E7
    .BYTE $FE,$FD
    .byte $02,$02
    .byte $02,$02

conveyorbeltrl5:
    .byte $E8,$E9
    .BYTE $FE,$FD
    .byte $02,$02
    .byte $02,$02

jumpablefloor:
    .byte $b2,$b3
    .BYTE $EC,$EC
    .byte $04,$04
    .byte $00,$00

jumpablefloorreversed:
    .byte $EC,$EC
    .BYTE $B2,$B3
    .byte $00,$00
    .byte $04,$04

waterfalltop:
    .byte $EA,$EB
    .BYTE $FA,$FB
    .byte $00,$00
    .byte $00,$00

waterfall1:
    .byte $AC,$AD
    .BYTE $BC,$BD
    .byte $00,$00
    .byte $00,$00
waterfall2:
    .byte $CC,$CD
    .BYTE $DC,$DD
    .byte $00,$00
    .byte $00,$00
waterfall3:
    .byte $AE,$AF
    .BYTE $BE,$BF
    .byte $00,$00
    .byte $00,$00
waterfall4:
    .byte $CE,$CF
    .BYTE $DE,$DF
    .byte $00,$00
    .byte $00,$00

MetaTileList:
    .word brick ;00
    .word brick_hole ;01
    .word brick_dark_left ;02
    .word brick_dark_right ;03
    .word brick_bright_left ;04
    .word brick_bright_right ; 05
    .word earth ;06
    .word earth_top ;07
    .word arch_tl ;08
    .word arch_tr ;09
    .word arch_bl ;0A
    .word arch_br ;0B
    .word moon_tl ;0C
    .word moon_tr ;0D
    .word moon_bl ;0E 
    .word moon_br ;0F
    .word crack_v ;10
    .word crack_h ;11
    .word crack ;12
    .word brick_lip_l;13
    .word brick_lip_r;14
    .word brick_bulge_l;15
    .word brick_bulge_r;16
    .word water_l;17
    .word water_r;18 
    .word window_l;19
    .word window_r;1A
    .word bars_l;1B
    .word bars_r;1C
    .word floor ;1d
    .word floor_no_collide ;1e
    .word floor_l ;1f
    .word floor_r ;20
    .word floor_corner_l ;21
    .word floor_corner_r ;22
    .word floor_side_l ;23
    .word floor_side_r ;24
    .word floor_corner_bot_l ;25
    .word floor_corner_bot_r ;26
    .word floor_corner_bot_mid ;27
    .word floor_corner_bot ;28
    .word candles ;29
    .word brazier ;2a
    .word pal1 ;2b
    .word pal2 ;2c
    .word pal3 ;2d
    .word pal4 ;2e
    .word brick_divided ; 2f
    .word floor_middle ; 30
    .word floor_half ; 31
    .word floor_half_reversed ;32
    .word laseremitterbottom ;33
    .word laseremittertop ;34
    .word conveyorBelt1 ;35
    .word conveyorBelt2 ;36
    .word conveyorBelt3 ;37
    .word conveyorBelt4 ;38
    .word conveyorBelt5 ;39
    .word conveyorbeltrl1 ;3A
    .word conveyorbeltrl2 ;3B
    .word conveyorbeltrl3 ;3C
    .word conveyorbeltrl4 ;3D
    .word conveyorbeltrl5 ;3E
    .word jumpablefloor ;3f
    .word jumpablefloorreversed ;40

    .word waterfalltop ;41 
    .word waterfall1 ;42
    .word waterfall2 ; 43
    .word waterfall3 ; 44
    .word waterfall4 ; 45
    .word earth_edge_l
    .word earth_edge_r


MetaTileListTitle:
    .word titleblank
    .word titlewordplayer1
    .word titlewordplayer2
    .word titlewordplayer3
    .word titlewordplayer4
    .word titleone1
    .word titleone2
    .word titletwo1
    .word titletwo2
    .word titlethree1
    .word titlethree2
    .word titlethree3
    .word titlefour1
    .word titlefour2
    .word titlefour3

titleblank:
    .byte $00,$00
    .byte $00,$00
titlewordplayer1:
    .byte $47,$48
    .byte $57,$58
titlewordplayer2:
    .byte $49,$4A
    .byte $59,$5A
titlewordplayer3:
    .byte $4B,$4C
    .byte $5B,$5C
titlewordplayer4:
    .byte $4D,$4E
    .byte $5D,$5C
titleone1:
    .byte $00,$00
    .byte $00,$00
titleone2:
    .byte $00,$00
    .byte $00,$00
titletwo1:
    .byte $00,$00
    .byte $00,$00
titletwo2:
    .byte $00,$00
    .byte $00,$00
titlethree1:
    .byte $00,$00
    .byte $00,$00
titlethree2:
    .byte $00,$00
    .byte $00,$00
titlethree3:
    .byte $00,$00
    .byte $00,$00
titlefour1:
    .byte $00,$00
    .byte $00,$00
titlefour2:
    .byte $00,$00
    .byte $00,$00
titlefour3:
    .byte $00,$00
    .byte $00,$00

PalletteDataBlack:
    .byte $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0F
    .byte $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0F

PaletteData:
    .byte $0F,$01,$11,$21,  $0F,$01,$03,$0A,  $0F,$07,$16,$27, $0F,$05,$15,$30  ;background palette data  
    .byte $0f,$2c,$16,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data

ScreenDefault:
    .byte $24,$00,$00,$00,$00,$00,$00,$15,$15,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$13,$12,$14,$00,$11,$0C,$0D,$15,$00,$00,$00,$00,$00,$23
    .byte $24,$35,$36,$37,$38,$39,$11,$0E,$0F,$3A,$3B,$3C,$3D,$3E,$00,$23
    .byte $24,$00,$04,$05,$04,$05,$00,$10,$01,$00,$04,$05,$00,$04,$05,$23
    .byte $24,$34,$04,$05,$00,$00,$30,$30,$1D,$19,$1A,$21,$22,$19,$1A,$23
    .byte $24,$2b,$04,$05,$00,$08,$09,$00,$05,$02,$00,$02,$10,$02,$00,$23
    .byte $29,$2b,$04,$05,$00,$0A,$0B,$00,$00,$02,$00,$02,$00,$02,$40,$29
    .byte $24,$2b,$21,$22,$21,$22,$21,$21,$10,$21,$1F,$20,$3F,$00,$00,$23
    .byte $24,$33,$23,$24,$02,$00,$00,$04,$05,$00,$40,$02,$00,$3F,$00,$23
    .byte $29,$12,$25,$26,$32,$00,$03,$04,$05,$02,$00,$32,$00,$1D,$30,$23
    .byte $24,$10,$08,$09,$02,$00,$31,$04,$05,$41,$3f,$02,$00,$10,$05,$23
    .byte $24,$00,$0A,$0B,$02,$31,$03,$19,$1A,$42,$3f,$19,$1A,$00,$00,$23
    .byte $00,$00,$32,$32,$00,$00,$41,$00,$00,$43,$3f,$00,$00,$00,$00,$00
    .byte $07,$07,$39,$39,$39,$39,$2B,$3F,$3F,$2B,$3A,$3A,$3A,$3A,$07,$07
    .byte $06,$06,$06,$06,$06,$06,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06
TitleScreen:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$01,$02,$03,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

VictoryScreen:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$01,$02,$03,$04,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %00100010, %00000000, %00000000, %11000000, %00110000, %00000000, %00000000, %10001000
    .byte %00100010, %01011010, %01011010, %00001100, %00000011, %01011010, %01001000, %10011010
    .byte %00100010, %00000000, %00000000, %00010010, %00010010, %01001000, %00010010, %10001000
    .byte %00100011, %10100000, %10000000, %00100000, %10000000, %10100000, %00000000, %10101000
    .byte %00110010, %10101010, %00100100, %00000001, %00000100, %10000101, %10000000, %10101001
    .byte %00100010, %00000000, %10000000, %00000010, %00001000, %00100000, %00000100, %10001001
    .byte %10100000, %10100000, %10100000, %10000000, %00100000, %10100000, %10100000, %00000000
    .byte %10101010, %10101010, %10101010, %00000000, %00000000, %10101010, %10101010, %10101010

;;;;;;;;;;;;
;;; LOOK UP TABLES
;;;;;;;;;;;

Sin:
    .byte $01,$FF,$01,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$00
    .byte $FF,$00,$FF,$00,$FF,$00,$00,$FF,$00,$00,$FF,$00,$00,$00,$00

GameStatePath:
    .word DoTitleLogic
    .word DoMapSelectLogic
    .word DoGameLogic
    .word DoVictoryScreenLogic
    .word WaitForGameLoad

PlayerSpawnTable:
    .word NoSpawn
    .word SpawnPlayerPort1
    .word SpawnPlayerPort2
    .word SpawnPlayerPort3
    .word SpawnPlayerPort4

DestroyEntityList: ; defines behaviours for when an entity is destroyed
    .word NoDeathAction ; 0 
    .word Player1Death ; 1
    .word Player2Death ; 2
    .word Player3Death ; 3
    .word Player4Death ; 4
    .word NoDeathAction ; 5
    .word NoDeathAction ; 6
    .word NoDeathAction ; 7
    .word NoDeathAction ; 8
    .word NoDeathAction ; 9
    .word NoDeathAction ; 10
    .word NoDeathAction ; 11
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction
    .word NoDeathAction



MetaSpriteListPlayer:
    .word PlayerSpriteIdle1 ; 0
    .word PlayerSpriteIdle2 ; 1
    .word PlayerSpriteRun1 ; 2
    .word PlayerSpriteRun2 ; 3
    .word PlayerSpriteCrouch ; 4
    .word PlayerSpriteJump ; 5
    .word PlayerSpriteSlide ; 6
    .word PlayerSpriteFire ; 

MetaSpriteListPlayerWithHat:
    .word PlayerSpriteIdleHat1 ; 0
    .word PlayerSpriteIdleHat2 ; 1
    .word PlayerSpriteRunHat1 ; 2
    .word PlayerSpriteRunHat2 ; 3
    .word PlayerSpriteCrouchHat ; 4
    .word PlayerSpriteJumpHat ; 5
    .word PlayerSpriteSlideHat ; 6
    .word PlayerSpriteFireHat ; 


AnimationStringsPlayer:
    .word AnimationStringPlayerIdle
    .word AnimationStringPlayerRunning
    .word AnimationStringPlayerCrouching
    .word AnimationStringPlayerJumping
    .word AnimationStringPlayerFalling
    .word AnimationStringPlayerFallingEnd
    .word AnimationStringPlayerRunning
    .word AnimationStringPlayerFiring

MetaSpriteListPlayer2:
    .word Player2Sprite1
    .word Player2Sprite2
    .word Player2Sprite3
    .word Player2Sprite4
    .word Player2Sprite5
    .word Player2Sprite6
    .word Player2Sprite7

MetaSpriteListPlayer3:
    .word Player3Sprite1
    .word Player3Sprite2
    .word Player3Sprite3
    .word Player3Sprite4
    .word Player3Sprite5
    .word Player3Sprite6
    .word Player3Sprite7

MetaSpriteListPlayer4:
    .word Player4Sprite1
    .word Player4Sprite2
    .word Player4Sprite3
    .word Player4Sprite4
    .word Player4Sprite5
    .word Player4Sprite6
    .word Player4Sprite7
 
MetaSpriteListSlider:
    .word MetaSpriteSlider1
    .word MetaSpriteSlider2


MetaSpriteListProjectileSpell:
    .word ProjectileSpellSprite1
    .word ProjectileSpellSprite2
    .word ProjectileSpellSprite3
    .word ProjectileSpellSprite4
    .word ProjectileSpellSprite5
    .word ProjectileSpellSprite6

MetaSpriteListExplosion:
    .word ExplosionSprite1
    .word ExplosionSprite2
    .word ExplosionSprite3
    .word ExplosionSprite4


MetaSpriteListLightningEmitter:
    .word LightningEmitterSprite1 

MetaSpriteListSparks: 
    .word SparksSprite1 
    .word SparksSprite2

MetaSpriteListLightning:
    .word LightningSprite1
    .word LightningSprite2
    .word LightningSprite3
    .word LightningSprite4
    .word LightningSprite5
    .word LightningSprite6

MetaSpriteListFireball:
    .word FireballSprite1
    .word FireballSprite2
    .word FireballSprite3
    .word FireballSprite4

MetaSpriteListRespawner:
    .word RespawnerSprite1
    .word RespawnerSprite2 

MetaSpriteListPortal:
    .word PortalSprite1 
    .word PortalSprite2
    .word PortalSprite3 
    .word PortalSprite4

MetaSpriteListBroomStick:
    .word BroomStickSprite

MetaSpriteListIceBeam:
    .word IcebeamSprite1 ;0
    .word IcebeamSprite2 ;1
    .word IceCrystalSprite1   ;2
    .word IceCrystalSprite2 ;3
    .word IceCrystalSprite3 ;4

MetaSpriteListTeleporter:
    .word TeleporterSprite1
    .word TeleporterSprite2
    .word TeleporterSprite3
    .word TeleporterSprite4
    .word TeleporterSprite5
    .word TeleporterSprite6
    .word TeleporterSprite7
    .word TeleporterSprite8
    .word TeleporterSprite9

MetaSpriteListHat:
    .word HatSprite

MetaSpriteListScoreTextPlayerOne:
    .word ScoreTextPlayer1Sprite

MetaSpriteListScoreTextPlayerTwo:
    .word ScoreTextPlayer2Sprite

MetaSpriteListScoreTextPlayerThree:
    .word ScoreTextPlayer3Sprite

MetaSpriteListScoreTextPlayerFour:
    .word ScoreTextPlayer4Sprite


MetaSpriteListScoreNumber:
    .word ScoreNumber0Sprite
    .word ScoreNumber1Sprite
    .word ScoreNumber2Sprite
    .word ScoreNumber3Sprite
    .word ScoreNumber4Sprite
    .word ScoreNumber5Sprite
    .word ScoreNumber6Sprite
    .word ScoreNumber7Sprite
    .word ScoreNumber8Sprite
    .word ScoreNumber9Sprite

MetaSpriteListVerticalLaser:
    .word VerticalLaserSprite1
    .word VerticalLaserSprite2
    .word VerticalLaserSprite3


PlayerSpriteIdle1:
    .byte $00,$00,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
PlayerSpriteIdle2:
    .byte $00,$01,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteRun1:
    .byte $00,$02,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteRun2:
    .byte $00,$03,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteCrouch:
    .byte $00,$04,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteJump:
    .byte $00,$06,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteSlide:
    .byte $00,$08,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $00,$09,$00,$07
    .byte $FF ; termination byte
PlayerSpriteFire:
    .byte $00,$07,$00,$00
    .byte $FF

PlayerSpriteIdleHat1:
    .byte $00,$00,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FA,$16,$00,$00
    .byte $FF ; termination byte 
PlayerSpriteIdleHat2:
    .byte $00,$01,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FA,$16,$00,$00
    .byte $FF ; termination byte
PlayerSpriteRunHat1:
    .byte $00,$02,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteRunHat2:
    .byte $00,$03,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteCrouchHat:
    .byte $00,$04,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteJumpHat:
    .byte $00,$06,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSpriteSlideHat:
    .byte $00,$08,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $00,$09,$00,$07
    .byte $FF ; termination byte
PlayerSpriteFireHat:
    .byte $00,$07,$00,$00
    .byte $FF


AnimationStringPlayerIdle:
    .byte $00,$1A,$01,$1A,$FF ; Animation frame -> Timer FF=repeat
AnimationStringPlayerRunning:
    .byte $02,$08,$03,$08,$FF
AnimationStringPlayerCrouching:
    .byte $01 ; animation length
    .byte $04
    .byte $FF ; loop
AnimationStringPlayerJumping:
    .byte $01 ; animation frame
    .byte $05 ; ani length
    .byte $FF ; loop
AnimationStringPlayerFalling:
    .byte $00,$80,$FF
AnimationStringPlayerFallingEnd:
    .byte $04,$09,$FE
AnimationStringPlayerFiring:
    .byte $01 ; animation length
    .byte $07
    .byte $FF

Player2Sprite1:
    .byte $00,$00,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
Player2Sprite2:
    .byte $00,$01,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player2Sprite3:
    .byte $00,$02,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player2Sprite4:
    .byte $00,$03,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player2Sprite5:
    .byte $00,$04,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player2Sprite6:
    .byte $00,$05,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player2Sprite7:
    .byte $00,$06,$01,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

Player3Sprite1:
    .byte $00,$00,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
Player3Sprite2:
    .byte $00,$01,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player3Sprite3:
    .byte $00,$02,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player3Sprite4:
    .byte $00,$03,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player3Sprite5:
    .byte $00,$04,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player3Sprite6:
    .byte $00,$05,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player3Sprite7:
    .byte $00,$06,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

Player4Sprite1:
    .byte $00,$00,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
Player4Sprite2:
    .byte $00,$01,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player4Sprite3:
    .byte $00,$02,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player4Sprite4:
    .byte $00,$03,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player4Sprite5:
    .byte $00,$04,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player4Sprite6:
    .byte $00,$05,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
Player4Sprite7:
    .byte $00,$06,$03,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

NoDraw:
    .byte $FF ; termination byte
MetaSpriteSlider1:
    .byte $00,$01,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
MetaSpriteSlider2:
    .byte $00,$02,$02,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

ProjectileSpellSprite1:
    .byte $00,$10,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
ProjectileSpellSprite2:
    .byte $00,$11,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite3:
    .byte $00,$12,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite4:
    .byte $00,$13,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite5:
    .byte $00,$14,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite6:
    .byte $00,$15,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

ExplosionSprite1: 
    .byte $00,$41,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ExplosionSprite2: 
    .byte $00,$42,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ExplosionSprite3: 
    .byte $00,$43,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ExplosionSprite4: 
    .byte $00,$44,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

LightningEmitterSprite1:
    .byte $00,$C4,$00,$00
    .byte $FF

SparksSprite1:
    .byte $00,$DA,$00,$00
    .byte $FF
SparksSprite2:
    .byte $00,$DB,$00,$00
    .byte $FF

LightningSprite1:
    .byte $00,$D4,$00,$00
    .byte $FF
LightningSprite2:
    .byte $00,$D5,$00,$00
    .byte $FF
LightningSprite3:
    .byte $00,$D6,$00,$00
    .byte $FF
LightningSprite4:
    .byte $00,$D7,$00,$00
    .byte $FF
LightningSprite5:
    .byte $00,$D8,$00,$00
    .byte $FF
LightningSprite6:
    .byte $00,$D9,$00,$00
    .byte $FF

FireballSprite1:
    .byte $00,$40,$00,$00
    .byte $FF
FireballSprite2:
    .byte $00,$41,$00,$00
    .byte $FF
FireballSprite3:
    .byte $00,$42,$00,$00
    .byte $FF
FireballSprite4:
    .byte $00,$43,$00,$00
    .byte $FF

RespawnerSprite1:
    .byte $00,$E0,$00,$00
    .byte $00,$E1,$00,$08
    .byte $08,$F0,$00,$00
    .byte $08,$F1,$00,$08
    .byte $FF

RespawnerSprite2:
    .byte $00,$E2,$00,$00
    .byte $00,$E3,$00,$08
    .byte $08,$F2,$00,$00
    .byte $08,$F3,$00,$08
    .byte $FF

PortalSprite1:
    .byte $04,$B2,$00,$04
    .byte $FF 

PortalSprite2:
    .byte $00,$A0,$00,$00
    .byte $00,$A1,$00,$08
    .byte $08,$B0,$00,$00
    .byte $08,$B1,$00,$08
    .byte $FF

PortalSprite3:
    .byte $00,$C0,$00,$00
    .byte $00,$C1,$00,$08
    .byte $08,$D0,$00,$00
    .byte $08,$D1,$00,$08
    .byte $FF

PortalSprite4:
    .byte $00,$C2,$00,$00
    .byte $00,$C3,$00,$08
    .byte $08,$D2,$00,$00
    .byte $08,$D3,$00,$08
    .byte $FF

BroomStickSprite:
    .byte $00,$52,$00,$00
    .byte $00,$53,$00,$08
    .byte $00,$54,$00,$0F
    .byte $FF


IcebeamSprite1:
    .byte $00,$90,$00,$00
    .byte $FF
IcebeamSprite2:
    .byte $00,$91,$00,$00
    .byte $FF
IceCrystalSprite1:
    .byte $00,$80,$00,$00
    .byte $FF
IceCrystalSprite2:
    .byte $00,$81,$00,$00
    .byte $FF
IceCrystalSprite3:
    .byte $00,$82,$00,$00
    .byte $FF

TeleporterSprite1:
    .byte $00,$B4,$00,$00
    .byte $FF
TeleporterSprite2:
    .byte $00,$B5,$00,$00
    .byte $FF
TeleporterSprite3:
    .byte $00,$B6,$00,$00
    .byte $FF
TeleporterSprite4:
    .byte $00,$B7,$00,$00
    .byte $FF
TeleporterSprite5:
    .byte $00,$B8,$00,$00
    .byte $FF
TeleporterSprite6:
    .byte $00,$B9,$00,$00
    .byte $FF
TeleporterSprite7:
    .byte $00,$BA,$00,$00
    .byte $FF
TeleporterSprite8:
    .byte $00,$BB,$00,$00
    .byte $FF
TeleporterSprite9:
    .byte $00,$BC,$00,$00
    .byte $FF

HatSprite:
    .byte $00,$16,$00,$00
    .byte $FF

ScoreTextPlayer1Sprite:
    .byte $00,$0b,$00,$00
    .BYTE $FF

ScoreTextPlayer2Sprite:
    .byte $00,$0c,$00,$00
    .BYTE $FF

ScoreTextPlayer3Sprite:
    .byte $00,$0D,$00,$00
    .BYTE $FF

ScoreTextPlayer4Sprite:
    .byte $00,$0E,$00,$00
    .BYTE $FF

ScoreNumber0Sprite:
    .byte $00, $9F,$00,$00
    .byte $FF
ScoreNumber1Sprite:
    .byte $00, $0F,$00,$00
    .byte $FF

ScoreNumber2Sprite:
    .byte $00, $1F,$00,$00
    .byte $FF
ScoreNumber3Sprite:
    .byte $00, $2F,$00,$00
    .byte $FF
ScoreNumber4Sprite:
    .byte $00, $3F,$00,$00
    .byte $FF
ScoreNumber5Sprite:
    .byte $00, $4F,$00,$00
    .byte $FF
ScoreNumber6Sprite:
    .byte $00, $5F,$00,$00
    .byte $FF
ScoreNumber7Sprite:
    .byte $00, $6F,$00,$00
    .byte $FF
ScoreNumber8Sprite:
    .byte $00, $7F,$00,$00
    .byte $FF
ScoreNumber9Sprite:
    .byte $00, $8F,$00,$00
    .byte $FF

VerticalLaserSprite1:
    .byte $00,$92,$00,$00
    .byte $00,$93,$00,$08
    .byte $08,$A2,$00,$00
    .byte $08,$A3,$00,$08

    .byte $10,$92,$00,$00
    .byte $10,$93,$00,$08
    .byte $18,$A2,$00,$00
    .byte $18,$A3,$00,$08

    .byte $20,$92,$00,$00
    .byte $20,$93,$00,$08
    .byte $28,$A2,$00,$00
    .byte $28,$A3,$00,$08

    .byte $30,$92,$00,$00
    .byte $30,$93,$00,$08
    .byte $38,$A2,$00,$00
    .byte $38,$A3,$00,$08

    .byte $ff

VerticalLaserSprite2:
    .byte $00,$94,$00,$00
    .byte $00,$95,$00,$08
    .byte $08,$A4,$00,$00
    .byte $08,$A5,$00,$08

    .byte $10,$94,$00,$00
    .byte $10,$95,$00,$08
    .byte $18,$A4,$00,$00
    .byte $18,$A5,$00,$08

    .byte $20,$94,$00,$00
    .byte $20,$95,$00,$08
    .byte $28,$A4,$00,$00
    .byte $28,$A5,$00,$08

    .byte $30,$94,$00,$00
    .byte $30,$95,$00,$08
    .byte $38,$A4,$00,$00
    .byte $38,$A5,$00,$08
    .byte $ff

VerticalLaserSprite3:
    .byte $00,$96,$00,$00
    .byte $00,$97,$00,$08
    .byte $08,$a6,$00,$00
    .byte $08,$a7,$00,$08

    .byte $10,$96,$00,$00
    .byte $10,$97,$00,$08
    .byte $18,$a6,$00,$00
    .byte $18,$a7,$00,$08

    .byte $20,$96,$00,$00
    .byte $20,$97,$00,$08
    .byte $28,$a6,$00,$00
    .byte $28,$a7,$00,$08

    .byte $30,$96,$00,$00
    .byte $30,$97,$00,$08
    .byte $38,$a6,$00,$00
    .byte $38,$a7,$00,$08
    .byte $ff


;; Animation Strings 
AnimationPortalExpand:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $FF 

AnimationPortalWait:
    .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $FF

AnimationPortalContract:
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $FF 


JumpStrength:
    .byte $FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$01
JumpStrengthReleased:
    .byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
HeadbonkStrength:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01


SpawnerSpeedRamp:
    .byte $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$FF

.include "famistudio_ca65.s"
.include "testsong.s"


.segment "SONG1"
; 
; .segment "DPCM"


.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
    .word IRQ
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "title_screen_bank.chr"
    .incbin "castle_set-bank1.chr"
    ; .incbin "castle_set-bank2.chr"