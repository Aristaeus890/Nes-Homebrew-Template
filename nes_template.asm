; This is a template for a basic NES rom
; it uses ines 1.0, not 2.0

;FEATURES
; - Entity System
; - Bank Swapping between 2 chr banks
; - Animations 

; INes 1.0
.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $02 ; 2 * 8KB CHR ROM
.byte %00010011 ; mapper and mirroring
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte "<"
.byte "3Matt" ; filler bytes to fill out the end of the header

; FamiStudio config.
FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1 
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_USE_RELEASE_NOTES  = 1
FAMISTUDIO_DPCM_OFF           = $e000

; NESASM-specific config.
FAMISTUDIO_NESASM_ZP_RSSET  = $00b4
FAMISTUDIO_NESASM_BSS_RSSET = $300
FAMISTUDIO_NESASM_CODE_BANK = 0
FAMISTUDIO_NESASM_CODE_ORG  = $8000

.define FAMISTUDIO_CA65_ZP_SEGMENT ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT BSS
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE

; .include "famistudio_ca65.s"

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
.endscope

.scope CollisionType
    NoCollision = 0 
    LayerOne = 1
    LayerTwo = 1
    LayerThree = 1
    LayerFour = 1
.endscope

.scope PlayerState
    OnGround = $00
    Jumping = $01
    Falling = $02
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
    attributes .byte ;
    collisionlayer .byte ; which of the 4 active palettes this sprite should use
    generalpurpose .byte ; this has no specific use, it can be defined on a per entity basis
    animationframe .byte
    animationtimer .byte
.endstruct

.segment "ZEROPAGE" ; 0-FF. One page of ram that is faster access than rest of the ram. Use for values most frequently used
    MAXENTITIES =15; max allowed number of entities. Each entity takes a number of bytes in the zero page equal to the entity struct
    ; this CANNOT run over the end of the zero page or it will not work. If you want more entities, you will need to dedicate a non zero
    ; page ram segment to it
    entities: .res .sizeof(Entity) * MAXENTITIES ; 6 * 30 = 180/256 bytes
    entity_mem = .sizeof(Entity) * MAXENTITIES ; mem used

    world: .res 2 ; this is a pointer to the address of the current screen we want to fetch tiles on
    seed: .res 2 ; the seed for the pseudo rng. This will be inited to anything by 0 later
    jumppointer: .res 2 ; used to jump to specic places in memory with jump tables
    jumppointer2: .res 2
    nmidone: .res 1 ; value to check to see if the nmi is done
    scrollx: .res 1 ; how far the screen is scrolled in x dir
    scrolly: .res 1 ; how far the screen is scrolled in y dir
    var_mem: .res 4 ; sometimes we need to jump to a subroutine and do something with more data than can be juggled with x/y/a
    buttons: .res 1 ; this holds the state of player input for controller 1
    buttons_p2: .res 2 ; this holds the state of player input for controller 1
    currenttable: .res 1
    currentrowodd: .res 1 ; holds the currrent row when drawing tiles
    currentroweven: .res 1
    currentcollisionaddress: .res 2
    ButtonFlag: .res 1
    framecount: .res 1
    currentbank: .res 1
    oambufferoffset: .res 1
    vxhigh: .res 1
    vxlow: .res 1
    playerstate: .res 1
    vyhigh: .res 1
    vylow: .res 1
    playerstateP2: .res 1
    vxlowp2: .res 1
    vyhighP3: .res 1
    vylowP3: .res 1
    playerstateP3: .res 1
    vyhighP4: .res 1
    vylowP4: .res 1
    playerstateP4: .res 1
    spritebufferposition: .res 1
    temp: .res 1
    temp2: .res 1
    rng: .res 1 ; rng is stored here once a frame
    rectangle1: .res 4 
    rectangle2: .res 4

    ;While we're here, we'll define some locations that will act as buffers in memory. This could be anywhere, its just here for organisation
    SpriteBuffer = $0200 ;$0200 -> $02FF ; A page of ram that will contain sprite info that will be read to the ppu each frame
    TileBufferH = $0300 ; $0300 ->  ; Tiles that need to be written when the screen has scrolled are stored here
    CollisionMap = $0310 ; 0310 -> 0400 ; 240 byte collision map for tile collision
    CurrentBackgroundPalette = $04C0 ; -> 04CF
    SpawnerIndex = $04D0 ; -> 04D0 
    SpawnerStack = $04D1 ; -> 051D 3X15 bytes, x,y, entity
    DestructionIndex = $051E ; -> 051E  
    DestructionStack = $051F ; -> 0523 5 bytes  
    
    PPUControl = $2000 
    PPUMask= $2001 
    PPUStatus = $2002
    OAMAddress =$2003
    OAMData = $2004
    PPUAddress = $2005 
    PPUScroll = $2006
    PPUData = $2007 
    OAMDMA = $4014

    player1spawnx =20
    player1spawny =20
    player2spawnx =235
    player2spawny =20

.segment "RAM"



.segment "STARTUP" ; this is called when the console starts. Init a few things here, otherwise little here
    Reset:
        SEI ; Disables all interrupts
        CLD ; disable decimal mode (nes 6502 does not support decimals)

        ; Disable sound IRQ
        LDX #$40
        STX $4017

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

.segment "CODE" ; the bulk of code will  be placed here. This will begin running from the start once startup has finished


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

    ; $3F00
    LDA #$3F
    STA $2006
    LDA #$00
    STA PPUScroll

    LDX #$00


LoadPalettes:
    FillPaletteRam:
        LDA PaletteData, X
        STA CurrentBackgroundPalette, X 
        INX 
        CPX #$20 
        BNE FillPaletteRam
        LDX #$00 
    LoadtoPPU:
        LDA CurrentBackgroundPalette, X 
        STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
        INX
        CPX #$20
        BNE LoadtoPPU 

SetMirroring: ; TODO make sure this actually works? mmc1 is weird
    ;LDA #$80
    ;STA $8000
    LDA #%10000000
    STA $8000

    ; MMC1 is nearly unique in that to configure it you have to do serial writes to the same port

    LDA #%00000010  
    STA $8000
    LSR
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000

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
    STA PPUScroll
    LDA #$00
    STA PPUScroll

    LDX #$00
    LDY #$00
                

; LoadWorld:
;     ; note this is a quick and dirty way to fill an initial screen. It doesn't use meta tiles but an uncompressed block of bytes
;     ; LDA (world), Y ; load a byte from the data in the address stored in 'world' offset by y
;     LDA #$21
;     STA $2007 ; write that byte to the ppu. The ppu then automatically increments, so we can just write to it repeatedly
;     INY
;     CPX #$03 ; we need to write 960 bytes to the ppu, so we use X to check if we've gone through Y the required number of times
;     BNE :+
;     CPY #$E0 ; 
;     BEQ DoneLoadingWorld
; :
;     CPY #$00
;     BNE LoadWorld
;     INX
;     INC world+1
;     JMP LoadWorld

; DoneLoadingWorld:
;     LDX #$00




SetAttributes:
    LDX #$00
    LDA #$23
    STA PPUScroll
    LDA #$C0
    STA PPUScroll
    AttributeLoop:
    LDA AttributesDefault, X 
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop

    LDX #$00
    LDY #$00    

; Set Control
; to configure MMC1 mapper, we need to write to 8000 repeatedly
LDA #%00000010 ; This configures it to horizontal mirroring, 32kb of prg rom and two 8kb chr banks

STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000

; Set Bank to firstbank
LDA #%00000010

STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000

    ;                             l
    ; set up for a screen load    V

    ; LDA #< ScreenDefault ; take the low byte
    ; STA world ; store low byte in z page
    ; LDA #> ScreenDefault ; take the high  setubyte
    ; STA world+1 ; etc

    ; LDA #<CollisionMap 
    ; STA currentcollisionaddress
    ; LDA #>CollisionMap
    ; STA currentcollisionaddress+1

    ; LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    ; STA $PPUScroll
    ; LDA #$00
    ; STA $PPUScroll

    ; JSR LoadSingleScreen

    LDA #<ScreenDefault2
    STA world 
    LDA #>ScreenDefault2
    STA world +1

    LDA #<CollisionMap 
    STA currentcollisionaddress
    LDA #>CollisionMap
    STA currentcollisionaddress+1


    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUScroll
    LDA #$00
    STA PPUScroll

    JSR LoadSingleScreen


; LDY #$18
; LDX #$20
; LDA #EntityType::Player
; JSR SpawnEntity

; LDY #$18
; LDX #$30
; LDA #EntityType::Player2
; JSR SpawnEntity

; LDY #$18
; LDX #$40
; LDA #EntityType::Player3
; JSR SpawnEntity

; LDY #$18
; LDX #$50
; LDA #EntityType::Player4
; JSR SpawnEntity


LDY #$18
LDX #$20
LDA #EntityType::Player
JSR SpawnEntity


LDY #$b0
LDX #$70
LDA #EntityType::LightningEmitter
JSR SpawnEntity

; LDY #$88
; LDX #$20
; LDA #EntityType::ProjectileSpell
; JSR SpawnEntity

LDY #$50
LDX #$30
LDA #EntityType::Explosion
JSR SpawnEntity

LDY #$80
LDX #80
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$60
LDX #$20
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$40
LDX #$30
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$40
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$50
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$60
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$70
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$80
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$90
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$00
LDX #$A0
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$0B
LDX #$B0
LDA #EntityType::Slider
JSR SpawnEntity

LDY #$0A
LDX #$C0
LDA #EntityType::Slider
JSR SpawnEntity

    ; Load screen also enables interrupts and rendering to finish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    JSR Setrng
    ; LDY #$00
    ; LDX #$20
    ; LDA #EntityType::Slider
    ; JSR SpawnEntity
    JSR DoGameLogic
    
    ; INC scrollx 
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR AlternateBanks
    ; JSR DoScroll       
    JSR OAMBuffer   ; Sprite data is written to the buffer here
    ; JSR WriteToTileBuffer ; write to the tile buffer when scrolling

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
    PHA
    TXA 
    PHA 
    TYA 
    PHA

    LDA #$00
    STA PPUMask ; disable rendering before we access the ppu for safety

    ;JSR DrawColumnNMI
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  
    JSR ReadScroll  ; Send the current scroll to the ppu
    ;JSR UpdatePaletteNMI
    ;JSR LoadAttributesNMI

    LDA #%10010000
    ORA currenttable
    ORA #%00000100
    STA PPUControl ; ???

    LDA #%10011110
    STA PPUMask ; reenable rendering for the ppu
    ;JSR SoundPlayFrame
    INC nmidone 

    ; Bring the stack values back into the registers
    PLA
    TAY 
    PLA 
    TAX 
    PLA 
    
    RTI

; Loading into OAMDMA automatically takes what you give as a high byte and writes 256 bytes to the PPU (so 02 == 0200 all thr way to FF)
; In a real nes this neads to be done every frame b/c dynamic ram degradation, its technically possible to avoid in some emulators, but best just to do it. 
ReadSprites:
    LDA #$00
    STA $2003
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display automatically.
    STA OAMDMA
    LDX #$00
RTS

ReadScroll:
    SetScroll:
    LDA PPUStatus ; reading from PPUStatus sets a latch that allows you to set the scroll in PPUAddress
    LDA scrollx
    STA PPUAddress
    LDA scrolly
    STA PPUAddress
RTS


;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

DoGameLogic:
    JSR ProcessSpawnStack
    JSR ReadButtons
    JSR ProcessEntities
    JSR ProcessDestructionStack
    
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

ProcessEntities:
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X ; load the current entity type
        ASL ; transfer current entity type to y
        TAY  
        LDA ProcessEntityList, Y ; use y as a jump pointer to jump to the process function for that entity!
        STA jumppointer
        LDA ProcessEntityList+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    SkipEntity:
        JMP EntityComplete

    ProcessPlayer:
        LDA playerstate
        ASL 
        TAY 
        LDA PlayerStateMachine, Y
        STA jumppointer
        LDA PlayerStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessPlayer2:
        LDA playerstateP2
        ASL 
        TAY 
        LDA PlayerStateMachine, Y
        STA jumppointer
        LDA PlayerStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessPlayer3:
        LDA playerstateP3
        ASL 
        TAY 
        LDA PlayerStateMachine, Y
        STA jumppointer
        LDA PlayerStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessPlayer4:
        LDA playerstateP4
        ASL 
        TAY 
        LDA PlayerStateMachine, Y
        STA jumppointer
        LDA PlayerStateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessSlider:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA SliderStateMachine, Y 
        STA jumppointer
        LDA SliderStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)


    ProcessProjectileSpell:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA ProjectileSpellStateMachine, Y 
        STA jumppointer
        LDA ProjectileSpellStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)
    
    ProcessExplosion:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA ExplosionStateMachine, Y 
        STA jumppointer
        LDA ExplosionStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessLightningEmitter:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA LightningEmitterStateMachine, Y 
        STA jumppointer
        LDA LightningEmitterStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessSparks:
        ; TXA 
        ; JSR AddEntityToDestructionStack

        DEC entities+Entity::animationtimer, X
        LDA entities+Entity::animationtimer, X
        BPL :+
        LDA #$02
        STA entities+Entity::animationtimer, X
        LDA #$01 
        EOR entities+Entity::animationframe, X
        STA entities+Entity::animationframe, X
        INC entities+Entity::generalpurpose, X
        LDA entities+Entity::generalpurpose, X
        CMP #$05
        BNE :+
        TXA 
        JSR AddEntityToDestructionStack
        :
    JMP EntityComplete

    ProcessLightning:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA LightningStateMachine, Y 
        STA jumppointer
        LDA LightningStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)


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

PlayerStateMachine:
    .word PlayerInit
    .word PlayerOnFloor
    .word PlayerJumping
    .word PlayerFalling

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

SliderStateMachine:
    .word SliderInit
    .word SliderRight
    .word SliderLeft

ExplosionStateMachine:
    .word ExplosionInit
    .word ExplosionExplode

; Entity Behaviours

PlayerInit:
    LDA entities+Entity::type
    CMP #EntityType::Player
    BNE :+ 
    LDA #%00000000
    STA entities+Entity::attributes, X 
    LDA #$01 

    STA entities+Entity::collisionlayer, X 
    STA playerstate
    JMP EntityComplete
    :
    CMP #EntityType::Player2
    BNE :+ 
    LDA #%00000001
    STA entities+Entity::attributes, X 
    LDA #$01 
    STA playerstate
    JMP EntityComplete
    :
    CMP #EntityType::Player3
    BNE :+ 
    LDA #%00000010
    STA entities+Entity::attributes, X 
    LDA #$01 
    STA playerstate
    JMP EntityComplete
    :
    CMP #EntityType::Player4
    BNE :+ 
    LDA #%00000011
    STA entities+Entity::attributes, X 
    LDA #$01 
    STA playerstate
    JMP EntityComplete
    :
    JMP EntityComplete
PlayerOnFloor:
    DEC entities+Entity::animationtimer, X
    LDA entities+Entity::animationtimer, X
    BNE :+
    LDA #$20 
    STA entities+Entity::animationtimer, x
    LDA entities+Entity::animationframe, X
    EOR #$01 
    STA entities+Entity::animationframe, X
    : 
    JSR CollideDown
    BNE :+
    LDA #$03 
    STA playerstate
    STA entities+Entity::animationframe
    JMP EntityComplete
    :
    JSR CheckRight
    BEQ :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
        LDA #%01000000
        EOR entities+Entity::attributes, X 
        STA entities+Entity::attributes, X
    :
    JSR CheckLeft
    BEQ :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
        LDA #%01000000
        EOR entities+Entity::attributes, X 
        STA entities+Entity::attributes, X
    :
    JSR CheckA
    BEQ :+
    LDA #$02
    STA playerstate
    LDA #$FE
    STA vyhigh
    LDA #$00
    STA vylow
    STA entities+Entity::generalpurpose, X
    LDA #$04
    STA entities+Entity::animationframe, X
    :
    JMP EntityComplete 
PlayerJumping:
    JSR  CheckA
    LDA #$01
    STA entities+Entity::animationframe, X
    JSR CheckRight
    BEQ :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
    :
    JSR CheckLeft
    BEQ :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
    :
    LDY entities+Entity::generalpurpose, X 
    LDA JumpStrength, Y 
    CMP #$01 ; iF 01 we've reached the end of the dataset
    BNE :+
    LDA #$03 
    STA playerstate 
    JMP EntityComplete
    :
    CLC 
    ADC entities+Entity::ypos, X 
    STA entities+Entity::ypos, X
    INC entities+Entity::generalpurpose, X 
    JMP EntityComplete
PlayerFalling:
    LDA #$00 
    STA entities+Entity::animationframe, X
    JSR CheckRight
    BEQ :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
    :
    JSR CheckLeft
    BEQ :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
    :
    JSR CollideDown
    BEQ :+
        LDA #$01 
        STA playerstate
        STA entities+Entity::animationframe
        JMP EntityComplete
    :    
    LDA vylow
    CLC 
    ADC #$10
    STA vylow
    LDA vyhigh
    ADC $00
    CMP #$02
    BCC :+
    LDA #$02 
    :
    STA vyhigh
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X
    
    JMP EntityComplete

ProjectileSpellMovingLeft:
    JSR CollideLeft
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
    JSR AddEntityToDestructionStack
    TXA 
    JSR AddEntityToDestructionStack

    ; JSR CollideRight
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

SliderInit:
    LDA #%00000001
    STA entities+Entity::attributes, X 
    LDA #$08
    STA entities+Entity::animationtimer, X
    LDA #$01 
    LDA #%00000001
    STA entities+Entity::collisionlayer, X 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

SliderRight: 
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01
    EOR entities+ Entity::animationframe, X
    STA entities+ Entity::animationframe, X
    LDA #$0A 
    STA entities+ Entity::animationtimer, X
    :
    JSR CollideRight
    BEQ :+ 
    LDA #$02
    STA entities+Entity::generalpurpose, X
    LDA entities+Entity::attributes, X
    EOR #%01000000
    STA entities+Entity::attributes, X 
    JMP EntityComplete
    :
    INC entities+Entity::xpos, X 
    JSR CollideDown
    BNE :+
    INC entities+Entity::ypos, X 
    :
    JMP EntityComplete
SliderLeft:
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$01
    EOR entities+ Entity::animationframe, X
    STA entities+ Entity::animationframe, X
    LDA #$0A 
    STA entities+ Entity::animationtimer, X
    :
    JSR CollideLeft
    BEQ :+ 
    LDA #$01
    STA entities+Entity::generalpurpose, X
    LDA entities+Entity::attributes, X
    EOR #%01000000
    STA entities+Entity::attributes, X 
    JMP EntityComplete
    :
    DEC entities+Entity::xpos, X 
    JSR CollideDown
    BNE :+
    INC entities+Entity::ypos, X 
    :
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


ClearEntity:
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

ApplyGravity:
    LDA vxhigh
    BCC DoGravDown
    JSR CollideUp
    BNE :+
    STA vyhigh
    STA vylow
    RTS 
    :
    DoGravDown:
        JSR CollideDown
        BNE EndGravity
        LDA vylow
        CLC 
        ADC #$10
        STA vylow
        LDA vyhigh
        ADC #$00
        CMP #$02 
        BCC :+
        LDA #$02
        :
        LDA #$01
        STA vyhigh
        ADC entities+Entity::ypos, X 
        STA entities+Entity::ypos, X 
        RTS 
EndGravity:
    LDA #$00 
    STA vyhigh
    STA vylow
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

 

EndScreenLoad:
    ; LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
    ; STA PPUControl
    ; ; Enabling sprites and background for left-most 8 pixels
    ; ; Enable sprites and background
    ; LDA #%001`11110
    ; STA $PPUMask

LDA #%10110000 ; enable NMI, change background to use second chr set of tiles ($1000)
STA PPUControl
; Enabling sprites and background for left-most 8 pixels
; Enable sprites and background
LDA #%00111110
STA PPUMask
CLI
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
        LDA #$00
        STA entities+Entity::generalpurpose, X 
        STA entities+Entity::animationtimer, X 
        STA entities+Entity::collisionlayer, X 
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

AlternateBanks:
    LDA framecount
    BEQ :+
    RTS
    :
    LDA currentbank
    EOR #$02 
    STA currentbank 
    JSR SetBank
    RTS

SetBank: ; sets A as the bank to be used
; lower 5 bits. lowest ignored in 8kb mode

    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
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
    ; Ping $4016 with a 1 to get it ready to send buttons 
    LDA #$01
    STA $4016
    STA buttons        ; Put 1 into buttons so we can use it to manipulate the carry flag in 8 loops time
    ROR A ; sets the carry
    STA $4016

    ;TODO make this a loop by slotting a one back into the carry flag after the 8th loop
    ButtonLoop:
        LDA $4016
        ROR 
        ROL buttons ; ror + rol moves the 
    
        BCC ButtonLoop ; after 8 loops, the 1 will shift back into the carry and end the loop

    CLC 
    LDA #$01
    STA $4017
    STA buttons_p2
    ROR 
    STA $4017

    ButtonLoopP2:
        LDA $4016
        ROR 
        ROL buttons_p2
        BCC ButtonLoopP2
RTS


CheckA:
    LDA buttons 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckARelease
    LDA ButtonFlag ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA ButtonFlag
    LDA #$01 
    RTS

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA ButtonFlag
        AND #$01
        BEQ :+
        LDA ButtonFlag
        EOR #$01 
        STA ButtonFlag
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA ButtonFlag
    ORA #$02
    STA ButtonFlag
    LDA ButtonReturn::Press
    RTS

    CheckBRelease:
        LDA ButtonFlag
        AND #$02
        BEQ :+
        LDA ButtonFlag
        EOR #$02 
        STA ButtonFlag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckSelectRelease 
    LDA ButtonFlag
    ORA #$04 
    STA ButtonFlag    
    LDA ButtonReturn::Press
    RTS
    CheckSelectRelease:
        LDA ButtonFlag
        AND #$04
        BEQ :+
        LDA ButtonFlag
        EOR #$04 
        STA ButtonFlag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA ButtonFlag
    ORA #$08
    STA ButtonFlag
    LDA ButtonReturn::Press
    RTS
    CheckStartRelease:
        LDA ButtonFlag
        AND #$08
        BEQ :+
        LDA ButtonFlag
        EOR #$08 
        STA ButtonFlag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA ButtonFlag
    ORA #$10
    STA ButtonFlag
    LDA ButtonReturn::Press
    RTS
    CheckUpRelease:
        LDA ButtonFlag
        AND #$10
        BEQ :+
        LDA ButtonFlag
        EOR #$10 
        STA ButtonFlag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA ButtonFlag 
    ORA #$20 
    STA ButtonFlag 
    LDA ButtonReturn::Press
    RTS
    CheckDownRelease:
        LDA ButtonFlag
        AND #$20
        BEQ :+
        LDA ButtonFlag
        EOR #$20 
        STA ButtonFlag
        LDA ButtonReturn::Release
        RTS 
:
LDA ButtonReturn::NoPress
RTS 

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckLeftRelease
    LDA ButtonFlag
    ORA #$40 
    STA ButtonFlag 
    LDA #$01
    RTS
    CheckLeftRelease:
        LDA ButtonFlag
        AND #$04
        BEQ :+
        LDA ButtonFlag
        EOR #$04 
        STA ButtonFlag
        LDA #$02
        RTS 
:
LDA #$00
RTS 

CheckRight:

    LDA buttons
    AND #%00000001
    BEQ CheckRightRelease
    LDA ButtonFlag 
    ORA #$80 
    STA ButtonFlag
    LDA #$01
    RTS
    CheckRightRelease:
        LDA ButtonFlag
        AND #$80
        BEQ :+
        LDA ButtonFlag
        EOR #$80 
        STA ButtonFlag
        LDA #$02
        RTS 
:
LDA #$00
RTS 

EndButtons:
RTS

InputA:
    JSR CollideDown
    BEQ EndInputA
    EndInputA:
        RTS 
InputB:
    RTS 
InputUp:
    RTS 
InputDown:
    RTS 
InputLeft:
    LDA vxlow ; load the subpixel speed
    SEC
    SBC #$10 ; add player speed
    STA vxlow ; store new subpixel
    LDA vxhigh
    SBC #$00 ; add without clearing the carry. If vx low overflowed the true PX val goes up by 1
    CMP #$FE ; compare to max speed
    BCC :+ ; if speed is lower than max speed jump forward
    LDA #$FE ; else load max speed
    :
    STA vxhigh ; store true pixel speed
    ADC entities+Entity::xpos, X ; add speed to current entity pos
    STA entities+Entity::xpos, X
    STA entities+Entity::xpos, X

    LDA vxlow ; apply friction
    CLC 
    ADC #$20
    STA vxlow
    BCC :+
    INC vxhigh
    :
RTS 

InputRight:
    LDA vxlow ; load the subpixel speed
    CLC 
    ADC #$20 ; add player speed
    STA vxlow ; store new subpixel
    LDA vxhigh
    ADC #$00 ; add without clearing the carry. If vx low overflowed the true PX val goes up by 1
    CMP #$01 ; compare to max speed
    BCS :+ ; if speed is lower than max speed jump forward
    LDA #$02 ; else load max speed
    :
    STA vxhigh ; store true pixel speed
    ; add speed to current entity pos
    ADC entities+Entity::xpos, X
    STA entities+Entity::xpos, X

    LDA vxlow ; apply friction
    SEC 
    SBC #$20
    STA vxlow
    BCS :+
    DEC vxhigh
    :
RTS 



InputStart:
    RTS 
InputSelect:
    RTS
InputARelease:
    RTS
InputBRelease:
    RTS 
InputUpRelease:
    RTS
InputDownRelease:
    RTS
InputLeftRelease:
    RTS
InputRightRelease:
    RTS
InputStartRelease:
    RTS 
InputSelectRelease:
    RTS

;;;;;;;;
; Collision
;;;;;;;;

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

CollideDown:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 ; get the centre of the sprite(ish)
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
    TXA 
    PHA 
    STA temp 
    ; compare collision bits
    LDX #$00
    LDA temp2
    BEQ CollideSpriteComplete
    SpriteCollideLoop:
    LDA entities+Entity::collisionlayer, X
    BEQ CollideSpriteComplete
    EOR temp2
    AND #%00000001
    BEQ :+
    AND #%00000010
    BEQ :+
    AND #%00000100
    BEQ :+
    AND #%00001000
    BEQ :+
    JMP CollideSpriteComplete
    :

    CPX temp 
    BEQ CollideSpriteComplete
    LDA entities+Entity::type, X 
    BEQ CollideSpriteComplete

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
    LDA #$00
    RTS

NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

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
        ASL 
        TAY 
        LDA DrawSpriteList, Y
        STA jumppointer
        LDA DrawSpriteList+1, Y  
        STA jumppointer+1
        JMP (jumppointer)

    DrawSpriteInit:
        LDY #$00
    DrawSpriteLoop:  
        LDA jumppointer, Y

        ; get y offset 
        LDA (jumppointer), Y
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

; Draw list 
SkipDraw:
    JMP CheckEndSpriteDraw

DrawPlayer1:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListPlayer, Y 
    STA jumppointer
    LDA MetaSpriteListPlayer+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawPlayer2:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListPlayer2, Y 
    STA jumppointer
    LDA MetaSpriteListPlayer2+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawPlayer3:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListPlayer3, Y 
    STA jumppointer
    LDA MetaSpriteListPlayer3+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawPlayer4:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListPlayer4, Y 
    STA jumppointer
    LDA MetaSpriteListPlayer4+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawSlider:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListSlider, Y 
    STA jumppointer
    LDA MetaSpriteListSlider+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawProjectileSpell:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListProjectileSpell, Y 
    STA jumppointer
    LDA MetaSpriteListProjectileSpell+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawExplosion:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListExplosion, Y 
    STA jumppointer
    LDA MetaSpriteListExplosion+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawLightningEmitter:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListLightningEmitter, Y 
    STA jumppointer
    LDA MetaSpriteListLightningEmitter+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawLightning:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListLightning, Y 
    STA jumppointer
    LDA MetaSpriteListLightning+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawSparks:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListSparks, Y 
    STA jumppointer
    LDA MetaSpriteListSparks+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

;;;;;;;;
;; DEATH FUNCTIONS
;; These are all called from ProcessDestructionStack and MUST return there when they finish
;;;;;;;';'

NoDeathAction:
    PLA 
    PLA 
    PLA
    JMP ProcessDestructionStack
RespawnPlayer:
    PLA 
    PLA 
    PLA

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


;;;;;;;;;;;;H

;;;; TILE HANDLING
;;;;;;;;;;;;




; Meta tile definitions. The first 4 bytes refer to tiles in chr rom
; The 5th byte is the collision for the block. 0=no collide 1 = collide
brick:
    .byte $00,$01,$10,$11 ; chr rom reference bytes
    .byte $00 ; collision byte
brick_hole:
    .byte $02,$03,$12,$13
    .byte $00
brick_dark_left:
    .byte $04,$05,$14,$15
    .byte $00
brick_dark_right:
    .byte $06,$07
    .byte $16,$17
    .byte $00
brick_bright_left:
    .byte $08,$09
    .byte $18,$19
    .byte $00 
brick_bright_right:
    .byte $0A,$0B
    .byte $1A,$1B
    .byte $00
earth:
    .byte $20,$21
    .byte $30,$31
    .byte $01
earth_top:
    .byte $22,$23
    .byte $32,$33
    .byte $01
arch_tl:
    .byte $0C,$0D
    .byte $1C,$1D
    .byte $00
arch_tr:
    .byte $0E,$0F
    .byte $1E,$1F
    .byte $00
arch_bl:
    .byte $2C,$2D
    .byte $3C,$3D
    .byte $00
arch_br:    
    .byte $2E,$2F
    .byte $3E,$3F
    .byte $00
moon_tl:
    .byte $40,$41
    .byte $50,$51
    .byte $00
moon_tr:
    .byte $42,$43
    .byte $52,$53
    .byte $00
moon_bl:
    .byte $60,$61
    .byte $70,$71
    .byte $00
moon_br:
    .byte $62,$63
    .byte $72,$73
    .byte $00
crack_v:
    .byte $44
    .byte $45
    .byte $54
    .byte $55
    .byte $00
crack_h:
    .byte $64
    .byte $65 
    .byte $74
    .byte $75
    .byte $00
crack:
    .byte $46
    .byte $47
    .byte $56
    .byte $57
    .byte $00
brick_lip_l:
    .byte $28
    .byte $29
    .byte $00
    .byte $01
    .byte $00
brick_lip_r:
    .byte $2A
    .byte $2B 
    .byte $10
    .byte $11
    .byte $00
brick_bulge_l:
    .byte $28
    .byte $29
    .byte $38
    .byte $39
    .byte $00
brick_bulge_r:
    .byte $2A
    .byte $2B
    .BYTE $3A 
    .BYTE $3B
    .byte $00
water_l:
    .byte $4C
    .byte $4D
    .BYTE $5C 
    .BYTE $5D
    .byte $01
 water_r:
    .byte $4E
    .byte $4F
    .BYTE $5E 
    .BYTE $5F
    .byte $01 
window_l:
    .byte $6C
    .byte $6D
    .BYTE $7C 
    .BYTE $7D
    .byte $00    
window_r:
    .byte $6E
    .byte $6F
    .BYTE $7E 
    .BYTE $7F 
    .byte $00
bars_l:
    .byte $8C
    .byte $8D
    .BYTE $9C 
    .BYTE $9D
    .byte $00
bars_r:
    .byte $8E,$8F
    .BYTE $9E,$9F
    .byte $00
floor:
    .byte $8A,$8B
    .byte $9A,$9B
    .byte $01     
floor_no_collide:
    .byte $88,$89 
    .byte $98,$99 
    .byte $00
floor_l:
    .byte $68,$69
    .byte $78,$79
    .byte $01
floor_r:
    .byte $6A,$6B
    .byte $7A,$7B
    .byte $01
floor_corner_l:
    .byte $A8,$A9 
    .byte $B8,$B9
    .byte $01 
floor_corner_r:
    .byte $AA,$AB 
    .byte $BA,$BB
    .byte $01 
floor_side_l:
    .byte $C8,$C9 
    .byte $D8,$D9 
    .byte $01 
floor_side_r:
    .byte $CA,$CB 
    .byte $DA,$DB
    .byte $01 
floor_corner_bot_l:
    .byte $E8,$E9 
    .byte $F8,$F9
    .byte $01 
floor_corner_bot_r:
    .byte $EA,$EB 
    .byte $FA,$FB
    .byte $01 
floor_corner_bot_mid:
    .byte $E9,$EA 
    .byte $F9,$FA
    .byte $01 
floor_corner_bot:
    .byte $E6,$E7 
    .byte $F6,$F7
    .byte $01  
candles:
    .BYTE $A6,$A7 
    .byte $B6,$B7
    .byte $00 
brazier:
    .BYTE $A4,$A5 
    .byte $B4,$B5
    .byte $00
pal1:
    .byte $EE,$EE
    .byte $EE,$EE
    .byte $00
pal2:
    .byte $EF,$EF
    .byte $EF,$EF
    .byte $00
pal3:
    .byte $FE,$FE
    .byte $FE,$FE
    .byte $00
pal4:
    .byte $FF,$FF
    .byte $FF,$FF
    .byte $00
brick_divided:
    .byte $26, $27 
    .byte $36, $37
    .byte $00
floor_middle:
    .byte $A9,$AA
    .byte $B9,$BA
    .byte $01
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

PaletteData:
    .byte $0F,$01,$11,$21,  $0F,$01,$03,$0A,  $0F,$16,$07,$27, $0F,$05,$15,$30  ;background palette data  
    .byte $0F,$17,$27,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data

ScreenDefault: ; the  format of a screen is that each byte represents 1 meta tile, made up of 4 8x8 pixel blocks to save huge
; amounts ofbytes in the long run
    .byte $02,$1B,$00,$03,$02,$00,$00,$03,$02,$00,$00,$03,$02,$00,$00,$03
    .byte $02,$00,$1C,$03,$02,$00,$00,$03,$02,$00,$12,$03,$11,$0C,$00,$03
    .byte $02,$00,$00,$03,$02,$12,$1C,$03,$02,$00,$12,$03,$11,$0E,$0F,$03
    .byte $02,$00,$00,$03,$02,$12,$1C,$03,$02,$00,$00,$03,$02,$10,$11,$03
    .byte $02,$00,$00,$03,$02,$10,$00,$03,$02,$19,$1A,$03,$02,$19,$1A,$03
    .byte $02,$13,$14,$03,$02,$13,$14,$03,$02,$13,$14,$03,$02,$13,$14,$03
    .byte $02,$05,$04,$03,$02,$04,$05,$03,$02,$04,$05,$03,$10,$05,$04,$03
    .byte $11,$05,$04,$03,$02,$04,$05,$03,$02,$04,$05,$03,$02,$05,$04,$03
    .byte $00,$03,$02,$02,$03,$03,$02,$02,$03,$03,$02,$02,$03,$03,$02,$02
    .byte $03,$05,$04,$02,$03,$04,$05,$02,$03,$04,$05,$02,$03,$05,$04,$02
    .byte $03,$05,$04,$02,$03,$04,$05,$02,$03,$04,$05,$02,$03,$05,$04,$02
    .byte $03,$05,$04,$08,$09,$04,$05,$19,$1A,$04,$05,$19,$1A,$05,$04,$02
    .byte $03,$05,$04,$0A,$0B,$04,$05,$02,$03,$04,$05,$02,$03,$05,$04,$02
    .byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$18,$17,$18,$07
    .byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06

ScreenDefault2:
    .byte $24,$00,$00,$00,$00,$00,$00,$15,$15,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$13,$12,$14,$00,$11,$0C,$0D,$15,$0,$00,$00,$00,$00,$23
    .byte $24,$11,$21,$30,$30,$22,$11,$0E,$0F,$15,$21,$22,$1C,$21,$22,$23
    .byte $24,$00,$04,$05,$04,$05,$00,$10,$01,$00,$04,$05,$00,$04,$05,$23
    .byte $24,$00,$04,$05,$00,$00,$1D,$1B,$1D,$19,$1A,$21,$22,$19,$1A,$23
    .byte $24,$00,$04,$05,$00,$08,$09,$00,$05,$02,$00,$02,$10,$02,$00,$23
    .byte $29,$00,$04,$05,$00,$0A,$0B,$00,$00,$02,$00,$02,$00,$02,$00,$29
    .byte $24,$00,$21,$22,$00,$22,$21,$00,$10,$21,$1F,$20,$00,$00,$21,$23
    .byte $24,$12,$23,$24,$02,$00,$00,$04,$05,$00,$00,$02,$00,$00,$00,$23
    .byte $29,$12,$25,$26,$02,$00,$03,$04,$05,$02,$00,$02,$00,$1D,$30,$23
    .byte $24,$10,$08,$09,$02,$00,$03,$04,$05,$02,$00,$02,$00,$10,$05,$23
    .byte $24,$00,$0A,$0B,$02,$00,$03,$19,$1A,$02,$00,$19,$1A,$00,$00,$23
    .byte $24,$00,$1F,$20,$00,$00,$1F,$20,$1F,$20,$00,$00,$1F,$20,$00,$23
    .byte $29,$2F,$04,$04,$05,$2F,$05,$05,$04,$04,$2F,$2F,$05,$04,$2F,$29
    .byte $07,$1D,$1E,$1E,$1D,$1D,$1E,$1E,$1E,$1E,$1D,$1D,$1E,$1E,$1D,$07

AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %00100010, %00000000, %00000000, %11000000, %00110000, %00000000, %00000000, %10001000
    .byte %00100010, %01011010, %01011010, %00001100, %00000011, %01011010, %01001000, %10011010
    .byte %00100010, %00000000, %00000000, %00010010, %00010010, %01001000, %00010010, %10001000
    .byte %00100011, %10100000, %10000000, %00100000, %10000000, %10100000, %00000000, %10101000
    .byte %00110010, %10101010, %00000100, %00000001, %00000100, %00000101, %00000000, %10101001
    .byte %00100010, %00000000, %00000000, %00000000, %00000000, %00000000, %00000100, %10001001
    .byte %00110010, %01011010, %01010000, %01011010, %01011010, %01010000, %01011010, %10001000
    .byte %00101010, %00000000, %10101010, %00000000, %00000000, %10101010, %00000000, %10001010

;;;;;;;;;;;;
;;; LOOK UP TABLES
;;;;;;;;;;;

DestroyEntityList: ; defines behaviours for when an entity is destroyed
    .word NoDeathAction ; 0 
    .word RespawnPlayer ; 1
    .word RespawnPlayer ; 2
    .word RespawnPlayer ; 3
    .word RespawnPlayer ; 4
    .word DeathExplosion ; 5
    .word NoDeathAction ; 6
    .word NoDeathAction ; 7
    .word NoDeathAction ; 8
    .word NoDeathAction ; 9
    .word NoDeathAction ; 10
    .word NoDeathAction ; 11


ProcessEntityList: ; Jump table for processing entities
    .word SkipEntity
    .word ProcessPlayer
    .word ProcessPlayer2
    .word ProcessPlayer3
    .word ProcessPlayer4
    .word ProcessSlider
    .word ProcessProjectileSpell
    .word ProcessExplosion
    .word ProcessLightningEmitter
    .word ProcessSparks 
    .word ProcessLightning 

DrawSpriteList: ; this is a list of sprite definitions
    .word SkipDraw
    .word DrawPlayer1
    .word DrawPlayer2
    .word DrawPlayer3
    .word DrawPlayer4
    .word DrawSlider
    .word DrawProjectileSpell
    .word DrawExplosion
    .word DrawLightningEmitter 
    .word DrawSparks
    .word DrawLightning

MetaSpriteListPlayer:
    .word PlayerSprite1
    .word PlayerSprite2
    .word PlayerSprite3
    .word PlayerSprite4
    .word PlayerSprite5
    .word PlayerSprite6
    .word PlayerSprite7

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

PlayerSprite1:
    .byte $00,$00,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
PlayerSprite2:
    .byte $00,$01,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSprite3:
    .byte $00,$02,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSprite4:
    .byte $00,$03,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSprite5:
    .byte $00,$04,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSprite6:
    .byte $00,$05,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
PlayerSprite7:
    .byte $00,$06,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte

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

JumpStrength:
    .byte $FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$00,$00,$00,$01

.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "castle_set-bank1.chr"
    .incbin "castle_set-bank2.chr"