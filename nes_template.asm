; This is a template for a basic NES rom
; it uses ines 1.0, not 2.0


; INes 1.0
.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $02 ; 2 * 8KB CHR ROMF
.byte %00010011 ; mapper and mirroring
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte $00 ; unused extension
.byte "<"
.byte "3Matt" ; filler bytes to fill out the end of the header

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
.endscope

.scope Banktype
    GameBank = 0
    TitleBank = 1
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
    buttonsp2: .res 1 ; this holds the state of player input for controller 1
    buttonsp3: .res 1
    buttonsp4: .res 1
    buttonflag: .res 1
    buttonflagp2: .res 1
    buttonflagp3: .res 1 
    buttonflagp4: .res 1 
    currenttable: .res 1
    currentrowodd: .res 1 ; holds the currrent row when drawing tiles
    currentroweven: .res 1
    currentcollisionaddress: .res 2
    
    framecount: .res 1
    currentbank: .res 1
    oambufferoffset: .res 1
    animationtrack: .res 1
    animationtracktimer: .res 1
    xposlow: .res 1
    yposlow: .res 1
    xposlowp2: .res 1
    yposlowp2: .res 1
    vxhigh: .res 1
    vxlow: .res 1
    vyhigh: .res 1 
    vylow: .res 1
    xdir: .res 1
    playerstate: .res 1
    projectilecountp1: .res 1
    projectilecooldownp1: .res 1
    vxlowp2: .res 1
    vxhighp2: .res 1
    vylowp2: .res 1
    vyhighp2: .res 1
    playerstatep2: .res 1
    projectilecountp2: .res 1 
    projectilecooldownp2: .res 1
    vxhighp3: .res 1
    vxlowp3: .res 1
    vyhighp3: .res 1
    vylowp3: .res 1
    playerstatep3: .res 1
    vxhighp4: .res 1
    vxlowp4: .res 1
    vyhighp4: .res 1
    vylowp4: .res 1
    playerstatep4: .res 1
    spritebufferposition: .res 1
    temp: .res 1
    temp2: .res 1
    rng: .res 1 ; rng is stored here once a frame
    rectangle1: .res 4 
    rectangle2: .res 4
    drawflags: .res 1 ; Change Bank ;

    PPUControl = $2000 
    PPUMask= $2001 
    PPUStatus = $2002
    OAMAddress =$2003
    OAMData = $2004
    PPUScroll = $2005
    PPUAddress = $2006 
    PPUData = $2007 
    OAMDMA = $4014

.segment "OAM"
SpriteBuffer: .res 256        ; sprite OAM data to be uploaded by DMA
; SpriteBuffer = $0200

.segment "RAM"
    ; SpriteBuffer: .res 256
    TileBufferH: .res 16
    CollisionMap: .res 240
    CurrentBackgroundPalette: .res 16
    SpawnerIndex: .res 1
    SpawnerStack: .res 16
    DestructionIndex: .res 1
    DestructionStack: .res 5 
    PlayerSpawnIndex: .res 1
    PlayerSpawnStack: .res 4
    ScoreP1: .res 1
    ScoreP2: .res 1
    ScoreP3: .res 1
    ScoreP4: .res 1
    GameTimer: .res 1
    GameTimerDisplay: .res 4
    GameTimerBuffer: .res 4
    GameState: .res 1
    ; SpriteBuffer = $0200 ;$0200 -> $02FF ; A page of ram that will contain sprite info that will be read to the ppu each frame
    ; TileBufferH = $0400 ; $0300 ->  ; Tiles that need to be written when the screen has scrolled are stored here
    ; CollisionMap = $0410 ; 0310 -> 0400 ; 240 byte collision map for tile collision
    ; CurrentBackgroundPalette = $05C0 ; -> 04CF
    ; SpawnerIndex = $05D0 ; -> 04D0 
    ; SpawnerStack = $05D1 ; -> 051D 3X15 bytes, x,y, entity
    ; DestructionIndex = $061E ; -> 051E  
    ; DestructionStack = $061F ; -> 0523 5 bytes  
    ; PlayerSpawnIndex = $0624
    ; PlayerSpawnStack = $0625 ; -> 0528 4 bytes



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
; ; Constants 
; .scope TileNumbers
; NUMZERO = #F0
; NUMONE = #F1
; NUMTWO = #F2
; NUMTHREE = #F3
; NUMFOUR = #F4
; NUMFIVE = #F5
; NUMSIX = #F6
; NUMSEVEN = #F7
; NUMEIGHT = #F8
; NUMNINE = #F9
; NUMCOLON = #FB
; .endscope


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

    LDA #<ScreenDefault2
    STA world 
    LDA #>ScreenDefault2
    STA world +1

    LDA #<CollisionMap 
    STA currentcollisionaddress
    LDA #>CollisionMap
    STA currentcollisionaddress+1


    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA PPUAddress
    LDA #$00
    STA PPUAddress

    JSR LoadSingleScreen

    LDA #$00
    STA currentbank
    JSR SetBank
    ; JSR SetMirroring

LDA #$04
JSR AddEntityToPlayerSpawnStack

LDA #$03
JSR AddEntityToPlayerSpawnStack
LDA #$02
JSR AddEntityToPlayerSpawnStack
LDA #$01
JSR AddEntityToPlayerSpawnStack
LDX #$50
LDY #$40 



ldx #<music_data_untitled
ldy #>music_data_untitled
        
lda #$01 ; NTSC
jsr famistudio_init
lda #$00
jsr famistudio_music_play

LDA #$09
STA GameTimerDisplay
LDA #$09
STA GameTimerDisplay+1
LDA #$09
STA GameTimerDisplay+2
LDA #$09
STA GameTimerDisplay+3
LDA #$09




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    JSR ManageGameState
    JSR SelectGameStatePath
    JSR Setrng
    ; DEC scrolly
    
    ; INC scrollx 
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    ; JSR CheckForBankSwap       
    JSR OAMBuffer   ; Sprite data is written to the buffer here
    JSR famistudio_update
    
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
    LDA #%11000000
    STA PPUControl

    ;JSR DrawColumnNMI
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  

    ;Clock Update
    LDA PPUStatus
    LDA #$23
    STA PPUAddress
    LDA #$6D
    STA PPUAddress
    LDA GameTimerBuffer
    STA PPUData
    LDA GameTimerBuffer+1
    STA PPUData
    LDA GameTimerBuffer+2
    STA PPUData
    LDA GameTimerBuffer+3
    STA PPUData

    ; JSR UpdateClockNMI
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
    STA PPUScroll
    LDA scrolly
    STA PPUScroll
RTS

UpdateClockNMI:
    LDA PPUStatus
    LDA #$23
    STA PPUAddress
    LDA #$6D
    STA PPUAddress
    LDA GameTimerBuffer
    STA PPUData
    LDA GameTimerBuffer+1
    STA PPUData
    LDA GameTimerBuffer+2
    STA PPUData
    LDA GameTimerBuffer+3
    STA PPUData
    RTS

;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

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

DoGameLogic:
    JSR ProcessSpawnStack
    JSR ReadButtons
    JSR ProcessEntities
    JSR ProcessDestructionStack
    JSR ProcessPlayerSpawnStack
    RTS 

DecrementTimers:
    ; Do these every frame 
    ; LDA projectilecooldownp1
    ; BEQ :+
    ;     DEC projectilecooldownp1
    ; :
 
   ; RTS
    ; do these once per second 
    ; LDA framecount
    ; BEQ :+
    ; RTS 
    ; :
    ; DEC GameTimer

    LDA GameTimerDisplay+3
    SEC 
    SBC #$01
    STA GameTimerDisplay+3

    BCS ConvertTimer

    LDA #$09
    STA GameTimerDisplay+3
    LDA GameTimerDisplay+2
    SEC 
    SBC #$01
    STA GameTimerDisplay+2
   
    BCS ConvertTimer 

    LDA #$06
    STA GameTimerDisplay+2
    LDA GameTimerDisplay+1
    SEC 
    SBC #$01
    STA GameTimerDisplay+1

    BCS ConvertTimer 

    ; LDA #$09
    ; STA GameTimerDisplay+1
    ; LDA GameTimerDisplay
    ; SEC 
    ; SBC #$01
    ; STA GameTimerDisplay

    ; BCS ConvertTimer

    ; LDA #$09
    ; STA GameTimerDisplay

    ConvertTimer:
        LDA GameTimerDisplay
        CLC 
        ADC #$F0
        STA GameTimerBuffer

        LDA GameTimerDisplay+1
        CLC 
        ADC #$F0
        STA GameTimerBuffer+1

        LDA GameTimerDisplay+2
        CLC 
        ADC #$F0
        STA GameTimerBuffer+2

        LDA GameTimerDisplay+3
        CLC 
        ADC #$F0
        STA GameTimerBuffer+3



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
    LDX PlayerSpawnIndex
    BEQ EndProcessPlayerSpawnStack
    PlayerSpawnStackLoop:
    DEC PlayerSpawnIndex
    LDA PlayerSpawnIndex, X  
    ASL 
    TAX
    LDA PlayerSpawnTable, X
    STA jumppointer
    LDA PlayerSpawnTable+1, X
    STA jumppointer+1
    JMP (jumppointer)

EndProcessPlayerSpawnStack:
    RTS

NoSpawn:
    RTS
; we never go here

SpawnPlayerPort1:
    LDX #$10
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
        LDA playerstatep2
        ASL 
        TAY 
        LDA Player2StateMachine, Y
        STA jumppointer
        LDA Player2StateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessPlayer3:
        LDA playerstatep3
        ASL 
        TAY 
        LDA Player3StateMachine, Y
        STA jumppointer
        LDA Player3StateMachine+1, Y
        STA jumppointer+1
        JMP (jumppointer)
    ProcessPlayer4:
        LDA playerstatep4
        ASL 
        TAY 
        LDA Player4StateMachine, Y
        STA jumppointer
        LDA Player4StateMachine+1, Y
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

    ProcessFireball:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA FireBallStateMachine, Y 
        STA jumppointer
        LDA FireBallStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)
    
    ProcessRespawner:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA RespawnerStateMachine, Y 
        STA jumppointer
        LDA RespawnerStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessPortal:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA RespawnerPortalStateMachine, Y 
        STA jumppointer
        LDA RespawnerPortalStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessBroomStick:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA RespawnerBroomStickStateMachine, Y 
        STA jumppointer
        LDA RespawnerBroomStickStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessIceBeam:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA IceBeamStateMachine, Y 
        STA jumppointer
        LDA IceBeamStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessTeleporter:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA TeleportStateMachine, Y 
        STA jumppointer
        LDA TeleportStateMachine+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessTeleporterP2:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA TeleportStateMachineP2, Y 
        STA jumppointer
        LDA TeleportStateMachineP2+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessTeleporterP3:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA TeleportStateMachineP3, Y 
        STA jumppointer
        LDA TeleportStateMachineP3+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessTeleporterP4:
        LDA entities+Entity::generalpurpose, X
        ASL 
        TAY 
        LDA TeleportStateMachineP4, Y 
        STA jumppointer
        LDA TeleportStateMachineP4+1, Y 
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
    .word PlayerHeadbonk
    .word PlayerJumpReleased
    .word PlayerFalling
    ; .word PlayerMoving
    .word PlayerDisabled

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
    STA playerstate
    JMP EntityComplete

Player2Init:
    LDA #%00000001
    STA entities+Entity::attributes, X 
    LDA #%00000010 
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA playerstatep2
    JMP EntityComplete

Player3Init:
    LDA #%00000010
    STA entities+Entity::attributes, X 
    LDA #$04
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA playerstatep3
    JMP EntityComplete

Player4Init:
    LDA #%00000011
    STA entities+Entity::attributes, X 
    LDA #$08
    STA entities+Entity::collisionlayer, X 
    LDA #$01
    STA playerstatep4
    JMP EntityComplete

PlayerOnFloor:
    LDA #$00
    STA temp ;
    STA temp2 ; use temp 2 to see if we've pressed l/r
    ; Check if we're shooting this frame 
    JSR CheckB
    ; if b, spawn fireball 
    CMP #ButtonReturn::Release
    BNE :+
        JSR PlayerAttemptSpawnFireball
    :
    ; check if right is pressed
    JSR CheckRight
    ; move right if pressed
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlow
        CLC 
        ADC #$50
        STA vxlow
        LDA vxhigh
        ADC #$00
        STA vxhigh
        ; Clamp veloctiy
        CMP #$01
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlow
        CMP #$30
        BCC :+
        LDA #$30
        STA vxlow
        LDA #$01
        JSR InitAnimation
    :
    CMP ButtonReturn::Release
    ; BNE :+
    ;     LDA #$00
    ;     JSR InitAnimation
    ; :

    ;Check Left Cl
    JSR CheckLeft
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlow
        SEC 
        SBC #$70
        STA vxlow
        LDA vxhigh
        SBC #$00
        STA vxhigh
        ; Clamp veloctiy
        CMP #$FF
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlow
        CMP #$30
        BCS :+
        LDA #$30
        STA vxlow
        LDA #$01
        JSR InitAnimation
    :
    CMP ButtonReturn::Release
    ; BNE :+
    ;     LDA #$00
    ;     JSR InitAnimation
    ; :

        ; Add to position
    JSR PlayerXMovement
        ; Eject from wall
    JSR CollideLeft2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhigh
        STA vxlow
        ; LDA #$00
        ; JSR InitAnimation
    :    
    JSR CollideRight2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhigh
        STA vxlow
        ; LDA #$00
        ; JSR InitAnimation
    :

        ; Move down and eject from floor and eject if needed
    LDA vylow ;load velocity subpixel
    CLC 
    ADC #$20 ; move subpixeldown
    STA vylow
    LDA vyhigh 
    ; Do NOT set carry, retain it to seeif subpixeloverflowered
    ADC #$00 ; if carry is set,this will add 1
    STA vyhigh

    LDA vylow
    CLC 
    ADC yposlow
    STA yposlow
    LDA entities+Entity::ypos, X 
    ADC #$00
    ADC vyhigh
    STA entities+Entity::ypos, X 
    INC entities+Entity::ypos, X
    JSR CollideDown2 ; eject from floor
    BNE :+ ; Skip ahead if we hit the floor
        LDA #$05
        STA playerstate
        LDA #$04
        JSR InitAnimation
        JMP EntityComplete
    :
    ; Kill velocity
    LDA #$00
    STA vyhigh
    STA vylow
    ; Check for jump
    JSR CheckA
    CMP #ButtonReturn::Press
    BNE :+
        LDA #$02
        STA playerstate
        LDA #$00
        STA entities+Entity::generalpurpose, X
        LDA #$05
        STA entities+Entity::animationframe, X
        LDA #$03
        JSR InitAnimation
    :
    
    JSR AdvancePlayerAnimation
    JMP EntityComplete 
PlayerJumping:
    LDA #$05 
    STA entities+Entity::animationframe, X 
    ; Do animation
    DEC entities+Entity::animationtimer, X 
    BNE :+
        ;if 0, reset to default jump frame
        LDA #$05 
        STA entities+Entity::animationframe, X
    :

    ; Check if we're shooting this frame 
    JSR CheckB
    ; if b, spawn fireball 
    CMP #ButtonReturn::Press
    BNE :+
        JSR PlayerAttemptSpawnFireball  
        ;Also set animation sprite to fire
        LDA #$07
        STA entities+Entity::animationframe,X
        ; Set animation timer
        STA entities+Entity::animationtimer,X
    :
    ; check if right is pressed
    JSR CheckRight
    CMP #ButtonReturn::Press
    ; move right if pressed
    BNE :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
        LDA entities+Entity::attributes, X
        AND #%01000000 
        STA entities+Entity::attributes, X
        ; Eject from wall
        JSR CollideRight2
    :

    ;Check Left Cl
    JSR CheckLeft
    CMP #ButtonReturn::Press
    BNE :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
        LDA #%01000000
        ORA entities+Entity::attributes, X 
        STA entities+Entity::attributes, X
        JSR CollideLeft2
    :
    ; Do y movement
    LDY entities+Entity::generalpurpose, X 
    LDA JumpStrength, Y 
    CMP #$01 ; iF 01 we've reached the end of the dataset
    BNE :+ 
    STA playerstate ;1 is terminating so we can feed that back into the state
    JMP EntityComplete
    :
    CLC 
    ADC entities+Entity::ypos, X 
    STA entities+Entity::ypos, X
    INC entities+Entity::generalpurpose, X
    JSR CollideUp2
    BEQ :+
    LDA #$03 ; enter headbonk state
    STA playerstate
    : 
    JMP EntityComplete
PlayerHeadbonk:

    ; Check if we're shooting this frame 
    JSR CheckB
    ; if b, spawn fireball 
    CMP #ButtonReturn::Press
    BNE :+
        JSR PlayerAttemptSpawnFireball  
    :
    LDA #$01
    STA entities+Entity::animationframe, X
    ; check if right is pressed
    JSR CheckRight
    CMP #ButtonReturn::Press
    ; move right if pressed
    BNE :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
        LDA entities+Entity::attributes, X
        AND #%01000000 
        STA entities+Entity::attributes, X
        ; Eject from wall
        JSR CollideRight2
    :

    ;Check Left Cl
    JSR CheckLeft
    CMP #ButtonReturn::Press
    BNE :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
        LDA #%01000000
        ORA entities+Entity::attributes, X 
        STA entities+Entity::attributes, X
        JSR CollideLeft2
    :

    LDY entities+Entity::generalpurpose, X 
    LDA HeadbonkStrength, Y 
    CMP #$01
    BNE :+
    STA playerstate
    JMP EntityComplete
    :
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X 
    INC entities+Entity::generalpurpose, X 
    JSR CollideUp2
    JMP EntityComplete

PlayerJumpReleased:
    ; Check if we're shooting this frame 
    JSR CheckB
    ; if b, spawn fireball 
    CMP #ButtonReturn::Release
    BNE :+
        JSR PlayerAttemptSpawnFireball  
    :
    LDA #$01
    STA entities+Entity::animationframe, X
    ; check if right is pressed
    JSR CheckRight
    CMP #ButtonReturn::Press
    ; move right if pressed
    BNE :+
        LDA #$01
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos,X
        LDA entities+Entity::attributes, X
        AND #%01000000 
        STA entities+Entity::attributes, X
        ; Eject from wall
        JSR CollideRight2
    :

    ;Check Left Cl
    JSR CheckLeft
    CMP #ButtonReturn::Press
    BNE :+
        LDA #$FF 
        CLC 
        ADC entities+Entity::xpos,X
        STA entities+Entity::xpos, X
        LDA #%01000000
        ORA entities+Entity::attributes, X 
        STA entities+Entity::attributes, X
        JSR CollideLeft2
    :

    LDY entities+Entity::generalpurpose, X 
    LDA JumpStrengthReleased, Y 
    CMP #$01
    BNE :+
    STA playerstate
    JMP EntityComplete
    :
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X 
    INC entities+Entity::generalpurpose, X 
    JSR CollideUp2
    JMP EntityComplete

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
        LDA xposlow
        CLC 
        ADC vxlow
        STA xposlow
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


PlayerApplyFriction:
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

PlayerDisabled:
    JMP EntityComplete
PlayerIdle:
    JMP EntityComplete
PlayerFalling:
    ; check if right is pressed
    JSR CheckRight
    ; move right if pressed
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlow
        CLC 
        ADC #$50
        STA vxlow
        LDA vxhigh
        ADC #$00
        STA vxhigh
        ; Clamp veloctiy
        CMP #$01
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlow
        CMP #$30
        BCC :+
        LDA #$30
        STA vxlow
        LDA #$01
        JSR InitAnimation
    :
    CMP ButtonReturn::Release
    ; BNE :+
    ;     LDA #$00
    ;     JSR InitAnimation
    ; :

    ;Check Left Cl
    JSR CheckLeft
    CMP #ButtonReturn::Press
    BNE :+
        INC temp2
        LDA vxlow
        SEC 
        SBC #$70
        STA vxlow
        LDA vxhigh
        SBC #$00
        STA vxhigh
        ; Clamp veloctiy
        CMP #$FF
        BNE :+
        ; LDA #$01
        ; STA vxhigh
        LDA vxlow
        CMP #$30
        BCS :+
        LDA #$30
        STA vxlow
        LDA #$01
        JSR InitAnimation
    :
    CMP ButtonReturn::Release
    ; BNE :+
    ;     LDA #$00
    ;     JSR InitAnimation
    ; :

        ; Add to position
    JSR PlayerXMovement
        ; Eject from wall
    JSR CollideLeft2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhigh
        STA vxlow
        ; LDA #$00
        ; JSR InitAnimation
    :    
    JSR CollideRight2
    BEQ :+
        ; If we ejected, zero velocity
        LDA #$00
        STA vxhigh
        STA vxlow
        ; LDA #$00
        ; JSR InitAnimation
    :


    LDA vylow ;load velocity subpixel
    CLC 
    ADC #$20 ; move subpixeldown
    STA vylow
    LDA vyhigh 
    ; Do NOT set carry, retain it to seeif subpixeloverflowered
    ADC #$00 ; if carry is set,this will add 1
    STA vyhigh

    LDA vylow
    CLC 
    ADC yposlow
    STA yposlow
    LDA entities+Entity::ypos, X 
    ADC #$00
    ADC vyhigh
    STA entities+Entity::ypos, X 

    JSR CollideDown2
    BNE :+
        JSR AdvancePlayerAnimation
        JMP EntityComplete
    :
    LDA #$01 
    STA playerstate
    LDA #$00
    STA vyhigh
    STA vylow
    LDA #$05
    JSR InitAnimation
    JMP EntityComplete

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
    ADC yposlowp2
    STA yposlowp2
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
            STA playerstatep2
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


PlayerAttemptSpawnFireball:
    LDA projectilecooldownp1 ; load cooldown
    BEQ :+ ; if timer is zero, continue, else return
        RTS 
    :
    LDA projectilecountp1
    CMP #$03
    BCS PlayerEndSpawnFireball
    TXA 
    PHA 
    LDA entities+Entity::attributes, X 
    STA temp 
    LDA entities+Entity::ypos, X
    SEC 
    SBC #$01
    TAY 
    LDA entities+Entity::xpos, X 
    CLC 
    ADC #$09 
    TAX 
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
    LDA #%00001110 ; collide with p2 3 and 4
    STA entities+Entity::collisionlayer, x
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
    ; LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
    ; STA PPUControl
    ; ; Enabling sprites and background for left-most 8 pixels
    ; ; Enable sprites and background
    ; LDA #%001`11110
    ; STA $PPUMask

LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
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
        LDA temp 
        STA entities+Entity::attributes, X 
        ; some entities will override temp in their init, but there are circumstances where you want e.g. the attributes of the entity that spawned this one
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

CheckForBankSwap:
    LDA #%10000000
    BIT drawflags
    BEQ :+
        RTS 
    LDA currentbank
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LDA #%10000000
    EOR drawflags
    STA drawflags
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
    LDA currentbank
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
    ReadButtonLoopP3:
        LDA $4016
        ROR 
        ROL buttonsp3
        BCC ReadButtonLoopP3

    LDA #$01
    STA $4017
    STA buttonsp2
    LSR
    STA $4017

    ReadButtonLoopP2:
        LDA $4017
        ROR 
        ROL buttonsp2
        BCC ReadButtonLoopP2
    LDA #$01
    LSR 
    ReadButtonLoopP4:
        LDA $4017 
        ROR 
        ROL buttonsp4
        BCC ReadButtonLoopP4
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
CollideLeft2:
; LDA #$3f
;     STA $2001
    
    LDA #$00
    STA var_mem
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

    BEQ :+
        STA var_mem
        INC entities+Entity::xpos, X 
        JMP CollideLeft2Loop
    :

    ; LDA #$1E
    ; STA $2001
    LDA var_mem
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
; LDA #$3f
;     STA $2001
    

    LDA #$00
    STA var_mem

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

    BEQ :+
        STA var_mem
        DEC entities+Entity::xpos, X 
        JMP CollideRight2Loop
    :
    ; LDA #$1E
    ; STA $2001
    
    LDA var_mem
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
    STA var_mem

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
    ORA rectangle1

    BEQ :+
        STA var_mem
        DEC entities+Entity::ypos, X 
        JMP CollideDown2Loop
    :
    ; LDA #$1E
    ; STA $2001
    
    LDA var_mem
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
    STA var_mem
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
        STA var_mem 
        INC entities+Entity::ypos, X 
        JMP CollideUp2Loop
    :
    LDA var_mem
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
    ; LDA temp2
    ; BEQ CollideSpriteComplete
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
    CLC
    LDA entities+Entity::xpos, X
    STA rectangle2
    ; CLC 
    ADC #$07
    STA rectangle2+1
    LDA entities+Entity::ypos, X
    STA rectangle2+2 
    ; CLC 
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
    LDY #$00
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


DrawFireball:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListFireball, Y 
    STA jumppointer
    LDA MetaSpriteListFireball+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawRespawner:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListRespawner, Y 
    STA jumppointer
    LDA MetaSpriteListRespawner+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawBroomStick:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListBroomStick, Y 
    STA jumppointer
    LDA MetaSpriteListBroomStick+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawPortal:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListPortal, Y 
    STA jumppointer
    LDA MetaSpriteListPortal+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

DrawIceBeam:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListIceBeam, Y 
    STA jumppointer
    LDA MetaSpriteListIceBeam+1, Y
    STA jumppointer+1
    LDA (jumppointer), Y 
    CMP #$FF
    BNE :+
    JMP CheckEndSpriteDraw
    :
    JMP DrawSpriteInit

; DrawIceCrystal:
;     LDA entities+Entity::animationframe, X 
;     ASL 
;     TAY 
;     LDA MetaSpriteListIceCrystal, Y 
;     STA jumppointer
;     LDA MetaSpriteListIceCrystal+1, Y
;     STA jumppointer+1
;     LDA (jumppointer), Y 
;     CMP #$FF
;     BNE :+
;     JMP CheckEndSpriteDraw
;     :
;     JMP DrawSpriteInit

DrawTeleporter:
    LDA entities+Entity::animationframe, X 
    ASL 
    TAY 
    LDA MetaSpriteListTeleporter, Y 
    STA jumppointer
    LDA MetaSpriteListTeleporter+1, Y
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
    JSR SpawnPlayerPort1
    ; LDY #$50
    ; LDX #$50 
    ; LDA EntityType::Player
    ; JSR SpawnEntity
    JMP ProcessDestructionStack
RespawnPlayer2:
    PLA 
    PLA 
    PLA
    JSR SpawnPlayerPort2
    ; LDY #$50
    ; LDX #$50 
    ; LDA EntityType::Player
    ; JSR SpawnEntity
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
    .byte $EE,$EE
    .byte $EE,$EE
    .byte $00,$00
    .byte $00,$00
pal2:
    .byte $EF,$EF
    .byte $EF,$EF
    .byte $00,$00
    .byte $00,$00
pal3:
    .byte $FE,$FE
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
    .byte $01,$01
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
    .word floor_half_reversed

PaletteData:
    .byte $0F,$01,$11,$21,  $0F,$01,$03,$0A,  $0F,$07,$16,$27, $0F,$05,$15,$30  ;background palette data  
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
    .byte $24,$00,$13,$12,$14,$00,$11,$0C,$0D,$15,$00,$00,$00,$00,$00,$23
    .byte $24,$11,$31,$31,$00,$00,$11,$0E,$0F,$15,$00,$00,$31,$31,$00,$23
    .byte $24,$00,$04,$05,$04,$05,$00,$10,$01,$00,$04,$05,$00,$04,$05,$23
    .byte $24,$00,$04,$05,$00,$00,$30,$30,$1D,$19,$1A,$21,$22,$19,$1A,$23
    .byte $24,$00,$04,$05,$00,$08,$09,$00,$05,$02,$00,$02,$10,$02,$00,$23
    .byte $29,$00,$04,$05,$00,$0A,$0B,$00,$00,$02,$00,$02,$00,$02,$00,$29
    .byte $24,$00,$21,$22,$00,$22,$21,$00,$10,$21,$1F,$20,$00,$00,$31,$23
    .byte $24,$12,$23,$24,$02,$00,$00,$04,$05,$00,$00,$02,$00,$00,$00,$23
    .byte $29,$12,$25,$26,$32,$00,$03,$04,$05,$02,$00,$32,$00,$1D,$30,$23
    .byte $24,$10,$08,$09,$02,$00,$31,$04,$05,$31,$00,$02,$00,$10,$05,$23
    .byte $24,$00,$0A,$0B,$02,$31,$03,$19,$1A,$02,$00,$19,$1A,$00,$00,$23
    .byte $31,$31,$32,$32,$31,$31,$00,$00,$31,$31,$31,$31,$32,$32,$31,$31
    .byte $29,$2F,$04,$04,$05,$2F,$05,$05,$04,$04,$2F,$2F,$05,$04,$2F,$29
    .byte $07,$1D,$1E,$1E,$1D,$1D,$1E,$1E,$1E,$1E,$1D,$1D,$1E,$1E,$1D,$07

AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %00100010, %00000000, %00000000, %11000000, %00110000, %00000000, %00000000, %10001000
    .byte %00100010, %01011010, %01011010, %00001100, %00000011, %01011010, %01001000, %10011010
    .byte %00100010, %00000000, %00000000, %00010010, %00010010, %01001000, %00010010, %10001000
    .byte %00100011, %10100000, %10000000, %00100000, %10000000, %10100000, %00000000, %10101000
    .byte %00110010, %10101010, %00100100, %00000001, %00000100, %10000101, %10000000, %10101001
    .byte %00100010, %00000000, %10000000, %00000010, %00001000, %00100000, %00000100, %10001001
    .byte %00001010, %01011000, %01011010, %11111010, %11111010, %01011010, %01010010, %10001010
    .byte %00101010, %00000000, %10101010, %00000000, %00000000, %10101010, %00000000, %10001010

;;;;;;;;;;;;
;;; LOOK UP TABLES
;;;;;;;;;;;

GameStatePath:
    .word DoGameLogic
    .word DoGameLogic
    .word DoGameLogic

PlayerSpawnTable:
    .word NoSpawn
    .word SpawnPlayerPort1
    .word SpawnPlayerPort2
    .word SpawnPlayerPort3
    .word SpawnPlayerPort4

DestroyEntityList: ; defines behaviours for when an entity is destroyed
    .word NoDeathAction ; 0 
    .word NoDeathAction ; 1
    .word RespawnPlayer2 ; 2
    .word NoDeathAction ; 3
    .word NoDeathAction ; 4
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
    .word ProcessFireball
    .word ProcessRespawner
    .word ProcessPortal
    .word ProcessBroomStick
    .word ProcessIceBeam
    .word ProcessTeleporter
    .word ProcessTeleporterP2
    .word ProcessTeleporterP3
    .word ProcessTeleporterP4

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
    .word DrawFireball
    .word DrawRespawner
    .word DrawPortal 
    .word DrawBroomStick
    .word DrawIceBeam
    .word DrawTeleporter
    .word DrawTeleporter
    .word DrawTeleporter
    .word DrawTeleporter


MetaSpriteListPlayer:
    .word PlayerSpriteIdle1 ; 0
    .word PlayerSpriteIdle2 ; 1
    .word PlayerSpriteRun1 ; 2
    .word PlayerSpriteRun2 ; 3
    .word PlayerSpriteCrouch ; 4
    .word PlayerSpriteJump ; 5
    .word PlayerSpriteSlide ; 6
    .word PlayerSpriteFire ; 

AnimationStringsPlayer:
    .word AnimationStringPlayerIdle
    .word AnimationStringPlayerRunning
    .word AnimationStringPlayerCrouching
    .word AnimationStringPlayerJumping
    .word AnimationStringPlayerFalling
    .word AnimationStringPlayerFallingEnd
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
    .byte $04,$09,$FE,$00
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
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01


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
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "castle_set-bank1.chr"
    ; .incbin "castle_set-bank2.chr"
    .incbin "title_screen_bank.chr"