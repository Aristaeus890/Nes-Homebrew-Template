
.include "constants.asm" 

; INes 2.0 
.segment "HEADER" 
.byte "NES" ;0-2 
.byte $1a ;3
.byte $02 ; 2 * 16KB PRG ROM - least sig byte
.byte $06 ; 5 * 8KB CHR ROM  - lsb
.byte %01000001 ; mapper and mirroring
.byte %00001000 ; console type 
.byte $00
.byte $00 ; size msb
.byte $00 ; prg ram
.byte $00 ; chr ram size
.byte $00 ; cpu timing
.byte $00
.byte $00
.byte $00

.segment "ZEROPAGE" ; 0-FF. One page of ram that is faster access than rest of the ram. Use for values most frequently used
    MAXENTITIES =15; max allowed number of entities. Each entity takes a number of bytes in the zero page equal to the entity struct
    ; this CANNOT run over the end of the zero page or it will not work. If you want more entities, you will need to dedicate a non zero
    ; page ram segment to it
    vxhigh: .res 1
    vxlow: .res 1
    vxhighleft : .res 1 
    vxlowleft: .res 1
    entities: .res .sizeof(Entity) * MAXENTITIES ; 6 * 30 = 180/256 bytes
    entity_mem = .sizeof(Entity) * MAXENTITIES ; mem used

    CurrentColumnHigh: .res 1
    CurrentColumnLow: .res 1
    world: .res 2 ; this is a pointer to the address of the current screen we want to fetch tiles on
    metatilepointer: .res 2
    pallettepointer: .res 2
    attributespointer: .res 2

    seed: .res 2 ; the seed for the pseudo rng. This will be inited to anything by 0 later
    jumppointer: .res 2 ; used to jump to specic places in memory with jump tables
    irqpointer: .res 2
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
    rectangle1: .res 4 
    rectangle2: .res 4
    DrawFlags: .res 1 ;
    animateflags: .res 1 
    AttributesAddressHigh: .res 1
    AttributesAddressLow: .res 1
    UpperScrollX: .res 1


.segment "OAM" ;= $0200
    SpriteBuffer: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "RAM" ;= $0300
    CollisionMap: .res 240
    CurrentPPUMask: .res 1
    CurrentBackgroundPalette: .res 16
    CurrentSpritePallette: .res 16
    GameTimer: .res 2
    GameTimerConverted: .res 2
    GameTimerAddress: .res 2
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
    WaterfallBuffer1: .res 8
    WaterfallBuffer2: .res 8
    WaterfallAddress1: .res 2
    WaterfallAddress2: .res 2
    WaterfallAddress3: .res 2
    WaterfallAddress4: .res 2


    CurrentMap: .res 1
    CurrentPlayerCount: .res 1
    MenuSelectorIndex: .res 1
    CurrentPalette: .res 1
    PlayerCountBuffer: .res 6
    MapBuffer: .res 12
    PaletteBuffer: .res 6
    CurrentTowerRotation: .res 1
    WindowIndex: .res 1
    WindowOffset1: .res 4
    WindowOffset2: .res 4
    WindowOffset3: .res 4
    WindowOffset4: .res 4
    NMIEnableLevelEffects: .res 1
    CurrentCloudBank: .res 1
    CloudbankTimer: .res 1
    ParticlesRngIndex: .res 1
    ParticlesType: .res MAXPARTICLES
    ParticlesX: .res MAXPARTICLES
    ParticlesY: .res MAXPARTICLES
    ParticlesSpriteNo: .res MAXPARTICLES
    ParticlesAttributes: .res MAXPARTICLES
    ParticlesSubX: .res MAXPARTICLES
    ParticlesSubY: .res MAXPARTICLES
    ParticlesVX: .res MAXPARTICLES
    ParticlesVY: .res MAXPARTICLES
    ParticlesVXLow: .res MAXPARTICLES
    ParticlesVYLow: .res MAXPARTICLES
    ParticlesTimer1: .res MAXPARTICLES
    ParticlesSign: .res MAXPARTICLES
    WindSign: .res 1
    ParticlesWindStrengthXHigh: .res 1
    ParticlesWindStrengthXLow: .res 1
    ParticlesWindStrengthYHigh: .res 1
    ParticlesWindStrengthYLow: .res 1
    WindState: .res 1
    WindTimerHigh: .RES 1
    WindTimerLow: .res 1
    ParticlesCount: .res 1

    EmitterType: .res MAXEMITTERS
    EmitterX: .res MAXEMITTERS
    EmitterY: .res MAXEMITTERS
    EmitterWidth: .res MAXEMITTERS
    EmitterHeight: .res MAXEMITTERS
    EmitterParticleNumber: .res MAXEMITTERS

    ParticlesGravity: .res 1

    rng: .res 8
    rngindex: .res 1
    NMIIrqValue: .res 1
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
:
    BIT PPUStatus
    BPL :-
:
    BIT PPUStatus
    BPL :-

LDA #$02
STA OAMDMA

LDX #$00

LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #$23
STA AttributesAddressHigh
LDA #$C0
STA AttributesAddressLow

; LDA #<MetaTileListTitle
; STA metatilepointer
; LDA #>MetaTileListTitle
; STA metatilepointer+1

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
BIT PPUStatus 
LDA #$3F
STA PPUAddress
LDA #$00
STA PPUAddress


LDX #$00
LoadtoPPU:
    LDA CurrentBackgroundPalette, X
    LDA PaletteData, X  
    STA PPUData ; $3F00, $3F01, $3F02 => $3F1F
    INX
    CPX #32
    BNE LoadtoPPU 

; For the prng to work, it needs a seed of any nonzero number to start at
InitSeed: 
    LDA #$08
    STA seed
    STA seed+1

SetAttributes:
    LDX #$00
    BIT PPUStatus
    LDA #$23
    STA PPUAddress
    LDA #$C0
    STA PPUAddress
    AttributeLoop:
    LDA AttributesDefault, X 
    STA PPUData
    INX
    CPX #64
    BNE AttributeLoop

BIT PPUStatus
LDA #$20
STA PPUAddress
LDA #$00
STA PPUAddress


LDA #<IrqDoNothing
STA irqpointer
LDA #>IrqDoNothing
STA irqpointer+1

BIT PPUControl
LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
STA PPUAddress
LDA #$00
STA PPUAddress
; JSR LoadSingleScreen

LDA #$01
sta MIRRORING 

LDA #<ScreenDefault
STA world
LDA #>ScreenDefault
STA world+1


; BIT PPUControl
; LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
; STA PPUAddress
; LDA #$00
; STA PPUAddress

LDX #$00
LDY #$00
:
STX PPUData
INX 
CPX #$00
BNE :-

; jsr SwapRightBankToRampart
; JSR InitOptionsScreen
jsr InitGameHat

; ldx #<music_data_untitled
; ldy #>music_data_untitled
        
; lda #$01 ; NTSC
; jsr famistudio_init
; lda #$00
; jsr famistudio_music_play

LDA #$02
STA playerflags

:
BIT PPUStatus
BPL :-

; Enable Rendering
LDA #%10010000
STA PPUControl
LDA #%00011110
STA PPUMask
CLI



.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:

    JSR SelectGameStatePath
    JSR Setrng
    JSR FillTileBuffer
    JSR IncFrameCount   ; Counts to 59 then resets to 0

    JSR OAMBuffer   ; Sprite data is written to the buffer here
    JSR OAMBufferParticles
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
NMI: 
    ; load the current registers onto the stack. This doesn't matter if we finish our logic before the nmi is called, 
    ;but if it is, we need to preserve the registers so that we can carry on where we left off 
    PHA ; 2
    TXA ; 2
    PHA ; 2
    TYA ;2
    PHA  ;2

    ;10

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

    ; ; Check for level specific commands
    LDA NMIEnableLevelEffects
    BEQ :+
        LDA CurrentMap
        ASL 
        TAY 
        LDA NMIAnimateCommands, Y 
        STA jumppointer
        LDA NMIAnimateCommands+1, Y 
        STA jumppointer+1
        JMP (jumppointer)    
    :
    NMILevelUpdateReturnPoint:
    LDA NMIIrqValue ;2
    STA IRQLATCH ;4
    STA IRQRELOAD ;4
    STA IRQENABLE ;4

    ; read sprites
    LDA #$00
    STA OAMAddress 
    LDA #$02 
    STA OAMDMA
    
    ; LDA #%10010000
    ; STA PPUControl

    ; bit PPUStatus
    ; LDA #$20
    ; sta PPUAddress
    ; lda #$00
    ; sta PPUAddress

    LDA #%10010000
    STA PPUControl

    BIT PPUStatus
    ; LDA UpperScrollX
    LDA #$00
    STA PPUScroll
    LDA #$00
    STA PPUScroll


    INC nmidone;5



    ; Bring the stack values back into the registers
    PLA;2
    TAY ;2
    PLA ;2
    TAX ;2
    PLA ;2
    
    RTI ;6


NMISwapPallette:
    BIT PPUStatus
    LDA #$3F 
    STA PPUAddress
    LDA #$00 
    STA PPUAddress
    ; LDA #$13
    ; STA PPUData
    LDA CurrentBackgroundPalette
    STA PPUData
    LDA CurrentBackgroundPalette+1
    STA PPUData
    LDA CurrentBackgroundPalette+2
    STA PPUData
    LDA CurrentBackgroundPalette+3
    STA PPUData
    LDA CurrentBackgroundPalette+4
    STA PPUData
    LDA CurrentBackgroundPalette+5
    STA PPUData
    LDA CurrentBackgroundPalette+6
    STA PPUData
    LDA CurrentBackgroundPalette+7
    STA PPUData
    LDA CurrentBackgroundPalette+8
    STA PPUData
    LDA CurrentBackgroundPalette+9
    STA PPUData
    LDA CurrentBackgroundPalette+10
    STA PPUData
    LDA CurrentBackgroundPalette+11
    STA PPUData
    LDA CurrentBackgroundPalette+12
    STA PPUData
    LDA CurrentBackgroundPalette+13
    STA PPUData
    LDA CurrentBackgroundPalette+14
    STA PPUData
    LDA CurrentBackgroundPalette+15
    STA PPUData
    LDA CurrentBackgroundPalette+16
    STA PPUData
    LDA CurrentBackgroundPalette+17
    STA PPUData
    LDA CurrentBackgroundPalette+18
    STA PPUData
    LDA CurrentBackgroundPalette+19
    STA PPUData
    LDA CurrentBackgroundPalette+20
    STA PPUData
    LDA CurrentBackgroundPalette+21
    STA PPUData
    LDA CurrentBackgroundPalette+22
    STA PPUData
    LDA CurrentBackgroundPalette+23
    STA PPUData
    LDA CurrentBackgroundPalette+24
    STA PPUData
    LDA CurrentBackgroundPalette+25
    STA PPUData
    LDA CurrentBackgroundPalette+26
    STA PPUData
    LDA CurrentBackgroundPalette+27
    STA PPUData
    LDA CurrentBackgroundPalette+28
    STA PPUData
    LDA CurrentBackgroundPalette+29
    STA PPUData
    LDA CurrentBackgroundPalette+30
    STA PPUData
    LDA CurrentBackgroundPalette+31
    STA PPUData

    lda #$00
    STA DrawFlags 
JMP NMIDrawCommandReturnPoint

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
    LDA #$0b
    STA DrawFlags
    LDA #$01
    STA NMIEnableLevelEffects
    :
JMP NMIDrawCommandReturnPoint

NMIDrawOptionsScreen:
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
    LDA #$0A
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

NMIUpdateAttributes:
    BIT PPUStatus
    LDA #$23
    STA PPUAddress
    LDA #$C0
    STA PPUAddress

    LDX #64
    LDY #$00

    :
    LDA (attributespointer), Y 
    STA PPUData
    INY
    DEX
    BPL :-

    LDA #$04 
    STA DrawFlags



JMP NMIDrawCommandReturnPoint

NMIDarkenPallette:
    ; BIT PPUStatus
    ; LDA #$3F 
    ; STA PPUAddress
    ; LDA #$00
    ; STA PPUAddress
    ; LDA #$0F
    ; LDX #32
    ; :
    ; STA PPUData
    ; DEX
    ; BPL :-
    ; BIT PPUStatus
    ; LDA #$20
    ; STA PPUAddress
    ; LDA #00
    ; STA PPUAddress
    ; LDA #$03
    STA DrawFlags
JMP NMIDrawCommandReturnPoint

NMIUpdateRampart: ;180
; JMP NMILevelUpdateReturnPoint ;

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
JMP NMILevelUpdateReturnPoint ;

NMIUpdateTower:
    BIT PPUStatus
    LDA #$20
    STA PPUAddress
    LDA #$8E
    STA PPUAddress
    LDA WindowOffset1
    STA PPUData
    LDA WindowOffset1+1
    STA PPUData
    LDA WindowOffset1+2
    STA PPUData
    LDA WindowOffset1+3
    STA PPUData

    BIT PPUStatus
    LDA #$20
    STA PPUAddress
    LDA #$AE
    STA PPUAddress
    LDA WindowOffset2
    STA PPUData
    LDA WindowOffset2+1
    STA PPUData
    LDA WindowOffset2+2
    STA PPUData
    LDA WindowOffset2+3
    STA PPUData

    BIT PPUStatus
    LDA #$20
    STA PPUAddress
    LDA #$CE
    STA PPUAddress
    LDA WindowOffset3
    STA PPUData
    LDA WindowOffset3+1
    STA PPUData
    LDA WindowOffset3+2
    STA PPUData
    LDA WindowOffset3+3
    STA PPUData

    BIT PPUStatus
    LDA #$20
    STA PPUAddress
    LDA #$EE
    STA PPUAddress
    LDA WindowOffset4
    STA PPUData
    LDA WindowOffset4+1
    STA PPUData
    LDA WindowOffset4+2
    STA PPUData
    LDA WindowOffset4+3
    STA PPUData

    BIT PPUStatus
    LDA #$22
    STA PPUAddress
    LDA #$98
    STA PPUAddress
    LDA WindowOffset4+1
    STA PPUData
    LDA WindowOffset4+2
    STA PPUData
    LDA WindowOffset4+3
    STA PPUData
    LDA WindowOffset4
    STA PPUData



    BIT PPUStatus
    LDA #$22
    STA PPUAddress
    LDA #$B8
    STA PPUAddress
    LDA WindowOffset1+2
    STA PPUData
    LDA WindowOffset1+3
    STA PPUData
    LDA WindowOffset1
    STA PPUData
    LDA WindowOffset1+1
    STA PPUData


JMP NMILevelUpdateReturnPoint ;

NMIUpdateRest:
JMP NMILevelUpdateReturnPoint ;

NMIUpdateGarden:
JMP NMILevelUpdateReturnPoint ;

NMIUpdateAnimatedTilesOptions:

    LDA #%10010000 ;2
    STA PPUControl ;4
    BIT PPUStatus ;4
    LDA #$21
    STA PPUAddress
    LDA #$10
    STA PPUAddress
    LDA PlayerCountBuffer
    STA PPUData
    LDA PlayerCountBuffer+1
    STA PPUData
    LDA PlayerCountBuffer+2
    STA PPUData
    LDA PlayerCountBuffer+3
    STA PPUData

    BIT PPUStatus ;4
    LDA #$21
    STA PPUAddress
    LDA #$90
    STA PPUAddress
    LDA MapBuffer
    STA PPUData
    LDA MapBuffer+1
    STA PPUData
    LDA MapBuffer+2
    STA PPUData
    LDA MapBuffer+3
    STA PPUData
    LDA MapBuffer+4
    STA PPUData
    LDA MapBuffer+5
    STA PPUData
    LDA MapBuffer+6
    STA PPUData
    LDA MapBuffer+7
    STA PPUData
    LDA MapBuffer+8
    STA PPUData
    LDA MapBuffer+9
    STA PPUData
    LDA MapBuffer+10
    STA PPUData
    LDA MapBuffer+11
    STA PPUData

    BIT PPUStatus
    LDA #$22
    STA PPUAddress
    LDA #$10
    STA PPUAddress

    LDA PaletteBuffer
    STA PPUData
    LDA PaletteBuffer+1
    STA PPUData
    LDA PaletteBuffer+2
    STA PPUData
    LDA PaletteBuffer+3
    STA PPUData
    LDA PaletteBuffer+4
    STA PPUData
    LDA PaletteBuffer+5
    STA PPUData

JMP NMIDrawCommandReturnPoint


IRQ:
    ; STA IRQDISABLE
    ; RTI
    JMP (irqpointer)

IrqDoNothing:
    STA IRQDISABLE
RTI

IRQRampart:
    PHA 
    LDA #%00111110
    STA PPUMask
    STA IRQDISABLE
    PLA
RTI

IRQTower1:
    PHA 
    ; BIT PPUStatus
    ; LDA #$00
    ; STA PPUScroll
    ; STA PPUScroll


    LDA #%10011110
    STA PPUMask

    LDA #<IRQTower2
    STA irqpointer
    LDA #>IRQTower2
    STA irqpointer+1
    LDA #$40
    STA IRQLATCH
    STA IRQRELOAD
    STA IRQDISABLE
    STA IRQENABLE
    PLA 
RTI

IRQTower2:
    PHA
    ; BIT PPUStatus
    ; LDA #$00
    ; STA PPUScroll
    ; STA PPUScroll

    LDA #%00111110
    STA PPUMask
    STA IRQDISABLE
    LDA #<IRQTower1
    STA irqpointer
    LDA #>IRQTower1
    STA irqpointer+1
    PLA
RTI
;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

; in a = level data to load
PrepareLevelData:
    ASL 
    TAY 
    LDA MapHeaders, Y 
    STA jumppointer
    LDA MapHeaders+1, Y 
    STA jumppointer+1

    LDY #$00
    LDA (jumppointer), Y 
    STA world
    INY 
    LDA (jumppointer), Y 
    STA world+1

    INY 
    LDA (jumppointer), Y 
    STA pallettepointer
    INY 
    LDA (jumppointer), Y 
    STA pallettepointer+1

    INY 
    LDA (jumppointer), Y
    STA metatilepointer
    INY 
    LDA (jumppointer), Y 
    STA metatilepointer+1

    INY 
    LDA (jumppointer), Y
    STA attributespointer
    INY 
    LDA (jumppointer), Y 
    STA attributespointer+1

    INY 
    LDA (jumppointer), Y
    STA irqpointer
    INY 
    LDA (jumppointer), Y 
    STA irqpointer+1

    INY 
    LDA (jumppointer), Y 
    STA CurrentPPUMask

    INY 
    LDA (jumppointer), Y 
    STA NMIIrqValue
RTS

LoadPalletteToRam:
    LDY #$00
    LoadPalletteToRamLoop:
    LDA (pallettepointer), Y
    STA CurrentBackgroundPalette, Y
    INY 
    CPY #$20
    BNE LoadPalletteToRamLoop
RTS

MakePaletteBlack:
    LDY #$00


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


jsr SwapLeftBankToTitle
JSR SwapRightBankToTitle
LDA #$03
STA DrawFlags

LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #$00
JSR PrepareLevelData

LDA #GState::GameStateTitleScreen
STA GameState

RTS 

InitOptionsScreen:
;Clear all sprite entities
LDX #$00
LDA #$00
:
STA entities, X
INX 
CPX #entity_mem
BNE :-

LDA #EntityType::OptionsScreenSelector
LDX #$40
LDY #$40
JSR SpawnEntity

jsr SwapLeftBankToTitle
JSR SwapRightBankToTitle

LDA #$09
STA DrawFlags


LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #$05
JSR PrepareLevelData


LDA #GState::GameStateMapSelect
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

; LDX #$80
; LDY #$20
; LDA #EntityType::Player 
; JSR SpawnEntity


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

LDA #$04
STA GameState


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

JSR SwapLeftBankToGame
; JSR SwapRightBankToTower

LDA #$02
STA CurrentMap
JSR PrepareLevelData
JSR LoadCollisionData

LDA CurrentMap
JSR InitMap
JSR LoadPalletteToRam

LDA #$0c
STA DrawFlags


RTS

InitVictoryScreen:
LDA #$20
STA CurrentColumnHigh
LDA #$00
STA CurrentColumnLow

LDA #GState::GameStateVictory
STA GameState

JSR SwapRightBankToTitle
JSR SwapLeftBankToTitle

LDA #$01
JSR PrepareLevelData
JSR LoadPalletteToRam

LDA #DFlags::ClearScreenForVictory
STA DrawFlags

RTS




InitMap:
    ; LDY #$00 
    ; :
    ; LDA (world), Y 
    ; STA CollisionMap, Y 
    ; INY 
    ; CPY #$F0 
    ; BEQ :+
    ; JMP :-
    ; :


    LDA CurrentMap
    ASL
    TAY 
    LDA InitMapList, Y 
    STA jumppointer
    LDA InitMapList+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine
RTS 

InitMapRampart:

    JSR SwapRightBankToRampart
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
    LDA #$01
    STA NMIEnableLevelEffects
RTS

InitMapGarden:
RTS

InitMapRest:
    ldx #%00000010
    lda #SNOWBANK1
    STX BANKSELECT
    STA BANKDATA
    ldx #%00000011
    lda #SNOWBANK2
    STX BANKSELECT
    STA BANKDATA
    ldx #%00000100
    lda #SNOWBANK3
    STX BANKSELECT
    STA BANKDATA
    ldx #%00000101
    lda #SNOWBANK4
    STX BANKSELECT
    STA BANKDATA

    ; LDA #$02
    ; STA temp
    ; LDA #$10
    ; STA temp2
    ; STA temp3
    ; LDX #$80
    ; LDY #$B0
    ; JSR SpawnEmitter





    LDA #$01
    STA NMIEnableLevelEffects
RTS

InitMapTower:
    JSR SwapRightBankToTower
    LDA #TOWERBANK1
    STA CurrentTowerRotation
    JSR SetTowerRotatorBank


    LDA #$C0
    STA WindowOffset1
    LDA #$D0
    STA WindowOffset2
    LDA #$E0
    STA WindowOffset3
    LDA #$F0
    STA WindowOffset4

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
    JSR JumpToPointerRoutine
    RTS

DoTitleLogic:
    JSR ReadButtons
    JSR ProcessEntities
    JSR ProcessTitleInput
    RTS

DoMapSpecificLogic:
    LDA CurrentMap
    ASL 
    TAY 
    LDA MapSpecificLogicTree, Y 
    STA jumppointer
    LDA MapSpecificLogicTree+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine
RTS

MapRampartLogic:
    JSR UpdateConveyorBelts
    JSR UpdateWaterfalls

    LDA rng
    AND #%00000001
    BNE :+
    LDX #MAXPARTICLES-1
    LDA ParticlesType, X
    BNE :+
    JSR Prng
    TAX
    LDY #$00
    LDA #$05
    STA temp
    JSR SpawnParticles
    :


    RTS 

MapTowerLogic:

    ldA #$01
    BIT framecount
    bne :++
    LDA CurrentTowerRotation
    CLC 
    ADC #$01
    CMP #TOWERBANK1+4
    BNE :+
    lda #TOWERBANK1
    :
    sta CurrentTowerRotation
    JSR SetTowerRotatorBank
    :

    LDX WindowIndex

    LDA TowerOffset1, X 
    STA WindowOffset1
    CLC
    ADC #$01
    STA WindowOffset1+1
    CLC
    ADC #$01
    STA WindowOffset1+2
    CLC
    ADC #$01
    STA WindowOffset1+3

    INC WindowIndex
    LDA WindowIndex
    CMP #$04
    BNE :+
    LDA #$00
    STA WindowIndex
    :

    LDA WindowIndex
    CMP #$02
    BNE :+
    INC CurrentCloudBank
    LDA CurrentCloudBank
    CMP #CLOUDSBANK8+1
    BNE :+
    LDA #CLOUDSBANK1
    STA CurrentCloudBank
    :

    NoCloudUpdate:
    LDA CurrentCloudBank
    LDX #%00000100
    STX BANKSELECT
    STA BANKDATA

    LDA WindowOffset1
    CLC 
    ADC #$10
    STA WindowOffset2
    ADC #$10
    STA WindowOffset3
    ADC #$10
    STA WindowOffset4

    LDA WindowOffset1+1
    CLC 
    ADC #$10
    STA WindowOffset2+1
    ADC #$10
    STA WindowOffset3+1
    ADC #$10
    STA WindowOffset4+1

    LDA WindowOffset1+2
    CLC 
    ADC #$10
    STA WindowOffset2+2
    ADC #$10
    STA WindowOffset3+2
    ADC #$10
    STA WindowOffset4+2

    LDA WindowOffset1+3
    CLC 
    ADC #$10
    STA WindowOffset2+3
    ADC #$10
    STA WindowOffset3+3
    ADC #$10
    STA WindowOffset4+3

RTS 

TowerOffset1:
    .byte $80,$84,$88,$8C
TowerOffset2:
    .byte 1,5,9,13
TowerOffset3:
    .byte 2,6,10,14
TowerOffset4:
    .byte 3,7,11,15

MapGardenLogic:
    RTS


MapRestLogic:
    INC UpperScrollX


    LDX #MAXPARTICLES-1
    LDA ParticlesType, X
    BNE :++
    JSR Prng
    TAX
    :
    JSR Prng
    CMP #$58
    BCC :-
    CMP #$C0 
    BCS :-

    TAY
    LDA #$02
    STA temp
    JSR SpawnParticles
    :


    JSR ProcessWind

    LDA #%00000011
    BIT framecount
    BEQ :+
        RTS
    :
    LDA CurrentTowerRotation
    CLC 
    ADC #$01
    CMP #MAXSNOWBANK
    BNE :+
    LDA #MINSNOWBANK
    :
    STA CurrentTowerRotation
    JSR SetTowerRotatorBank


RTS


DoGameLogic:
    JSR ReadButtons
    JSR DecrementTimers
    JSR ProcessEntities
    JSR ProcessParticles
    ; JSR ProcessDestructionStack
    ; JSR ProcessPlayerSpawnStack
    JSR DoMapSpecificLogic
    JSR CheckForGameEnd
    RTS 

DoOptionsLogic:
    JSR ReadButtons
    JSR ProcessOptionsInput
    JSR ProcessEntities
    JSR FillOptionsBuffer
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

ProcessOptionsInput:
    lda framecount
    beq :+
    rts
    :

    JSR CheckDown
    Bne :++
    INC MenuSelectorIndex
    LDA MenuSelectorIndex
    CMP #OPTIONSNUMBER
    BCC :+
    LDA #$00
    :
    STA MenuSelectorIndex
    :

    JSR CheckUp
    BEQ :+
    DEC MenuSelectorIndex
    BPL :+ 
    LDA #OPTIONSNUMBER-1
    STA MenuSelectorIndex
    :

    LDA MenuSelectorIndex
    ASL
    TAY 
    LDA OptionsChangerList, Y 
    STA jumppointer
    LDA OptionsChangerList+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine

    ; JSR CheckStart
    ; BEQ :+
    ; `Hat
    ; :
    LDA rng
    cmp #$ff
    BNE :+
    ; LDA #$03
    ; STA CurrentMap
    jsr InitGameHat
    :

RTS

OptionsChangerList:
    .word OptionsChangePlayerCount
    .word OptionsChangeMap
    .word OptionsChangePalette

OptionsChangePlayerCount:
    JSR CheckLeft
    CMP #ButtonReturn::Release
    BNE :+
    DEC CurrentPlayerCount
    :
    JSR CheckRight
    CMP #ButtonReturn::Release
    BNE :+
    INC CurrentPlayerCount
    :

    LDA CurrentPlayerCount
    CMP #$04
    BNE :+
    LDA #$00
    STA CurrentPlayerCount
    :
    LDA CurrentPlayerCount
    BPL :+
    LDA #$03
    STA CurrentPlayerCount
    :
RTS

OptionsChangeMap:
    JSR CheckLeft
    CMP #ButtonReturn::Release
    BEQ :+
    DEC CurrentMap
    :
    JSR CheckRight
    CMP #ButtonReturn::Release
    BNE :+
    INC CurrentMap
    :

    LDA CurrentMap
    CMP #$04
    BNE :+
    LDA #$00
    STA CurrentMap
    :
    LDA CurrentMap
    BPL :+
    LDA #$03
    STA CurrentMap
    :


RTS

OptionsChangePalette:
    JSR CheckLeft
    CMP #ButtonReturn::Release
    BNE :+
    DEC CurrentPalette
    :
    JSR CheckRight
    CMP #ButtonReturn::Release
    BNE :+
    INC CurrentPalette
    :
    LDA CurrentPalette
    CMP #$04
    BNE :+
    LDA #$00
    STA CurrentPalette
    :
    LDA CurrentPalette
    BPL :+
    LDA #$03
    STA CurrentPalette
    :
RTS

FillOptionsBuffer:
    LDX #$00
    LDY #$00
    :
    LDA CurrentPlayerCount
    STA PlayerCountBuffer, X
    INX 
    CPX #$09
    BNE :-

    LDA CurrentMap
    ASL 
    TAY 
    LDA MapNameList, Y 
    STA jumppointer
    LDA MapNameList+1, Y 
    STA jumppointer+1

    LDY #$00
    :
    LDA (jumppointer), Y 
    STA MapBuffer, Y 
    INY 
    CPY #$12
    BNE :- 

    LDA CurrentPalette
    ASL 
    TAY 
    LDA PaletteNameList, Y 
    STA jumppointer
    LDA PaletteNameList+1, Y
    STA jumppointer+1

    LDY #$00
    :
    LDA (jumppointer), Y 
    STA PaletteBuffer, Y 
    INY 
    CPY #$07
    BNE :-


RTS


ProcessTitleInput:
    ; JSR CheckStart
    ; CMP #ButtonReturn::Release
    LDA rng
    CMP #$ff
    BNE :+
        ; JSR InitGameHat
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

SwapRightBankToRampart:
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

SwapRightBankToTower:

LDX #%00000010
LDY #TOWERBANKBODY1
STX BANKSELECT
STY BANKDATA

LDX #%00000011
LDY #TOWERBANKBODY2
STX BANKSELECT
STY BANKDATA

LDX #%00000100
LDY #CLOUDSBANK1
STX BANKSELECT
STY BANKDATA


LDX #%00000101
LDY #TOWERBANK1
STX BANKSELECT
STY BANKDATA
; LDX #%0000010
; LDY #TOWERBANK
; STX BANKSELECT
; STY BANKDATA
; RTS

SetTowerRotatorBank:
LDX #%00000101
LDY CurrentTowerRotation
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

;xpos in x 
;ypos in y 
;type in temp
;width in temp2 
;height in temp3
SpawnEmitter:
    TYA 
    PHA 
    TXA 
    PHA
    LDX #$00
    SpawnEmitterLoop:
        CPX #MAXEMITTERS
        BEQ EndEmitterSpawn
        LDA EmitterType, X 
        BEQ :+
        INX 
        JMP SpawnEmitterLoop
        :
        PLA
        STA EmitterX, X 
        PLA 
        STA EmitterY, X
        LDA temp
        STA EmitterType, X 
        LDA temp2
        STA EmitterWidth, X
        LDA temp3
        STA EmitterHeight, X
        RTS 
    EndEmitterSpawn:
    PLA 
    PLA
RTS

ProcessEmitters:
    LDY #$00
    LDX #$00
    ProcessEmitterLoop:
    jmp NextEmitter
    LDA ParticlesType, X 
    BEQ NextEmitter

    ASL 
    TAY 
    LDA EmitterStates, Y 
    STA jumppointer
    LDA EmitterStates+1, Y 
    STA jumppointer+1
    JMP (jumppointer)
    
    NextEmitter:
    INX
    CPX #MAXEMITTERS
    BEQ :+
    JMP ProcessEmitterLoop
    :
RTS

;x = xpos
; y = ypos
; temp = type
SpawnParticles:
    TYA 
    PHA 
    TXA 
    PHA
    LDX #$00
    ; LDA ParticlesCount
    ; CMP #MAXPARTICLES
    ; BEQ
    SpawnParticleLoop:
        CPX #MAXPARTICLES
        BEQ EndParticleSpawn
        LDA ParticlesType, X 
        BEQ :+
        INX 
        JMP SpawnParticleLoop
        :
        PLA
        STA ParticlesX, X 
        PLA 
        STA ParticlesY, X
        LDA #$00
        STA ParticlesSubX, X
        STA ParticlesSubY, X
        STA ParticlesVX, X 
        LDA #$01
        STA ParticlesVY, X
        lda #$00
        STA ParticlesVXLow, X 
        sta ParticlesVYLow, X
        LDA #%00000011
        STA ParticlesAttributes, X
        LDA #$00
        sta ParticlesTimer1, X
        LDA #%00000011
        STA ParticlesSign, X
        ; lda #$bd
        LDA rng 
        and #%00000001
        ADC #$bd
        STA ParticlesSpriteNo, x
        LDA temp
        STA ParticlesType, X 
        RTS 
    EndParticleSpawn:
    PLA 
    PLA
RTS

ProcessParticles:

    LDY #$00
    LDX #$00
    ProcessParticlesLoop:
    LDA ParticlesType, X 
    BEQ NextParticle

    ASL 
    TAY 
    LDA ParticleStates, Y 
    STA jumppointer
    LDA ParticleStates+1, Y 
    STA jumppointer+1
    JMP (jumppointer)
    
    NextParticle:
    INX
    CPX #MAXPARTICLES
    BEQ :+
    JMP ProcessParticlesLoop
    :


RTS

ProcessWind:
    LDA WindState
    ASL 
    TAY 
    LDA WindStateMachine, Y 
    STA jumppointer
    LDA WindStateMachine+1, Y 
    STA jumppointer+1
    JSR JumpToPointerRoutine
RTS

WindInactive:
    RTS 

WindWaiting:
    ; LDA #$80
    ; STA ParticlesWindStrengthXLow
    DEC WindTimerLow
    BEQ :+
    RTS 
    :
    LDA #$02
    STA WindState
    LDA #$FF
    STA WindTimerLow
    JSR Prng
    AND #%00000001
    CMP #$01
    BEQ :+
    LDA #$00
    :
    STA WindSign
RTS

WindIncreasing:
    LDA ParticlesWindStrengthXLow
    CLC 
    ADC #$01
    STA ParticlesWindStrengthXLow
    LDA ParticlesWindStrengthXHigh
    ADC #$00
    STA ParticlesWindStrengthXHigh

    DEC WindTimerLow
    BNE :+
    LDA #$03
    STA WindState
    LDA #$FF
    STA WindTimerLow
    AND #$00000001
    STA WindTimerHigh
    :
RTS 

WindDecreasing:
    LDA ParticlesWindStrengthXLow
    SEC 
    SBC #$01 
    STA ParticlesWindStrengthXLow
    LDA ParticlesWindStrengthXHigh
    SBC #$00
    STA ParticlesWindStrengthXHigh

    LDA ParticlesWindStrengthXLow
    BEQ :+
    RTS 
    :
    LDA ParticlesWindStrengthXHigh
    BEQ :+
    RTS 
    :
    LDA #$FF 
    STA WindTimerLow
    LDA #$01 
    STA WindState
RTS

WindBlowing:
    DEC WindTimerLow
    BNE :+
    LDA #$04
    STA WindState
    :
RTS

ProcessEntities:
    LDX #$00
    ProcessEntitiesLoop:

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
    .word CrystalStateMachine
    .word OptionsScreenSelectorStateMachine

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
    .word ProjectileSpellInit
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

 CrystalStateMachine:
    .word CrystalInit
    .word CrystalIdle
; Entity Behaviours

OptionsScreenSelectorStateMachine:
    .word OptionScreenSelectorInit
    .word OptionScreenSelectorProcess

;;;;;

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
    JSR PlayerApplyFriction
    JSR PlayerAttack
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; :
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
    JSR ApplyFriction
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; apply movement
    JSR PlayerApplyVelocityX
    JSR EjectFromLeftWall
    JSR EjectFromRightWall

    ; jump velocity handling
    LDY playerjumptrack
    LDA JumpStrength, Y
    CMP #$01
    BEQ :+
    INC playerjumptrack
    CLC 
    ADC entities+Entity::ypos, X
    STA entities+Entity::ypos, X
    JMP NoJumpTrackEnd
    :
    ; Check if we're inside a jumpable platform
    JSR CollideDown2
    CMP #$04
    BEQ :+
    LDA #$06
    STA entities+Entity::generalpurpose, X
    JMP NoJumpTrackEnd
    :
    LDA #$07
    STA entities+Entity::generalpurpose, X
    NoJumpTrackEnd:
    ;check for headbonk
    JSR CollideUp2
    CMP #04
    BEQ :+
    CMP #$00
    BEQ :+
        LDA #$05
        STA entities+Entity::generalpurpose, X
        JSR EjectFromTopWall
        JMP EntityComplete
    :
    

    JMP EntityComplete
PlayerJumpReleased:
    JMP EntityComplete
PlayerHeadbonk:
    ; left/right movement
    JSR ApplyFriction
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; apply movement
    JSR PlayerApplyVelocityX
    JSR EjectFromLeftWall
    JSR EjectFromRightWall

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
    JSR EjectFromTopWall
    JMP EntityComplete
PlayerFalling:
    JSR ApplyFriction
    JSR CheckA
    ; left/right movement
    JSR CheckRight
    Beq :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    JSR PlayerApplyVelocityX
    JSR EjectFromRightWall
    JSR EjectFromLeftWall

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
    ; DEC entities+Entity::ypos, X
        JSR EjectFromBottomWallAndPlatforms
    :
    JSR CollideRight2
    CMP #$04
    BNE :+
        LDA #$07 
        STA entities+Entity::generalpurpose, X
        JMP EndCheckForFloor
    :

    EndCheckForFloor:
    
    JMP EntityComplete
PlayerFallingThroughPlatform:
    JSR ApplyFriction
    ; left/right movement
    JSR CheckRight
    BEQ :+
        JSR PlayerAddSpeed
    :
    JSR CheckLeft
    BEQ :+
        JSR PlayerSubSpeed
    :
    ; apply movement
    JSR PlayerApplyVelocityX
    JSR EjectFromLeftWall
    JSR EjectFromRightWall

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
        JSR CollideUp2
        CMP #$04
        BEQ :+
        LDA #$06
        sta entities+Entity::generalpurpose, X
    :
    jmp EntityComplete
PlayerDisabled:
    JMP EntityComplete


PlayerApplyFriction:
    LDA vxlow
    SEC 
    SBC #FRICTION
    STA vxlow
    LDA vxhigh
    SBC #$00
    STA vxhigh
    BPL :+
    LDA #$00
    STA vxhigh
    STA vxlow
    :

    LDA vxlowleft
    SEC 
    SBC #FRICTION
    STA vxlowleft
    LDA vxhighleft
    SBC #$00
    STA vxhighleft
    BPL :+
    LDA #$00
    STA vxhighleft
    STA vxlowleft
    :
    LDA vxhigh
    BPL :+
    LDA #$00
    STA vxhigh
    STA vxlow
    :
    LDA vxhighleft
    BPL :+
    LDA #$00
    STA vxhighleft
    STA vxlowleft
    :


RTS


PlayerApplyVelocityX:
    LDA entities+Entity::xpossub, X 
    CLC 
    ADC vxlow
    STA entities+Entity::xpossub, X 
    LDA entities+Entity::xpos, X 
    ADC vxhigh
    STA entities+Entity::xpos, X


    LDA entities+Entity::xpossub, X 
    SEC 
    SBC vxlowleft
    STA entities+Entity::xpossub, X 
    LDA entities+Entity::xpos, X 
    SBC vxhighleft
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
    LDA vxhigh
    ADC #$00
    STA vxhigh
    ; :
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
    LDA vxlowleft
    CLC 
    ADC #PLAYERACCELLERATION
    STA vxlowleft
    LDA vxhighleft
    ADC #$00
    STA vxhighleft
    ; :
    ; cap speed
    LDA vxhighleft
    CMP #$01
    BCC :+
    LDA #$01 
    STA vxhighleft
    LDA #00
    STA vxlowleft
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
    .byte $06
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

ProjectileSpellInit:
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
    BEQ :+
    ; if 0, it is facing left and needs to go left
    LDA #$01
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete
    :
    LDA #$02 
    STA entities+Entity::generalpurpose, X 
    JMP EntityComplete

AddEntityToDestructionStack:
    RTS

ProjectileSpellMovingLeft:
    JSR CollideLeft2
    BEQ :+
        TXA 
        JSR AddEntityToDestructionStack
    :
    DEC entities+Entity::xpos, X
    LDA #$01
    BIT framecount
    BNE :++
    LDA entities+Entity::animationframe, X
    SEC 
    SBC #$01
    BPL :+
    LDA #$09
    :
    STA entities+Entity::animationframe, X
    :
    JMP EntityComplete
ProjectileSpellMovingRight:
    JSR CollideRight2
    BEQ :+
        TXA 
        JSR AddEntityToDestructionStack
    :
    INC entities+Entity::xpos, X
    LDA #$01
    BIT framecount
    BNE :++
    LDA entities+Entity::animationframe, X
    CLC 
    ADC #$01
    CMP #$0a
    BNE :+
    LDA #$00
    :
    STA entities+Entity::animationframe, X
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

CrystalInit:
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JSR Prng 
    AND #%00000011
    lda #%00000001
    STA entities+Entity::attributes, X
    STA entities+Entity::animationtimer, X

CrystalIdle:
    DEC entities+Entity::animationtimer, X 
    BNE :+
    LDA #$10
    sta entities+Entity::animationtimer, X
    LDA #%00000001
    EOR entities+Entity::animationframe, X
    STA entities+Entity::animationframe, X
    :
    JMP EntityComplete

OptionScreenSelectorInit:
    LDA #$01
    STA entities+Entity::generalpurpose, X
    JMP EntityComplete

OptionScreenSelectorProcess:

    LDA MenuSelectorIndex
    asl 
    asl
    asl
    asl
    ASL
    ADC #$40
    STA entities+Entity::ypos, X

    LDY entities+Entity::animationtimer, X
    LDA Sin, y 
    CLC 
    ADC entities+Entity::xpos, x 
    STA entities+Entity::xpos, x 
    INY 
    CPY #68 
    BNE :+
    LDY #$00
    :
    TYA 
    STA entities+Entity::animationtimer, x

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

EmitterProcessSnow:

JMP NextEmitter

EmitterProcessHovering:
    LDA EmitterParticleNumber, X 
    CMP #5
    BNE :+
    JMP NextEmitter 
    :
    INC EmitterParticleNumber, X
    LDA #$02
    STA temp
    LDX #$10
    LDY #$10
    JSR SpawnParticles
JMP NextEmitter

ParticlesProcessWind:
JMP NextParticle

ParticlesProcessHoveringVertical:

    LDY ParticlesTimer1, X 
    LDA Sin, Y 
    CLC 
    ADC ParticlesY, X
    STA ParticlesY, X
    ; LDA Sin, y 
    ; CLC 
    ; adc ParticlesX, x 
    ; sta ParticlesX,x

    INY
    CPY #68
    BNE :+
    LDY #$00
    :
    TYA
    STA ParticlesTimer1, X 

    LDA WindSign
    BEQ :+
    ; JSR Prng
    ; AND #%00000001
    ; CLC 
    ; ADC ParticlesX, X 
    ; STA ParticlesX, X
    LDA ParticlesSubX, X 
    CLC 
    ADC ParticlesWindStrengthXLow
    STA ParticlesSubX,X
    LDA ParticlesX, X 
    ADC ParticlesWindStrengthXHigh
    STA ParticlesX, X 
    JMP NextParticle
    :
    ; JSR Prng
    ; AND #%00000001
    ; SEC 
    ; SBC ParticlesX, X 
    ; STA ParticlesX, X
    LDA ParticlesSubX, X 
    SEC 
    SBC ParticlesWindStrengthXLow
    STA ParticlesSubX,X
    LDA ParticlesX, X 
    SBC ParticlesWindStrengthXHigh
    STA ParticlesX, X 

JMP NextParticle

FlipParticleSignY:
        LDA #%00000010
        EOR ParticlesSign, X 
RTS

FlipParticleSignX:
        LDA #%00000001
        EOR ParticlesSign, X 
RTS

ParticlesProcessHoveringHorizontal:
    INC ParticlesY, X
JMP NextParticle

ParticlesProcessHoveringBoth:
    INC ParticlesY, X
JMP NextParticle

ParticlesProcessRain:
    LDA ParticlesSubY, X 
    CLC 
    ADC #$80
    STA ParticlesSubY
    LDA ParticlesY, X 
    ADC #$06
    STA ParticlesY, X 
    CMP #$F0 
    BCC :+
    LDA rng
    STA ParticlesX, X 
    AND #%00000111
    STA ParticlesY, X
    :
    INC ParticlesX, X
    INC ParticlesX, X
    INC ParticlesX, X


JMP NextParticle

ParticlesApplyMovement:
    ; APPLY FRICTION
    ; LDA ParticlesVX, X
    ; BMI :+
    ; LDA ParticlesVXLow, X 
    ; SEC 
    ; SBC #PARTICLESFRICTION
    ; STA ParticlesVXLow, X 
    ; LDA ParticlesVX, X 
    ; SBC #$00
    ; STA ParticlesVX, X
    ; :
    ; CAP SPEED
    ; LDA ParticlesVX, X 
    ; CMP #PARTICLESPEEDMAXVX
    ; BCC :+
    ; LDA #PARTICLESPEEDMAXVX
    ; STA ParticlesVX, X
    ; :

    ; APPLY VELOCITY

    LDA ParticlesVYLow, X
    BMI :+
    LDA ParticlesSubY, X
    CLC
    ; ADC #$40
    ADC ParticlesVYLow, x
    STA ParticlesSubY, X 
    LDA ParticlesY, X 
    ADC #$00
    STA ParticlesY, X 
    JMP NextParticle
    :
    LDA ParticlesSubY, X
    CLC 
    ADC #128
    SEC
    SBC ParticlesVYLow, x
    STA ParticlesSubY, X 
    LDA ParticlesY, X 
    SBC #$00
    STA ParticlesY, X 

JMP NextParticle

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
    lda #$00
    RTS
    LDA #$00
    STA currentrowodd
    STA currentroweven

    LDA #$00 
    LDY #$00
    LDX #$00
STX PPUData
INX 
CPX #$00
BNE :-
RTS


    LoadScreenLoop:
        LoadOddRow:
            LDY currentrowodd
            LDA (world), Y ; get a meta tile
            ASL
            TAY ; move metatile ref to Y
            LDA MetaTileList, Y
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
            INX 
            CPX #$10
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
                
                LDY #$00
                LDA (jumppointer), Y 
                STA PPUData
                INY 
                LDA (jumppointer), Y 
                STA PPUData
                INC currentroweven

                ; INY ; 5th byte is collision data 
                ; LDA (jumppointer), Y
                ; PHA 
                ; TXA 
                ; TAY
                ; PLA 
                ; STA (currentcollisionaddress), Y

                INX 
                CPX #$10
                BNE LoadEvenRowLoop

        ; LDA currentcollisionaddress
        ; CLC 
        ; ADC #$10
        ; STA currentcollisionaddress
        ; LDA currentcollisionaddress+1
        ; ADC #$00 
        ; STA currentcollisionaddress+1
        LDA currentrowodd
        CLC 
        ADC #$10
        STA currentrowodd
        LDA currentroweven
        CLC 
        ADC #$10
        STA currentroweven
        CMP #$40
        BNE LoadScreenLoop

EndScreenLoad:
; LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
; STA PPUControl
; ; Enabling sprites and background for left-most 8 pixels
; ; Enable sprites and background
; LDA #%00111110
; STA PPUMask
; CLI
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
    LDA #ButtonReturn::Press
    RTS
    CheckSelectRelease:
        LDA buttonflag
        AND #$04
        BEQ :+
        LDA buttonflag
        EOR #$04 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA buttonflag
    ORA #$08
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS
    CheckStartRelease:
        LDA buttonflag
        AND #$08
        BEQ :+
        LDA buttonflag
        EOR #$08 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA buttonflag
    ORA #$10
    STA buttonflag
    LDA #ButtonReturn::Press
    RTS
    CheckUpRelease:
        LDA buttonflag
        AND #$10
        BEQ :+
        LDA buttonflag
        EOR #$10 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
RTS 

CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA buttonflag 
    ORA #$20 
    STA buttonflag 
    LDA #ButtonReturn::Press
    RTS
    CheckDownRelease:
        LDA buttonflag
        AND #$20
        BEQ :+
        LDA buttonflag
        EOR #$20 
        STA buttonflag
        LDA #ButtonReturn::Release
        RTS 
:
LDA #ButtonReturn::NoPress
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

EjectFromRightWallAndPlatforms:
    JSR CollideRight2 ; check initial position
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

EjectFromLeftWallAndPlatforms:
    JSR CollideLeft2 ; check initial position
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

EjectFromBottomWallAndPlatforms:
    JSR CollideDown2 ; check initial position
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

EjectFromTopWallAndPlatforms:
    JSR CollideUp2 ; check initial position
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
    LDA MetaTileListTower, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileListTower+1, Y ; 4/5 
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
    LDA MetaTileListTower, Y 
    STA jumppointer 
    LDA MetaTileListTower+1, Y 
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

CollideRight2:

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
    LDA MetaTileListTower, Y ;4/5
    STA jumppointer ; 3
    LDA MetaTileListTower+1, Y ; 4/5 
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
    LDA MetaTileListTower, Y 
    STA jumppointer 
    LDA MetaTileListTower+1, Y 
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

CollideDown2:

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
    LDA MetatilelistSnow, Y 
    STA jumppointer
    LDA MetatilelistSnow+1, Y 
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
    LDA entities+Entity::ypos, X 
    SEC 
    SBC temp2
    CMP #$09 
    BCS :+
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
    LDA MetatilelistSnow, Y 
    STA jumppointer
    LDA MetatilelistSnow+1, Y 
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
    BCS :+
    INY
    :
    LDA entities+Entity::ypos, X
    SEC 
    SBC temp2
    CMP #$09
    BCS :+
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

CollideUp2:
    CollideUp2Loop:
    ; Get x position and divide it by 16 X/256 -> X/16. It now corresponds to the 16x15 collision array
    LDA entities+Entity::xpos, X  
    LSR 
    LSR 
    LSR 
    LSR ; divide by 16
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
    LDA entities+Entity::ypos, X  
    SEC 
    SBC temp2 
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
    BCC :+
    INY
    :
    LDA entities+Entity::ypos, X 
    SEC 
    SBC temp2 
    CMP #$09 
    BCC :+
    INY 
    INY 
    :
    LDA (jumppointer), Y
    ORA rectangle1
    
    ; BEQ :+

    ;     CMP #$04
    ;     BEQ :+

    ;     STA temp3 
    ;     INC entities+Entity::ypos, X 
    ;     JMP CollideUp2Loop
    ; :
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
    ROR
    STA rng+1
    ROR 
    STA rng+2
    ROR 
    STA rng+3
    ROR 
    STA rng+4
    ROR 
    STA rng+5
    ROR 
    STA rng+7
    ROR 
    STA rng+8
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

OAMBufferParticles:
    LDX #$00
    LDY spritebufferposition
    Drawparticleloop:
    CPY #$00 
    BNE :+
    RTS
    :

    LDA ParticlesType, X 
    BEQ :+

    LDA ParticlesY, X
    STA SpriteBuffer, Y
    INY 
    LDA ParticlesSpriteNo, X
    STA SpriteBuffer, Y 
    INY
    LDA ParticlesAttributes, X
    STA SpriteBuffer, Y 
    INY
    LDA ParticlesX, X
    STA SpriteBuffer, Y
    INY 

    :
    INX 
    CPX #MAXPARTICLES
    BNE Drawparticleloop    

RTS

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


.include "data.asm"

; .include "famistudio_ca65.s"
; .include "testsong.s"


; .segment "SONG1"
; 
; .segment "DPCM"


.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
    .word IRQ
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "title_screen_bank.chr"
    .incbin "castle_set-bank1.chr"
    .incbin "tower1.chr"
    .incbin "cloudbank.chr"
    .incbin "snow.chr"

    ; .incbin "castle_set-bank1.chr"
    ; .incbin "tower3.chr"
    ; .incbin "castle_set-bank2.chr"