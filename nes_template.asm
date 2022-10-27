; This is a template for a basic NES rom
; it uses ines 1.0, not 2.0

;FEATURES
; - Entity System
; - Bank Swapping between 2 chr banks


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

.scope EntityType ; NB this should match the order in the process entity list in order for the jump table to hit the right address
    NoEntity = 0
    Player = 1
    Player2 = 2
.endscope

.struct Entity ;  base entity structure
    type .byte ; each entity has its own type assigned, if you somehow make more than 255 entities than idk what to do
    xpos .byte ; x position
    ypos .byte ; y position
    spriteno .byte ;
    palette .byte ; which of the 4 active palettes this sprite should use
    generalpurpose .byte ; this has no specific use, it can be defined on a per entity basis
.endstruct

.segment "ZEROPAGE" ; 0-FF. One page of ram that is faster access than rest of the ram. Use for values most frequently used
    MAXENTITIES = 30; max allowed number of entities. Each entity takes a number of bytes in the zero page equal to the entity struct
    ; this CANNOT run over the end of the zero page or it will not work. If you want more entities, you will need to dedicate a non zero
    ; page ram segment to it
    entities: .res .sizeof(Entity) * MAXENTITIES ; 6 * 30 = 180/256 bytes
    entity_mem = .sizeof(Entity) * MAXENTITIES ; mem used

    world: .res 2 ; this is a pointer to the address of the current screen we want to fetch tiles on
    seed: .res 2 ; the seed for the pseudo rng. This will be inited to anything by 0 later
    jumppointer: .res 2 ; used to jump to specic places in memory with jump tables
    nmidone: .res 1 ; value to check to see if the nmi is done
    scrollx: .res 1 ; how far the screen is scrolled in x dir
    scrolly: .res 1 ; how far the screen is scrolled in y dir
    var_mem: .res 2 ; sometimes we need to jump to a subroutine and do something with more data than can be juggled with x/y/a
    buttons: .res 1 ; this holds the state of player input for controller 1
    buttons_p2: .res 2 ; this holds the state of player input for controller 1
    currenttable: .res 1
    currentrowodd: .res 1 ; holds the currrent row when drawing tiles
    currentroweven: .res 1
    ButtonFlag: .res 1
    framecount: .res 1
    currentbank: .res 1

    ;While we're here, we'll define some locations that will act as buffers in memory. This could be anywhere, its just here for organisation
    SpriteBuffer = $0200 ;$0200 -> $02FF ; A page of ram that will contain sprite info that will be read to the ppu each frame
    TileBufferH = $0300 ; $0300 -> 031F ; Tiles that need to be written when the screen has scrolled are stored here
    CurrentBackgroundPalette = $04C0 ; -> 04CF
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
        STX $2000
        STX $2001

        STX $4010

    :
        BIT $2002 ; this waits for a vblank
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
    BIT $2002
    BPL :-

    LDA #$02
    STA $4014
    NOP

    ; $3F00
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

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
    LDA #$01
    STA seed
    STA seed+1

InitWorld:
    LDA #< ScreenDefault ; take the low byte
    STA world ; store low byte in z page
    LDA #> ScreenDefault ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

; setup address in PPU for nametable data
    BIT $2002 ; reading 2002 sets the latch to access scrolling and writing to the PPU 
    LDA #$20 ; write the address of the part of vram we want to start at, upper byte first 20 -> 00
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
    LDY #$00

LoadSingleScreen:
    LDA #< ScreenDefault ; take the low byte
    STA world ; store low byte in z page
    LDA #> ScreenDefault ; take the high byte
    STA world+1 ; etc

    ; LDA #$0F ; push this to the stack as we need two loops 
    ; PHA 
    LDA #$00
    STA currentrowodd
    STA currentroweven

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
            STA $2007
            INY 
            LDA (jumppointer), Y 
            STA $2007
            INC currentrowodd
            DEX 
            BNE LoadOddRow ; break this loop when we've done one row

        LDY #$00

        LoadEvenRow:
            LDX #$10
            LoadEvenRowLoop:
                LDY currentroweven
                LDA (world), Y
                ASL
                TAY 
                LDA MetaTileList, Y
                STA jumppointer
                lda MetaTileList+1, Y
                STA jumppointer+1 
                LDY #$02

                LDA (jumppointer), Y 
                STA $2007
                INY 
                LDA (jumppointer), Y 
                STA $2007
                INC currentroweven
                DEX 
                BNE LoadEvenRowLoop

        LDA currentroweven
        CLC 
        ADC $10
        STA currentrowodd
        STA currentroweven
        CMP #$F0 
        BNE LoadScreenLoop

EndScreenLoad:
                

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
    STA $2006
    LDA #$C0
    STA $2006
    AttributeLoop:
    LDA AttributesDefault, X 
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop

    LDX #$00
    LDY #$00    

; Spawn a player 
LDA PlayerData
STA jumppointer
LDA PlayerData+1
STA jumppointer+1
LDA #$50
LDX #$50
JSR SpawnEntity
LDA PlayerData
STA jumppointer
LDA PlayerData+1
STA jumppointer+1
LDA #$60
LDX #$90
JSR SpawnEntity
LDA PlayerData
STA jumppointer
LDA PlayerData+1
STA jumppointer+1
LDA #$10
LDY #$30
JSR SpawnEntity 


; Set Control
; to configure MMC1 mapper, we need to write to 8000 repeatedly
LDA #%00000011 ; This configures it to horizontal mirroring, 32kb of prg rom and two 8kb chr banks

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

; Enable interrupts
CLI

LDA #%10010000 ; enable NMI, change background to use second chr set of tiles ($1000)
STA $2000
; Enabling sprites and background for left-most 8 pixels
; Enable sprites and background
LDA #%00111110
STA $2001


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    JSR DoGameLogic 
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR AlternateBanks
    ;JSR DoScroll       
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
    STA $2001 ; disable rendering before we access the ppu for safety

    ;JSR DrawColumnNMI
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  
    JSR ReadScroll  ; Send the current scroll to the ppu
    ;JSR UpdatePaletteNMI
    ;JSR LoadAttributesNMI

    LDA #%10010000
    ORA currenttable
    ORA #%00000100
    STA $2000 ; ???

    LDA #%00111110
    STA $2001 ; reenable rendering for the ppu
    ;JSR SoundPlayFrame
    INC nmidone 

    ; Bring the stack values back into the registers
    PLA
    TAY 
    PLA 
    TAX 
    PLA 
    
    RTI

; Loading into 4014 automatically takes what you give as a high byte and writes 256 bytes to the PPU (so 02 == 0200 all thr way to FF)
; In a real nes this neads to be done every frame b/c dynamic ram degradation, its technically possible to avoid in some emulators, but best just to do it. 
ReadSprites:
    LDA #$00
    STA $2003
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display automatically.
    STA $4014
    LDX #$00
RTS

ReadScroll:
    SetScroll:
    LDA $2002 ; reading from 2002 sets a latch that allows you to set the scroll in 2005
    LDA scrollx
    STA $2005
    LDA scrolly
    STA $2005
RTS


;;;;;;;;;;;;;;;;;;;;
;; Functions to call in the main loop
;;;;;;;;;;;;;;;;;;;;

DoGameLogic:
    JSR ProcessEntities

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
        JSR PlayerOneBehaviour
        JMP EntityComplete
    ProcessPlayer2:
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
    DoneProcessingEntities:
    NOP ; why are there nops here? 
    NOP
    NOP
    NOP
RTS

PlayerOneBehaviour:
    LDA entities+Entity::xpos, X
    CLC 
    ADC #$01 
    STA entities+Entity::xpos, X
    JSR CheckButtons
RTS 


ProcessEntityList: ; Jump table for processing entities
    .word SkipEntity
    .word ProcessPlayer
    .word ProcessPlayer2

; DrawEntityList:
;     .word 

DrawSpriteList: ; this is a list of 
    .word CheckEndSpriteDraw
    .word DrawSingleSprite
    .word DrawSingleSprite

; This expects to find an x and a y pos in variable memory. It expheects an address in jumppointer pointing to the entity data
SpawnEntity:
    PHA 
    TXA 
    PHA ; x and y are on the stack
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
        ;INC currententitynumber unused 
        PLA; grab x pos we set before jumping here
        STA entities+Entity::xpos, X
        PLA ; grab y pos 
        STA entities+Entity::ypos, X
        LDA jumppointer ; grab entitiy type 
        STA entities+Entity::type, X
        LDA jumppointer+1
        STA entities+Entity::spriteno, X
        LDA jumppointer+2
        STA entities+Entity::palette, X
        JMP EndEurydiceSpawn
EndEurydiceSpawn:
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

;;;;;;;;;;;;;
;Input 
;;;;;;;;;;;;;
ReadButtons:
    ; Ping $4016 with a 1 to get it ready to send buttons 
    CLC
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

CheckButtons:
CheckA:
    LDA buttons 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckARelease
    LDA ButtonFlag ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA ButtonFlag
    JSR InputA
    JMP CheckB

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA ButtonFlag
        AND #$01
        BEQ CheckB
        LDA ButtonFlag
        EOR #$01 
        STA ButtonFlag
        JSR InputARelease
CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA ButtonFlag
    ORA #$02
    STA ButtonFlag
    JSR InputB
    JMP CheckSelect

    CheckBRelease:
        LDA ButtonFlag
        AND #$02
        BEQ CheckSelect
        LDA ButtonFlag
        EOR #$02 
        STA ButtonFlag
        JSR InputBRelease

CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckSelectRelease 
    LDA ButtonFlag
    ORA #$04 
    STA ButtonFlag
    JSR InputSelect
    JMP CheckStart

    CheckSelectRelease:
        LDA ButtonFlag
        AND #$04 
        BEQ CheckStart
        LDA ButtonFlag
        EOR #$04 
        STA ButtonFlag
        JSR InputSelectRelease

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA ButtonFlag
    ORA #$08
    STA ButtonFlag
    JSR InputStart
    JMP CheckUp

    CheckStartRelease:
        LDA ButtonFlag
        AND #$08 
        BEQ CheckUp
        LDA ButtonFlag
        EOR #$08 
        STA ButtonFlag
        JSR InputStartRelease

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA ButtonFlag
    ORA #$10
    STA ButtonFlag
    JSR InputUp
    JMP CheckDown

    CheckUpRelease:
        LDA ButtonFlag
        AND #$10
        BEQ CheckDown
        LDA ButtonFlag 
        EOR #$10
        STA ButtonFlag
        JSR InputUpRelease
        
CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA ButtonFlag 
    ORA #$20 
    STA ButtonFlag 
    JSR InputDown
    JMP CheckLeft

    CheckDownRelease:
        LDA ButtonFlag
        AND #$20 
        BEQ CheckLeft 
        LDA ButtonFlag
        EOR #$20 
        STA ButtonFlag
        JSR InputDownRelease   
  
CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckLeftRelease
    LDA ButtonFlag
    ORA #$40 
    STA ButtonFlag 
    JSR InputLeft
    JMP CheckRight

    CheckLeftRelease:
        LDA ButtonFlag
        AND #$40 
        BEQ CheckRight 
        LDA ButtonFlag
        EOR #$40 
        STA ButtonFlag
        JSR InputLeftRelease

CheckRight:

    LDA buttons
    AND #%00000001
    BEQ CheckRightRelease
    LDA ButtonFlag 
    ORA #$80 
    STA ButtonFlag
    JSR InputRight 
    JMP EndButtons

    CheckRightRelease:
        LDA ButtonFlag
        AND #$80
        BEQ EndButtons
        LDA ButtonFlag 
        EOR #$80 
        STA ButtonFlag
        JSR InputRightRelease
 
EndButtons:
RTS

InputA:
    RTS 
InputB:
    RTS 
InputUp:
    RTS 
InputDown:
    RTS 
InputLeft:
    RTS
InputRight:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OAM Buffer Handling ; All sprite data for the frame must be written into the OAM so that it can be sent to the PPU in vblank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OAMBuffer:

    ClearSpriteBuffer:
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
            PHA
    DrawSprites:
        LDA entities+Entity::type, X 
        ASL 
        TAY 
        LDA DrawSpriteList, Y 
        STA jumppointer
        LDA DrawSpriteList+1, Y 
        STA jumppointer+1
        PLA 
        TAY 
        JMP (jumppointer)
  
    DrawSingleSprite:
        LDA entities+Entity::ypos, X 
        SEC 
        SBC #$01 ; for #reasons you always need to sub one from the y 
        STA SpriteBuffer,Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY 
        JMP CheckEndSpriteDraw

    DrawFourBlockSprites: ; This expects the sprie to be arranged horizontally in chrram top left -> top 
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
    
        ;Sprite 2
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
         
        ;sprite 3
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        
        ;sprite 4
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 

        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
        JMP CheckEndSpriteDraw

    CheckEndSpriteDraw:
        TXA 
        CLC 
        ADC #.sizeof(Entity)
        TAX 
        CPX #entity_mem
        BEQ EndSpriteDraw
        TYA
        PHA
        JMP DrawSprites

    EndSpriteDraw:
        RTS 

;;;;;;;;;;;;
;;;; TILE HANDLING
;;;;;;;;;;;;





brick:
    .byte $00
    .byte $01
    .byte $10
    .byte $11
brick_hole:
    .byte $02
    .byte $03
    .byte $12
    .byte $13
brick_dark_left:
    .byte $04
    .byte $05
    .byte $14
    .byte $15
brick_dark_right:
    .byte $06
    .byte $07
    .byte $16
    .byte $17
brick_bright_left:
    .byte $08
    .byte $09
    .byte $18
    .byte $19 
brick_bright_right:
    .byte $0A
    .byte $0B
    .byte $1A 
    .byte $1B
earth:
    .byte $20
    .byte $21
    .byte $30
    .byte $31
earth_top:
    .byte $22
    .byte $23
    .byte $32
    .byte $33
arch_tl:
    .byte $0C
    .byte $0D
    .byte $1C
    .byte $1D
arch_tr:
    .byte $0E
    .byte $0F
    .byte $1E
    .byte $1F
arch_bl:
    .byte $2C
    .byte $2D
    .byte $3C
    .byte $3D
arch_br:    
    .byte $2E
    .byte $2F
    .byte $3E
    .byte $3F
moon_tl:
    .byte $40
    .byte $41
    .byte $50
    .byte $51
moon_tr:
    .byte $42
    .byte $43
    .byte $52
    .byte $53
moon_bl:
    .byte $60
    .byte $61
    .byte $70
    .byte $71
moon_br:
    .byte $62
    .byte $63
    .byte $72
    .byte $73
crack_v:
    .byte $44
    .byte $45
    .byte $54
    .byte $55
crack_h:
    .byte $64
    .byte $65 
    .byte $74
    .byte $75
crack:
    .byte $46
    .byte $47
    .byte $56
    .byte $57
brick_lip_l:
    .byte $28
    .byte $29
    .byte $00
    .byte $01
brick_lip_r:
    .byte $2A
    .byte $2B 
    .byte $10
    .byte $11
brick_bulge_l:
    .byte $28
    .byte $29
    .byte $38
    .byte $39
brick_bulge_r:
    .byte $2A
    .byte $2B
    .BYTE $3A 
    .BYTE $3B
water_l:
    .byte $4C
    .byte $4D
    .BYTE $5C 
    .BYTE $5D
 water_r:
    .byte $4E
    .byte $4F
    .BYTE $5E 
    .BYTE $5F 
window_l:
    .byte $6C
    .byte $6D
    .BYTE $7C 
    .BYTE $7D
window_r:
    .byte $6E
    .byte $6F
    .BYTE $7E 
    .BYTE $7F 
bars_l:
    .byte $8C
    .byte $8D
    .BYTE $9C 
    .BYTE $9D
bars_r:
    .byte $8E
    .byte $8F
    .BYTE $9E 
    .BYTE $9F


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
PaletteData:
    .byte $0F,$01,$11,$21,  $0F,$02,$03,$13,  $0F,$07,$17,$27, $0F,$00,$10,$20  ;background palette data  
    .byte $0F,$27,$14,$1A,  $0F,$09,$1C,$0C,  $0F,$2C,$30,$27, $0F,$0F,$36,$17  ;sprite palette data

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


AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
    .byte %01000100, %00010001, %01000100, %00010001, %01000100, %00010001, %01011101, %01010111
    .byte %00000100, %00000001, %00000100, %00000001, %00000100, %00000001, %00000100, %00000001
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00010101, %01000101, %00010101, %01000101, %00010101, %01000101, %00010101, %01000101
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %00000000, %10000000
    .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010


.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "castle_set-bank1.chr"
    .incbin "castle_set-bank2.chr"