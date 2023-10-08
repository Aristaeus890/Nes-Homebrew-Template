;Data

; Meta tile definitions. The first 4 bytes refer to tiles in chr rom
; The 5th byte is the collision for the block. 0=no collide 1 = collide









;Palette Data
PaletteDataList:
    .word PaletteDataCandyLand
    .word PaletteDataGray



;;;;;;;;;;;;
;;; LOOK UP TABLES
;;;;;;;;;;;

Sin:
    .byte $00,$00,$01,$00,$00,$01,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01
    .byte $00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$ff,$00,$00,$ff,$00,$00,$ff,$00,$00,$00,$ff,$00,$00,$00,$ff
    .byte $00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00



.align 256

MapSpecificLogicTree:
    .word MapRampartLogic
    .word MapTowerLogic
    .word MapRestLogic
    .word MapGardenLogic


InitMapList:
    .word InitMapRampart
    .word InitMapTower
    .word InitMapRest
    .word InitMapGarden


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
    .word MetaSpriteListCrystal
    .word MetaspriteListOptionSelector


PaletteNameList:
    .word PaletteNameBlood
    .word PaletteNameCandy
    .word PaletteNameByzantine
    .word PaletteNameGrayscale


MapNameList:
    .word MapNameRampart
    .word MapNameTower
    .word MapNameGarden
    .word MapNameRest



NMIDrawCommands:
    .word NMINoCommand ; 0 
    .word NMIClearVerticalColumn1
    .word NMIClearVerticalColumn2
    .word NMIWriteVertTiles1 ; 3
    .word NMISwapPallette ; 4
    .word NMIClearVictoryScreen ;5
    .word NMIDrawVictoryScreen ; 6
    .word NMIDrawVictoryPlayer ; 7
    .word NMIDrawVictoryPlayer ; 8
    .word NMIDrawOptionsScreen ;9
    .word NMIUpdateAnimatedTilesOptions ;a
    .word NMIUpdateAttributes
    .word NMIDarkenPallette

NMIAnimateCommands:
    .word NMIUpdateRampart
    .word NMIUpdateTower 
    .word NMIUpdateGarden
    .word NMIUpdateRest

EmitterStates:
    .word DummyEntry
    .word EmitterProcessSnow
    .word EmitterProcessHovering

WindStateMachine:
    .word WindWaiting ;0
    .word WindWaiting;1
    .word WindIncreasing;2
    .word WindBlowing;3
    .word WindDecreasing;4

ParticleStates:
    .word DummyEntry
    .word ParticlesProcessWind
    .word ParticlesProcessHoveringVertical
    .word ParticlesProcessHoveringHorizontal
    .word ParticlesProcessHoveringBoth
    .word ParticlesProcessRain

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
    .word earth_edge_l;46
    .word earth_edge_r;47
    .word jumpablefloorcentered;48
    .word jumpablefloorcentered2;49
    .word jumpablefloorcentered3;4a
    .word jumpablefloorcentered4;4b
    .word woodplatform;4c
    .word watergrate;4d

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
    .byte $C4,$C5 
    .byte $D4,$D5
    .byte $01,$01
    .byte $01,$01
floor_corner_bot_r:
    .byte $C2,$C3 
    .byte $D2,$D3
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
    .byte $01,$01
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
    .BYTE $fc,$fc
    .byte $04,$04
    .byte $00,$00

jumpablefloorreversed:
    .byte $92,$93
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
jumpablefloorcentered:
    .byte $00,$B2
    .BYTE $00,$00
    .byte $00,$03
    .byte $00,$00
jumpablefloorcentered2:
    .byte $B3,$00
    .BYTE $00,$00
    .byte $00,$03
    .byte $00,$00
jumpablefloorcentered3:
    .byte $00,$00
    .BYTE $00,$B2
    .byte $00,$00
    .byte $00,$03
jumpablefloorcentered4:
    .byte $00,$00
    .BYTE $B3,$00
    .byte $00,$00
    .byte $03,$00
woodplatform:
    .byte $82,$83
    .BYTE $a2,$a3
    .byte $03,$03
    .byte $00,$00
watergrate:
    .byte $ea,$eb
    .BYTE $fa,$fb
    .byte $03,$00
    .byte $00,$00



MetaTileListTitle:
    .word titleblank
    .word titlepressstart1
    .word titlepressstart2
    .word titlepressstart3

    .word titleplayercount1
    .word titleplayercount2
    .word titleplayercount3

    .word titlegamemode1
    .word titlegamemode2

    .word titlemap
    .word titlepalette1
    .word titlepalette2

titleblank:
    .byte $ff,$ff
    .byte $ff,$ff
titlepressstart1:
    .byte $B0,$B1
    .byte $ff,$ff
titlepressstart2:
    .byte $B2,$B3
    .byte $ff,$ff
titlepressstart3:
    .byte $B4,$B5
    .byte $ff,$ff
titleplayercount1:
    .byte $B6,$B7
    .byte $ff,$ff
titleplayercount2:
    .byte $B8,$B9
    .byte $ff,$ff
titleplayercount3:
    .byte $BA,$BB
    .byte $ff,$ff
titlegamemode1:
    .byte $BC,$BD
    .byte $ff,$ff
titlegamemode2:
    .byte $CC,$CD
    .byte $ff,$ff
titlemap:
    .byte $BE,$BF
    .byte $ff,$ff
titlepalette1:
    .byte $DC,$DD
    .byte $ff,$ff
titlepalette2:
    .byte $DE,$DF
    .byte $ff,$ff


MetaTileListTower:
    .word TowerPal1 ;00
    .word TowerPal2 ;01
    .word TowerPal3 ;02
    .word TowerPal4;03
    .word Rotator1;04
    .word Rotator2;05
    .word Rotator3;06
    .word Rotator4;07
    .word Rotator5;08
    .word Rotator6;09
    .word Rotator7;0A
    .word Rotator8;0B
    .word Rotator9;0C
    .word Rotator10;0D
    .word Rotator11;0E
    .word Rotator12;0F
    .word Rotator13;10
    .word Rotator14;11
    .word Rotator15;12
    .word Rotator16;13
    .word RotatorCuff1;14
    .word RotatorCuff2;15
    .word Corner1 ;16
    .word Corner2 ;17
    .word Corner3 ;18
    .word Corner4;19
    .word WindowEdgeTop;1A
    .word WindowEdgeBottom;1B
    .word WindowEdgeLeft;1C
    .word WindowEdgeRight;1D
    .word Panel ;1E
    .word PanelTiled;1F 
    .word PanelLettered1;20
    .word PanelLettered2;21
    .word PanelLettered3;22
    .word PanelLettered4;23
    .word Brick1;24
    .word Brick2;25
    .word Brick3;26
    .word Brick4;27
    .word Brick5;28
    .word Brick6;29
    .word Brick7;2a
    .word Brick8;2b
    .word Pillar1;2c
    .word Pillar2;2d
    .word PillarHalf1;2e
    .word PillarHalf2;2f
    .word PlatformHalfReversed;30
    .word PlatformHalf;31
    .word PlatformFull;32;
    .word Letters1;33
    .word Letters2;34
    .word Letters3;35
    .word Letters4;36
    .word Letters5;37
    .word Letters6;38
    .word Letters7;39
    .word Letters8;3a
    .word LettersVined1 ;3b
    .word LettersVined2 ;3c
    .word LettersVined3 ;3d
    .word LettersVined4 ;3E
    .word LettersVined5 ;3F
    .word Diamond ;40
    .word DiamondVined ;41
    .word DiamondSmall;42
    .word PillarVined ;43
    .word PillarVined2;44
    .word WindowCornerTL2 ;45
    .word WindowCornerTR2;46
    .word WindowCornerBL2;47
    .word WindowCornerBR2;48
    .word PlatformWithPillar;49
    .word PlatformWithPillar2;4A
    .word Diamond2;4B
    .word PanelVined;4C
    .word PanelVined2;4D
    .word PanelTiled2;4e
    .word PanelTiled3;4f
    .word PanelTiled4;50

TowerPal1:
    .byte $6F,$6F
    .byte $6F,$6F 
    .byte $00,$00
    .byte $00,$00
TowerPal2:
    .byte $7D,$7D
    .byte $7D,$7D 
    .byte $00,$00
    .byte $00,$00

TowerPal3:
    .byte $7E,$7E
    .byte $7E,$7E 
    .byte $00,$00
    .byte $00,$00
TowerPal4:
    .byte $7F,$7F
    .byte $7F,$7F 
    .byte $00,$00
    .byte $00,$00
Rotator1:
    .byte $C0,$C1
    .byte $D0,$D1
    .byte $00,$00
    .byte $02,$02
Rotator2:
    .byte $C2,$C3
    .byte $D2,$D3
    .byte $00,$00
    .byte $02,$02
Rotator3:
    .byte $C4,$C5
    .byte $D4,$D5
    .byte $00,$00
    .byte $02,$02
Rotator4:
    .byte $C6,$C7
    .byte $D6,$D7
    .byte $00,$00
    .byte $02,$02
Rotator5:
    .byte $C8,$C9
    .byte $D8,$D9
    .byte $00,$00
    .byte $02,$02
Rotator6:
    .byte $CA,$CB
    .byte $DA,$DB
    .byte $00,$00
    .byte $02,$02
Rotator7:
    .byte $CC,$CD
    .byte $DC,$DD
    .byte $00,$00
    .byte $02,$02
Rotator8:
    .byte $CE,$CF
    .byte $DE,$DF
    .byte $00,$00
    .byte $02,$02
Rotator9:
    .byte $E0,$E1
    .byte $F0,$F1
    .byte $01,$01
    .byte $00,$00
Rotator10:
    .byte $E2,$E3
    .byte $F2,$F3
    .byte $01,$01
    .byte $00,$00
Rotator11:
    .byte $E4,$E5
    .byte $F4,$F5
    .byte $01,$01
    .byte $00,$00
Rotator12:
    .byte $E6,$E7
    .byte $F6,$F7
    .byte $01,$01
    .byte $00,$00
Rotator13:
    .byte $E8,$E9
    .byte $F8,$F9
    .byte $01,$01
    .byte $00,$00
Rotator14:
    .byte $EA,$EB
    .byte $FA,$FB
    .byte $01,$01
    .byte $00,$00
Rotator15:
    .byte $EC,$ED
    .byte $FC,$FD
    .byte $01,$01
    .byte $00,$00
Rotator16:
    .byte $EE,$EF
    .byte $FE,$FF
    .byte $01,$01
    .byte $00,$00
RotatorCuff1:
    .byte $C8,$07
    .byte $F8,$17
    .byte $00,$00
    .byte $00,$00
RotatorCuff2:
    .byte $07,$C7
    .byte $17,$F7
    .byte $00,$00
    .byte $00,$00
Corner1:
    .byte $07,$08
    .byte $09,$00
    .byte $00,$00
    .byte $00,$00
Corner2:
    .byte $0A,$0B
    .byte $01,$0C
    .byte $00,$00
    .byte $00,$00
Corner3:
    .byte $0D,$10
    .byte $0E,$0F
    .byte $00,$00
    .byte $00,$00
Corner4:
    .byte $11,$1A
    .byte $1B,$1C
    .byte $00,$00
    .byte $00,$00
WindowEdgeTop:
    .byte $13,$14
    .byte $02,$02
    .byte $00,$00
    .byte $00,$00
WindowEdgeBottom:
    .byte $12,$12
    .byte $03,$04
    .byte $00,$00
    .byte $00,$00
WindowEdgeLeft:
    .byte $17,$20
    .byte $18,$20
    .byte $00,$00
    .byte $00,$00
WindowEdgeRight:
    .byte $21,$19
    .byte $21,$1A
    .byte $00,$00
    .byte $00,$00
Panel: 
    .byte $03,$04
    .byte $13,$14
    .byte $00,$00
    .byte $00,$00
PanelTiled: 
    .byte $05,$05
    .byte $05,$05
    .byte $00,$00
    .byte $00,$00
PanelLettered1:
    .byte $07,$08
    .byte $17,$18
    .byte $00,$00
    .byte $00,$00
PanelLettered2:
    .byte $09,$0A
    .byte $19,$1A
    .byte $00,$00
    .byte $00,$00
PanelLettered3:
    .byte $0B,$0C
    .byte $1B,$1C
    .byte $00,$00
    .byte $00,$00
PanelLettered4:
    .byte $0D,$0E
    .byte $1D,$1E
    .byte $00,$00
    .byte $00,$00
 Brick1:
    .byte $26,$27
    .byte $36,$37
    .byte $00,$00
    .byte $00,$00
 Brick2:
    .byte $27,$28
    .byte $37,$38
    .byte $00,$00
    .byte $00,$00
 Brick3:
    .byte $28,$29
    .byte $38,$39
    .byte $00,$00
    .byte $00,$00
 Brick4:
    .byte $29,$2a
    .byte $39,$3a
    .byte $00,$00
    .byte $00,$00
 Brick5:
    .byte $2A,$2B
    .byte $3A,$3B
    .byte $00,$00
    .byte $00,$00
 Brick6:
    .byte $2B,$2C
    .byte $3B,$3C
    .byte $00,$00
    .byte $00,$00
 Brick7:
    .byte $2C,$2D
    .byte $3C,$3D
    .byte $00,$00
    .byte $00,$00
 Brick8:
    .byte $2D,$27
    .byte $3D,$37
    .byte $00,$00
    .byte $00,$00
 Pillar1:
    .byte $2E,$2F
    .byte $3E,$3F
    .byte $00,$00
    .byte $00,$00
 Pillar2:
    .byte $4E,$4F
    .byte $5E,$5F
    .byte $00,$00
    .byte $00,$00
PillarHalf1:
    .byte $3E,$3F
    .byte $6F,$6F
    .byte $00,$00
    .byte $00,$00
PillarHalf2:
    .byte $6F,$6F
    .byte $3E,$3F
    .byte $00,$00
    .byte $00,$00
 PlatformHalf:
    .byte $22,$23
    .byte $29,$38
    .byte $01,$01
    .byte $00,$00
 PlatformHalfReversed:
    .byte $28,$39
    .byte $22,$23
    .byte $00,$00
    .byte $01,$01
 PlatformFull:
    .byte $30,$31
    .byte $40,$41
    .byte $01,$01
    .byte $01,$01
 Letters1:
    .byte $07,$08
    .byte $17,$18
    .byte $00,$00
    .byte $00,$00
 Letters2:
    .byte $08,$09
    .byte $18,$18
    .byte $00,$00
    .byte $00,$00
 Letters3:
    .byte $09,$0A
    .byte $19,$1A
    .byte $00,$00
    .byte $00,$00
 Letters4:
    .byte $0A,$0B
    .byte $1A,$1B
    .byte $00,$00
    .byte $00,$00
 Letters5:
    .byte $0B,$0C
    .byte $1B,$1C
    .byte $00,$00
    .byte $00,$00
 Letters6:
    .byte $0C,$0D
    .byte $1C,$1D
    .byte $00,$00
    .byte $00,$00
 Letters7:
    .byte $0D,$0E
    .byte $1E,$1E
    .byte $00,$00
    .byte $00,$00
 Letters8:
    .byte $0E,$0F
    .byte $1E,$1F
    .byte $00,$00
    .byte $00,$00
LettersVined1:
    .byte $44,$45
    .byte $54,$55
    .byte $00,$00
    .byte $00,$00
LettersVined2:
    .byte $45,$46
    .byte $55,$56
    .byte $00,$00
    .byte $00,$00
LettersVined3:
    .byte $46,$47
    .byte $56,$57
    .byte $00,$00
    .byte $00,$00
LettersVined4:
    .byte $47,$48
    .byte $57,$58
    .byte $00,$00
    .byte $00,$00
LettersVined5:
    .byte $48,$49
    .byte $58,$59
    .byte $00,$00
    .byte $00,$00
Diamond:
    .byte $32,$33
    .byte $42,$43
    .byte $00,$00
    .byte $00,$00
DiamondVined:
    .byte $50,$51
    .byte $60,$61
    .byte $00,$00
    .byte $00,$00
DiamondSmall:
    .byte $52,$52
    .byte $52,$52
    .byte $00,$00
    .byte $00,$00
PillarVined:
    .byte $4a,$4b
    .byte $5a,$5b
    .byte $00,$00
    .byte $00,$00
PillarVined2:
    .byte $4c,$4d
    .byte $5c,$5d
    .byte $00,$00
    .byte $00,$00
WindowCornerTL2:
    .byte $05,$06
    .byte $05,$11
    .byte $00,$00
    .byte $00,$00
WindowCornerTR2:
    .byte $05,$05
    .byte $10,$15
    .byte $00,$00
    .byte $00,$00
WindowCornerBL2:
    .byte $05,$01
    .byte $05,$05
    .byte $00,$00
    .byte $00,$00
WindowCornerBR2:
    .byte $00,$06
    .byte $17,$07
    .byte $00,$00
    .byte $00,$00
PlatformWithPillar:
    .byte $22,$23
    .byte $2E,$2F
    .byte $04,$04
    .byte $00,$00
PlatformWithPillar2:
    .byte $2E,$2F
    .byte $22,$23
    .byte $00,$00
    .byte $04,$04
Diamond2:
    .byte $00,$01
    .byte $10,$11
    .byte $00,$00
    .byte $00,$00
PanelVined:
    .byte $69,$6A
    .byte $79,$7A
    .byte $00,$00
    .byte $00,$00
PanelVined2:
    .byte $6B,$6C
    .byte $7B,$7C
    .byte $00,$00
    .byte $00,$00
PanelTiled2:
    .byte $06,$05
    .byte $16,$05
    .byte $00,$00
    .byte $00,$00
PanelTiled3:
    .byte $05,$05
    .byte $06,$05
    .byte $00,$00
    .byte $00,$00
PanelTiled4:
    .byte $16,$05
    .byte $06,$16
    .byte $00,$00
    .byte $00,$00



MetatilelistSnow:
    .word SnowPal1 ;00
    .word SnowPal2 ;01
    .word SnowPal3 ;02
    .word Mountain1 ;03
    .word Mountain2;4
    .word Mountain3;5
    .word Mountain4;6
    .word Mountain5;7
    .word Mountain6;8
    .word Mountain7;9
    .word Mountain8;a
    .word Mountain9;b
    .word Mountain10;c
    .word Mountain11;d
    .word Mountain12;e
    .word Mountain13;f
    .word Mountain14;10
    .word Mountain15;11
    .word Mountain16;12
    .word Mountain1a;13
    .word Mountain1b;14
    .word Mountain2a;15
    .word Mountain4a;16
    .word Clouds1;17
    .word Clouds2;18
    .word Clouds3;19
    .word Clouds4;1a
    .word SnowFloor1;1B
    .word SnowFloor2;1C
    .word SnowFloor3;1D
    .word SnowFloor4;1E
    .word SnowFloor5;1F
    .word SnowFloor6;20
    .word CaveBackground1;21
    .word CaveBackground2;22
    .word CaveBackground3;23
    .word CaveBackground4;24
    .word CaveBackground5;25
    .word CaveBackground6;26
    .WORD CavePillar ;27
    .word CavePlatform ;28
    .word CrystalBottom1 ;29
    .word CrystalBottom1 ;2A
    .word CrystalTop1 ;2B
    .word CrystalTop2 ;2C
    .word CrystalLeft1 ;2D
    .word CrystalLeft2 ;2E
    .word CrystalRight1 ;2F
    .word CrystalRight2 ;30
    .word GraveLeft;31
    .word GraveRight;32
    .word GraveCentre;33
    .word Fan ;34
    .word BigFan1 ;35
    .word BigFan2 ;36
    .word BigFan3;37
    .word BigFan4;38
    .word GirderHorizontalLeft;39
    .word GirderHorizontalMiddle;3A
    .word GirderHorizontalRight;3B
    .word Machinery;3C
    .word WaterfallSnow;3D
    .word WaterfallGirder;3E
    .word Cave1 ;3F
    .word Cave2;40
    .word WaterfallSnowSplit1;41
    .word WaterfallSnowSplit2
    .word CaveLeft;43
    .word CaveRight ;44
    .word PlatformSmallLeft;45
    .word PlatformSmallRight;46
    .word PlatformSmallCentered;47
    .word SnowBlock1;48
    .word SnowBlock2;49
    .word CrystalCenteredBottom1;4a
    .word CrystalCenteredBottom2;4b
    .word CrystalCenteredTop1;4c
    .word CrystalCenteredTop2;4d
    .word Grill1;4e
    .word Grill2;4f
    .Word GirderVerticalLeft ;50
    .word GirderVerticalRight;51
    .word Gear;52
    .WORD GirderVerticalCentred


SnowPal1:
    .byte $BF,$BF
    .byte $BF,$BF
    .byte $00,$00
    .byte $00,$00
SnowPal2:
    .byte $16,$05
    .byte $06,$16
    .byte $00,$00
    .byte $00,$00
SnowPal3:
    .byte $16,$05
    .byte $06,$16
    .byte $00,$00
    .byte $00,$00
Mountain1:
    .byte $00,$01
    .byte $10,$11
    .byte $00,$00
    .byte $00,$00
Mountain2:
    .byte $02,$03
    .byte $12,$13
    .byte $00,$00
    .byte $00,$00
Mountain3:
    .byte $04,$05
    .byte $14,$15
    .byte $00,$00
    .byte $00,$00
Mountain4:
    .byte $06,$07
    .byte $16,$17
    .byte $00,$00
    .byte $00,$00
Mountain5:
    .byte $20,$21
    .byte $30,$31
    .byte $00,$00
    .byte $00,$00
Mountain6:
    .byte $22,$23
    .byte $32,$33
    .byte $00,$00
    .byte $00,$00
Mountain7:
    .byte $24,$25
    .byte $34,$35
    .byte $00,$00
    .byte $00,$00
Mountain8:
    .byte $26,$27
    .byte $36,$37
    .byte $00,$00
    .byte $00,$00
Mountain9:
    .byte $40,$41
    .byte $50,$51
    .byte $00,$00
    .byte $00,$00
Mountain10:
    .byte $42,$43
    .byte $52,$53
    .byte $00,$00
    .byte $00,$00
Mountain11:
    .byte $42,$43
    .byte $70,$71
    .byte $00,$00
    .byte $00,$00
Mountain12:
    .byte $40,$41
    .byte $72,$73
    .byte $00,$00
    .byte $00,$00
Mountain13:
    .byte $60,$61
    .byte $54,$55
    .byte $00,$00
    .byte $00,$00
Mountain14:
    .byte $62,$63
    .byte $56,$57
    .byte $00,$00
    .byte $00,$00
Mountain15:
    .byte $74,$75
    .byte $54,$55
    .byte $00,$00
    .byte $00,$00
Mountain16:
    .byte $76,$76
    .byte $54,$55
    .byte $00,$00
    .byte $00,$00
Mountain1a:
    .byte $28,$29
    .byte $38,$39
    .byte $00,$00
    .byte $00,$00
Mountain1b:
    .byte $2A,$2B
    .byte $3A,$3B
    .byte $00,$00
    .byte $00,$00
Mountain2a:
    .byte $2C,$2D
    .byte $3C,$3D
    .byte $00,$00
    .byte $00,$00
Mountain4a:
    .byte $2E,$2F
    .byte $3E,$3F
    .byte $00,$00
    .byte $00,$00
Clouds1:
    .byte $08,$09
    .byte $18,$19
    .byte $00,$00
    .byte $00,$00
Clouds2:
    .byte $0a,$0b
    .byte $1A,$1B
    .byte $00,$00
    .byte $00,$00

Clouds3:
    .byte $0C,$0D
    .byte $1C,$1D
    .byte $00,$00
    .byte $00,$00
Clouds4:
    .byte $0E,$0F
    .byte $1E,$1F
    .byte $00,$00
    .byte $00,$00
SnowFloor1:
    .byte $48,$49
    .byte $58,$59
    .byte $01,$01
    .byte $01,$01
SnowFloor2:
    .byte $4A,$4B
    .byte $5A,$5B
    .byte $01,$01
    .byte $01,$01
SnowFloor3:
    .byte $4C,$4D
    .byte $5C,$5D
    .byte $01,$01
    .byte $01,$01
SnowFloor4:
    .byte $80,$81
    .byte $90,$91
    .byte $01,$01
    .byte $01,$01
SnowFloor5:
    .byte $82,$83
    .byte $92,$93
    .byte $01,$01
    .byte $01,$01
SnowFloor6:
    .byte $84,$85
    .byte $94,$95
    .byte $01,$01
    .byte $01,$01
CaveBackground1:
    .byte $68,$69
    .byte $78,$79
    .byte $00,$00
    .byte $00,$00
CaveBackground2:
    .byte $6A,$6B
    .byte $7A,$7B
    .byte $00,$00
    .byte $00,$00
CaveBackground3:
    .byte $6C,$6D
    .byte $7C,$7D
    .byte $00,$00
    .byte $00,$00
CaveBackground4:
    .byte $68,$7B
    .byte $79,$7A
    .byte $00,$00
    .byte $00,$00
CaveBackground5:
    .byte $7A,$6B
    .byte $6A,$7A
    .byte $00,$00
    .byte $00,$00
CaveBackground6:
    .byte $78,$7D
    .byte $6C,$6B
    .byte $00,$00
    .byte $00,$00
CavePillar:
    .byte $6E,$6F
    .byte $7E,$7F
    .byte $00,$00
    .byte $00,$00
CavePlatform:
    .byte $4e,$4F
    .byte $5E,$5F
    .byte $03,$03
    .byte $00,$00
CrystalBottom1:
    .byte $C0,$C1
    .byte $D0,$D1
    .byte $00,$00
    .byte $00,$00
CrystalBottom2:
    .byte $C2,$C3
    .byte $D2,$D3
    .byte $00,$00
    .byte $00,$00
CrystalTop1:
    .byte $C4,$C5
    .byte $D4,$D5
    .byte $00,$00
    .byte $00,$00
CrystalTop2:
    .byte $C6,$C7
    .byte $D6,$D7
    .byte $00,$00
    .byte $00,$00
CrystalLeft1:
    .byte $E4,$E5
    .byte $F4,$F5
    .byte $00,$00
    .byte $00,$00
CrystalLeft2:
    .byte $E6,$E7
    .byte $F6,$F7
    .byte $00,$00
    .byte $00,$00
CrystalRight1:
    .byte $E0,$E1
    .byte $F0,$F1
    .byte $00,$00
    .byte $00,$00
CrystalRight2:
    .byte $E2,$E3
    .byte $F2,$F3
    .byte $00,$00
    .byte $00,$00
GraveLeft:
    .byte $A0,$A1
    .byte $B0,$B1 
    .byte $01,$01
    .byte $01,$01

GraveRight:
    .byte $A4,$A5
    .byte $B4,$B5 
    .byte $01,$01
    .byte $01,$01
GraveCentre:
    .byte $A2,$A3
    .byte $B2,$B3 
    .byte $01,$01
    .byte $01,$01
Fan:
    .byte $c8,$C9
    .byte $D8,$D9 
    .byte $00,$00
    .byte $00,$00
BigFan1:
    .byte $CC,$CD
    .byte $DC,$DD 
    .byte $00,$00
    .byte $00,$00
BigFan2:
    .byte $CE,$CF
    .byte $DE,$DF 
    .byte $00,$00
    .byte $00,$00
BigFan3:
    .byte $EC,$ED
    .byte $FC,$FD 
    .byte $00,$00
    .byte $00,$00
BigFan4:
    .byte $EE,$EF
    .byte $FE,$FF 
    .byte $00,$00
    .byte $00,$00
GirderHorizontalLeft:
    .byte $A8,$A9
    .byte $BF,$BF 
    .byte $03,$03
    .byte $00,$00
GirderHorizontalMiddle:
    .byte $A9,$A9
    .byte $BF,$BF 
    .byte $03,$03
    .byte $00,$00
GirderHorizontalRight:
    .byte $A9,$AA
    .byte $BF,$BF 
    .byte $03,$03
    .byte $00,$00
Machinery:
    .byte $EA,$EB
    .byte $FA,$FB 
    .byte $00,$00
    .byte $00,$00
WaterfallSnow:
    .byte $E8,$E9
    .byte $F8,$F9 
    .byte $00,$00
    .byte $00,$00
WaterfallGirder:
    .byte $A9,$A9
    .byte $F8,$F9 
    .byte $00,$00
    .byte $00,$00
Cave1:
    .byte $AB,$85
    .byte $BF,$BF 
    .byte $01,$03
    .byte $00,$00
Cave2:
    .byte $84,$AC
    .byte $BF,$BF 
    .byte $03,$01
    .byte $00,$00
WaterfallSnowSplit1:
    .byte $6C,$E8
    .byte $6C,$E8 
    .byte $00,$00
    .byte $00,$00
WaterfallSnowSplit2:
    .byte $E9,$6D
    .byte $E9,$6D
    .byte $00,$00
    .byte $00,$00
CaveLeft:
    .byte $BF,$6D
    .byte $BF,$6D 
    .byte $00,$00
    .byte $00,$00
CaveRight:
    .byte $6C,$BF
    .byte $6C,$BF 
    .byte $00,$00
    .byte $00,$00
PlatformSmallLeft:
    .byte $BF,$BF 
    .byte $84,$BF
    .byte $00,$00
    .byte $03,$00
PlatformSmallRight:
    .byte $Bf,$BF 
    .byte $Bf,$85
    .byte $00,$00
    .byte $00,$03
PlatformSmallCentered:
    .byte $Bf,$BF 
    .byte $84,$85
    .byte $00,$00
    .byte $03,$03
SnowBlock2:
    .byte $AB,$AC 
    .byte $BB,$BC
    .byte $01,$01
    .byte $01,$01
SnowBlock1:
    .byte $AB,$AD 
    .byte $BC,$BC
    .byte $01,$01
    .byte $01,$01
CrystalCenteredTop1:
    .byte $Bf,$C0
    .byte $Bf,$D0 
    .byte $00,$00
    .byte $00,$00
CrystalCenteredTop2:
    .byte $C1,$BF
    .byte $D1,$BF 
    .byte $00,$00
    .byte $00,$00
CrystalCenteredBottom1:
    .byte $Bf,$C4
    .byte $Bf,$d4 
    .byte $00,$00
    .byte $00,$00
CrystalCenteredBottom2:
    .byte $C5,$BF
    .byte $D5,$BF 
    .byte $00,$00
    .byte $00,$00
Grill1:
    .byte $86,$87
    .byte $96,$97 
    .byte $01,$01
    .byte $01,$01
Grill2:
    .byte $8A,$8A
    .byte $96,$97 
    .byte $01,$01
    .byte $01,$01
GirderVerticalLeft:
    .byte $9f,$e5
    .byte $9F,$f5 
    .byte $00,$00
    .byte $00,$00
GirderVerticalRight:
    .byte $e2,$9F
    .byte $f2,$9F 
    .byte $00,$00
    .byte $00,$00
GirderVerticalCentred:
    .byte $9F,$e7
    .byte $9F,$f7 
    .byte $00,$00
    .byte $00,$00
Gear:
    .byte $CA,$CB
    .byte $DA,$DB 
    .byte $00,$00
    .byte $00,$00


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


MapHeaders:
    .word MapHeaderRampart
    .word MapheaderTower
    .word MapHeaderSnow
    .word MapHeaderTitle
    .word MapHeaderVictory
    .word MapHeaderOptions

MapHeaderRampart:
    .word ScreenDefault ; map screen
    .word PaletteDataRampart ; default palette
    .word MetaTileList 
    .word AttributesRampart
    .word IRQRampart
    .byte %10011110 ; inital emphasis 
    .byte $70 ; irq initial timer


MapHeaderTitle:
    .word ScreenTitle ; map screen
    .word PaletteData ; pal
    .word MetaTileListTitle
    .word AttributesBlank
    .word IrqDoNothing
    .byte %00011110
    .byte $00 


MapHeaderVictory:
    .word ScreenVictory
    .word PaletteData
    .word MetaTileListTitle

MapHeaderOptions:
    .word ScreenOptions ; map screen
    .word PaletteData ; pal
    .word MetaTileListTitle
    .word AttributesBlank
    .word IrqDoNothing
    .byte %00011110
    .byte $00 

MapheaderTower:
    .word ScreenTower
    .word PaletteDataTower
    .word MetaTileListTower
    .word AttributesTower
    .word IrqDoNothing
    .byte %00011110
    .byte $00

MapHeaderSnow:
    .word ScreenSnow
    .word PaletteData
    .word MetatilelistSnow
    .word AttributesSnow
    .word IRQTower1
    .byte %11011110 ; colour emphasis
    .byte $10 ; padding

GameStatePath:
    .word DoTitleLogic
    .word DoOptionsLogic
    .word DoGameLogic
    .word DoVictoryScreenLogic
    .word WaitForGameLoad

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
    .word ProjectileSpellSprite7
    .word ProjectileSpellSprite8
    .word ProjectileSpellSprite9
    .word ProjectileSpellSprite10


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

MetaSpriteListCrystal:
    .word CrystalSprite1
    .word CrystalSprite2
    .word CrystalSprite3

MetaspriteListOptionSelector:
    .word OptionSelectorSprite1


PalletteDataBlack:
    .byte $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0F
    .byte $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0f, $0f,$0f,$0f,$0F

PaletteDataGray:
    .byte $0f,$00,$10,$37,  $0F,$01,$03,$0A,  $0F,$07,$16,$27, $0F,$05,$15,$30  ;background palette data  
    .byte $0f,$06,$16,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data

PaletteDataCandyLand:
    .byte $04,$15,$26,$37,  $04,$09,$1a,$2b,  $04,$06,$17,$14, $04,$05,$15,$30  ;background palette data  
    .byte $04,$06,$16,$30,  $04,$1A,$2A,$30,  $04,$13,$23,$30, $04,$2d,$3D,$30  ;sprite palette data


PaletteDataTower:
    .byte $0f,$07,$17,$27,  $0F,$07,$17,$1A,  $0F,$03,$13,$14, $0F,$07,$18,$28  ;background palette data  
    .byte $0f,$06,$16,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data

PaletteDataRampart:
    .byte $0f,$01,$12,$21,  $0F,$07,$18,$28,  $0F,$01,$09,$1a, $0F,$05,$16,$26  ;background palette data  
    .byte $0f,$06,$16,$30,  $0F,$1A,$2A,$30,  $0F,$13,$23,$30, $0F,$2d,$3D,$30  ;sprite palette data


; Map Headers

MapNameTower:
    .byte $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$d9,$DA,$DB
MapNameRest:
    .BYTE $E0,$E1,$E2,$E3,$E4,$E5,$FF,$FF,$FF,$FF,$FF,$FF
MapNameRampart:
    .BYTE $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$FF,$FF,$FF
MapNameGarden:
    .BYTE $C0,$C1,$C2,$C3,$C4,$C5,$C6,$FF,$FF,$FF,$FF,$FF


PaletteNameBlood:
    .byte $f9,$FA,$FB,$FC,$FD,$FF
PaletteNameCandy:
    .byte $C7,$C8,$C9,$CA,$CB,$FF
PaletteNameByzantine:
    .byte $E6,$E7,$E8,$E9,$EA,$FF
PaletteNameGrayscale:
    .byte $EB,$EC,$ED,$EE,$EF,$EF



ScreenTitle:
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

ScreenOptions:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$04,$05,$06,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$0A,$0B,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$07,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


ScreenVictory:
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

ScreenTower:
    .byte $43,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $44,$45,$1A,$1A,$46,$4b,$16,$1A,$1A,$17,$4B,$3A,$4C,$37,$4D,$3F
    .byte $43,$1C,$42,$42,$1D,$4b,$1C,$00,$00,$1D,$4B,$38,$37,$3C,$3b,$3E
    .byte $44,$47,$1B,$1B,$48,$4b,$1C,$00,$00,$1D,$4B,$2C,$31,$31,$31,$24
    .byte $08,$09,$0A,$0B,$2C,$1f,$18,$1B,$1B,$19,$1F,$2E,$04,$05,$06,$07
    .byte $10,$11,$12,$13,$2D,$4D,$49,$4B,$4B,$2C,$1F,$2C,$0C,$0D,$0E,$0F
    .byte $14,$33,$35,$3A,$1F,$4E,$2C,$3D,$3B,$49,$50,$1F,$33,$3B,$3C,$15
    .byte $08,$09,$0A,$0B,$2C,$1F,$49,$3E,$3C,$2C,$1F,$4A,$04,$05,$06,$07
    .byte $10,$11,$12,$13,$2D,$50,$2C,$3F,$3D,$49,$1F,$2C,$0C,$0D,$0E,$0F
    .byte $2C,$24,$27,$24,$2C,$1F,$49,$3E,$3E,$2C,$24,$45,$1A,$1A,$46,$44
    .byte $2D,$1E,$1E,$1E,$1E,$1F,$2C,$3D,$3F,$49,$27,$1C,$24,$24,$1D,$43
    .byte $2C,$31,$31,$31,$31,$4E,$49,$3C,$3E,$2C,$26,$47,$31,$31,$48,$43
    .byte $2D,$40,$40,$40,$40,$40,$2C,$3B,$3D,$49,$1E,$1E,$31,$31,$1E,$44
    .byte $2D,$32,$30,$32,$30,$32,$49,$30,$30,$2D,$32,$32,$32,$32,$32,$44
    .byte $2C,$24,$24,$24,$24,$24,$2C,$24,$24,$2D,$24,$24,$24,$24,$24,$43

ScreenSnow:
    .byte $19,$1a,$17,$18,$19,$1a,$17,$18,$19,$1a,$17,$18,$19,$1a,$17,$18
    .byte $03,$04,$05,$06,$13,$04,$05,$14,$03,$04,$05,$06,$15,$16,$05,$06
    .byte $07,$08,$09,$0A,$07,$08,$09,$0A,$07,$08,$09,$0A,$07,$08,$09,$0A
    .byte $0B,$0C,$0D,$0E,$0B,$0C,$0D,$0E,$0B,$0C,$0D,$0E,$0B,$0C,$0D,$0E
    .byte $0F,$10,$11,$12,$0F,$10,$11,$12,$0F,$10,$11,$12,$0F,$10,$11,$12
    .byte $39,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3A,$3b
    .byte $00,$29,$2a,$00,$00,$00,$00,$00,$00,$00,$51,$50,$00,$35,$36,$12
    .byte $00,$3F,$40,$00,$00,$00,$00,$00,$00,$00,$51,$50,$00,$37,$38,$00
    .byte $43,$41,$42,$44,$4C,$4D,$00,$29,$00,$00,$51,$50,$00,$39,$3B,$00
    .byte $43,$41,$42,$44,$48,$49,$00,$3f,$40,$00,$39,$3B,$00,$34,$34,$00
    .byte $43,$41,$42,$44,$4A,$4B,$43,$41,$42,$44,$50,$51,$00,$39,$3B,$00
    .byte $43,$41,$42,$44,$00,$52,$43,$41,$42,$28,$47,$28,$00,$50,$51,$00
    .byte $43,$31,$33,$32,$29,$3C,$2A,$41,$42,$27,$2C,$27,$00,$50,$51,$00
    .byte $1B,$1C,$1D,$1E,$1F,$20,$39,$3E,$3E,$3B,$4E,$4F,$4E,$4E,$4F,$4f
    .byte $00,$00,$00,$00,$00,$00,$00,$3E,$3E,$00,$00,$00,$00,$00,$00,$00



PaletteData:
    .byte $0F,$11,$21,$30, $0F,$0A,$1B,$2C, $0F,$0F,$09,$0A, $0F,$13,$23,$33
    .byte $0F,$06,$16,$30, $0F,$1A,$2A,$30, $0F,$13,$23,$30, $0F,$2d,$3D,$30

AttributesDefault: ; each attribute byte sets the pallete for a block of pixels
    .byte %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %10100000
    .byte %01100110, %10011010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
    .byte %10101010, %10101010, %01011010, %10101011, %10101010, %01011010, %10100110, %10101001
    .byte %10101010, %10101010, %10101010, %00100010, %10000100, %01011010, %10100110, %10101010
    .byte %00100100, %01010101, %01011111, %01010011, %01011000, %01011010, %01011010, %01011010
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101

ScreenDefault:
    .byte $24,$00,$00,$00,$00,$00,$00,$15,$15,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$13,$12,$14,$00,$11,$0C,$0D,$15,$00,$00,$00,$00,$00,$23
    .byte $25,$00,$00,$19,$1a,$00,$11,$0E,$0F,$00,$00,$00,$00,$00,$00,$26
    .byte $05,$08,$09,$00,$00,$08,$09,$13,$14,$00,$00,$00,$00,$00,$00,$04
    .byte $05,$21,$22,$4c,$4c,$21,$22,$40,$40,$00,$1D,$31,$1D,$31,$1D,$04
    .byte $05,$26,$25,$08,$09,$26,$25,$4A,$4B,$00,$13,$10,$00,$11,$14,$04
    .byte $05,$00,$00,$0A,$0B,$00,$00,$00,$00,$00,$13,$12,$00,$00,$14,$04
    .byte $22,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$4C,$21
    .byte $24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$23
    .byte $24,$00,$31,$31,$40,$00,$4d,$3F,$3F,$4D,$00,$1D,$40,$1D,$40,$23
    .byte $25,$40,$00,$00,$00,$00,$00,$4A,$4B,$00,$00,$00,$00,$00,$00,$26
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $07,$07,$39,$39,$39,$39,$2B,$48,$49,$2B,$3A,$3A,$3A,$3A,$07,$07
    .byte $06,$06,$06,$06,$06,$06,$00,$00,$00,$00,$06,$06,$06,$06,$06,$06




AttributesSnow: ; each attribute byte sets the pallete for a block of pixels
    .byte %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111, %00001111
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %10100000, %10100000
    .byte %01100110, %10011010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
    .byte %10101010, %10101010, %01011010, %10101011, %10101010, %01011010, %10100110, %10101001
    .byte %10101010, %10101010, %10101010, %00100010, %10000100, %01011010, %10100110, %10101010
    .byte %00100100, %01010101, %01011111, %01010011, %01011000, %01011010, %01011010, %01011010
    .byte %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101


AttributesTower: ; each attribute byte sets the pallete for a block of pixels
    .byte %00010001, %00000000, %00000000, %00000000, %00000000, %00000000, %01010101, %01010101
    .byte %00010001, %00000000, %00000000, %10001000, %00100010, %00000000, %11110101, %11110101
    .byte %00000000, %11110000, %00000000, %01110000, %00010000, %00000000, %11110000, %00000000
    .byte %00000000, %11110000, %00000000, %01110100, %00011101, %00000000, %11110101, %00000001
    .byte %00000000, %00000000, %00000000, %01110100, %00011101, %00000000, %00000000, %01000000
    .byte %11000000, %11110000, %00110000, %01110100, %00011101, %00000000, %11111010, %01000100
    .byte %11000000, %11110000, %11110000, %11110100, %00111101, %11110000, %11111111, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100

AttributesRampart: ; each attribute byte sets the pallete for a block of pixels
    .byte %00010001, %10101010, %00000000, %11000000, %00110000, %00000000, %00000000, %01000100
    .byte %00010001, %00000000, %00000000, %00001100, %00000011, %00000000, %00000000, %10000100
    .byte %01000100, %00010101, %01000101, %01010101, %00010001, %00000000, %00000000, %00000000
    .byte %01011000, %01010010, %01011000, %01010010, %01010000, %01010000, %01010000, %01000000
    .byte %00010001, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00010001, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %01010000, %11110000, %11110000, %01000000, %00010000, %11110000, %11110000, %01010100
    .byte %00010101, %01010101, %01010101, %00000000, %00000000, %01010101, %01010101, %00000101


AttributesBlank:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %01000100


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
    .byte $00,$20,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte 
ProjectileSpellSprite2:
    .byte $00,$21,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite3:
    .byte $00,$22,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite4:
    .byte $00,$23,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite5:
    .byte $00,$24,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite6:
    .byte $00,$25,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite7:
    .byte $00,$26,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite8:
    .byte $00,$27,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite9:
    .byte $00,$28,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
    .byte $FF ; termination byte
ProjectileSpellSprite10:
    .byte $00,$29,$00,$00 ;yoffset -> sprite no -> palette -> xoffset
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

CrystalSprite1:
    .byte $00,$CB,$00,$00
    .byte $ff

CrystalSprite2:
    .byte $00,$CC,$00,$00
    .byte $ff

CrystalSprite3:
    .byte $00,$00,$00,$00

    .byte $ff

OptionSelectorSprite1:
    .byte $05,$12,$00,$00
    .byte $00,$13,$00,$40
    .byte $ff

OptionSelectorSprite2:
    .byte $05,$12,$00,$00
    .byte $00,$13,$00,$40
    .byte $ff

OptionSelectorSprite3:
    .byte $05,$12,$00,$00
    .byte $00,$13,$00,$40
    .byte $ff

OptionSelectorSprite4:
    .byte $05,$12,$00,$00
    .byte $00,$13,$00,$40
    .byte $ff

OptionSelectorSprite5:
    .byte $05,$12,$00,$00
    .byte $00,$13,$00,$40
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


