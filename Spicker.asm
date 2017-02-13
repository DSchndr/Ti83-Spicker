; -==ASMSPicker==-
; *By MrApplecraft*
; Parts of code by Luis Querido (from TInuke) and Suronome (Func that loads the text...)
;--------------------------------------------------------------------------------------------------

; TODO:
;  - An fking menu and the scrolling is still to friggin slow.
;  - Modify noprogs and show somewhere an icon that shows that the programs are hidden, else youre fucked if your teacher catches you :P
;  - Code cleanups
;  - Use $FE & $FF for inverted text
;  - Some other stuff (i cant remember)
;  - Redo the "TextLoader".

; If you think this code is good you should check your doc.
; If you modify the code, please contribute / send an copy of it to me to make it less awful.
; You can do whatever you want with it, i just needed it (ofc :D) and wanted to learn assembler.
;--------------------------------------------------------------------------------------------------

.nolist
#define equ .equ
#define EQU .equ
#include "ti83.inc" ; 

.list
.org 9327h

;Progstart
;--------------------------------------------------------------------------------------------------
	call _runindicoff							;We dont want that shitty thing in the corner...


;About
;Draws the Text for the Aboutscreen
;--------------------------------------------------------------------------------------------------
About:
	push BC 									;We dont want to fuck things in our keyloop up.
	call Rahmen									;How about drawing an nice "Outline" :D
	ld a,$0C
	call FastHorizLine
	ld a,$0D
	call FastHorizLine
	call _GRBUFCPY								;Copy our shit onto the display.
	call Invert
	ld d,0										;We dont want that our text is "somewhere" ;)
	ld b,d
	ld hl,Abouttxt1
	call Drawsmalltext

	ld b,$06
	ld hl,Abouttxt2
	call Drawsmalltext
	call Deinvert

	ld b,$02
	ld a,b
	call AddtoD
	ld hl,Abouttxt3
	call Drawsmalltext

	ld hl,Abouttxt4
	call Drawsmalltext

	ld hl,Abouttxt5
	call Drawsmalltext

	ld hl,Abouttxt6
	call Drawsmalltext

	ld hl,Abouttxt7
	call Drawsmalltext

	ld hl,Abouttxt8	
	call Drawsmalltext

	ld a,$05
	call AddtoD
	ld hl,Abouttxt9
	call Drawsmalltext
	pop bc 										;hey look, b isnt touched at all :D
	jp KeyLoop


;KeyLoop
; "Main" function, Loops and checks if an button is pressed.
;--------------------------------------------------------------------------------------------------

KeyLoop:
	call ResetKeyBoard
	ld a, $FE 									;Select group.
	out (1), a
	in a, (1) 									;Test for keys.
	cp $FD
	jr z, Butleft
	cp $FB
	jr z, Butright	
	
	call ResetKeyBoard
	ld a, $FD 									;Select group.
	out (1), a
	in a, (1) 									;Test for keys.
	cp $BF
	jp z, Getthehelloutofhere

	call ResetKeyBoard
	ld a, $BF 									;Select group.
	out (1), a
	in a, (1) 									;Test for keys.
	cp $BF
	call z, Fakememclr
	cp $EF
	call z, DeathNote
	cp $F7
	call z, Suicide

	jr KeyLoop

DecandgotoLp:
	pop bc
	dec b
	jr KeyLoop

Butright:
    inc b
    jr CompareButton

Butleft:
	ld a,b
	cp 0
	jp z, About
    dec b
    jr CompareButton

;CompareButton
; Gets called by Butleft/right and does the job to display the correct text
;--------------------------------------------------------------------------------------------------

CompareButton:					;Compares value stored in a and displays txt.
	push bc
	ld a,b
	cp 8						;bcuz we dont have an 9th tab :)
	jr z, DecandgotoLp
	cp 1
	jp z,Txt1
	cp 2
	jp z,Txt2
	cp 3
	jp z,Txt3
	cp 4
	jp z,Txt4
	cp 5
	jp z,Txt5
	cp 6
	jp z,Txt6
	cp 7
	jp z,Txt7
	pop bc	
	jp KeyLoop					;Jump back to the loop




;TxtX
;Tabs that get drawn.
;--------------------------------------------------------------------------------------------------

Txt1:
	ld a,$00       ;Welcher Titel / Which Title
	ld b,$00       	;Welcher Text / Which Text (From where)
	jr LdText

Txt2:
	ld a,$01
	ld b,$09
	jr LdText

Txt3:
	ld a,$02
	ld b,$12
	jr LdText

Txt4:
	ld a,$02
	ld b,$1B
	jr LdText

Txt5:
	ld a,$04
	ld b,$24
	jr LdText

Txt6:
	ld a,$04
	ld b,$2D
	jr LdText

Txt7:
	ld a,$06
	ld b,$36

LdText:
	call InitText
	ld a,b
	jp RestofInitText

; MISC ROUTINES
;--------------------------------------------------------------------------------------------------

RestofInitText:
	push bc
	call LoadNewText
	pop bc
	pop bc
	jp KeyLoop

InitText:
	push bc
	push af
	call Rahmen
	pop af
	push af
	call LoadNewTitel
	call Drawsmalltext
	call Deinvert
	pop af
	pop bc
	ret

Rahmen:											;Zeichnet Rahmen, Cleared den buffer&lcd
	call _GrBufClr								;
	call _clrlcdfull							;und erledigt die arbeit wie Currow&col zurückzusetzen und den text zu invertieren
	
	ld a,$0
	call FastVerticalLine
	ld a,$5F
	call FastVerticalLine
	ld a,$3F
	call FastHorizLine
	call _GRBUFCPY
	
	call Invert
	ld d,$00
	ld b,$01
	ret

;LoadNew
;From Suronome.
;Gets "position" from a, adds offset to hl?
;--------------------------------------------------------------------------------------------------

LoadNew:
; erstmal tun wir den index von a in hl und multiplizieren ihn dann mit zwei, da ein pointer zwei byte groß ist
 
	ld h,0
	ld l,a
	add hl,hl

; jetzt addieren wir den texte offset hinzu
	add hl,de

; nun ist in hl ein pointer zum pointer vom text, also tun wir ihn in de und dann in hl per 'ex de,hl'
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	ret

LoadNewTitel:
	push de
	ld de,Titel
	call LoadNew 
	pop de
	ret

LoadNewText:
	ld b,$07

	push af
	add a,$08
	ld e,a
	pop af

LoadNewTextLP:
	push de 	;de in den Stack
	push af	;af in den Stack

	push de 	;de in den Stack
	ld de,Texte
	call LoadNew
	pop de

	ld d,b 
	ld b,$01

	call Drawsmalltext
	ld b,d

	pop af
	pop de

	cp a,e
	ret z
	inc a
	jr LoadNewTextLP


;Drawsmalltext
;Input: Pointer to text in HL
;Input: PenRow in D
;Input: PenCol in B
;Output: $06 added to D, Displays HL
;Destroys: ?

Drawsmalltext:
	ld a,d
	ld (PENROW),a
	ld a,b
	ld (PENCOL),a
	push de
	call _vputs
	pop de
	ld a,$06
	call AddtoD
	ret

; Couldnt find anything about the builtin graphing bullshit, so i "stole" this bcuz it is quite convinient ;)
; This is from an cemetech topic.

;FastVerticalLine
;Input: A
;Output: Vertical line at A
;Destroys: af,bc,de,hl 

FastVerticalLine:
	push bc
	push de
	ld hl,plotsscreen
	ld d,0
	ld e,a
	srl e
	srl e
	srl e
	add hl,de
	and $07
	ld b,a
	inc b
	ld a,1
vertloop1:
	rrca
	djnz vertloop1
	ld c,a
	ld b,64
	ld e,12
vertloop2:
	ld a,c
	or (hl)
	ld (hl),a
	add hl,de
	djnz vertloop2
	pop bc
	pop de
	ret

;FastHorizLine
;Input: A
;Output: Horizontal line at A
;Destroys: HL,BC

FastHorizLine:
	push bc
	ld l,a  
	ld h,0  
	add a,a      
	add a,l        
	ld l,a                      
	add hl,hl      
	add hl,hl      
	ld bc,plotsscreen  
	add hl,bc  
	ld b,12  
horizloop:  
	ld (hl),%11111111  
	inc hl  
	djnz horizloop
	pop bc  
	ret

Fakememclr:										;Zeigt "Mem Cleared" an und wartet bis jemand etwas drückt
	call Getthehelloutofhere					;I have no clue why this works XD
	ld hl,3
	ld (currow),hl
	ld hl,Memcleared
	call _puts			;display text
	call _getkey
	ret

;Helper Routines
;--------------------------------------------------------------------------------------------------


ResetKeyBoard:
	ld a, $FF 					;Resets the keypad.
	out (1), a
	ret

Invert:
	set textInverse, (iy+textflags)	
	ret

Deinvert:
	res textInverse, (iy+textflags)	
	ret

;AddtoD
;Input: A
;Output: Adds A to D

AddtoD:
	add a,d
	ld d,a
	ret

Getthehelloutofhere:
	call _GrBufClr
	call _bufclear
	call _homeup
	call _clrscrnfull
	jp _eraseeol

Suicide:					;Should remove ALL evidence of this program. ---NEEDS TO BE COMPLETED---
	ret 

DeathNote:					;Function of another program (TInuke). Crashes calc that is connected.
    call _clrlcdfull
    call _homeUp
    ld   hl, SendinDeathNote1
    call _puts

    ld   hl, Deathnote1
    call DNF1
    ld   a, 08h
    call DNF2

    ld   hl, Deathnote2
    call DNF1
    call _newline

    ld   hl, SendinDeathNote2
    call _puts
    call Getthehelloutofhere

DNF2:
    push af      
    ld   a, 16h
    ld   (asm_ind_call), a  
    call 051EFh
    pop  af      
    dec  a       
    cp   00h
    jr   nz, DNF2
    ret          

DNF1:
    ld   a, 0Bh
    ld   (asm_ind_call), a  
    ld   a, (hl) 
    call 051EFh
    inc  hl      
    ld   a, (hl) 
    cp   0FFh
    jr   nz, DNF1
    ret  


; DATA
; From here you would most likely think чиво он тут делает блять. (Yes, i can speak in potato, tea brake and cyka blyat language)
; The answer is simple, storing text in the most awful way XD
;--------------------------------------------------------------------------------------------------
Thnks:
  .db "Thx to Luis Querido and Suronome and ti for no complete docs :P",0
SendinDeathNote1:
	.db "Ka",0
SendinDeathNote2:
	.db "Poof :D",0

Deathnote1:
     .db  03h, 0C9h, 09h, 00h
     .db  00h
     .db  00h
     .db  13h, 00h
     .db  00h
     .db  00h
     .db  00h
     .db  00h
     .db  00h
     .db  13h, 00h
     .db  0FFh

Deathnote2:
     .db  03h
     .db  "V", 00h
     .db  00h
     .db  0FFh


Memcleared:
    .db "  Mem Cleared",0

Abouttxt1:
	.db "  -==*ASMSpicker   v1.1*==-  ",0
Abouttxt2:
	.db "  Made by MrApplecraft ",0
Abouttxt3:
	.db $05,$7b,"Clear]: ",$22,"LehrerButton :)",$22,0
Abouttxt4:
	.db $05,$7b,"Links/Rechts]: Seite + / -",0
Abouttxt5:
	.db	$05,$7b,"2nd]: Menu N/A",0
Abouttxt6:
	.db $05,$7b,"MODE]: Fakememorycleared",0
Abouttxt7:
	.db $05,$7b,"WINDW]: Crashed den GTR",0
Abouttxt8:
	.db $05,$7b,"Y=]: Crashed GTR an I/O",0
Abouttxt9:
	.db $1c," Have fun  ",$5e,$5f,$5e,"  | V: Physik",0

; Mother of god...
Texte:
	.dw Text1,Text2,Text3,Text4,Text5,Text6,Text7,Text8,Text9,Text10,Text11,Text12,Text13,Text14,Text15,Text16,Text17,Text18,Text19,Text20,Text21,Text22,Text23,Text24,Text25,Text26,Text27,Text28,Text29,Text30,Text31,Text32,Text33,Text34,Text35,Text36,Text37,Text38,Text39,Text40,Text41,Text42,Text43,Text44,Text45,Text46,Text38,Text48,Text49,Text50,Text51,Text52,Text53,Text54,Text55,Text56,Text57,Text58,Text59,Text60,Text61,Text62,Text63

Titel:
	.dw Titel1,Titel2,Titel3,Titel4,Titel5,Titel6,Titel7

; Titel
;--------------------------------------------------------------------------------------------------

Titel1:
	.db "1",$1a," ",0
Titel2:
	.db "2",$1a," ",0
Titel3:
	.db "3",$1a," ",0
Titel4:
	.db "4",$1a," ",0
Titel5:
	.db "5",$1a," ",0
Titel6:
	.db "6",$1a," ",0
Titel7:
	.db "7",$1a," ",0
Titel8:
	.db "8",$1a," ",0
Titel9:
	.db "9",$1a," ",0
Titel10:
	.db "10",$1a," ",0

;1. Tab
;--------------------------------------------------------------------------------------------------
Text1:
	.db "",0
Text2:
	.db "",0
Text3:
	.db "",0
Text4:
	.db "",0
Text5:
	.db "",0
Text6:
	.db "",0
Text7:
	.db "",0
Text8:
	.db "",0
Text9:
	.db	"",0

;2. Tab
;--------------------------------------------------------------------------------------------------
Text10:
	.db	"",0
Text11:
	.db	"",0
Text12:
	.db	"",0
Text13:
	.db "",0
Text14:
	.db	"",0
Text15:
	.db	"",0
Text16:
	.db	"",0
Text17:
	.db	"",0
Text18:
	.db	"",0

;3. Tab
;--------------------------------------------------------------------------------------------------

Text19:
	.db	"",0
Text20:
	.db	"",0
Text21:
	.db	"",0
Text22:
	.db	"",0
Text23:
	.db	"",0
Text24:
	.db	"",0
Text25:
	.db	"",0
Text26:
	.db	"",0
Text27:
	.db	"",0

;4. Tab
;--------------------------------------------------------------------------------------------------

Text28:
	.db	"",0
Text29:
	.db	"",0
Text30:
	.db	"",0
Text31:
	.db	"",0
Text32:
	.db	"",0
Text33:
	.db	"",0
Text34:
	.db	"",0
Text35:
	.db "",0
Text36:
	.db	"",0

;5. Tab
;--------------------------------------------------------------------------------------------------

Text37:
	.db	"",0
Text38:
	.db	"",0
Text39:
	.db "",0
Text40:
	.db	"",0
Text41:
	.db "",0
Text42:
	.db	"",0
Text43:
	.db	"",0
Text44:
	.db	"",0
Text45:
	.db	"",0
;6. Tab
;--------------------------------------------------------------------------------------------------

Text46:
	.db	"",0
;Text47:
;	.db "",0	;replaced with 38 in the pointer thing cuz same txt and i need memory :P									
Text48:
	.db	"",0
Text49:
	.db	"",0
Text50:
	.db	"",0
Text51:
	.db	"",0
Text52:
	.db	"",0
Text53:
	.db	"",0
Text54:
	.db	"",0

;7. Tab
;--------------------------------------------------------------------------------------------------

Text55:
	.db	"",0
Text56:
	.db	"",0
Text57:
	.db	"",0
Text58:
	.db	"",0
Text59:
	.db	"",0
Text60:
	.db	"",0
Text61:
	.db "",0
Text62:
	.db	"",0
Text63:
	.db	"",0

.end

;24 is an magical number
