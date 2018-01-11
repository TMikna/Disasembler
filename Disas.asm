;;	    LOCALS @@

		
		Uzdarymas macro handle            ;ax, bx
			mov ah, 3Eh
			mov bx, handle
			int 21h
			endm
			
		Skaityk macro kiekis              ;ax, bx, cx, dx, si
		    mov al, [skBuf+0]
			xor ah, ah
			mov si, ax
			
	        mov ah, 3Fh
			mov bx, HandleI
			mov cx, kiekis
			lea dx, [skBuf+si]
			int 21h
			
			add [skBuf+0], cl
		
			endm
			
		SpausdinkSimboli macro simbolis   ;ax, dx
; i konsole, buvo reikalingas pradzioje, paprastesniam tikrinimui
			mov ah, 02h
			mov dl, simbolis
			int 21h
			endm
			
		Rasyk macro kiek             ;ax, bx, cx, dx cia kiek - kiek baitu uzima komanda (mnemonika)
			mov cx, kiek
			add cx, 36d
			mov ah, 40h
			mov bx, handleO
			mov dx, offset Buf
			int 21h
			endm
			
		Kopijuok macro iskur, ikur, kiekk  ;si, di, cx       ;kiek - zodzio registras arba immediate
			push es
			push ds
			pop es
			cld
			mov cx, kiekk
			lea si, iskur
			lea di, ikur
			add [dBuf], cl
			rep movsb
			pop es
			endm
			
		RaskSReg macro   
			mov al, reg
			add al, reg
			call KurdBuf
			xor bx, bx
			mov bl, al
			kopijuok [sregbuf+bx], [dBuf+di], 2
			endm
			
		RaskReg macro     ;!Reik nustatyti w, pats neranda reg    ;ax, bx, di       ;Nustato ir į dBuf iraso registr
			mov al, 16d
			mov bl, w
			mul bl
			add al, reg
			add al, reg
			
			xor bx, bx
			mov bl, [dBuf+0]
			mov di, bx
			
			mov bl, al
			kopijuok [regbuf+bx], [dBuf+di], 2
			endm
			
			
		RaskRm macro   ;!Reik nustatyti w, pats neranda mod ir reg jei reikia;    ax, bx, di
		   local neAtmintis, Atmintis, RmPab, NetiesiogineAdr, trumpas, ilgas
		   cmp modd, 3
		   je neAtmintis
		   jmp Atmintis
		   
		   neAtmintis:
		   mov al, rm
		   mov reg, al
		   RaskReg
		   call TReg
		   jmp RmPab
		   
		   Atmintis:
		   call prefIdBuf
		   cmp modd, 0
		   jne NetiesiogineAdr
		   cmp rm, 6h
		   jne NetiesiogineAdr
		   
		   call Tiesioginis
		   ret
		   ; mov ax, word ptr [SkBuf+1]    ; kitas tiesioginio vard rasymo variantas
		   ; mov bl, [dBuf+0]
		   ; xor bh
		   ; mov [dBuf+bx], ah
		   ; inc bx
		   ; mov [dBuf+bx], al		   
		   ; jmp @@RmPab
		   
		   NetiesiogineAdr:                   ;tikrina netiesioginį
		   KopSimbl '['
		   mov al, rm
		   mov bl, 5
		   mul bl
		   
		   xor bx, bx                         ; galima naudoti call KurdBuf
		   mov bl, [dBuf+0]
		   mov di, bx
		   
		   mov bx, ax
		   cmp rm, 3
		   ja trumpas
		   kopijuok [rmbuf+bx], [dBuf+di], 5
		   jmp ilgas
		   trumpas:
		   kopijuok [rmbuf+bx], [dBuf+di], 2
		   ilgas:
		   call Poslinkis
		   KopSimbl ']'
		   
		   RmPab:
		   endm
		   
			
		   
		KopSimbl macro simbolis               ; di, bx, padidina dBuf skaitliuka
            call KurdBuf
            mov [dbuf+bx], simbolis
		    inc [dBuf+0]
			endm
			
		HexToASCII macro KiekBaitu            ;KiekBaitu - immediate arba breg, >0  ;al, cx, bx, si rodo i sekanti skbuf el.   kodas is skBuf verciamas i ascii koda ir -> dBuf (dvigubai daugiau baitu)
			local TesiamKonvertavima
			xor ch, ch
			mov cl, KiekBaitu
			TesiamKonvertavima:
			call KurskBuf
			sub bl, cl
			mov al, [skBuf+bx]
			push ax
            shr al, 4
			call ALtoASCII
			pop ax
			and al, 0Fh
			call ALtoASCII
			loop TesiamKonvertavima
			endm
		   
		AdresacijosBitai macro wreik   ; jei w?=1, tai w=1, jei =0 tada call TW
			local wreikia, wnereikia
			mov al, wreik
			cmp al, 0
			je wreikia
			mov w, 1h
			jne wnereikia
			wreikia:
			call TW
			wnereikia:
            call TD
			call TModd
			call TReg
			call TRm
			endm
			
		ParuoskDiMnem macro pradAl          ; paruosia di, mnemonikos spausdinimui (di - poslinkis nuo buf pradz) ; ax, bl, di
			mov al, pradAl                  ;?-88d
			add al, [skbuf+1]
			mov bl, 8
			mul bl
			mov di, ax
			endm
			
					
		AddIP macro Dydis     ; dydis - kiek baitu ilgio poslinkis, gali būt 1 arba 2, immediate   ;ax, bx, cx, dx, si, di   ;prie disp prideda IP ir iraso i dbuf
			local BytePtr, WordPtr
			
			skaityk Dydis
			call KurskBuf
			mov bl, [IP+1]
			mov bh, [IP+0]
			add bx, si
			dec bx
			mov cl, Dydis
			cmp cl, 1
			je BytePtr
			add bx, word ptr [skBuf+2]
			jmp WordPtr
			BytePtr:
			mov al, byte ptr [skBuf+2]
			cbw
			add bx, ax
			WordPtr:
			mov word ptr [skBuf+si], bx
			add skbuf, 2
			HexToASCII 2
			call Apsuk
			sub skBuf, 2
			endm
			
		 retendp macro
			ret 
			endp
			endm

;PSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPS			
		KoPabaiga macro MnemBuf         ;MnemIlgis saugiausia su immediate
			push di
			call KurdBuf
			Kopijuok enteris, [dBuf+bx], 2
			inc bx
			Kopijuok [dBuf+1], [Buf+36], bx                 ;PVZ (pradzia)
			pop di
			Kopijuok MnemBuf, [Buf+28], 8                 ;PVZ (pradzia)
			push bx
			call MasKod
			call IPIrasymas
			pop bx
			Rasyk bx
			call PasiruoštiNaujai
			endm
			

;PSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPSPS
			
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

.model small

.stack 200h

.data
	
	kl_NeraParametru db 'Klaida: Prasome paleisti is naujo ir ivesti parametrus', 0Dh, 0Ah, '$'
	kl_MazaiParametru db 'Klaida: Neteisingai ivesti parametrai', 0Dh, 0Ah, '$'
	kl_IvNeatidarytas db 'Atidarant ivedimo faila kilo klaida', 0Dh, 0Ah, '$'
	kl_IsvNeatidarytas db 'Kuriant rezultatu faila kilo klaida', 0Dh, 0Ah, '$'
	
	sregbuf db 'ESCSSSDS'
	regbuf db 'ALCLDLBLAHCHDHBHAXCXDXBXSPBPSIDI'
	rmbuf db 'BX+SIBX+DIBP+SIBP+DISI   DI   BP   BX'
    Mnem db 'MOV     PUSH    POP     XCHG    IN      OUT     XLATB   LEA     LES     LDS     CBW     CWD     CALL    WAIT    PUSHF   POPF    SAHF    LAHF    '
	MnemLg db 'ROL     ROR     RCL     RCR     SHL     SHR     NOT     TEST    SAR     ' ;patvarkyti test vieta
	MnemVPer db 'JO      JNO     JB      JAE     JZ      JNZ     JBE     JA      JS      JNS     JPE     JPO     JL      JGE     JLE     JG      INTO    IRET    INT     RET     RETF    JMP     LOOPNZ  LOOPZ   LOOP    JCXZ    '
	MnemSW db 'ADD     OR      ADC     SBB     AND     SUB     XOR     CMP     '
	MnemEil db 'MOVSB   MOVSW   CMPSB   CMPSW   TEST    TEST    STOSB   STOSW   LODSB   LODSW   SCASB   CSASW   '
	MnemFX db 'INC     DEC     CALL    CALL    JMP     JMP     PUSH    HLT     CMC     REPE    REPNE   CLC     STC     CLI     STI     CLD     STD     TEST            NOT     NEG     MUL     IMUL    DIV     IDIV    LOCK    '
	MnemBDC db 'AAM     AAD                     DAA     DAS     AAA     AAS     '
	Mnem4x5x db 'INC     DEC     PUSH    POP     '
	abuptr db 'BYTE PTR WORD PTR FAR '
	Prefbuf db 00, '     ES:     CS:     SS:     DS:'	
    dbbuf db 'DB      NOP                 (unused)ESC     '	;taip pat tarpai valymui
	enteris db 0Dh, 0Ah
	IP db 01h,00h

	
	eregbuf db 'EAXECXEDXEBXESPEBPESIEDI'
		
	handleI dw ?
	handleO dw ?
	
	w db ?
	d db ?
	s db ?
	modd db ?
	reg db ?
	rm db ?
	
	
; nereik?	skaitykKiek dw ?                      ;skaiciuot skBuf elementu sk
	
	
	IvVard db 64, 00, 64 dup (0)
	IsvVard db 64, 00, 64 dup (0)
	Buf db '072A:', 251 dup (' ')          ;Buferis į kurį surašau viską ir po to išvedu
	dBuf db 1, 00, 32 dup (?)            ;dBuf+0 - kiek jame jau yra elementų;    Darbinis buferis, į kurį rašau dali komandos, kuria poto perkeliu i buf, kur sudelioju eilute ir isspausdinu
	skbuf db 1, 00, 32 dup ('$')         ;skBuf+0 - kiek jame jau yra elementų;   Buferis į kurį skaitau kodą
	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
	.code
	mov ax, @data
	mov ds, ax
	
	call FailuAtidarimas
	
	SkaitomIrDirbam:
	
	Skaityk 1                                           ; Nuskaitom opk
	cmp ax, 0
	jne Toliaudirbam                                   ; tikrinam ar ax = 0 (t. y. ar dar nepasibaigė failas)
	jmp KodasBaigesi
	
	Toliaudirbam:
	call pref
	call xorOPK
	call cmpOPK
	
	cmp dx, 0F0FFh                                      ;tuo atveju, jei kodas neatpazystamas, spausdinama db ir neatpazintas baitas
	jne NeDB
	call dbNeatpazinta
	NeDB:
	
	jmp SkaitomIrDirbam
	KodasBaigesi:
	
	pab:
	
	Uzdarymas HandleI
	Uzdarymas Handleo
	
mov ah, 4Ch
int 21h
	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc	
;CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc	


		proc xorOPK
		
			mov al, [skBuf+1]
			shr al, 4
			ret
			endp
			
		proc cmpOPK
			cmp al, 0Eh
			ja LygiaiF
			jne maziauE
			call OPKEx
			ret
			LygiaiF:
			call OPKFx
			ret
			maziauE:
			cmp al, 0Dh
			jne maziauD
			call OPKDx
			ret
			maziauD:
			cmp al, 0Ch
			jne maziauC
			call OPKCx
			ret
			maziauC:
			cmp al, 0Bh
			jne maziauB
			call OPKBx
			ret
			maziauB:
			cmp al, 0Ah
			jne maziauA
			call OPKAx
			ret
			maziauA:
			cmp al, 9h
			jne maziau9
			call OPK9x
			ret
			maziau9:				
			cmp al, 8h
			jne maziau8
			call OPK8x 
			ret
			maziau8:
			cmp al, 6h
			jb maziau6
			jne nera6
			
			mov dx, 0F0FFh
			ret
			nera6:
			call OPK7x
			ret
			maziau6:
			cmp al, 4h
			jb maziau4
			call OPK4x5x
			ret
			maziau4:
			cmp al, 2h
			jb maziau2
			call OPK2x3x
			ret
			maziau2:
			call OPK0x1x
			ret
			endp
			
		proc OPK0x1x
			mov dl, [skbuf+1]
			and dl, 0fh
			cmp dl, 0Eh
			jb Maziau0E
			call PushPopSeg
			ret
			Maziau0E:
			cmp dl, 08h
			jb Maziau08
			call AddOr00_0308_0D
			ret
			Maziau08:
			cmp dl, 06h
			jb Maziau06 
			call PushPopSeg
			ret
			Maziau06:
			call AddOr00_0308_0D
			ret
			endp
			
		proc OPK2x3x
			mov dl, [skbuf+1]
			and dl, 0fh
			cmp dl, 0Eh
			jb Maziau0Eh
			  call AritmetiniuAdresasMnemSW
			  mov di, bx
			  KoPabaiga [MnemBDC+di]
			  ret
			Maziau0Eh:
			cmp dl, 08h
			jb Maziau08h
			  call AddOr00_0308_0D
			  ret
			Maziau08h:
			cmp dl, 06h
			jb Maziau06h 
			  call AritmetiniuAdresasMnemSW
			  mov di, bx
			  KoPabaiga [MnemBDC+di]
			  ret
			Maziau06h:
			  call AddOr00_0308_0D
			  ret
			endp
			
		proc OPK4x5x
			call PushPopDecInc4x5x
			ret
			endp
			
		proc OPK7x
			AddIP 1
			ParuoskDiMnem 90h
			KoPabaiga [MnemVPer+di]
			ret
			endp
			
		proc OPK8x
			skaityk 1
			cmp [skbuf+1], 8Ch
			je SegMov
			jb nedaugiau8C
			cmp [skbuf+1], 8Eh
			je SegMov
			jb Lea8Dh
			ja popRegMem
			
			jmp nedaugiau8C
			popRegMem:
			call pop8F
			ret
			Lea8Dh:
			call lea8D
			ret
			SegMov:
			call mov8E8C
			ret
			nedaugiau8C:
			
			cmp [skBuf+1], 88h
			jb neSegRegMOV
			call mov88_8B
			ret
			neSegRegMOV:
			
			cmp [skBuf+1], 86h
			jb Maziau86
			call Xchg8687
			ret
			Maziau86:
			cmp [skBuf+1], 84h
			jb Maziau84
			call Test8485
			ret
			Maziau84:
			call KomandosSuS
			ret
			endp
			
		proc OPK9x
			cmp [skbuf+1], 98h
			jb xchg9x
			call FlagsPP98_9F
			ret
			xchg9x:
			call xchg90_97
			ret
			endp
			
		proc OPKAx
			call TW
			call TD
			mov reg, 0
			
			cmp [skbuf+1], 0A4h
			jb MaziauA4
			call EilA4_AF
			ret
			MaziauA4:
			call MovA0_A3
			ret
			endp
			
		proc OPKBx
			call MovB0_BF
			ret
			endp
			
		proc OPKCx
			cmp [skbuf+1], 0CAh
			jb MaziauCA
			cmp [skbuf+1], 0CEh
			jb MaziauCE
			call IntoIret
			ret
			MaziauCE:
			cmp [skbuf+1], 0CCh
			jb MaziauCC
			call IntCDCC
			ret
			MaziauCC:
			call RetCACBC2C3
			ret
			
			MaziauCA:
			cmp [skbuf+1], 0C7h
			ja DaugiauC7
			cmp [skbuf+1], 0C2h
			jb DaugiauC7
			                  
			cmp [skbuf+1], 0C6h
			jb MaziauC6
			call MovC6C7
			ret
			MaziauC6:
			cmp [skbuf+1], 0C4h
			jb MaziauC4
			call LesLds
			ret
			MaziauC4:
			call RetCACBC2C3
			ret
			
			DaugiauC7:           ; Daugiau C8 arba maziau C1
			mov dx, 0F0FFh
			ret
			endp
			
		proc OPKDx
			cmp [skBuf+1], 0D8h
			jb MaziauD8
			call EscD8_DF
			ret
			MaziauD8:
			cmp [skBuf+1], 0D6h
			ja XlatbD7
			jb MaziauD6
			mov dx, 0F0FFh
			ret
			XlatbD7:
			KoPabaiga [Mnem+30h]
			ret
			MaziauD6:
			cmp [skBuf+1], 0D4h
			jb MaziauD4
			call AamAad
			ret
			MaziauD4:
			call LogD0_D3
			ret
			endp
			
		proc OPKEx
			cmp [skBuf+1], 0ECh
			jb MaziauEC
			call InOutEC_EF_E4_E7
			ret
			MaziauEC:
			cmp [skBuf+1], 0EAh
			ja LygiaiEB
			je LygiaiEA
			jb MaziauEA
			LygiaiEB:
			call JmpEB
			ret
			LygiaiEA:
			call Tiesiog4bAdr
			KoPabaiga [MnemVPer+0A8h]
			ret
			MaziauEA:
			cmp [skBuf+1], 0E8h
			jb MaziauE8
			call CallJmpE8E9
			ret
			MaziauE8:
			cmp [skBuf+1], 0E4h
			jb MaziauE4
			call InOutEC_EF_E4_E7
			ret
			MaziauE4:
			call LoopJcxzE0_E3
			ret
			endp
			
		proc OPKFx
			cmp [skBuf+1], 0FEh
			jb MaziauFE
			call OpkFEFF
			ret
			MaziauFE:
			cmp [skBuf+1], 0F8h
			jb MaziauF8
			call ClStF8_FD
			ret
			MaziauF8:
			cmp [skBuf+1], 0F6h
			jb MaziauF6
			call AritmF6F7
			ret
			MaziauF6:
			cmp [skBuf+1], 0F4h
			jb MaziauF4
			call ClStF8_FD
			ret
			MaziauF4:
			cmp [skBuf+1], 0F1h
			ja DaugiauF1
			jb MaziauF1
			mov dx, 0F0FFh
			ret
			DaugiauF1:
			call RepF2F3
			ret
			MaziauF1:
			Kopijuok [dbbuf+28d], [dbuf+1], 8
			KoPabaiga [MnemFX+0C8h]
			ret
			endp
			
			
		proc FailuAtidarimas
			
			mov ch, 0
			mov cl, es:[0080h]
			cmp cl, ch
			jne ParametraiYra
			
			mov ah, 09h
			lea dx, kl_NeraParametru
			int 21h
			jmp pab
			
			ParametraiYra:
		   ;----------; skaitau pirmą parametrą
			cld      
			dec cx	
			mov di, 82h
			mov si, 00h
			mov al, ' '
			
			PPSkaitymas:
			scasb
			je PPPerskaitytas
			mov ah, es:[di-1]
			mov [IvVard+si], ah
			inc si
			dec cx
			cmp cx, 0
			je TrukstaPar                    ;; jei nuskaitomas tik 1 failas reiškia truksta parametrų
			jmp PPSkaitymas
			
			TrukstaPar:
			mov ah, 09h
			mov dx, offset kl_MazaiParametru
			int 21h
			jmp pab
			
			PPPerskaitytas:

			;---------;skaitau antrą parametrą
			mov si, 00h
			dec cx	
			APSkaitymas:
			mov ah, es:di
			mov [IsvVard+si], ah 
			inc di
			inc si
			loop APSkaitymas
			
			
		   ;-------;atsidarau duomenu faila
			mov ax, 3D00h                       
			mov dx, offset IvVard         
			int 21h
			
			mov handleI, ax
			
		   jnc IvdimasAtidaryta               ; tikrinu ar neįvyko klaida
			mov ah, 09h
			mov dx, offset kl_IvNeatidarytas
			int 21h
			jmp pab
			
			IvdimasAtidaryta:
			
		   ;-----------;atsidarau rezultatu faila
			mov ah, 3Ch
			mov cx, 00h
			mov dx, offset IsvVard
			int 21h
			
			mov handleO, ax
			
			jnc IsvedimasAtidaryta
			mov ah, 09h
			lea dx, kl_IsvNeatidarytas
			int 21h
			jmp pab
			
			IsvedimasAtidaryta:
			
			ret
			endp
		
;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''		
		proc TW                                     ;tikrinu pirma OPK baitą (antra skbuf elementa), paskutinį bitą
			mov al, [skbuf+1]
			mov w, al
			and w, 00000001b
			ret
			endp
			
		proc TD                                     ;tikrinu pirma OPK baitą (antra skbuf elementa), priespaskutinį bitą
			mov al, [skbuf+1]
			mov d, al
			and d, 00000010b
			ret
			endp
			
		proc TModd                                  ;tikrinu antra OPK baitą (trecia skbuf elementa), palieku 2 pirmus bitus
		   mov al, [skbuf+2]
		   shr al, 6
		   mov modd, al 
		   ret
		   endp
		   
		   
		   
		proc TReg                                   ;tikrinu antra OPK baitą (trecia skbuf elementa), palieku 3 bitus vidury
		   mov al, [skbuf+2]
		   mov reg, al
		   mov cl, 2
		   shl reg, cl
		   shr reg, 5  
		   ret
		   endp
		   
		proc TRm                                    ;tikrinu antra OPK baitą (trecia skbuf elementa), palieku 3 paskutinius bitus
		   mov al, [skbuf+2]
		   mov rm, al
		   and rm, 00000111b
		   ret
           endp
		   
		proc TRegOpk                               ;tikrinu opk baitą (pirma perskaitytą) 3 paskutinius bitus, al lieka nepakeistas OPK
			mov al, [skbuf+1]
			mov reg, al
			and reg, 00000111b
			ret
		   endp
;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		
		proc Apsuk                   ;apsuka 2 paskutinius dBuf elementus (zodzius)
		     call KurdBuf
			 sub bx, 4
		     xchg ax, word ptr [dBuf+bx]
		     xchg ax, word ptr[dBuf+bx+2]
		     xchg ax, word ptr[dBuf+bx]
			 ret 
			 endp
			 
		proc KurdBuf       ; nustato di, bx i kuria vieta kopijuot, di, bx
		    mov bl, [dBuf+0]
			xor bh, bh
			mov di, bx
			ret
			endp KurdBuf
			
		proc KurskBuf       ; nustato si, bx kiek yra elementu, si, bx
			mov bl, [skBuf+0]
			xor bh, bh
			mov si, bx
			ret
			endp
			
		proc Tiesioginis   ;patvarkyti    ;bx, ah,                    ;tikrina tiesiogini adresa
			call prefIdBuf
			KopSimbl '['
			Skaityk 2                         		   
			HexToASCII 2
			call Apsuk
			KopSimbl ']'
			ret
			endp
		  
		proc Poslinkis
		    cmp modd, 0
			jne @@modnenulis
			ret
			@@modnenulis:
			KopSimbl '+'
			mov cl, modd
			xor ch, ch
			skaityk cx
			HexToASCII modd
			cmp modd, 2
			jne @@ViensB
			call Apsuk
			@@ViensB:
			ret
			endp
			
		proc ALtoASCII                                  ; al esandi vienzenkli skaiciu pavercia ascii skaicium ir iraso i dBuf, padidina dBuf skaitliuka
			add al, 30h
			cmp al, 39h
			jna @@Skaitmuo
			add al, 7h
			@@Skaitmuo:
			KopSimbl al
			ret
			endp
			
		proc MasKod
		    mov [dBuf+0], 1
		    mov al, [skBuf+0]
			dec al
			HexToASCII al
            dec si			                    ; si is HexToASCII
			add si, si
			Kopijuok [dBuf+1], [Buf+10], si     ;bx=2*[skBuf+0]
			ret
			endp
			
		proc IPIrasymas
			call KurskBuf
			mov dx, si
			Kopijuok [IP+0], [skBuf+1], 2
			mov [skBuf+0], 3
			mov [dBuf+0], 1
			HexToASCII 2
			Kopijuok [dBuf+1], [Buf+5], 4
			dec dx
			add [IP+1], dl
			adc [IP], 0
			ret
			endp
			
		proc PasiruoštiNaujai
			Kopijuok [dbbuf+11], [buf+11], 17d                ;isvalyti isvedimo buferi
			mov dBuf, 1
			mov skBuf, 1
			mov Prefbuf, 0
			mov al, 10h
			ret
			endp
		
		proc PtrKopijavimas                          ; idejau mod tikrinima pries darydamas Fx komandas
			cmp modd, 3h
			jne PtrReikalingas
			ret
			PtrReikalingas:
		    call KurdBuf
			mov ax, 9
			mov bl, w
			mul bl
			mov bx, ax
			Kopijuok [abuptr+bx], [dBuf+di], 9
			ret
			endp
			
		proc RegRmIBuf        ; i Buf iraso reg ir rm reikiama tavrka (pagal d bita) BUTINA pries vykdant ivikditi AdresacijosBitai makrosą, jei reikia psikeisti d
			cmp d, 0
			jne neFromReg
			jmp fromReg
			neFromReg:
			RaskReg
			KopSimbl ','
			RaskRm
			jmp toReg
			
			fromReg:
			RaskRm
			KopSimbl ','
			RaskReg
			
			toReg:
			ret
			endp
			
		proc pref                                   ;Bp reikia nepakeisti iki "AdresacijosBitai" yvykdymo
			cmp [skBuf+1], 26h
			je PrefYra
			cmp [skBuf+1], 2Eh
			je PrefYra				
			cmp [skBuf+1], 36h
			je PrefYra
			cmp [skBuf+1], 3Eh
			je PrefYra
			ret
			PrefYra:
			mov al, [skBuf+1]
			mov Prefbuf, al
			dec skBuf
			skaityk 1
			ret
			endp
			
		proc prefIdBuf
			cmp [Prefbuf+0], 0
			jne PrefixYra
			ret
			PrefixYra:
			call KurdBuf
			mov bl, [Prefbuf+0]
			mov bh, 0
			sub bx, 20h
			Kopijuok [Prefbuf+bx], [dBuf+di] , 3
			mov [Prefbuf+0], 0
			ret
			endp
			
		proc TiesiogIvedS                  ; s nagrinėju kaip d
			KopSimbl ','
			cmp w, 0
			jne wb1

			skaityk 1
			HexToASCII 1
			ret
			
			wb1:
			cmp d, 0
			jne sw11
			skaityk 2
			HexToASCII 2
			call Apsuk
			ret
			
			sw11:
			skaityk 1
			call KurskBuf
			dec bx
			cmp [skBuf+bx], 0h
			jl ImmNeigiamas
			KopSimbl '+'
			HexToASCII 1
			ret
			ImmNeigiamas:
			KopSimbl '-'
			neg [skBuf+bx]
			HexToASCII 1
			neg [skBuf+bx]
			ret
			endp
			
		proc SpecialusSW   ;DB
			mov ax, 4201h
			mov bx, handleI
			mov cx, 0ffffh
			mov dx, 0ffffh
			int 21h
			mov skBuf, 2
			mov dbuf, 1
			HexToASCII 1
			KoPabaiga dbbuf
			ret
			endp
			
		proc Tiesiog4bAdr
			Skaityk 4
			HexToASCII 2
			call Apsuk
			Kopijuok [skBuf+2], [skBuf+6], 2
			add [skBuf+0], 2
			sub [dBuf+0], 2
			KopSimbl ':'
			HexToASCII 2
			call Apsuk
			sub [skBuf+0], 2
			ret
			endp
			
		proc Dataa
			KopSimbl ','
			skaityk 1
			HexToASCII 1
			cmp w, 0
			je data1b
			skaityk 1
			HexToASCII 1
			call Apsuk
			data1b:
			ret
			endp
		
		proc MnemPagalRegDi
			mov al, reg
			mov bl, 8h
			mul bl
			mov di, ax
			ret
			endp
			
		proc INirOUT
			cmp dl, 0
			je portas
			call KurdBuf
			Kopijuok [regbuf+20], [dBuf+di], 2
			ret
			portas:
			skaityk 1
			HexToASCII 1
			ret
			endp
			
		proc AritmetiniuAdresasMnemSW
			mov al, [skBuf+1]
			shr al, 4
			add al, al
			mov bx, 8
			mul bl
			mov bl, [skBuf+1]
			and bl, 08h
			add bl, al
			ret
			endp
			
;KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK		
		
		proc mov88_8B
			AdresacijosBitai 0
			call RegRmIBuf
			KoPabaiga [Mnem+0]
			ret
			endp
		
		proc mov8E8C
			AdresacijosBitai 1
			cmp d, 1
			je ISeg
			jmp IsSeg
			
			ISeg:
			RaskSReg
			KopSimbl ','
			call PtrKopijavimas
			RaskRm
			jmp NeIsSeg
			
			IsSeg:
			call PtrKopijavimas
			RaskRm
			KopSimbl ','
			RaskSReg
			
			NeIsSeg:
			KoPabaiga Mnem
			ret
			endp
			
		proc lea8D
			AdresacijosBitai 0
			mov d, 1
			call RegRmIBuf
			KoPabaiga [Mnem+56]
			ret
			endp
			
		proc pop8F
			AdresacijosBitai 0
			call PtrKopijavimas
			RaskRm
			KoPabaiga [Mnem+16]
			ret
			endp
			
		proc xchg8687
			AdresacijosBitai 0
			call RegRmIBuf
			KoPabaiga [Mnem+24]
			ret
			endp
			
		proc Test8485                    ;pratestuoti
			AdresacijosBitai 0
			mov d, 1
			call RegRmIBuf
			KoPabaiga [MnemLg+64]        ;taisytas poslinkis
			ret
			endp
			
		proc KomandosSuS
			AdresacijosBitai 0
			cmp w, 0
			jne NeDb82
			cmp d, 2
			jne NeDb82
			call SpecialusSW
			ret
			NeDb82:
			cmp modd, 3h
			je NoForPtr
			call PtrKopijavimas
			NoForPtr:
			RaskRm
			call TiesiogIvedS
			
			call MnemPagalRegDi
			KoPabaiga [MnemSW+di]
			ret
			endp
			
		
			
		proc FlagsPP98_9F
			
			cmp [skbuf+1], 9Ah
			jne NeCall9A
			call Tiesiog4bAdr
			NeCall9A:
			cmp [skbuf+1], 9Bh
			jne neWait
			Kopijuok [dbbuf+28d], [dbuf+1], 8
			neWait:
			ParuoskDiMnem 072h            ;-88d
			KoPabaiga [Mnem+di]
			ret
			endp
			
		proc Xchg90_97
		    
			call TRegOpk
			cmp reg, 0
			jne neNOP
			KoPabaiga [dbbuf+8]
			ret
			neNOP:
			Kopijuok [regbuf+16], [dBuf+1], 2
			KopSimbl ','
			mov w, 1
			RaskReg
			KoPabaiga [Mnem+24]
			ret
			endp
			
		proc MovA0_A3
			
			cmp d, 0
			je @@neFromReg2
			jmp @@fromReg2
			@@neFromReg2:
			
			RaskReg
			KopSimbl ','
			call Tiesioginis
			jmp @@toReg2
			
			@@fromReg2:
			call Tiesioginis
			KopSimbl ','
			RaskReg
			
			@@toReg2:
			KoPabaiga [Mnem+0]
			ret
			endp
			
		proc EilA4_AF
			cmp [skbuf+1], 0A8h
			je TestA8A9
			cmp [skbuf+1], 0A9h
			je TestA8A9
			jmp neTest
			TestA8A9:
			RaskReg
			call Dataa
			
			neTest:
			ParuoskDiMnem 5Ch
			KoPabaiga [MnemEil+di]
			ret
			endp
			
		proc MovB0_BF
			call TRegOpk
			mov al, [skbuf+1]
			and al, 00001000b
			shr al, 3
			mov w, al
			RaskReg
			call Dataa
			KoPabaiga [Mnem]
			ret
			endp
			
		proc IntoIret
			ParuoskDiMnem 42h
			KoPabaiga [MnemVPer+di]
			ret
			endp
			
		proc IntCDCC
			cmp [skBuf+1], 0CDh
			jne int_3
			Skaityk 1
			HexToASCII 1
			jmp neint_3
			int_3:
			KopSimbl '3'
			neint_3:
			KoPabaiga [MnemVPer+90h]
			ret
			endp
			
		proc RetCACBC2C3
			call TW
			cmp w, 0
			jne DataNer
			mov w, 1
			skaityk 2
			HexToASCII 2
			call Apsuk
			DataNer:
			cmp [skBuf+1], 0CAh
			ja vidinis
			KoPabaiga [MnemVPer+0A0h]
		    ret
			vidinis:
			KoPabaiga [MnemVPer+098h]
			ret
			endp
		
		proc MovC6C7
			skaityk 1
			AdresacijosBitai 0
			call PtrKopijavimas
			RaskRm
			call Dataa
			KoPabaiga [Mnem+0]
			ret 
			endp
			
		proc LesLds
			skaityk 1
			AdresacijosBitai 1
			mov d, 1
			call RegRmIBuf
			ParuoskDiMnem 44h
			KoPabaiga [Mnem+di]
			ret 
			endp
			
		proc AamAad
			Skaityk 1
			HexToASCII 1
			ParuoskDiMnem 02Ch
			KoPabaiga [MnemBDC+di]
			ret 
			endp
			
		proc LogD0_D3
			skaityk 1
			AdresacijosBitai 0
			cmp modd, 3
			je PtrNebus
			call PtrKopijavimas
			PtrNebus:
			RaskRm
			KopSimbl ','
			cmp d, 0
			je NeCl
			KopSimbl 'C'
			KopSimbl 'L'
			Jmp Yracl
			NeCl:
			KopSimbl '1'
			Yracl:
			call MnemPagalRegDi
			KoPabaiga [MnemLg+di]
			ret
			endp
			
		proc InOutEC_EF_E4_E7
			call TW
			call TD       ;tikrint in ar out
			mov reg, 0
			mov dl, [skBuf+1]
			and dl, 00001000b
			cmp d, 0
			je neOUTas
			jmp OUTas
			neOUTas:
			RaskReg
			KopSimbl ','
			call INirOUT
			KoPabaiga [Mnem+32d]
			ret
			OUTas:
			call INirOUT
			KopSimbl ','
			RaskReg
			KoPabaiga [Mnem+40d]
			ret
			endp
			
		proc CallJmpE8E9
			AddIP 2
			call TW
			cmp w, 0
			je CALLass
			KoPabaiga [MnemVPer+0A8h]
			ret
			CALLass:
			KoPabaiga [Mnem+60h]
			ret
			endp
			
		proc LoopJcxzE0_E3
			AddIP 1
			ParuoskDiMnem 36H
			KoPabaiga [MnemVPer+di]
			ret
			endp
			
		proc JmpEB
			AddIP 1
			KoPabaiga [MnemVPer+0A8h]
			ret
			endp
			
		proc OpkFEFF
			Skaityk 1
			AdresacijosBitai 0
			cmp reg, 3
			je ReikFar
			cmp reg, 5
			je ReikFar
			call PtrKopijavimas
			jmp NereikFar
			ReikFar:
			Kopijuok [abuptr+18], [dBuf+1], 4                 ;far'am netikrinu mod
			NereikFar:
			RaskRm
			call MnemPagalRegDi
			KoPabaiga [MnemFx+di]
			ret
			endp
			
		proc ClStF8_FD
			ParuoskDiMnem 013h
			KoPabaiga [MnemFx+di]
			ret
			endp
			
		proc AritmF6F7
			skaityk 1
			AdresacijosBitai 0
			call PtrKopijavimas
			RaskRm
			cmp reg, 0
			jne NeTestDataNereik
			call Dataa
			NeTestDataNereik:
			call MnemPagalRegDi
			add di, 136d
			KoPabaiga [MnemFx+di]
			ret
			endp
			
		proc RepF2F3
			skaityk 1
			mov al, 5Ch                  ;?-88d
			add al, [skbuf+2]
			mov bl, 8
			mul bl
			mov di, ax
			Kopijuok [MnemEil+di], [dBuf+1], 8
			ParuoskDiMnem 17h
			KoPabaiga [MnemFx+di]
			ret
			endp
			
			
		proc PushPopSeg
			cmp [skBuf+1], 0fh
			jne NeDb0F
			call dbNeatpazinta
			ret
			NeDb0F:
			mov al, [skBuf+1]
			and al, 00011000b
			shr al, 3
			add al, al
			xor bh, bh
			mov bl, al
			kopijuok [sregbuf+bx], [dBuf+1], 2
			call TW
			cmp w, 0
			je PUSHas
			KoPabaiga [Mnem+16]
			ret
			PUSHas:
			KoPabaiga [Mnem+8]
			ret
			endp
		
		proc AddOr00_0308_0D
			call AritmetiniuAdresasMnemSW
			push bx
			mov al, [skBuf+1]
			and al, 00000100b
			cmp al, 0
			jne SuData
			skaityk 1
			AdresacijosBitai 0
			call RegRmIBuf
			jmp IPabaiga
			SuData:
			call TW
			mov reg, 0
			RaskReg
			call Dataa
			IPabaiga:
			pop di
			KoPabaiga [MnemSW+di]
			ret
			endp
			
		proc PushPopDecInc4x5x
			call TRegOpk
			mov w, 1
			RaskReg
			call AritmetiniuAdresasMnemSW
			sub bx, 64d
			mov di, bx
			KoPabaiga [Mnem4x5x+di]
			ret
			endp
			
		proc dbNeatpazinta
			HexToASCII 1
			KoPabaiga dbbuf
			ret
			endp
			
		proc EscD8_DF
			skaityk 1
			HexToASCII 2
			KoPabaiga [dbbuf+36]
			ret
			endp
			
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


end
	
