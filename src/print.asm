;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Printing utilities, mostly wrappers or reimplementations of the underlying
; TI-OS bcall() routines. Some TI-OS routines work only from assembly language
; programs in RAM, and do not work from flash apps. They need to be
; reimplemented here so that they reside in the same flash page as the string
; that they will handle.
;-----------------------------------------------------------------------------

; Description: Print char A the multiple times.
; Input:
;   - A: char to print
;   - B: number of times
; Destroys: A, BC, DE, HL
printARepeatB:
    ld c, a ; C = char to print (saved)
    ; Check for B == 0
    ld a, b
    or a
    ret z
printARepeatBLoop:
    ld a, c
    bcall(_VPutMap) ; destroys A, DE, HL, but not BC
    djnz printARepeatBLoop
    ret

;-----------------------------------------------------------------------------

; Description: Convenience wrapper around VPutSN() that works for zero-length
; strings. Also provides an API similar to PutPS() which takes a pointer to a
; Pascal string directly, instead of providing the length and (char*)
; separately.
; Input: HL: Pascal-string
; Output; CF=1 if characters overflowed screen
; Destroys: A, HL
vPutPS:
    push bc
    ld a, (hl) ; A = length of Pascal string
    or a
    ret z
    ld b, a ; B = num chars
    inc hl ; HL = pointer to start of chars
vPutPN:
    ; implement inlined version of VPutSN() which works with flash string
    ld a, (hl)
    inc hl
    bcall(_VPutMap)
    jr c, vPutPNEnd
    djnz vPutPN
vPutPNEnd:
    pop bc
    ret

;-----------------------------------------------------------------------------

; Description: Convenience wrapper around SStringLength() that works for
; zero-length strings.
; Input: HL: Pascal string, in RAM
; Output: A = B = number of pixels
; Destroys: all but HL
smallStringWidth:
    ld a, (hl)
    or a
    ld b, a
    ret z
    bcall(_SStringLength) ; A, B = stringWidth; HL preserved
    ret

;-----------------------------------------------------------------------------

; Description: Erase to end of line using small font. Same as bcall(_EraseEOL).
; Prints a quad space (4 pixels side), 24 times, for 96 pixels.
; Destroys: B
vEraseEOL:
    ld b, 24
vEraseEOLLoop:
    ld a, SFourSpaces
    bcall(_VPutMap)
    djnz vEraseEOLLoop
    ret

;-----------------------------------------------------------------------------

; Description: Inlined version of bcall(_VPutS) with additional features:
;
; - Works for strings in flash (VPutS only works with strings in RAM).
; - Interprets the `Senter` and `Lenter` characters to move the pen to the
; beginning of the next line.
; - Supports inlined escape characters (escapeLargeFont, escapeSmallFont) to
; change the font dynamically.
; - Automatically adjusts the line height to be 7px for small font and 8px for
; large font.
;
; See TI-83 Plus System Routine SDK docs for VPutS() for a reference
; implementation of this function.
;
; Input: HL: pointer to string using small font
; Ouptut:
;    - unlike VPutS(), the CF does *not* show if all of string was rendered
; Destroys: all
smallFontHeight equ 7
largeFontHeight equ 8
escapeLargeFont equ $FE ; pseudo-char to switch to large font
escapeSmallFont equ $FF ; pseudo-char to switch to small font
vPutS:
    ; assume using small font
    ld c, smallFontHeight ; C = current font height
    res fracDrawLFont, (IY + fontFlags) ; start with small font
vPutSLoop:
    ld a, (hl) ; A = current char
    inc hl
vPutSCheckSpecialChars:
    or a ; Check for NUL
    ret z
    cp a, Senter ; Check for Senter (same as Lenter)
    jr z, vPutSEnter
    cp a, escapeLargeFont ; check for large font
    jr z, vPutSLargeFont
    cp a, escapeSmallFont ; check for small font
    jr z, vPutSSmallFont
vPutSNormal:
    bcall(_VPutMap) ; preserves BC, HL
    jr vPutSLoop
vPutSLargeFont:
    ld c, largeFontHeight
    set fracDrawLFont, (IY + fontFlags) ; use large font
    jr vPutSLoop
vPutSSmallFont:
    ld c, smallFontHeight
    res fracDrawLFont, (IY + fontFlags) ; use small font
    jr vPutSLoop
vPutSEnter:
    ; move to the next line
    push af
    push hl
    ld hl, PenCol
    xor a
    ld (hl), a ; PenCol = 0
    inc hl ; PenRow
    ld a, (hl) ; A = PenRow
    add a, c ; A += C (font height)
    ld (hl), a ; PenRow += 7
    pop hl
    pop af
    jr vPutSLoop

;-----------------------------------------------------------------------------

; Description: Inlined version of bcall(_PutS) which works for flash
; applications. See TI-83 Plus System Routine SDK docs for PutS() for a
; reference implementation. (I *think* that the _PutC() OS function interprets
; the `Lenter` character as a "newline" and moves the cursor to the next line.
; Need to verify.)
;
; Input: HL: pointer to C-string
; Output:
;   - CF=1 if the entire string was displayed, CF=0 if not
;   - curRow and curCol updated to the position after the last character
; Destroys: HL
putS:
    push bc
    push af
    ld a, (winBtm)
    ld b, a ; B = bottom line of window
putSLoop:
    ld a, (hl)
    inc hl
    or a ; test for end of string
    scf ; CF=1 if entire string displayed
    jr z, putSEnd
    bcall(_PutC)
    ld a, (curRow)
    cp b ; if A >= bottom line: CF=1
    jr c, putSLoop ; repeat if not at bottom
putSEnd:
    pop bc ; restore A (but not F)
    ld a, b
    pop bc
    ret

;-----------------------------------------------------------------------------

; Description: Inlined version of bcall(_PutPS) which works for Pascal strings
; in flash memory.
;
; Input: HL: pointer to Pascal string
; Destroys: A, B, C, HL
; Preserves: DE
putPS:
    ld a, (hl) ; A = length of Pascal string
    inc hl
    or a
    ret z
    ld b, a ; B = length of Pascal string (missing from SDK reference impl)
    ld a, (winBtm)
    ld c, a ; C = bottomRow (usually 8)
putPSLoop:
    ld a, (hl)
    inc hl
    bcall(_PutC)
    ; Check if next character is off-screen
    ld a, (curRow)
    cp c ; if currow == buttomRow: ZF=1
    ret z
    djnz putPSLoop
    ret
