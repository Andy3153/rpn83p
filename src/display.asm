;-----------------------------------------------------------------------------
; Display the RPN stack variables.
;
;   0: Status line: (up|down) (deg|rad) (fix|sci|eng) # small font
;   1: Debug line
;   2: Error code line: # small font
;   3: T: tttt # label small font, number large font
;   4: Z: zzzz
;   5: Y: yyyy
;   6: X: xxxx
;   7: [menu1][menu2][menu3][menu4][menu5] # small font
;-----------------------------------------------------------------------------

; Menu pixel columns:
;   - 96 px wide, 5 menus
;   - 18 px/menu = 90 px, 6 leftover
;   - 1 px between 5 menus, plus 1 px on far left and right = 6 px
;   - each menu spaced 19 px apart
;   - menuPenColStart - 0
;   - menuPenCol0 - 1
;   - menuPenCol1 - 20
;   - menuPenCol2 - 39
;   - menuPenCol3 - 58
;   - menuPenCol4 - 77
;   - menuPenColEnd - 96
menuCurRow      equ 7 ; row 7
menuPenRow      equ menuCurRow*8+1 ; row 7, in px, +1 for small font
menuPenWidth    equ 18 ; excludes 1 px space between menu items
menuPenColStart equ 0 ; left edge of menu line
menuPenCol0     equ 1 ; left edge of menu0
menuPenCol1     equ 20 ; left edge of menu1
menuPenCol2     equ 39 ; left edge of menu2
menuPenCol3     equ 58 ; left edge of menu3
menuPenCol4     equ 77 ; left edge of menu4
menuPenColEnd   equ 96

;-----------------------------------------------------------------------------

; Function: Set the display flags to dirty initially so that they are rendered.
initDisplay:
    set rpnFlagsFloatModeDirty, (iy + rpnFlags)
    set rpnFlagsTrigModeDirty, (iy + rpnFlags)
    set rpnFlagsBaseModeDirty, (iy + rpnFlags)
    set inputBufFlagsInputDirty, (iy + inputBufFlags)
    ret

; Function: Update the display, including the title, RPN stack variables,
; and the menu.
; Input: none
; Output:
; Destroys: all
displayAll:
    call displayStatus
    call displayErrorCode
    call displayStack
    call displayMenu

    ; Reset dirty flags
    res rpnFlagsStackDirty, (iy + rpnFlags)
    res rpnFlagsMenuDirty, (iy + rpnFlags)
    res rpnFlagsFloatModeDirty, (iy + rpnFlags)
    res rpnFlagsTrigModeDirty, (iy + rpnFlags)
    res rpnFlagsBaseModeDirty, (iy + rpnFlags)
    res inputBufFlagsInputDirty, (iy + inputBufFlags)
    ret

;-----------------------------------------------------------------------------
; Routines for displaying the status bar at the top.
;-----------------------------------------------------------------------------

; Function: Display the status bar, showing menu up/down arrows.
; Input: none
; Output: status line displayed
; Destroys: A, B, C, HL
displayStatus:
    call displayStatusMenu
    call displayStatusFloatMode
    call displayStatusTrig
    call displayStatusBase
    ret

; Description: Display the up and down arrows that indicate whether there are
; additional menus above or below the current set of 5 menu buttons.
; TODO: Maybe rename this "displayStatusArrows".
displayStatusMenu:
    bit rpnFlagsMenuDirty, (iy + rpnFlags)
    ret z

    ; TODO: maybe cache the numStrips of the current node to make this
    ; calculation a little shorter and easier.

    ; Determine if multiple menu strips exists.
    ld hl, menuGroupId
    ld a, (hl) ; A=menuGroupId
    inc hl
    ld b, (hl) ; B=menuStripIndex
    call getMenuNode ; HL = pointer to MenuNode
    inc hl
    inc hl
    inc hl
    ld c, (hl) ; C=numStrips

    ld hl, statusPenRow*$100 + statusMenuPenCol; $(penRow)(penCol)
    ld (PenCol), hl

    ; If numStrips==0: don't do anything. This should never happen if there
    ; are no bugs in the program.
    ld a, c ; A = numStrips
    or a
    jr z, displayStatusMenuClear

displayStatusMenuDownArrow:
    ; If stripIndex < (numStrips - 1): show Down arrow
    ld a, b ; A = stripIndex
    dec c ; C = numStrips - 1
    cp c
    jr nc, displayStatusMenuDownArrowNone
    ld a, SdownArrow
    bcall(_VPutMap)
    ; Add an extra space after the downArrow because when an upArrow is
    ; displayed immediately after, the 1px of space on the right side of the
    ; downArrow character seems to ellided so the downArrow occupies only 3px.
    ld a, Sspace
    jr displayStatusMenuDownArrowDisplay
displayStatusMenuDownArrowNone:
    ld a, SFourSpaces
displayStatusMenuDownArrowDisplay:
    bcall(_VPutMap)

displayStatusMenuUpArrow:
    ; If stripIndex > 0: show Up arrow
    ld a, b
    or a
    jr z, displayStatusMenuUpArrowNone
    ld a, SupArrow
    jr displayStatusMenuUpArrowDisplay
displayStatusMenuUpArrowNone:
    ld a, SFourSpaces
displayStatusMenuUpArrowDisplay:
    bcall(_VPutMap)
    ret

    ; clear 8 px
displayStatusMenuClear:
    call displayStatusMenuUpArrowNone
    jr displayStatusMenuUpArrowNone

;-----------------------------------------------------------------------------

; Description: Display the Degree or Radian trig mode.
displayStatusTrig:
    bit rpnFlagsTrigModeDirty, (iy + rpnFlags)
    ret z
displayStatusTrigUpdate:
    ld hl, statusPenRow*$100 + statusTrigPenCol; $(penRow)(penCol)
    ld (PenCol), hl
    bit trigDeg, (iy + trigFlags)
    jr z, displayStatusTrigRad
displayStatusTrigDeg:
    ld hl, mDegName
    jr displayStatusTrigPutS
displayStatusTrigRad:
    ld hl, mRadName
displayStatusTrigPutS:
    bcall(_VPutS)
    ret

;-----------------------------------------------------------------------------

; Description: Display the Base: BIN, OCT, DEC, HEX.
displayStatusBase:
    bit rpnFlagsBaseModeDirty, (iy + rpnFlags)
    ret z
displayStatusBaseUpdate:
    ld hl, statusPenRow*$100 + statusBasePenCol; $(penRow)(penCol)
    ld (PenCol), hl
    ; Determine current base mode.
    ld a, (baseMode)
    cp 2
    jr z, displayStatusBaseBin
    cp 8
    jr z, displayStatusBaseOct
    cp 16
    jr z, displayStatusBaseHex
displayStatusBaseDec: ; Use Base 10 for anything else
    ld hl, mDecName
    jr displayStatusBasePutS
displayStatusBaseHex:
    ld hl, mHexName
    jr displayStatusBasePutS
displayStatusBaseOct:
    ld hl, mOctName
    jr displayStatusBasePutS
displayStatusBaseBin:
    ld hl, mBinName
displayStatusBasePutS:
    bcall(_VPutS)
    ret

;-----------------------------------------------------------------------------

; Description: Display the floating point format: FIX, SCI, ENG
; Destroys: A, HL
displayStatusFloatMode:
    bit rpnFlagsFloatModeDirty, (iy + rpnFlags)
    ret z
    ld hl, statusPenRow*$100 + statusFloatModePenCol; $(penRow)(penCol)
    ld (PenCol), hl
    ; check float mode
    bit fmtExponent, (iy + fmtFlags)
    jr nz, displayStatusFloatModeSciOrEng
displayStatusFloatModeFix:
    ld hl, mFixName
    jr displayStatusFloatModeBracketDigit
displayStatusFloatModeSciOrEng:
    bit fmtEng, (iy + fmtFlags)
    jr nz, displayStatusFloatModeEng
displayStatusFloatModeSci:
    ld hl, mSciName
    jr displayStatusFloatModeBracketDigit
displayStatusFloatModeEng:
    ld hl, mEngName
    ; [[fallthrough]]
displayStatusFloatModeBracketDigit:
    ; Print the number of digit
    bcall(_VPutS)
    ld a, SlParen
    bcall(_VPutMap)
    ld a, (fmtDigits)
    cp 10
    jr nc, displayStatusFloatModeFloating
    add a, '0'
    jr displayStatusFloatModeDigit
displayStatusFloatModeFloating:
    ld a, '-'
displayStatusFloatModeDigit:
    bcall(_VPutMap)
    ld a, SrParen
    bcall(_VPutMap)
    ret

;-----------------------------------------------------------------------------
; Routines for displaying the error code and string.
;-----------------------------------------------------------------------------

; Function: Display the string corresponding to the current error code.
; Input: errorCode, errorCodeDisplayed
; Output:
;   - string corresponding to errorCode displayed
;   - errorCodeDisplayed updated
; Destroys: A, DE, HL
displayErrorCode:
    call checkErrorCodeDisplayed
    ret z

    ; Print error code string and its numerical code.
    ld hl, errorPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    call getErrorString
    bcall(_VPutS)
    ld a, Sspace
    bcall(_VPutMap)
    ;
    ld a, ' '
    bcall(_VPutMap)
    ld a, '('
    bcall(_VPutMap)
    ;
    ld a, (errorCode)
    ld hl, OP1
    call convertAToDec
    bcall(_VPutS)
    ;
    ld a, ')'
    bcall(_VPutMap)
    call vEraseEOL

    call saveErrorCodeDisplayed
    ret

;-----------------------------------------------------------------------------
; Routines for displaying the RPN stack variables.
;-----------------------------------------------------------------------------

; Function: Display the RPN stack variables
; Input: none
; Output: (rpnFlagsMenuDirty) reset
; Destroys: A, HL
displayStack:
    ; display YZT if stack is dirty
    bit rpnFlagsStackDirty, (iy + rpnFlags)
    call nz, displayStackYZT

    ; display X if stack or inputBuf are dirty
    bit rpnFlagsStackDirty, (iy + rpnFlags)
    jr nz, displayStackContinue
    bit inputBufFlagsInputDirty, (iy + inputBufFlags)
    jr nz, displayStackContinue
    ret
displayStackContinue:
    call displayStackX
    ret

;-----------------------------------------------------------------------------

displayStackYZT:
    ; print T label
    ld hl, stTPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    ld hl, msgTLabel
    bcall(_VPutS)

    ; print T value
    ld hl, stTCurCol*$100 + stTCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    call rclT
    call printOP1

    ; print Z label
    ld hl, stZPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    ld hl, msgZLabel
    bcall(_VPutS)

    ; print Z value
    ld hl, stZCurCol*$100 + stZCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    call rclZ
    call printOP1

    ; print Y label
    ld hl, stYPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    ld hl, msgYLabel
    bcall(_VPutS)

    ; print Y value
    ld hl, stYCurCol*$100 + stYCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    call rclY
    call printOP1

    ret

;-----------------------------------------------------------------------------

; Description: Render the X lines. There are 3 options:
; 1) if rpnFlagsArgMode, print the inputbuf as a command argument,
; 2) else if rpnFlagsEditing, print the inputBuf as a stack number,
; 3) else print the stX variable.
displayStackX:
    bit rpnFlagsArgMode, (iy + rpnFlags)
    jr nz, displayStackXArg
    bit rpnFlagsEditing, (iy + rpnFlags)
    jr nz, displayStackXInput
    ; [[fallthrough]]

displayStackXNormal:
    ; print X label
    ld hl, stXPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    ld hl, msgXLabel
    bcall(_VPutS)
    ; print the stX variable
    ld hl, stXCurCol*$100 + stXCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    call rclX
    jp printOP1

displayStackXInput:
    ; print X label
    ld hl, inputPenRow*$100 ; $(penRow)(penCol)
    ld (PenCol), hl
    ld hl, msgXLabel
    bcall(_VPutS)
    ; print the inputBuf
    ld hl, inputCurCol*$100 + inputCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    jr printInputBuf

#ifdef DEBUG
; This is the debug version which always shows the current X register, and
; prints the inputBuf on the debug line.
displayStackXDebug:
    ld hl, $0100 + stXCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    call rclX
    call printOP1
    ; print the inputBuf on the error line
    jp debugInputBuf
#endif

; Display the argBuf in the X register line.
displayStackXArg:
    ld hl, argCurCol*$100 + argCurRow ; $(curCol)(curRow)
    ld (CurRow), hl
    jr printArgBuf

;-----------------------------------------------------------------------------

; Function: Print the arg buffer.
; Input:
;   - argBuf (same as inputBuf)
;   - argBufPrompt
;   - (CurCol) cursor position
; Output:
;   - (CurCol) is updated
; Destroys: A, HL; BC destroyed by PutPS()
printArgBuf:
    ; Print prompt and contents of argBuf
    ld hl, (argPrompt)
    bcall(_PutS)
    ld a, ' '
    bcall(_PutC)
    ld hl, argBuf
    bcall(_PutPS)

    ; Append cursor if needed.
    ld a, (argBufSize)
    or a
    jr z, printArgBufTwoCursors
    cp 1
    jr z, printArgBufOneCursor
    jr printArgBufZeroCursor
printArgBufTwoCursors:
    ld a, cursorChar
    bcall(_PutC)
printArgBufOneCursor:
    ld a, cursorChar
    bcall(_PutC)
printArgBufZeroCursor:
    bcall(_EraseEOL)
    ret

;-----------------------------------------------------------------------------

; Function: Print the input buffer.
; Input:
;   - inputBuf
;   - (CurCol) cursor position
; Output:
;   - (CurCol) is updated
; Destroys: A, HL; BC destroyed by PutPS()
printInputBuf:
    ld hl, inputBuf
    bcall(_PutPS)
    ld a, cursorChar
    bcall(_PutC)
    ; Skip EraseEOL() if the PutC() above wrapped to next line
    ld a, (CurCol)
    or a
    ret z
    bcall(_EraseEOL)
    ret

;-----------------------------------------------------------------------------
; Routines for displaying the menu bar.
;-----------------------------------------------------------------------------

; Function: Display the bottom menus.
; Input: none
; Output: (rpnFlagsMenuDirty) reset
; Destroys: A, HL
displayMenu:
    bit rpnFlagsMenuDirty, (iy + rpnFlags)
    ret z

    ; get starting menuId
    call getCurrentMenuStripBeginId ; A=stripBeginId
    ; set up loop over 5 consecutive menu buttons
    ld d, a ; D = menuId
    ld e, 0 ; E = menuIndex [0,4]
    ld c, menuPenCol0 ; C = penCol
    ld b, 5 ; B = loop 5 times
displayMenuLoop:
    ld a, d ; A = menuId
    call getMenuName ; HL = menu name of menuId in A

    ld a, c ; A = penCol
    call printMenuAtA

    inc d ; D = menuId + 1
    inc e ; E =  menuIndex + 1

    ld a, c ; A = penCol
    add a, menuPenWidth + 1 ; A += menuWidth + 1 (1px spacing)
    ld c, a ; C += menuPenWidth + 1

    djnz displayMenuLoop
    ret

;-----------------------------------------------------------------------------

; Description: Print the menu C-string in HL to the menuPenCol in A, using the
; small and inverted font, centering the menu name in the middle of the 18 px
; width of a menu box.
; Inputs:
;   A: penCol
;   B: loop counter (must be preserved)
;   C: penCol (must be preserved)
;   D: menuId (ignored but must be preserved, useful for debugging)
;   E: menuIndex [0-4] (ignored but must be preserved, useful for debugging)
;   HL: C string
; Destroys: A, HL
; Preserves: BC, DE
printMenuAtA:
    push bc ; B = loop counter
    push de ; D = menuId; E = menuIndex

    ; Set (PenCol,PenRow), preserving HL
    ld (PenCol), a
    ld a, menuPenRow
    ld (PenRow), a

    ; Predict the width of menu name.
    ld de, menuName
    ld c, menuNameBufMax
    call copyCToPascal ; C, DE are preserved
    ex de, hl ; HL = menuName
    call sStringWidth ; A = B = string width

    ; Calculate the starting pixel to center the string
    ld a, menuPenWidth
    sub b ; A = menuPenWidth - stringWidth
    jr nc, printMenuAtAFitsInside
printMenuAtATooWide:
    xor a ; if string too wide (shouldn't happen), set to 0
printMenuAtAFitsInside:
    rra ; CF=0, divide by 2 for centering; A = padWidth

    ld c, a ; C = A = leftPadWidth
    push bc ; B = stringWidth; C = leftPadWidth
    set textInverse, (iy + textFlags)
printMenuAtALeftPad:
    ld b, a ; B = leftPadWidth
    ld a, Sspace
    call printARepeatB
printMenuAtAPrintName:
    ; Print the menu name
    ld hl, menuName
    call vPutPS
printMenuAtARightPad:
    pop bc ; B = stringWidth; C = leftPadWidth
    ld a, menuPenWidth
    sub c ; A = menuPenWidth - leftPadWidth
    sub b ; A = rightPadWidth = menuPenWidth - leftPadWidth - stringWidth
    jr z, printMenuAtAExit ; no space left
    jr c, printMenuAtAExit ; overflowed, shouldn't happen but be safe
    ; actually print the right pad
    ld b, a ; B = rightPadWidth
    ld a, Sspace
    call printARepeatB

printMenuAtAExit:
    res textInverse, (iy + textFlags)
    pop de ; D = menuId; E = menuIndex
    pop bc ; B = loop counter
    ret

;-----------------------------------------------------------------------------
; Low-level helper routines.
;-----------------------------------------------------------------------------

; Function: Print floating point number at OP1 at the current cursor. Erase to
; the end of line (but only if the floating point did not spill over to the
; next line).
; Input: OP1: floating point number
; Destroys: A, HL, OP3
printOP1:
    ld a, (baseMode)
    cp a, 16
    jr z, printOP1Base16
    cp a, 8
    jr z, printOP1Base8
    cp a, 2
    jp z, printOP1Base2
    ; [[fallthrough]]

;-----------------------------------------------------------------------------

; Function: Print floating point number at OP1 using base 10.
; Input: OP1: floating point number
; Destroys: A, HL, OP3
printOP1Base10:
    ld a, 15 ; width of output
    bcall(_FormReal)
    ld hl, OP3
    ; [[fallthrough]]

; Description: Print the C-string referenced by HL, and erase to the end of
; line, taking care that the cursor did not move to the next line
; automatically.
; Input: HL: pointer to C-string
; Output:
; Destroys: A, HL
printHLString:
    bcall(_PutS)
    ld a, (CurCol)
    or a
    ret z ; if spilled to next line, don't call EraseEOL
    bcall(_EraseEOL)
    ret

; Description: Print an indicator ("...") that the OP1 number cannot be
; rendered in the current base mode (hex, oct, or bin).
printOP1BaseInvalid:
    ld hl, msgInvalidBase
    jr printHLString

;-----------------------------------------------------------------------------

; Function: Print ingeger at OP1 at the current cursor in base 16. Erase to
; the end of line (but only if the floating point did not spill over to the
; next line).
; TODO: I think printOP1Base16(), printOP1Base8(), and printOP1Base2() can be
; combined into a single subroutine, saving memory.
; Destroys: all, OP1, OP2, OP3, OP4
printOP1Base16:
    call op2Set2Pow32 ; OP2 = 2^32
    bcall(_CpOP1OP2) ; if OP1 >= 2^32: CF=0
    jr nc, printOP1BaseInvalid

    bcall(_CkOP1FP0) ; if OP1 == 0: ZF=1
    jr z, printOP1Base16Valid

    bcall(_CkOP1Pos) ; if OP1 > 0: ZF=1
    jr nz, printOP1BaseInvalid

printOP1Base16Valid:
    bcall(_PushRealO1) ; FPS = OP1 (save)
    bcall(_Trunc) ; OP1 = trunc(OP1)
printOP1Base16String:
    call convertOP1ToU32OP3
    ld hl, OP3
    ld de, OP4
    call convertU32ToHexString

    ; Check if OP1 was a pure integer
    push de ; DE = hex string
    bcall(_PopRealO1) ; OP1 = original OP1
    bcall(_Frac) ; OP1 = frac(OP1)
    bcall(_CkOP1FP0) ; if frac(OP1) == 0: ZF=1
    pop hl ; HL = hex string
    jr z, printHLString
    ld a, '.'
    call appendCString
    jr printHLString

;-----------------------------------------------------------------------------

; Function: Print ingeger at OP1 at the current cursor in base 8. Erase to
; the end of line (but only if the floating point did not spill over to the
; next line).
; Destroys: all, OP1, OP2, OP3, OP4, OP5
printOP1Base8:
    call op2Set2Pow32 ; OP2 = 2^32
    bcall(_CpOP1OP2) ; if OP1 >= 2^32: CF=0
    jr nc, printOP1BaseInvalid

    bcall(_CkOP1FP0) ; if OP1 == 0: ZF=1
    jr z, printOP1Base8Valid

    bcall(_CkOP1Pos) ; if OP1 > 0: ZF=1
    jr nz, printOP1BaseInvalid

printOP1Base8Valid:
    bcall(_PushRealO1) ; FPS = OP1 (save)
    bcall(_Trunc) ; OP1 = trunc(OP1)
printOP1Base8String:
    call convertOP1ToU32OP3
    ld hl, OP3
    ld de, OP4
    call convertU32ToOctString

    ; Check if OP1 was a pure integer
    push de ; DE = rendered string
    bcall(_PopRealO1) ; OP1 = original OP1
    bcall(_Frac) ; OP1 = frac(OP1)
    bcall(_CkOP1FP0) ; if frac(OP1) == 0: ZF=1
    pop hl ; HL = rendered string
    jr z, printHLString
    ld a, '.'
    call appendCString
    jp printHLString

;-----------------------------------------------------------------------------

; Description: Print ingeger at OP1 at the current cursor in base 2. Erase to
; the end of line (but only if the floating point did not spill over to the
; next line). A single line can display a maximum of 15 digits, but we need
; space for a trailing ".", so the maximum number of binary digits is 14, which
; means that we can display numbers which are < 2^14.
; Input: OP1: non-negative floating point number < 2^14
; Destroys: all, OP1, OP2, OP3, OP4, OP5
printOP1Base2:
    call op2Set2Pow14 ; OP2 = 2^14
    bcall(_CpOP1OP2) ; if OP1 >= 2^14: CF=0
    jp nc, printOP1BaseInvalid

    bcall(_CkOP1FP0) ; if OP1 == 0: ZF=1
    jr z, printOP1Base2Valid

    bcall(_CkOP1Pos) ; if OP1 > 0: ZF=1
    jp nz, printOP1BaseInvalid

printOP1Base2Valid:
    bcall(_PushRealO1) ; FPS = OP1 (save)
    bcall(_Trunc) ; OP1 = trunc(OP1)
printOP1Base2String:
    call convertOP1ToU32OP3
    ld hl, OP3
    ld de, OP4
    call convertU32ToBinString

    ; Check if OP1 was a pure integer
    push de ; DE = rendered string
    bcall(_PopRealO1) ; OP1 = original OP1
    bcall(_Frac) ; OP1 = frac(OP1)
    bcall(_CkOP1FP0) ; if frac(OP1) == 0: ZF=1
    pop hl ; HL = rendered string
    jp z, printHLString
    ld a, '.'
    call appendCString
    jp printHLString

;-----------------------------------------------------------------------------

; Function: Convert A to a NUL-terminated C-string of 1 to 3 digits at the
; buffer pointed by HL. This is intended for debugging, so it is not optimized.
; TODO: This can probably be written to be faster and smaller.
; Input: HL: pointer to string buffer
; Output: HL unchanged, with 1-3 ASCII string, terminated by NUL
; Destroys: A, B, C
convertAToDec:
    push hl
convertAToDec100:
    ld b, 100
    call divideAByB
    or a
    jr z, convertAToDec10
    call convertAToChar
    ld (hl), a
    inc hl
convertAToDec10:
    ld a, b
    ld b, 10
    call divideAByB
    or a
    jr z, convertAToDec1
    call convertAToChar
    ld (hl), a
    inc hl
convertAToDec1:
    ld a, b
    call convertAToChar
    ld (hl), a
    inc hl
convertAToDec0:
    ld (hl), 0
    pop hl
    ret

; Function: Return A / B using repeated substraction.
; Input:
;   - A: numerator
;   - B: denominator
; Output: A = A/B (quotient); B=A%B (remainder)
; Destroys: C
divideAByB:
    ld c, 0
divideAByBLoop:
    sub b
    jr c, divideAByBLoopEnd
    inc c
    jr divideAByBLoop
divideAByBLoopEnd:
    add a, b ; undo the last subtraction
    ld b, a
    ld a, c
    ret

; Function: Convert A into an Ascii Char ('0'-'9','A'-'F').
; Destroys: A
convertAToChar:
    cp 10
    jr c, convertAToCharDec
    sub 10
    add a, 'A'
    ret
convertAToCharDec:
    add a, '0'
    ret

;-----------------------------------------------------------------------------

; Indicates number cannot be rendered in the current Base mode.
msgInvalidBase:
    .db "...", 0

; RPN stack variable labels
msgTLabel:
    .db "T:", 0
msgZLabel:
    .db "Z:", 0
msgYLabel:
    .db "Y:", 0
msgXLabel:
    .db "X:", 0
