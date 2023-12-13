;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Main input key code handlers.
;-----------------------------------------------------------------------------

; Description: Append a number character to inputBuf or argBuf, updating various
; flags.
; Input:
;   A: character to be appended
;   rpnFlagsEditing: whether we are already in Edit mode
; Output:
;   - CF set when append fails
;   - rpnFlagsEditing set
;   - dirtyFlagsInput set
; Destroys: all
handleKeyNumber:
    ; Any digit entry should cause TVM menus to go into input mode.
    res rpnFlagsTvmCalculate, (iy + rpnFlags)
    ; If not in input editing mode: lift stack and go into edit mode
    bit rpnFlagsEditing, (iy + rpnFlags)
    jr nz, handleKeyNumberCheckAppend
handleKeyNumberFirstDigit:
    ; Lift the stack, unless disabled.
    push af ; preserve A=char to append
    call liftStackIfEnabled
    pop af
    ; Go into editing mode.
    bcall(_ClearInputBuf)
    set rpnFlagsEditing, (iy + rpnFlags)
handleKeyNumberCheckAppend:
    ; Limit number of exponent digits to 2.
    bit inputBufFlagsEE, (iy + inputBufFlags)
    jr nz, handleKeyNumberAppendExponent
    ; Append A to mantissa.
    bcall(_AppendInputBuf)
    ret
handleKeyNumberAppendExponent:
    ; Append A to exponent.
    ld b, a
    ld a, (inputBufEELen)
    cp inputBufEELenMax
    ld a, b
    ret nc ; prevent more than 2 exponent digits
    ; Try to append. Check for buffer full before incrementing counter.
    bcall(_AppendInputBuf)
    ret c ; return if buffer full
    ld hl, inputBufEELen
    inc (hl)
    ret

;-----------------------------------------------------------------------------

; Description: Append '0' to inputBuf.
handleKey0:
    ld a, '0'
    jr handleKeyNumber

; Description: Append '1' to inputBuf.
handleKey1:
    ld a, '1'
    jr handleKeyNumber

; Description: Append '2' to inputBuf.
handleKey2:
    call checkBase8Or10Or16
    ret nz
    ld a, '2'
    jr handleKeyNumber

; Description: Append '3' to inputBuf.
handleKey3:
    call checkBase8Or10Or16
    ret nz
    ld a, '3'
    jr handleKeyNumber

; Description: Append '4' to inputBuf.
handleKey4:
    call checkBase8Or10Or16
    ret nz
    ld a, '4'
    jr handleKeyNumber

; Description: Append '5' to inputBuf.
handleKey5:
    call checkBase8Or10Or16
    ret nz
    ld a, '5'
    jr handleKeyNumber

; Description: Append '6' to inputBuf.
handleKey6:
    call checkBase8Or10Or16
    ret nz
    ld a, '6'
    jr handleKeyNumber

; Description: Append '7' to inputBuf.
handleKey7:
    call checkBase8Or10Or16
    ret nz
    ld a, '7'
    jr handleKeyNumber

; Description: Append '8' to inputBuf.
handleKey8:
    call checkBase10Or16
    ret nz
    ld a, '8'
    jr handleKeyNumber

; Description: Append '9' to inputBuf.
handleKey9:
    call checkBase10Or16
    ret nz
    ld a, '9'
    jp handleKeyNumber

; Description: Append 'A' to inputBuf.
handleKeyA:
    call checkBase16
    ret nz
    ld a, 'A'
    jp handleKeyNumber

; Description: Append 'B' to inputBuf.
handleKeyB:
    call checkBase16
    ret nz
    ld a, 'B'
    jp handleKeyNumber

; Description: Append 'C' to inputBuf.
handleKeyC:
    call checkBase16
    ret nz
    ld a, 'C'
    jp handleKeyNumber

; Description: Append 'D' to inputBuf.
handleKeyD:
    call checkBase16
    ret nz
    ld a, 'D'
    jp handleKeyNumber

; Description: Append 'E' to inputBuf.
handleKeyE:
    call checkBase16
    ret nz
    ld a, 'E'
    jp handleKeyNumber

; Description: Append 'F' to inputBuf.
handleKeyF:
    call checkBase16
    ret nz
    ld a, 'F'
    jp handleKeyNumber

; Description: Return ZF=1 if baseNumber is float, 8, 10, or 16.
checkBase8Or10Or16:
    ld a, (baseNumber)
    cp 8
    ret z
    cp 10
    ret z
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret z
    cp 16
    ret

; Description: Return ZF=1 if baseNumber is float, 10, or 16.
checkBase10Or16:
    ld a, (baseNumber)
    cp 10
    ret z
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret z
    cp 16
    ret

; Description: Return ZF=1 if baseNumber is (not float and base 16).
checkBase16:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret z
    ld a, (baseNumber)
    cp 16
    ret

; Description: Append a '.' if not already entered.
; Input: none
; Output: (iy+inputBufFlags) DecPnt set
; Destroys: A, DE, HL
handleKeyDecPnt:
    ; Do nothing in BASE mode.
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret nz
    ; Do nothing if a decimal point already exists.
    bit inputBufFlagsDecPnt, (iy + inputBufFlags)
    ret nz
    ; Also do nothing if 'E' exists. Exponents cannot have a decimal point.
    bit inputBufFlagsEE, (iy + inputBufFlags)
    ret nz
    ; try insert '.'
    ld a, '.'
    call handleKeyNumber
    ret c ; If CF: append failed so return without setting the DecPnt flag
    set inputBufFlagsDecPnt, (iy + inputBufFlags)
    ret

; Description: Handle the EE for scientific notation. The 'EE' is mapped to
; 2ND-COMMA by default on the calculator. For faster entry, we map the COMMA
; key (withouth 2ND) to be EE as well.
; Input: none
; Output: (inputBufEEPos), (inputBufFlagsEE, iy+inputBufFlags)
; Destroys: A, HL
handleKeyEE:
    ; Do nothing in BASE mode.
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret nz
    ; do nothing if EE already exists
    bit inputBufFlagsEE, (iy + inputBufFlags)
    ret nz
    ; try insert 'E'
    ld a, Lexponent
    call handleKeyNumber
    ret c ; If CF: append failed so return without setting the EE flag
    ; save the EE+1 position
    ld a, (inputBuf) ; position after the 'E'
    ld (inputBufEEPos), a
    ; set the EE Len to 0
    xor a
    ld (inputBufEELen), a
    ; set flag to indicate presence of EE
    set inputBufFlagsEE, (iy + inputBufFlags)
    ret

;-----------------------------------------------------------------------------

; Description: Implement the DEL functionality, which does slightly different
; things depending on the context:
;   - If not in edit mode, clear the inputBuf and go into edit mode.
;   - If the deleted char was a '.', reset the decimal point flag.
;   - If the deleted char was a '-', reset the negative flag.
; Input: none
; Output: (iy+inputBufFlags) updated
; Destroys: A, DE, HL
handleKeyDel:
    ; Clear TVM Calculate mode.
    res rpnFlagsTvmCalculate, (iy + rpnFlags)
    ; Check if non-zero error code is currently displayed. The handlerCode was
    ; already set to 0 before this was called, so simply returning will clear
    ; the previous errorCode.
    ld a, (errorCode)
    or a
    ret nz
    ; If not in edit mode, go into edit mode, clear the inputBuf, and just
    ; return because there is nothing to do with an empty inputBuf.
    ld hl, inputBuf
    bit rpnFlagsEditing, (iy + rpnFlags)
    jr nz, handleKeyDelInEditMode
    set rpnFlagsEditing, (iy + rpnFlags)
    ld (hl), 0 ; clear the inputBuf
    set dirtyFlagsInput, (iy + dirtyFlags)
    ret
handleKeyDelInEditMode:
    ; DEL pressed in edit mode.
    set dirtyFlagsInput, (iy + dirtyFlags)
    ld a, (hl) ; A = inputBufLen
    or a
    ret z ; do nothing if buffer empty
    ; shorten string by one
    ld e, a ; E = inputBufLen
    dec a
    ld (hl), a
    ; retrieve the character deleted
    ld d, 0
    add hl, de
    ld a, (hl)
handleKeyDelDecPnt:
    ; reset decimal point flag if the deleted character was a '.'
    cp a, '.'
    jr nz, handleKeyDelEE
    res inputBufFlagsDecPnt, (iy + inputBufFlags)
    ret
handleKeyDelEE:
    ; reset EE flag if the deleted character was an 'E'
    cp Lexponent
    jr nz, handleKeyDelEEDigits
    xor a
    ld (inputBufEEPos), a
    ld (inputBufEELen), a
    res inputBufFlagsEE, (iy + inputBufFlags)
    ret
handleKeyDelEEDigits:
    ; decrement exponent len counter
    bit inputBufFlagsEE, (iy + inputBufFlags)
    jr z, handleKeyDelExit
    cp signChar
    jr z, handleKeyDelExit ; no special handling of '-' in exponent
    ; check if EELen is 0
    ld hl, inputBufEELen
    ld a, (hl)
    or a
    jr z, handleKeyDelExit ; don't decrement len below 0
    ; decrement EELen
    dec a
    ld (hl), a
handleKeyDelExit:
    ret

;-----------------------------------------------------------------------------

; Description: Clear the X register and go into edit mode. If already in edit
; mode, clear the inputBuf. If the CLEAR is pressed when the input buffer is
; already empty, then go into ClearAgain mode. If CLEAR is pressed in
; ClearAgain mode, the RPN stack is cleared (like the CLST command).
; Input:
;   - errorCode
;   - inputBuf
;   - rpnFlagsEditing
; Output:
;   - inputBuf cleared
;   - editing mode set
;   - stack lift disabled
;   - handlerCode set to `errorCodeOk` or `errorCodeClearAgain`
; Destroys: A, HL
handleKeyClear:
    ; Clear TVM Calculate mode.
    res rpnFlagsTvmCalculate, (iy + rpnFlags)
    ; Check if an error code is currently displayed.
    ld a, (errorCode)
    or a
    jr z, handleKeyClearNormal
    ; We are here if a non-zero errorCode from the previous handler is
    ; displayed. Previously, we simply returned if that was true, causing the
    ; CLEAR button to clear the previous error code. But we now support hitting
    ; CLEAR twice to invoke CLST, so we should return for all error codes
    ; except errorCodeClearAgain.
    cp errorCodeClearAgain
    ret nz
    ; We are here if the last errorCode was errorCodeClearAgain. CLEAR was
    ; pressed again, but before going ahead and clearing the RPN stack, let's
    ; check that the inputBuf is still empty. I am not actually sure if it is
    ; possible to have an errorCodeClearAgain and also have a non-empty
    ; inputBuf, but if that's the case, simply return without doing anything.
    ld a, (inputBuf)
    or a
    ret nz ; not sure if inputBuf can ever be non-empty, but just ret if so
    res rpnFlagsEditing, (iy + rpnFlags)
    jp clearStack
handleKeyClearNormal:
    ; We are here if CLEAR was pressed when there are no other error conditions
    ; previously. If we are already in edit mode, then we clear the inputBuf.
    ; If not in edit mode, then we "clear the X register" by going into edit
    ; mode with an empty inputBuf.
    bit rpnFlagsEditing, (iy + rpnFlags)
    jr z, handleKeyClearToEmptyInput
    ; We are here if clearing the inputBuf.
    ld a, (inputBuf)
    or a
    ; If the inputBuf has stuff, then clear the inputBuf.
    jr nz, handleKeyClearToEmptyInput
handleKeyClearWhileClear:
    ; We are here if CLEAR was pressed while the inputBuffer was already empty.
    ; Go into ClearAgain mode, where the next CLEAR invokes CLST.
    ld a, errorCodeClearAgain
    ld (handlerCode), a
    ret
handleKeyClearToEmptyInput:
    ; We are here if we were not in edit mode, so CLEAR should "clear the X
    ; register" by going into edit mode with an emtpy inputBuf.
    bcall(_ClearInputBuf)
    set rpnFlagsEditing, (iy + rpnFlags)
    ; We also disable stack lift. Testing seems to show that this is not seem
    ; strictly necessary because handleNumber() handles the edit mode properly
    ; even if the stack lift is enabled. But I think it is safer to disable it
    ; in case handleKeyNumber() is refactored in the future to use a different
    ; algorithm.
    res rpnFlagsLiftEnabled, (iy + rpnFlags)
    ret

;-----------------------------------------------------------------------------

; Description: Handle (-) change sign. If in edit mode, change the sign in the
; inputBuf. Otherwise, change the sign of the X register. If the EE symbol
; exists, change the sign of the exponent instead of the mantissa.
; Input: none
; Output: (inputBuf), X
; Destroys: all, OP1
handleKeyChs:
    ; Do nothing in BASE mode. Use NEG function instead.
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    ret nz
    ; Clear TVM mode.
    res rpnFlagsTvmCalculate, (iy + rpnFlags)
    ; Toggle sign character in inputBuf in edit mode.
    bit rpnFlagsEditing, (iy + rpnFlags)
    jr nz, handleKeyChsInputBuf
handleKeyChsX:
    ; CHS of X register
    call rclX
    bcall(_InvOP1S)
    call stoX
    ret
handleKeyChsInputBuf:
    set dirtyFlagsInput, (iy + dirtyFlags)
    ; Change sign of Mantissa or Exponent.
    ld hl, inputBuf
    ld b, inputBufCapacity
    ld a, (inputBufEEPos) ; offset to EE digit, or 0 if 'E' does not exist
    ; [[fallthrough]]

; Description: Add or remove the '-' char at position A of the Pascal string at
; HL, with maximum length B.
; Input:
;   A: inputBuf offset where the sign ought to be
;   HL: pointer to Pascal string
;   B: max size of Pasal string
; Output:
;   (HL): updated with '-' removed or added
;   CF:
;       - Set if positive (including if '-' could not be added due to size)
;       - Clear if negative
; Destroys:
;   A, BC, DE, HL
flipInputBufSign:
    ld c, (hl) ; size of string
    cp c
    jr c, flipInputBufSignInside ; If A < inputBufLen: interior position
    ld a, c ; set A = inputBufLen, just in case
    jr flipInputBufSignAdd
flipInputBufSignInside:
    ; Check for the '-' and flip it.
    push hl
    inc hl ; skip size byte
    ld e, a
    ld d, 0
    add hl, de
    ld a, (hl)
    cp signChar
    pop hl
    ld a, e ; A=sign position
    jr nz, flipInputBufSignAdd
flipInputBufSignRemove:
    ; Remove existing '-' sign
    bcall(_DeleteAtPos)
    scf ; set CF to indicate positive
    ret
flipInputBufSignAdd:
    ; Add '-' sign.
    bcall(_InsertAtPos)
    ret c ; Return if CF is set, indicating insert '-' failed
    ; Set newly created empty slot to '-'
    ld a, signChar
    ld (hl), a
    or a ; clear CF to indicate negative
    ret

;-----------------------------------------------------------------------------

; Description: Handle the ENTER key.
; Input: none
; Output:
; Destroys: all, OP1, OP2, OP4
handleKeyEnter:
    call closeInputAndRecallNone
    call liftStack ; always lift the stack
    res rpnFlagsLiftEnabled, (iy + rpnFlags)
    ret

;-----------------------------------------------------------------------------
; Menu key handlers.
;-----------------------------------------------------------------------------

; Description: Go to the previous menu row, with rowIndex decreasing upwards.
; Input: none
; Output: (menuRowIndex) decremented, or wrapped around
; Destroys: all
handleKeyUp:
    ld hl, menuGroupId
    ld a, (hl) ; A = menuGroupId
    inc hl
    ld b, (hl) ; B = menuRowIndex
    call getMenuNode ; HL = pointer to MenuNode
    inc hl
    inc hl
    inc hl
    ; if numRows==1: return TODO: Check for 0, but that should never happen.
    ld c, (hl) ; C = numRows
    ld a, c
    cp 1
    ret z

    ; (menuRowIndex-1) mod numRows
    ld a, (menuRowIndex)
    or a
    jr nz, handleKeyUpContinue
    ld a, c ; A = numRows
handleKeyUpContinue:
    dec a
    ld (menuRowIndex), a

    set dirtyFlagsMenu, (iy + dirtyFlags)
    ret

;-----------------------------------------------------------------------------

; Description: Go to the next menu row, with rowIndex increasing downwards.
; Input: none
; Output: (menuRowIndex) incremented mod numRows
; Destroys: all
handleKeyDown:
    ld hl, menuGroupId
    ld a, (hl)
    inc hl
    ld b, (hl) ; menuRowIndex
    call getMenuNode
    inc hl
    inc hl
    inc hl
    ; if numRows==1: returt TODO: Check for 0, but that should never happen.
    ld c, (hl) ; numRows
    ld a, c
    cp 1
    ret z

    ; (menuRowIndex+1) mod numRows
    ld a, (menuRowIndex)
    inc a
    cp c
    jr c, handleKeyDownContinue
    xor a
handleKeyDownContinue:
    ld (menuRowIndex), a

    set dirtyFlagsMenu, (iy + dirtyFlags)
    ret

;-----------------------------------------------------------------------------

; Description: Go back up the menu hierarchy to the parent menu group. If
; already at the rootMenu, and the rowIndex is not 0, then reset the
; rowIndex to 0 so that we return to the default, top-level view of the menu
; hierarchy.
; Input: (menuGroupId), the current (child) menu group
; Output:
;   - (menuGroupId) at parentId
;   - (menuRowIndex) of the input (child) menu group
; Destroys: all
handleKeyExit:
    jp exitMenuGroup

;-----------------------------------------------------------------------------

; Description: Handle menu key 1 (left most).
; Input: none
; Destroys: all
handleKeyMenu1:
    ld a, 0
    jr handleKeyMenuA

; Description: Handle menu key 2 (2nd from left).
; Input: none
; Destroys: all
handleKeyMenu2:
    ld a, 1
    jr handleKeyMenuA

; Description: Handle menu key 3 (middle).
; Input: none
; Destroys: all
handleKeyMenu3:
    ld a, 2
    jr handleKeyMenuA

; Description: Handle menu key 4 (2nd from right).
; Input: none
; Destroys: all
handleKeyMenu4:
    ld a, 3
    jr handleKeyMenuA

; Description: Handle menu key 5 (right most).
; Input: none
; Destroys: all
handleKeyMenu5:
    ld a, 4
    jr handleKeyMenuA

; Description: Handle menu key 2ND 1 (left most).
; Input: none
; Destroys: all
handleKeyMenuSecond1:
    ld a, 0
    jr handleKeyMenuSecondA

; Description: Handle menu key 2ND 2 (2nd from left).
; Input: none
; Destroys: all
handleKeyMenuSecond2:
    ld a, 1
    jr handleKeyMenuSecondA

; Description: Handle menu key 2ND 3 (middle).
; Input: none
; Destroys: all
handleKeyMenuSecond3:
    ld a, 2
    jr handleKeyMenuSecondA

; Description: Handle menu key 2ND 4 (2nd from right).
; Input: none
; Destroys: all
handleKeyMenuSecond4:
    ld a, 3
    jr handleKeyMenuSecondA

; Description: Handle menu key 2ND 5 (right most).
; Input: none
; Destroys: all
handleKeyMenuSecond5:
    ld a, 4
    jr handleKeyMenuSecondA

; Description: Dispatch to the handler specified by the menu node at the menu
; button indexed by A (0: left most, 4: right most).
; Input: A: menu button index (0-4)
; Output: A: nodeId of the selected menu item
; Destroys: all
handleKeyMenuA:
    res rpnFlagsSecondKey, (iy + rpnFlags)
handleKeyMenuAltEntry:
    ld c, a ; save A (menu button index 0-4)
    call getCurrentMenuRowBeginId ; A=row begin id
    add a, c ; menu node ids are sequential starting with beginId
    jp dispatchMenuNode

; Description: Same as handleKeyMenuA() except that the menu key was invoked
; using the 2ND key, which sets the rpnFlagsSecondKey flag.
; Input: A: menu button index (0-4)
; Output: A: nodeId of the selected menu item
; Destroys: all
handleKeyMenuSecondA:
    set rpnFlagsSecondKey, (iy + rpnFlags)
    jr handleKeyMenuAltEntry

;-----------------------------------------------------------------------------
; Arithmetic functions.
;-----------------------------------------------------------------------------

; Description: Handle the Add key.
; Input: inputBuf
; Output:
; Destroys: all, OP1, OP2, OP4
handleKeyAdd:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jp nz, mBaseAddHandler
    call closeInputAndRecallXY
    bcall(_FPAdd) ; Y + X
    jp replaceXY

; Description: Handle the Sub key.
; Input: inputBuf
; Output:
; Destroys: all, OP1, OP2, OP4
handleKeySub:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jp nz, mBaseSubtHandler
    call closeInputAndRecallXY
    bcall(_FPSub) ; Y - X
    jp replaceXY

; Description: Handle the Mul key.
; Input: inputBuf
; Output:
; Destroys: all, OP1, OP2, OP4, OP5
handleKeyMul:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jp nz, mBaseMultHandler
    call closeInputAndRecallXY
    bcall(_FPMult) ; Y * X
    jp replaceXY

; Description: Handle the Div key.
; Input: inputBuf
; Output:
; Destroys: all, OP1, OP2, OP4
handleKeyDiv:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jp nz, mBaseDivHandler
    call closeInputAndRecallXY
    bcall(_FPDiv) ; Y / X
    jp replaceXY

;-----------------------------------------------------------------------------
; Constants: pi and e. There does not seem to be an existing bcall() that loads
; these constants. It's almost unbelievable, because these constants are shown
; on the calculator keyboard, and there are dozens of bcall() functions to load
; other values such as 0, 1, 2, 3 but I cannot find the bcall() to load these
; constants.
;-----------------------------------------------------------------------------

handleKeyPi:
    call closeInputAndRecallNone
    call op1SetPi
    jp pushX

handleKeyEuler:
    call closeInputAndRecallNone
    call op1SetEuler
    jp pushX

;-----------------------------------------------------------------------------
; Alegbraic functions.
;-----------------------------------------------------------------------------

; Description: y^x
handleKeyExpon:
    call closeInputAndRecallXY
    bcall(_YToX)
    jp replaceXY

; Description: 1/x
handleKeyInv:
    call closeInputAndRecallX
    bcall(_FPRecip)
    jp replaceX

; Description: x^2
handleKeySquare:
    call closeInputAndRecallX
    bcall(_FPSquare)
    jp replaceX

; Description: sqrt(x)
handleKeySqrt:
    call closeInputAndRecallX
    bcall(_SqRoot)
    jp replaceX

;-----------------------------------------------------------------------------
; Stack operations
;-----------------------------------------------------------------------------

handleKeyRollDown:
    call closeInputAndRecallNone
    jp rollDownStack

handleKeyExchangeXY:
    call closeInputAndRecallNone
    jp exchangeXYStack

handleKeyAns:
    call closeInputAndRecallNone
    call rclL
    jp pushX

;-----------------------------------------------------------------------------
; Transcendentals
;-----------------------------------------------------------------------------

handleKeyLog:
    call closeInputAndRecallX
    bcall(_LogX)
    jp replaceX

handleKeyALog:
    call closeInputAndRecallX
    bcall(_TenX)
    jp replaceX

handleKeyLn:
    call closeInputAndRecallX
    bcall(_LnX)
    jp replaceX

handleKeyExp:
    call closeInputAndRecallX
    bcall(_EToX)
    jp replaceX

;-----------------------------------------------------------------------------
; Trignometric
;-----------------------------------------------------------------------------

handleKeySin:
    call closeInputAndRecallX
    bcall(_Sin)
    jp replaceX

handleKeyCos:
    call closeInputAndRecallX
    bcall(_Cos)
    jp replaceX

handleKeyTan:
    call closeInputAndRecallX
    bcall(_Tan)
    jp replaceX

handleKeyASin:
    call closeInputAndRecallX
    bcall(_ASin)
    jp replaceX

handleKeyACos:
    call closeInputAndRecallX
    bcall(_ACos)
    jp replaceX

handleKeyATan:
    call closeInputAndRecallX
    bcall(_ATan)
    jp replaceX

;-----------------------------------------------------------------------------
; User registers, accessed through RCL nn and STO nn.
;-----------------------------------------------------------------------------

handleKeySto:
    call closeInputAndRecallNone
    ld hl, msgStoPrompt
    call startArgParser
    set inputBufFlagsArgAllowModifier, (iy + inputBufFlags)
    call processArgCommands
    ret nc ; do nothing if canceled
    cp argModifierIndirect
    ret nc ; TODO: implement this
    call rclX
    ; Implement STO{op}NN
    ld a, (argValue)
    cp regsSize ; check if command argument too large
    jp nc, handleKeyStoError
    ld c, a
    ld a, (argModifier)
    ld b, a
    jp stoOpNN
handleKeyStoError:
    ld a, errorCodeDimension
    ld (handlerCode), a
    ret

handleKeyRcl:
    call closeInputAndRecallNone
    ld hl, msgRclPrompt
    call startArgParser
    set inputBufFlagsArgAllowModifier, (iy + inputBufFlags)
    call processArgCommands
    ret nc ; do nothing if canceled
    cp argModifierIndirect
    ret nc ; TODO: implement this
    ; Implement RCL{op}NN, using slightly different algorithm for rclNN versus
    ; rclOpNN.
    ld a, (argValue)
    cp regsSize ; check if command argument too large
    jr nc, handleKeyRclError
    ld c, a
    ld a, (argModifier)
    or a
    jr nz, handleKeyRclOpNN
handleKeyRclNN:
    ; rclNN *pushes* RegNN on to the RPN stack.
    ld a, c
    call rclNN
    jp pushX
handleKeyRclOpNN:
    ; rcl{op}NN *replaces* the X register with (OP1 {op} RegNN).
    ld b, a
    push bc
    call rclX ; OP1=X
    pop bc
    call rclOpNN
    jp replaceX ; updates LastX
handleKeyRclError:
    ld a, errorCodeDimension
    ld (handlerCode), a
    ret

msgStoPrompt:
    .db "STO", 0
msgRclPrompt:
    .db "RCL", 0

;-----------------------------------------------------------------------------
; Buttons providing direct access to menu groups.
;-----------------------------------------------------------------------------

; Description: Handle the MATH key as the "HOME" key, going up to the top of
; the menu hierarchy.
handleKeyMath:
    ld a, mRootId
    jp dispatchMenuNode

; Description: Handle the MODE key as a shortcut to `ROOT > MODE`, except this
; saves the current MenuGroup as the jumpBack menu, and the ON/EXIT
handleKeyMode:
    ld a, mModeId
    jp dispatchMenuNodeWithJumpBack

; Description: Handle the STAT key as a shortcut to `ROOT > STAT`, but unlike
; `MODE`, this does *not* save the current MenuGroup in the jumpBack variables.
handleKeyStat:
    ld a, mStatId
    jp dispatchMenuNode

;-----------------------------------------------------------------------------
; QUIT. The mainExit routine will cleanup any application specific memory,
; including the call stack.
;-----------------------------------------------------------------------------

handleKeyQuit:
    jp mainExit

;-----------------------------------------------------------------------------
; Secret "DRAW" mode.
;-----------------------------------------------------------------------------

handleKeyDraw:
    call closeInput ; preserve rpnFlagsTvmCalculate
    ld hl, msgDrawPrompt
    call startArgParser
    call processArgCommands
    ret nc ; do nothing if canceled
    ; save (argValue)
    ld a, (argValue)
    ld (drawMode), a
    ; notify the dispatcher to clear and redraw the screen
    ld a, errorCodeClearScreen
    ld (handlerCode), a
    ret

handleKeyShow:
    call closeInput ; preserve rpnFlagsTvmCalculate
    call processShowCommands
    ret

; DRAW mode prompt.
msgDrawPrompt:
    .db "DRAW", 0
