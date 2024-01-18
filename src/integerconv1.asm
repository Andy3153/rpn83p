;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Functions to convert between TI-OS floating point and integer types, mostly
; to u40/i40 integers. Functions related to u32 integers are on Flash Page 0 in
; base.asm.
;
; Labels with Capital letters are intended to be exported to other flash pages
; and should be placed in the branch table on Flash Page 0. Labels with
; lowercase letters are intended to be private so do not need a branch table
; entry.
;------------------------------------------------------------------------------

; Description: Convert the u8 in A to floating pointer number in OP1. This
; supports the full range of A from 0 to 255, compared to the SetXXOP1()
; function in the SDK which supports only integers between 0 and 99.
; Input:
;   - A: u8 integer
; Output:
;   - OP1: floating point value of A
; Destroys: A, B, DE, OP2
; Preserves: C, HL
ConvertAToOP1PageOne:
    push af
    bcall(_OP1Set0)
    pop af
    ; [[fallthrough]]

; Description: Convert the u8 in A to floating point number, and add it to OP1.
; This is the same as addAToOP1() in base.asm, but is duplicated here to
; decouple this from BASE functions in base.asm, base1.asm, and baseops.asm.
; Input:
;   - A: u8 integer
;   - OP1: current floating point value, set to 0.0 to start fresh
; Destroys: A, B, DE, OP2
; Preserves: C, HL
addAToOP1PageOne:
    push hl
    ld b, 8 ; loop for 8 bits in u8
addAToOP1PageOneLoop:
    push bc
    push af
    bcall(_Times2) ; OP1 *= 2
    pop af
    sla a
    jr nc, addAToOP1PageOneCheck
    push af
    bcall(_Plus1) ; OP1 += 1
    pop af
addAToOP1PageOneCheck:
    pop bc
    djnz addAToOP1PageOneLoop
    pop hl
    ret

;------------------------------------------------------------------------------

; Description: Convert OP1 to u16(HL). Throw ErrDomain exception if:
;   - OP1 >= 2^16
;   - OP1 < 0
;   - OP1 is not an integer
; Input: OP1
; Output: HL=u16(OP1)
; Destroys: all, OP2
convertOP1ToHLPageOne:
    bcall(_CkPosInt) ; if OP1>=0 and OP1 is int: ZF=1
    jr nz, convertOP1ToHLErr
    call op2Set2Pow16PageOne
    bcall(_CpOP1OP2) ; if OP1 >= 2^16: CF=0
    jr nc, convertOP1ToHLErr
    jr convertOP1ToHLNoCheck
convertOP1ToHLErr:
    bcall(_ErrDomain)

; Description: Convert OP1 to u16(HL) without any boundary checks. Adapted from
; convertOP1ToU32NoCheck().
; Input: OP1
; Output: HL=u16(OP1)
; Destroys: all
convertOP1ToHLNoCheck:
    ; initialize the target u16 and check for 0.0
    ld hl, 0
    bcall(_CkOP1FP0) ; preserves HL
    ret z
    ; extract number of decimal digits
    ld de, OP1+1 ; exponent byte
    ld a, (de)
    sub $7F ; A = exponent + 1 = num digits in mantissa
    ld b, a ; B = num digits in mantissa
    jr convertOP1ToHLLoopEntry
convertOP1ToHLLoop:
    call multHLBy10
convertOP1ToHLLoopEntry:
    ; get next 2 digits of mantissa
    inc de ; DE = pointer to mantissa
    ld a, (de)
    ; Process first mantissa digit
    rrca
    rrca
    rrca
    rrca
    and $0F
    call addHLByA
    ; check number of mantissa digits
    dec b
    ret z
    ; Process second mantissa digit
    call multHLBy10
    ld a, (de)
    and $0F
    call addHLByA
    djnz convertOP1ToHLLoop
    ret

;------------------------------------------------------------------------------

; Description: Convert the i40 (signed integer) referenced by HL to a floating
; point number in OP1.
; Input: HL: pointer to i40 (must not be OP2)
; Output: OP1: floating point equivalent of u40(HL)
; Destroys: A, B, C, DE
; Preserves: HL, OP2
ConvertI40ToOP1:
    call isPosU40 ; ZF=1 if positive or zero
    jr z, ConvertU40ToOP1
    call negU40 ; U40=-U40
    call ConvertU40ToOP1
    bcall(_InvOP1S) ; invert the sign
    ret

; Description: Convert the u40 referenced by HL to a floating point number in
; OP1.
; Input: HL: pointer to u40 (must not be OP2)
; Output: OP1: floating point equivalent of u40(HL)
; Destroys: A, B, C, DE
; Preserves: HL, OP2
ConvertU40ToOP1:
    push hl
    bcall(_PushRealO2) ; FPS=[OP2 saved]
    bcall(_OP1Set0)
    pop hl
    push hl
    ld de, 4
    add hl, de ; HL points to most significant byte
    ld b, 5
convertU40ToOP1Loop:
    ld a, (hl)
    dec hl
    push bc
    call addAToOP1PageOne
    pop bc
    djnz convertU40ToOP1Loop
    bcall(_PopRealO2) ; FPS=[]; OP2=OP2 saved
    pop hl
    ret

;------------------------------------------------------------------------------

; Bit flags of the u40StatusCode.
;   - u40StatusCodeNegative and u40StatusCodeTooBig are usually fatal errors
;   which throw an exception.
;   - u40StatusCodeHasFrac is sometimes a non-fatal error, because the
;   operation will truncate to integer before continuing with the calculation.
;   - u40StatusCodeFatalMask can be used to check only the fatal codes using a
;   bitwise-and
u40StatusCodeNegative equ 0
u40StatusCodeTooBig equ 1
u40StatusCodeHasFrac equ 7
u40StatusCodeFatalMask equ $03

; Description: Convert OP1 to an i40 integer, throwing an Err:Domain if OP1 is
; not in the range of (-2^39,+2^39).
; Input:
;   - OP1: *signed* 40-bit integer as a floating point number
;   - HL: pointer to a i40 in memory, cannot be OP2
; Output:
;   - HL: OP1 converted to a i40, in little-endian format
;   - C: u40StatusCode
; Destroys: A, B, C, DE
; Preserves: HL, OP1, OP2
ConvertOP1ToI40:
    ld a, (OP1)
    bit 7, a ; ZF=0 if negative
    jr z, convertOP1ToI40Pos
    ; Handle negative. Strictly speaking, this code does not handle the
    ; smallest negative value of -2^39, but that's beyond the normal usage of
    ; this function, so it's not worth spending effort to handle that special
    ; case.
    res 7, a
    ld (OP1), a
    call convertOP1ToI40Pos
    set 7, a
    ld (OP1), a
    ret
convertOP1ToI40Pos:
    call op2Set2Pow39PageOne
    bcall(_CpOP1OP2) ; if OP1 >= 2^39: CF=0
    jr nc, convertOP1ToU40Error
    ; [[fallthrough]]

; Description: Convert OP1 to a u40 integer, throwing an Err:Domain exception
; if OP1 is:
; - not in the range of [0, 2^40), or
; - is negative, or
; - contains fractional part.
;
; See convertOP1ToU40NoCheck() to convert to U40 without throwing.
;
; Input:
;   - OP1: unsigned 40-bit integer as a floating point number
;   - HL: pointer to a u40 in memory, cannot be OP2
; Output:
;   - HL: OP1 converted to a u40, in little-endian format
;   - C: u40StatusCode
; Destroys: A, B, C, DE
; Preserves: HL, OP1, OP2
ConvertOP1ToU40:
    call convertOP1ToU40StatusCode ; OP3=u40(OP1); C=u40StatusCode
    ld a, c
    or a
    ret z
convertOP1ToU40Error:
    bcall(_ErrDomain) ; throw exception

; Description: Convert OP1 to u40 with u40StatusCode.
; Input:
;   - OP1: floating point number
;   - HL: pointer to u40 in memory, cannot be OP2
; Output:
;   - HL: pointer to u40
;   - C: u40StatusCode
; Destroys: A, B, C, DE
; Preserves: HL, OP1, OP2
convertOP1ToU40StatusCode:
    call clearU40 ; ensure u40=0 even when error conditions are detected
    push hl ; stack=[u40]
    ld c, 0 ; u40StatusCode
    push bc ; stack=[u40, u40StatusCode]
    bcall(_PushRealO2) ; FPS=[OP2 saved]
    ; check negative
    bcall(_CkOP1Pos) ; if OP1<0: ZF=0
    jr z, convertOP1ToU40StatusCodeCheckTooBig
    bcall(_PopRealO2) ; FPS=[]; OP2=OP2 saved
    pop bc ; stack=[u40]; C=u40StatusCode
    pop hl ; stack=[]; HL=u40
    set u40StatusCodeNegative, c
    ret
convertOP1ToU40StatusCodeCheckTooBig:
    call op2Set2Pow40PageOne
    bcall(_CpOP1OP2) ; if OP1 >= 2^40: CF=0
    jr c, convertOP1ToU40StatusCodeCheckInt
    bcall(_PopRealO2) ; FPS=[]; OP2=OP2 saved
    pop bc ; stack=[u40]; C=u40StatusCode
    pop hl ; stack=[]; HL=u40
    set u40StatusCodeTooBig, c
    ret
convertOP1ToU40StatusCodeCheckInt:
    bcall(_CkPosInt) ; if OP1>=0 and OP1 is int: ZF=1
    jr z, convertOP1ToU40StatusCodeValid
    bcall(_PopRealO2) ; FPS=[]; OP2=OP2 saved
    pop bc ; stack=[u40]; C=u40StatusCode
    pop hl ; stack=[]; HL=u40
    set u40StatusCodeHasFrac, c
    jr convertOP1ToU40StatusCodeContinue
convertOP1ToU40StatusCodeValid:
    bcall(_PopRealO2) ; FPS=[]; OP2=OP2 saved
    pop bc ; stack=[u40]; C=u40StatusCode
    pop hl ; stack=[]; HL=u40
convertOP1ToU40StatusCodeContinue:
    ; [[fallthrough]]

; Description: Convert floating point OP1 to a u40. This routine assume that
; OP1 is a floating point number between [0, 2^40). Fractional digits are
; ignored when converting to u40 integer. Use convertOP1ToU40() to perform a
; validation check that throws an exception.
; Input:
;   - OP1: unsigned 32-bit integer as a floating point number
;   - HL: pointer to a u40 in memory, cannot be OP2
; Output:
;   - HL: OP1 converted to a u40, in little-endian format
; Destroys: A, B, DE
; Preserves: HL, C
convertOP1ToU40NoCheck:
    ; initialize the target u40
    call clearU40
    bcall(_CkOP1FP0) ; preserves HL
    ret z
    ; extract number of decimal digits
    ld de, OP1+1 ; exponent byte
    ld a, (de)
    sub $7F ; A = exponent + 1 = num digits in mantissa
    ld b, a ; B = num digits in mantissa
    jr convertOP1ToU40LoopEntry
convertOP1ToU40Loop:
    call multU40By10
convertOP1ToU40LoopEntry:
    ; get next 2 digits of mantissa
    inc de ; DE = pointer to mantissa
    ld a, (de)
    ; Process first mantissa digit
    rrca
    rrca
    rrca
    rrca
    and $0F
    call addU40ByA
    ; check number of mantissa digits
    dec b
    ret z
convertOP1ToU40SecondDigit:
    ; Process second mantissa digit
    call multU40By10
    ld a, (de)
    and $0F
    call addU40ByA
    djnz convertOP1ToU40Loop
    ret
