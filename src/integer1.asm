;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Functions to convert between TI-OS floating point and u8, u16, and
; potentially u32 integers. These are similar to base1.asm, but are to be used
; by features which are *not* BASE menu functions. For example, these routines
; should not depend on baseWordSize, baseNumber, and so on. These routines may
; be duplicate some of the functionality in base.asm and baseops.asm, but
; keeping these separated will allow us to move BASE routines to other flash
; pages if needed.
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

; Description: Multiply HL by 10.
; Input: HL
; Output: HL=10*HL
; Preserves: all
multHLBy10:
    push de
    add hl, hl ; HL=2*HL
    ld d, h
    ld e, l
    add hl, hl ; HL=4*HL
    add hl, hl ; HL=8*HL
    add hl, de ; HL=10*HL
    pop de
    ret

; Description: Add A to HL.
; Input: HL, A
; Output: HL+=A
; Destroys: A
; Preserves: BC, DE
addHLByA:
    add a, l
    ld l, a
    ld a, 0
    adc a, h
    ld h, a
    ret

; Description: Divide HL by C
; Input: HL:dividend; C=divisor
; Output: HL:quotient; A:remainder
; Destroys: A, HL
; Preserves: BC, DE
divHLByC:
    push bc
    xor a ; A=remainder
    ld b, 16
divHLByCLoop:
    add hl, hl
    rla
    jr c, divHLByCOne ; remainder overflowed, so must substract
    cp c ; if remainder(A) < divisor(C): CF=1
    jr c, divHLByCZero
divHLByCOne:
    sub c
    inc l ; set bit 0 of quotient
divHLByCZero:
    djnz divHLByCLoop
    pop bc
    ret

; Description: Divide HL by BC
; Input: HL:dividend; BC=divisor
; Output: HL:quotient; DE:remainder
; Destroys: A, HL
; Preserves: BC
divHLByBC:
    ld de, 0 ; remainder=0
    ld a, 16 ; loop counter
divHLByBCLoop:
    ; NOTE: This loop could be made slightly faster by calling `scf` first then
    ; doing an `adc hl, hl` to shift a `1` into bit0 of HL. Then the `inc e`
    ; below is not required, but rather a `dec e` is required in the
    ; `divHLByBCZero` code path. Rearranging the code below would remove an
    ; extra 'jr' instruction. But I think the current code is more readable and
    ; maintainable, so let's keep it like this for now.
    add hl, hl
    ex de, hl ; DE=dividend/quotient; HL=remainder
    adc hl, hl ; shift CF into remainder
    ; remainder will never overflow, so CF=0 always here
    sbc hl, bc ; if remainder(DE) < divisor(BC): CF=1
    jr c, divHLByBCZero
divHLByBCOne:
    inc e ; set bit 0 of quotient
    jr divHLByBCNextBit
divHLByBCZero:
    add hl, bc ; add back divisor
divHLByBCNextBit:
    ex de, hl ; DE=remainder; HL=dividend/quotient
    dec a
    jr nz, divHLByBCLoop
    ret
