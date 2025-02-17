;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Functions related to parsing the inputBuf into a floating point number.
;
; This is now on Flash Page 1. Labels with Capital letters are intended to be
; exported to other flash pages and should be placed in the branch table on
; Flash Page 0. Labels with lowercase letters are intended to be private so do
; not need a branch table entry.
;------------------------------------------------------------------------------

; Description: Initialize variables and flags related to the input buffer.
; Output:
;   - inputBuf set to empty
;   - rpnFlagsEditing reset
; Destroys: A
InitInputBuf:
    res rpnFlagsEditing, (iy + rpnFlags)
    ; [[fallthrough]]

; Description: Clear the inputBuf.
; Input: inputBuf
; Output:
;   - inputBuf cleared
;   - dirtyFlagsInput set
; Destroys: none
ClearInputBuf:
    push af
    xor a
    ld (inputBuf), a
    set dirtyFlagsInput, (iy + dirtyFlags)
    pop af
    ret

; Description: Append character to inputBuf.
; Input:
;   A: character to be appended
; Output:
;   - CF set when append fails
;   - dirtyFlagsInput set
; Destroys: all
AppendInputBuf:
    ld c, a ; C=char
    call getInputMaxLen ; A=inputMaxLen
    cp inputBufCapacity ; if inputMaxLen>=inputBufCapacity: CF=0
    jr c, appendInputBufContinue
    ld a, inputBufCapacity ; A=min(inputMaxLen,inputBufCapacity)
appendInputBufContinue:
    ld b, a ; B=inputMaxLen
    ld a, c ; A=char
    ld hl, inputBuf
    set dirtyFlagsInput, (iy + dirtyFlags)
    jp AppendString

; Description: Close the input buffer by parsing the input, then copying the
; float value into X. If not in edit mode, no need to parse the inputBuf, the X
; register is not changed. Almost all functions/commands in RPN83P will call
; this function through the closeInput() function.
;
; This function determines 2 flags which affect the stack lift:
;
; - rpnFlagsLiftEnabled: *Always* set after this call. It is up to the calling
; handler to override this default and disable it if necessary (e.g. ENTER, or
; Sigma+).
; - inputBufFlagsClosedEmpty: Set if the inputBuf was an empty string before
; being closed. This flag is cleared if the inputBuf was *not* in edit mode to
; begin with.
;
; The rpnFlagsLiftEnabled is used by the next manual entry of a number (digits
; 0-9 usualy, sometimes A-F in hexadecimal mode). Usually, the next manual
; number entry lifts the stack, but this flag can be used to disable that.
; (e.g. ENTER will disable the lift of the next number).
;
; The inputBufFlagsClosedEmpty flag is used by functions which do not consume
; any value from the RPN stack, but simply push a value or two onto the X or Y
; registers (e.g. PI, E, or various TVM functions, various STAT functions). If
; the user had pressed CLEAR, to clear the input buffer, then it doesn't make
; sense for these functions to lift the empty string (i.e. 0) up when pushing
; the new values. These functions call pushToX() or pushToXY() which checks if
; the inputBuf was closed when empty. If empty, pushToX() or pushToXY() will
; *not* lift the stack, but simply replace the "0" in the X register with the
; new value. This flag is cleared if the inputBuf was not in edit mode, with
; the assumption that new X or Y values should lift the stack.
;
; Input:
;   - inputBuf: input buffer
; Output:
;   - rpnFlagsLiftEnabled: always set
;   - inputBufFlagsClosedEmpty: set if inputBuf was an empty string when closed
;   - inputBuf cleared to empty string
;   - OP1: value of inputBuf
; Destroys: all, OP1, OP2, OP4
CloseInputBuf:
    ld a, (inputBuf)
    or a
    jr z, closeInputBufEmpty
    ; inputBuf not empty
    res inputBufFlagsClosedEmpty, (iy + inputBufFlags)
    jr closeInputBufContinue
closeInputBufEmpty:
    set inputBufFlagsClosedEmpty, (iy + inputBufFlags)
closeInputBufContinue:
    call parseInputBuf ; OP1/OP2=float or complex
    jp ClearInputBuf

;------------------------------------------------------------------------------

; Description: Return the number of digits which are accepted or displayed for
; the given (baseWordSize) and (baseNumber).
;   - floating mode: inputBufFloatMaxLen
;   - BASE 2: inputMaxLen = baseWordSize
;       - 8 -> 8
;       - 16 -> 16
;       - 24 -> 24
;       - 32 -> 32
;   - BASE 8: inputMaxLen = ceil(baseWordSize / 3)
;       - 8 -> 3 (0o377)
;       - 16 -> 6 (0o177 777)
;       - 24 -> 8 (0o77 777 777)
;       - 32 -> 11 (0o37 777 777 777)
;   - BASE 10: inputMaxLen = ceil(log10(2^baseWordSize))
;       - 8 -> 3 (255)
;       - 16 -> 5 (65 535)
;       - 24 -> 8 (16 777 215)
;       - 32 -> 10 (4 294 967 295)
;   - BASE 16: inputMaxLen = baseWordSize / 4
;       - 8 -> 2 (0xff)
;       - 16 -> 4 (0xff ff)
;       - 24 -> 6 (0xff ff ff)
;       - 32 -> 8 (0xff ff ff ff)
;
; This version uses a lookup table to make the above transformations. Another
; way is to use a series of nested if-then-else statements (i.e. a series of
; 'cp' and 'jr nz' statements in assembly language). The nested if-then-else
; actually turned out to be about 80 bytes *smaller*. However, the if-then-else
; version is so convoluted that it is basically unreadable and unmaintainable.
; Use the lookup table implementation instead even though it takes up slightly
; more space.
;
; Input: rpnFlagsBaseModeEnabled, (baseWordSize), (baseNumber).
; Output: A: inputMaxLen
; Destroys: A
; Preserves: BC, DE, HL
getInputMaxLen:
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jr nz, getInputMaxLenBaseMode
    ; In normal floating point input mode, i.e. not BASE mode.
    ; Return either the normal float limit or the complex limit.
    ld hl, inputBuf
    call checkComplexDelimiterP ; CF=1 if complex
    jr nc, getInputMaxLenNormalMaxLen
    ; allow extra characters for complex numbers
    ld a, inputBufComplexMaxLen
    ret
getInputMaxLenNormalMaxLen:
    ld a, inputBufFloatMaxLen
    ret
getInputMaxLenBaseMode:
    ; If BASE mode, the maximum number of digits depends on baseNumber and
    ; baseWordSize.
    push de
    push hl
    call getBaseNumberIndex ; A=baseNumberIndex
    sla a
    sla a ; A=baseNumberIndex * 4
    ld e, a
    call getWordSizeIndexPageOne ; A=wordSizeIndex
    add a, e ; A=4*baseNumberIndex+wordSizeIndex
    ld e, a
    ld d, 0
    ld hl, wordSizeDigitsArray
    add hl, de
    ld a, (hl) ; A=maxLen
    pop hl
    pop de
    ret

; List of the inputDigit limit of the inputBuf for each (baseNumber) and
; (baseWordSize). Each group of 4 represents the inputDigits for wordSizes (8,
; 16, 24, 32) respectively.
wordSizeDigitsArray:
    .db 8, 16, 24, 32 ; base 2
    .db 3, 6, 8, 11 ; base 8
    .db 3, 5, 8, 10 ; base 10
    .db 2, 4, 6, 8 ; base 16

;------------------------------------------------------------------------------

; Description: Check various characteristics of the characters in the inputBuf
; by scanning backwards from the end of the string. The following conditions
; are checked:
;   - inputBufStateDecimalPoint: set if decimal point exists
;   - inputBufStateEE: set if exponent 'E' character exists
;   - inputBufStateComplex: set if complex number
;   - inputBufEEPos: pos of char after 'E', or the first character of the
;   number component if no 'E'
;   - inputBufEELen: number of EE digits if inputBufStateEE is set
; Input: inputBuf
; Output:
;   - C: inputBufState flags updated
;   - D: inputBufEEPos, pos of char after 'E' or at start of number
;   - E: inputBufEELen, number of EE digits if inputBufStateEE is set
; Destroys: BC, DE, HL
; Preserves: AF
GetInputBufState:
    push af
    ld hl, inputBuf
    ld c, (hl) ; C=len
    ld b, 0 ; BC=len
    inc hl ; skip past len byte
    add hl, bc ; HL=pointer to end of string
    ; swap B and C
    ld a, c ; A=len
    ld c, b ; C=inputBufState=0
    ld b, a ; B=len
    ; check for len==0
    or a ; if len==0: ZF=0
    jr z, getInputBufStateEnd
    ; D=inputBufEEPos=0; E=inputBufEELen=0
    ld de, 0
getInputBufStateLoop:
    ; Loop backwards from end of string and update inputBufState flags
    dec hl
    ld a, (hl)
    ; check for '0'-'9'
    call isValidUnsignedDigit ; if valid: CF=1
    jr nc, getInputBufStateCheckDecimalPoint
    ; if not EE: increment inputBufEELen
    bit inputBufStateEE, c
    jr nz, getInputBufStateCheckDecimalPoint
    inc e ; inputBufEELen++
getInputBufStateCheckDecimalPoint:
    cp '.'
    jr nz, getInputBufStateCheckEE
    set inputBufStateDecimalPoint, c
getInputBufStateCheckEE:
    cp Lexponent
    jr nz, getInputBufStateCheckTermination
    set inputBufStateEE, c
    ld d, b ; inputBufEEPos=B
getInputBufStateCheckTermination:
    call isComplexDelimiterPageOne ; ZF=1 if complex delimiter
    jr z, getInputBufStateComplex
    ; Loop until we reach the start of string
    djnz getInputBufStateLoop
    jr getInputBufStateEnd
getInputBufStateComplex:
    set inputBufStateComplex, c
getInputBufStateEnd:
    ; If no 'E', set inputBufEEPos to the start of current number, which could
    ; be the imaginary or angle part of a complex number.
    bit inputBufStateEE, c
    jr nz, getInputBufStateReturn
    ld d, b
getInputBufStateReturn:
    pop af
    ret

;------------------------------------------------------------------------------

; Description: Parse the input buffer into a real or complex number in OP1/OP2.
; Input: inputBuf filled with keyboard characters
; Output: OP1/OP2: real or complex number
; Destroys: all registers, OP1-OP5 (due to SinCosRad())
parseInputBuf:
    call initInputBufForParsing
    ld hl, inputBuf
    inc hl
    bit rpnFlagsBaseModeEnabled, (iy + rpnFlags)
    jr nz, parseBaseInteger
    ; parse a real or real component
    call parseFloat ; OP1=float; CF=0 if empty string
    rl b ; B=first.isNonEmpty
    call findComplexDelimiter ; if complex: CF=1, A=delimiter, HL=pointer
    ret nc
    ; parse the imaginary component
    ld c, a ; C=delimiter
    push bc ; stack=[delimiter,first.isNonEmpty]
    push hl
    bcall(_PushRealO1) ; FPS=[first]
    pop hl
    call parseFloat ; OP1=second; CF=0 if empty string
    ; Check if first and second were empty strings.
    jr c, parseInputBufNonEmptyImaginary
    pop bc ; stack=[]; B=first.isNonEmpty; C=delimiter
    push bc
    rr b ; CF=first.isNonEmpty
    jr c, parseInputBufNonEmptyImaginary
    ; We are here if both the real and imaginary components were empty strings.
    ; Check if the complex delimiter was a 'i'. If so, set the imaginary
    ; component to 1, so that a solitary 'i' is interpreted as just '0i1'
    ; instead of '0i0'.
    ld a, c ; A=delimiter
    cp LimagI
    jr nz, parseInputBufNonEmptyImaginary
    bcall(_OP1Set1) ; OP1=1.0
parseInputBufNonEmptyImaginary:
    call op1ToOp2PageOne ; OP2=second
    bcall(_PopRealO1) ; FPS=[]; OP1=first; OP2=second
    pop bc ; stack=[]; C=delimiter
    ; convert 2 real numbers in OP1/OP2 into a complex number
    ld a, c ; A=delimiter
    cp Langle
    jp z, pradToComplex
    cp Ldegree
    jp z, pdegToComplex
    jp rectToComplex

; Description: Parse the floating point number at HL.
; Input: HL: pointer to a floating point number C-string
; Output:
;   - OP1: floating point number
;   - CF: 0 if empty string, 1 non-empty
; Destroys: A, BC, DE
; Preserves: HL
parseFloat:
    call clearParseBuf
    call clearFloatBuf ; OP1=0.0
    ; Check for an emtpy string.
    ld a, (hl)
    call isValidScientificDigit ; CF=1 if valid
    ret nc
    call findDecimalPoint ; A=i8(decimalPointPos)
    call extractMantissaExponent ; extract mantissa exponent to floatBuf
    call extractMantissaSign ; extract mantissa sign to floatBuf
    call parseMantissa ; parse mantissa digits from inputBuf into parseBuf
    call extractMantissa ; convert mantissa digits in parseBuf to floatBuf
    call extractExponent ; extract exponent from inputBuf to floatBuf
    push hl
    ld hl, floatBuf
    call move9ToOp1PageOne
    pop hl
    scf ; CF=1
    ret

; Description: Parse the integer (base 2, 8, 10, 16) at HL.
; Input: HL: pointer to C string of an integer
; Output: OP1: floating point representation of integer
; Destroys: all
parseBaseInteger:
    ld a, (baseNumber)
    cp 16
    jr z, parseNumBase
    cp 8
    jr z, parseNumBase
    cp 2
    jr z, parseNumBase
    cp 10
    jr z, parseNumBase
    ; all others interpreted as base 10
    ld a, 10
    ld (baseNumber), a
    ; [[fallthrough]]

; Description: Parse the baseNumber in the C-string given by HL. The base mode
; be 2, 8 or 16. This subroutine will actually supports any baseNumber <= 36
; probably (10 numerals and 26 letters).
; Input:
;   - HL: pointer to C string
;   - A: base mode (2, 8, 10, 16)
; Output: OP1: floating point value
; Destroys: all
parseNumBase:
    push hl
    bcall(_SetXXOP1) ; OP1 = A = base
    bcall(_OP1ToOP4) ; OP4 = base
    bcall(_OP1Set0)
    pop hl
parseNumBaseLoop:
    ; get next digit
    ld a, (hl)
    inc hl
    or a
    ret z ; return on NUL
    push hl
    ; multiply by 'base' before adding the next digit
    push af
    bcall(_OP4ToOP2) ; OP2 = base
    bcall(_FPMult) ; OP1 *= base; destroys OP3
    pop af
    ; convert char into digit value
    cp 'A'
    jr c, parseNumBase0To9
parseNumBaseAToF:
    sub 'A'
    add a, 10
    jr parseNumBaseAddDigit
parseNumBase0To9:
    sub '0'
parseNumBaseAddDigit:
    bcall(_SetXXOP2) ; OP2 = A = digit value
    bcall(_FPAdd) ; OP1 += OP2
    pop hl
    jr parseNumBaseLoop

;------------------------------------------------------------------------------

; Description: Initialize the inputBuf for parsing by adding a NUL terminator
; to the Pascal string. The capacity of inputBuf is one character larger than
; necessary to hold the extra NUL character.
; Input: inputBuf
; Output: inputBuf with NUL terminator
; Destroys: A, DE, HL
initInputBufForParsing:
    ld hl, inputBuf
    ld e, (hl)
    xor a
    ld d, a
    inc hl ; skip len byte
    add hl, de ; HL=pointerToNUL
    ld (hl), a
    ret

; Description: Clear parseBuf by setting all digits to the character '0', and
; setting size to 0. The trailing '0' characters make it easy to construct the
; floating point number.
; Input: parseBuf
; Output: parseBuf initialized to '0's, and set to 0-length
; Destroys: A, B
; Preserves: HL
clearParseBuf:
    push hl
    xor a
    ld hl, parseBuf
    ld (hl), a
    ld a, '0'
    ld b, parseBufCapacity
    inc hl
clearParseBufLoop:
    ld (hl), a
    inc hl
    djnz clearParseBufLoop
    pop hl
    ret

; Description: Set floatBuf and OP1 to 0.0.
; Destroys: A, DE
; Preserves: HL
clearFloatBuf:
    push hl
    bcall(_OP1Set0)
    ld de, floatBuf
    bcall(_MovFrOP1)
    pop hl
    ret

;------------------------------------------------------------------------------

; Description: Parse the mantissa digits from inputBuf into parseBuf, ignoring
; negative sign, leading zeros, the decimal point, and the EE symbol. For
; example:
;   - "0.0" produces ""
;   - "-00.00" produces ""
;   - "0.1" produces "1"
;   - "-001.2" produces "12"
;   - "23E-1" produces "23"
; Input:
;   - HL: pointer to C-string
;   - parseBuf: Pascal string, initially set to empty string
; Output:
;   - parseBuf: filled with mantissa digits or an empty string if all 0
; Destroys: A, BC, DE
; Preserves: HL
parseMantissaLeadingFound equ 0 ; bit to set when lead digit found
parseMantissa:
    res parseMantissaLeadingFound, c
    push hl
parseMantissaLoop:
    ld a, (hl)
    inc hl
    call isValidFloatDigit ; if valid: CF=1
    jr nc, parseMantissaEnd
    cp signChar
    jr z, parseMantissaLoop
    cp '.'
    jr z, parseMantissaLoop
    cp '0'
    jr nz, parseMantissaNormalDigit
    ; Ignore '0' before a leading digit.
    bit parseMantissaLeadingFound, c
    jr z, parseMantissaLoop
parseMantissaNormalDigit:
    ; A: char to append
    set parseMantissaLeadingFound, c
    call appendParseBuf ; preserves BC, HL
    jr parseMantissaLoop
parseMantissaEnd:
    pop hl
    ret

;------------------------------------------------------------------------------

; Bit flags used by findDecimalPoint().
findDecimalPointLeadingFound equ 0 ; set if leading (non-zero) digit found
findDecimalPointDotFound equ 1; set if decimal point found

; Description: Find the position of the decimal point of the given number
; string. If the input string is effectively 0, then position of 1 is returned
; so that the final floating point number has an exponent of $80, which is the
; canonical representation of 0.0 in the TI-OS.
;
; The returned value is the number of places the the decimal point needs to be
; shifted to get back the original value after the mantissa is normalized with
; the leading non-zero digit immediately to the right of the decimal place. The
; normalized mantissa lies in the interval [0.1, 1.0). The shift can be a
; negative number for values less than 0.1.
;
; A string that has no leading digit will always parse to 0, for example "0" or
; "0.00" or "000.00". This condition will be detected and the position is
; returned as 1, so that the final floating point number has an exponent of $80
; (i.e. 0) which is the canonical representation of 0.0 in the TI-OS.
;
; For example, the following unnormalized number strings should return the
; indicated decimal point position:
;
;   - "0" -> .0, 1
;   - "00.000" -> .0, 1
;   - "123.4" ->  .1234, 3
;   - "012" ->  .12, 2
;   - "12" ->  .12, 2
;   - "1.2" ->  .12, 1
;   - ".12" -> .12, 0
;   - "0.123" -> .123, 0
;   - ".012" -> .12, -1
;   - "000.0012" -> .12, -2
;
; Here is the algorithm written in C:
;
; int8_t findDecimalPoint(const char *s) {
;   bool leadingFound = false;
;   bool dotFound = false;
;   int8_t pos = 0;
;   for (int8_t i=0; ; i++) {
;       char c = s[i];
;       if (!isValidFloatingDigit(c)) break;
;       if (c == '-') continue;
;       if (c == '.') {
;           dotFound = true;
;           continue;
;       }
;       // '0' is special only if no leading digit found yet
;       if (c == '0' && !leadingFound) {
;           if (dotFound) {
;               pos--;
;           }
;       } else { // c!='0' || leadingFound
;           leadingFound = true;
;           if (dotFound) break;
;           pos++;
;       }
;   }
;   return (leadingFound ? pos : 1);
;
; Input: HL: pointer to floating point C-string
; Output: A: decimalPointPos, signed integer
; Destroys: A, BC, DE
; Preservers: HL
findDecimalPoint:
    push hl
    xor a
    ld c, a ; pos
    ld d, a ; flags
findDecimalPointLoop:
    ld a, (hl)
    inc hl
    ; check valid floating point digit (excludes 'E')
    call isValidFloatDigit; if valid: CF=1
    jr nc, findDecimalPointEnd
    ; ignore and skip '-'
    cp signChar
    jr z, findDecimalPointLoop
    ; check for '.'
    cp '.'
    jr nz, findDecimalPointCheckZero
    set findDecimalPointDotFound, d
    jr findDecimalPointLoop
findDecimalPointCheckZero:
    ; check for '0' && !leadingFound
    cp '0'
    jr nz, findDecimalPointNormalDigit
    bit findDecimalPointLeadingFound, d
    jr nz, findDecimalPointNormalDigit
    ; decrement pos if dot found
    bit findDecimalPointDotFound, d
    jr z, findDecimalPointLoop
    dec c
    jr findDecimalPointLoop
findDecimalPointNormalDigit:
    set findDecimalPointLeadingFound, d
    bit findDecimalPointDotFound, d
    jr nz, findDecimalPointEnd
    inc c
    jr findDecimalPointLoop
findDecimalPointEnd:
    pop hl
    ld a, c
    bit findDecimalPointLeadingFound, d
    ret nz
    ld a, 1 ; if no leading digit found: return pos=1
    ret

;------------------------------------------------------------------------------

; Description: Append character in A to parseBuf
; Input:
;   - A: character to be appended
; Output:
;   - CF set when append fails
; Destroys: A, DE
; Preserves: BC, HL
appendParseBuf:
    push hl
    push bc
    ld hl, parseBuf
    ld b, parseBufCapacity
    call AppendString
    pop bc
    pop hl
    ret

;------------------------------------------------------------------------------

; Description: Set the exponent from the mantissa. The mantissaExp =
; decimalPointPos - 1. But the floating exponent is shifted by $80.
;   mantissaExponent = decimalPointPos - 1
;   floatingExponent = mantissaExponent + $80
;                    = decimalPointPos + $7F
;
; Input: A: decimalPointPos (from findDecimalPoint())
; Output: floatBufExp = decimalPointPos + $7F
; Destroys: A
extractMantissaExponent:
    add a, $7F
    ld (floatBufExp), a
    ret

; Description: Extract mantissa sign from the first character of the given
; string, and transfer it to the sign bit of the floatBuf.
; Input: HL: NUL terminated C-string
; Output: (floatBuf) sign set
; Destroys: none
; Preserves: HL
extractMantissaSign:
    ld a, (hl) ; A will be NUL if an empty string
    cp signChar
    ret nz ; '-' not found at first character
    push hl
    ld hl, floatBufType
    set 7, (hl)
    pop hl
    ret

; Description: Extract the normalized mantissa digits from parseBuf to
; floatBuf, 2 digits per byte. If the mantissa is an empty string or
; effectively 0, do nothing.
; Input: parseBuf
; Output: floatBuf updated
; Destroys: A, BC, DE
; Preserves: HL
extractMantissa:
    push hl
    ld hl, parseBuf
    ld a, (hl)
    or a
    jr z, extractMantissaEnd ; if mantissa is effectively 0 or "", do nothing
    inc hl
    ld de, floatBufMan
    ld b, parseBufCapacity/2
extractMantissaLoop:
    ; Loop 2 digits at a time.
    ld a, (hl)
    sub '0'
    sla a
    sla a
    sla a
    sla a
    ld c, a
    inc hl
    ld a, (hl)
    sub '0'
    or c
    ld (de), a
    inc de
    inc hl
    djnz extractMantissaLoop
extractMantissaEnd:
    pop hl
    ret

;-----------------------------------------------------------------------------

; Description: Extract the EE exponent digits (if any) to floatBuf. If the
; mantissa is effectively 0 or an empty string, do nothing.
; Input: HL: pointer to floating number C-string
; Output: floatBuf updated
; Destroys: A, BC, DE
; Preserves: HL
extractExponent:
    ; If the mantissa is effectively 0, then no need to parse the exponent.
    ld a, (parseBuf)
    or a
    ret z
    ;
    push hl
    call findExponent ; HL=pointerEEDigit; CF=1 if found
    jr nc, extractExponentEnd
    call parseExponent ; A=exponentValue of HL string
    call addExponent ; add EE exponent to floatBuf exponent
extractExponentEnd:
    pop hl
    ret

; Description: Find the next 'E' character and return the number of exponent
; digits.
; Input:
;   - HL: pointer to scientific floating point C-string
; Output:
;   - CF: 0 if not found, 1 if found
;   - HL: pointer to the first character after the 'E' symbol
; Destroys: BC, DE, HL
findExponent:
    ld a, (hl)
    inc hl
    cp Lexponent
    jr z, findExponentFound
    call isValidFloatDigit ; if isValidFloatDigit(A): CF=1
    jr c, findExponent
    ret ; CF=0
findExponentFound:
    scf ; CF=1
    ret

; Description: Parse the digits after the 'E' symbol in the inputBuf.
; Input: HL: pointer to EE digits
; Output: A: the exponent, in two's complement form
; Destroys: A, BC, DE, HL
parseExponent:
    ld b, 0 ; B=exponentValue
    ld d, rpnfalse ; D=isEENeg=false
    ; Check for valid char
    ld a, (hl); A==NUL if end of string
    inc hl
    call isValidSignedDigit ; if valid: CF=1
    jr nc, parseExponentEnd
    ; Check for '-'
    cp signChar
    jr z, parseExponentSetSign
    jr parseExponentDigits
parseExponentSetSign:
    ld d, rpntrue ; D=isEENeg=true
    ld a, (hl)
    inc hl
parseExponentDigits:
    ; process the first digit if any, A==NUL if end of string
    call isValidUnsignedDigit ; if valid: CF=1
    jr nc, parseExponentEnd
    sub '0'
    add a, b
    ld b, a
    ; process the second digit if any
    ld a, (hl) ; second of 2 digits, A==NUL if end of string
    inc hl
    call isValidUnsignedDigit; if valid: CF=1
    jr nc, parseExponentEnd
    ld c, a ; C=save A
    ld a, b
    call multABy10
    ld b, a
    ld a, c ; A=restored C
    sub '0'
    add a, b
    ld b, a
    ; [[fallthrough]]
parseExponentEnd:
    ld a, d ; A=isEENeg
    or a ; if isEENeg: ZF=0
    ld a, b ; A=exponentValue
    ret z
    neg
    ret

; Description: Multiply A by 10.
; Input: A
; Output: A
; Destroys: none
multABy10:
    push bc
    add a, a
    ld c, a ; C=2*A
    add a, a
    add a, a ; A=8*A
    add a, c ; A=10*A
    pop bc
    ret

; Description: Add the exponent in A to the floatBuf exponent.
; Input: A: EE exponent parsed from inputBuf
; Output: (floatBuf exponent) += A
; Destroys: A, B, HL
addExponent:
    ld b, a
    ld hl, floatBufExp
    ld a, (hl)
    sub $80
    add a, b ; 2's complement
    add a, $80
    ld (hl), a
    ret

;-----------------------------------------------------------------------------

; Description: Check if the character in A is a valid floating point digit
; which may be in scientific notation ('0' to '9', '-', '.', and 'E').
; Input: A: character
; Output: CF=1 if valid, 0 if not
; Destroys: none
isValidScientificDigit:
    cp Lexponent
    jr z, isValidDigitTrue
    ; [[fallthrough]]

; Description: Check if the character in A is a valid floating point digit ('0'
; to '9', '-', '.').
; Input: A: character
; Output: CF=1 if valid, 0 if not
; Destroys: none
isValidFloatDigit:
    cp '.'
    jr z, isValidDigitTrue

; Description: Check if the character in A is a valid signed integer ('0' to
; '9', '-').
; Input: A: character
; Output: CF=1 if valid, 0 if not
; Destroys: none
isValidSignedDigit:
    cp signChar ; '-'
    jr z, isValidDigitTrue
    ; [[fallthrough]]

; Description: Check if the character in A is a valid unsigned integer ('0' to
; '9').
; Input: A: character
; Output: CF=1 if valid, 0 if not
; Destroys: none
isValidUnsignedDigit:
    cp '0' ; if A<'0': CF=1
    jr c, isValidDigitFalse
    cp ':' ; if A<='9': CF=1
    ret c
    ; [[fallthrough]]

isValidDigitFalse:
    or a ; CF=0
    ret
isValidDigitTrue:
    scf ; CF=1
    ret

;-----------------------------------------------------------------------------

; Description: Set the complex delimiter to the character encoded by A. There
; are 3 complex number delimiters: LimagI (RECT), Langle (PRAD), Ldegree
; (PDEG). This routine converts them to other delimiters depending on the value
; of the targetDelimiter.
;
; The algorithm is as follows:
; - if delimiter==LimagI:
;     - if targetDelimiter==LimagI: do nothing
;     - if targetDelimiter==targetDelimiter
; - if delimiter in (Langle, Ldegree):
;     - if targetDelimiter==LimagI: delimiter=LimagI
;     - if targetDelimiter==(Langle,Ldegree): toggle to the other
; - if no delimiter: do nothing
;
; Input:
;   - A: targetDelimiter
;   - inputBuf
; Output:
;   - inputBuf updated
;   - CF: 1 if complex delimiter found, 0 if not found
; Destroys: A, BC, HL
; Preserves: DE
SetComplexDelimiter:
    ld c, a ; C=targetDelimiter
    ld hl, inputBuf
    ld b, (hl) ; B=len
    inc hl ; skip len byte
    ; Check for len==0
    ld a, b
    or a ; CF=0
    ret z
setComplexDelimiterLoop:
    ; Find the complex delimiter, if any
    ld a, (hl)
    inc hl
    cp LimagI
    jr z, setComplexDelimiterFromImagI
    cp Langle
    jr z, setComplexDelimiterFromAngle
    cp Ldegree
    jr z, setComplexDelimiterFromDegree
    ; Loop until end of buffer
    djnz setComplexDelimiterLoop
    or a; CF=0
    ret
setComplexDelimiterFromImagI:
    dec hl
    ld a, c
    jr setComplexDelimiterToTarget
setComplexDelimiterFromDegree:
    dec hl
    ld a, c ; A=targetDelimiter
    cp LimagI
    jr z, setComplexDelimiterToTarget
    ld a, Langle ; toggle
    jr setComplexDelimiterToTarget
setComplexDelimiterFromAngle:
    dec hl
    ld a, c ; A=targetDelimiter
    cp LimagI
    jr z, setComplexDelimiterToTarget
    ld a, Ldegree ; toggle
setComplexDelimiterToTarget:
    ld (hl), a
    scf
    ret

; Description: Find the location of the complex number delimiter (LimagI,
; Langle, or Ldegree).
; Input: HL: pointer to floating point number C-string
; Output:
;   - CF: 1 if complex number delimiter found, 0 otherwise
;   - A: delimiter char (LimagI, Langle, or Ldegree)
;   - HL: pointer to character after the delimiter
; Destroys: A, HL
findComplexDelimiter:
    ld a, (hl)
    inc hl
    call isComplexDelimiterPageOne ; if delimiter: ZF=1
    jr z, findComplexDelimiterFound
    call isValidScientificDigit ; if valid: CF=1
    jr c, findComplexDelimiter
    ret ; CF=0
findComplexDelimiterFound:
    scf ; CF=1
    ret

; Description: Check if complex delimiter exists in the given Pascal string.
; Input: HL: pointer to pascal string
; Output: CF=1 if complex, 0 otherwise
checkComplexDelimiterP:
    ld a, (hl) ; A=len
    inc hl
    or a ; ZF=0 if len==0; CF=0
    ret z
    ld b, a
checkComplexDelimiterPLoop:
    ld a, (hl)
    inc hl
    call isComplexDelimiterPageOne
    jr z, checkComplexDelimiterPFound
    djnz checkComplexDelimiterPLoop
    or a ; CF=0
    ret
checkComplexDelimiterPFound:
    scf ; CF=1
    ret

; Description: Return ZF=1 if A is a complex number delimiter. Same as
; isComplexDelimiter().
; Input: A: char
; Output: ZF=1 if delimiter
; Destroys: none
isComplexDelimiterPageOne:
    cp LimagI
    ret z
    cp Langle
    ret z
    cp Ldegree
    ret
