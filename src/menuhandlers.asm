;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Predefined Menu handlers.
;-----------------------------------------------------------------------------

; Description: Null handler. Does nothing.
; Input:
;   A: nodeId of the select menu item (ignored)
;   HL: pointer to MenuNode that was activated (ignored)
mNullHandler: ; do nothing
    ret

; Description: Null handler. Does nothing.
; Input:
;   A: nodeId of the select menu item (ignored)
;   HL: pointer to MenuNode that was activated (ignored)
mNotYetHandler:
    ld a, errorCodeNotYet
    jp setHandlerCode

; Description: General handler for menu nodes of type "MenuGroup". Selecting
; this should cause the menuGroupId to be set to this item, and the
; menuStripIndex to be set to 0
; Input:
;   A: nodeId of the select menu item
;   HL: pointer to MenuNode that was activated (ignored)
; Output: (menuGroupId) and (menuStripIndex) updated
; Destroys: A
mGroupHandler:
    ld (menuGroupId), a
    xor a
    ld (menuStripIndex), a
    set dirtyFlagsMenu, (iy + dirtyFlags)
    ret

;-----------------------------------------------------------------------------
; Handlers for the various menu nodes generated by compilemenu.py.
;-----------------------------------------------------------------------------

mHelpHandler:
    ld b, 0 ; B = pageNumber
mHelpHandlerLoop:
    ld a, b ; A = pageNumber
    call displayPage
mHelpHandlerGetKey:
    ; The SDK docs say that GetKey() destroys only A, DE, HL. But it looks like
    ; BC also gets destroyed if 2ND QUIT is pressed.
    push bc
    bcall(_GetKey) ; pause for key
    pop bc
    res onInterrupt, (IY+onFlags) ; reset flag set by ON button

mHelpHandlerCheckExit:
    or a ; A == ON
    jr z, mHelpHandlerExit
    cp kClear ; A == CLEAR
    jr z, mHelpHandlerExit
    cp kMath ; A == MATH
    jr z, mHelpHandlerExit
    cp kLeft ; A == LEFT
    jr z, mHelpHandlerPrevPageMaybe
    cp kUp ; A == UP
    jr z, mHelpHandlerPrevPageMaybe
    cp kQuit ; 2ND QUIT
    jp z, mainExit
    jr mHelpHandlerNextPage ; everything else to the next page

mHelpHandlerPrevPageMaybe:
    ; go to prev page if not already at page 0
    ld a, b
    or a
    jr z, mHelpHandlerGetKey
mHelpHandlerPrevPage:
    dec b
    jr mHelpHandlerLoop

mHelpHandlerNextPage:
    ; any other key goes to the next the page
    inc b
    ld a, b
    cp helpPageCount
    jr nz, mHelpHandlerLoop
mHelpHandlerExit:
    ; force rerendering of normal calculator display
    bcall(_ClrLCDFull)
    ld a, $FF
    ld (iy + dirtyFlags), a ; set all dirty flags
    call initDisplay
    call initMenu
    ret

; Description: Display the help page given by pageNumber in A.
; Input: A: pageNumber
; Destroys: none
displayPage:
    push af
    push bc
    push de
    push hl

    bcall(_ClrLCDFull)
    ld hl, 0
    ld (PenCol), hl

    ; Get the string for page A, and display it.
    ld hl, helpPages ; HL = (char**)
    call getString
    call vPutS

    pop hl
    pop de
    pop bc
    pop af
    ret

; Array of (char*) pointers to C-strings.
helpPageCount equ 4
helpPages:
    .dw msgHelpPage1
    .dw msgHelpPage2
    .dw msgHelpPage3
    .dw msgHelpPage4

msgHelpPage1:
    .db escapeLargeFont, "RPN83P", Lenter
    .db escapeSmallFont, "v0.4.0 (2023", Shyphen, "08", Shyphen, "15)", Senter
    .db "(c) 2023  Brian T. Park", Senter
    .db Senter
    .db "An RPN calculator for the", Senter
    .db "TI", Shyphen, "83 Plus and TI", Shyphen, "84 Plus", Senter
    .db "inspired by the HP", Shyphen, "42S.", Senter
    .db Senter
    .db SlBrack, "1/4", SrBrack, " Any key to continue...", Senter
    .db 0

msgHelpPage2:
    .db escapeLargeFont, "Stack Operation", Lenter
    .db escapeSmallFont, Senter
    .db "R", LdownArrow, " :  (", Senter
    .db "X", Sleft, Sconvert, "Y", ":  )", Senter
    .db "LastX", ":  2ND  ANS", Senter
    .db "R", LupArrow, " :  STK  R", LupArrow, Senter
    .db Senter
    .db Senter
    .db SlBrack, "2/4", SrBrack, " Any key to continue...", Senter
    .db 0

msgHelpPage3:
    .db escapeLargeFont, "Input Editing", Lenter
    .db escapeSmallFont, Senter
    .db "EE:  2ND EE or ,", Senter
    .db "+/-:  (-)", Senter
    .db "<-:  DEL", Senter
    .db "ClrX:  CLEAR", Senter
    .db Senter
    .db Senter
    .db SlBrack, "3/4", SrBrack, " Any key to continue...", Senter
    .db 0

msgHelpPage4:
    .db escapeLargeFont, "Menu Navigation", Lenter
    .db escapeSmallFont, Senter
    .db "Menu Home:  MATH", Senter
    .db "Menu Prev:  UP", Senter
    .db "Menu Next:  DOWN", Senter
    .db "Menu Back:  ON", Senter
    .db "Quit App:  2ND QUIT", Senter
    .db Senter
    .db SlBrack, "4/4", SrBrack, " Any key to return.", Senter
    .db 0

;-----------------------------------------------------------------------------
; Children nodes of MATH menu.
;-----------------------------------------------------------------------------

; mCubeHandler(X) -> X^3
; Description: Calculate X^3.
mCubeHandler:
    call closeInputAndRecallX
    bcall(_Cube)
    jp replaceX

; mCubeRootHandler(X) -> X^(1/3)
; Description: Calculate the cubic root of X. The SDK documentation has the OP1
; and OP2 flipped.
mCubeRootHandler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2=X
    bcall(_OP1Set3) ; OP1=3
    bcall(_XRootY) ; OP2^(1/OP1)
    jp replaceX

; mAtan2Handler(Y, X) -> atan2(Y + Xi)
;
; Description: Calculate the angle of the (Y, X) number in complex plane.
; Use bcall(_RToP) instead of bcall(_ATan2) because ATan2 does not seem produce
; the correct results. There must be something wrong with the documentation.
; (It turns out that the documentation does not describe the necessary values
; of the D register which must be set before calling this. Normally D should be
; set to 0. See https://wikiti.brandonw.net/index.php?title=83Plus:BCALLs:40D8
; for more details. Although that page refers to ATan2Rad(), I suspect a
; similar thing is happening for ATan2().)
;
; The real part (i.e. x-axis) is assumed to be entered first, then the
; imaginary part (i.e. y-axis). They becomes stored in the RPN stack variables
; with X and Y flipped, which is bit confusing.
mAtan2Handler:
    call closeInputAndRecallXY ; OP1=Y=real; OP2=X=imaginary
    bcall(_RToP) ; complex to polar
    bcall(_OP2ToOP1) ; OP2 contains the angle with range of (-pi, pi]
    jp replaceXY

;-----------------------------------------------------------------------------

; Alog2(X) = 2^X
mAlog2Handler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2 = X
    bcall(_OP1Set2) ; OP1 = 2
    bcall(_YToX) ; OP1 = 2^X
    jp replaceX

; Log2(X) = log_base_2(X) = log(X)/log(2)
mLog2Handler:
    call closeInputBuf
    bcall(_OP1Set2) ; OP2 = 2
    bcall(_LnX) ; OP1 = ln(2)
    bcall(_PushRealO1); FPS = ln(2)
    call rclX ; OP1 = X
    bcall(_LnX) ; OP1 = ln(X)
    bcall(_PopRealO2) ; OP2 = ln(2)
    bcall(_FPDiv) ; OP1 = ln(X) / ln(2)
    jp replaceX

; LogBase(Y, X) = log_base_X(Y) = log(Y)/log(X)
mLogBaseHandler:
    call closeInputAndRecallX
    bcall(_LnX) ; OP1 = ln(X)
    bcall(_PushRealO1); FPS = ln(X)
    call rclY ; OP1 = Y
    bcall(_LnX) ; OP1 = ln(Y)
    bcall(_PopRealO2) ; OP2 = ln(X)
    bcall(_FPDiv) ; OP1 = ln(Y) / ln(X)
    jp replaceXY

;-----------------------------------------------------------------------------
; Children nodes of NUM menu.
;-----------------------------------------------------------------------------

; mPercentHandler(Y, X) -> (Y, Y*(X/100))
; Description: Calculate the X percent of Y.
mPercentHandler:
    call closeInputAndRecallX
    call op2Set100
    bcall(_FPDiv)
    bcall(_OP1ToOP2)
    call rclY
    bcall(_FPMult)
    jp replaceX

; mPercentChangeHandler(Y, X) -> (Y, 100*(X-Y)/Y)
; Description: Calculate the change from Y to X as a percentage of Y. The
; resulting percentage can be given to the '%' menu key to get the delta
; change, then the '+' command will retrieve the original X.
mPercentChangeHandler:
    call closeInputBuf
    call rclY
    bcall(_OP1ToOP2) ; OP2 = Y
    bcall(_PushRealO1) ; FPS = Y
    call rclX ; OP1 = X
    bcall(_FPSub) ; OP1 = X - Y
    bcall(_PopRealO2) ; OP2 = Y
    bcall(_FPDiv) ; OP1 = (X-Y)/Y
    call op2Set100
    bcall(_FPMult) ; OP1 = 100*(X-Y)/Y
    jp replaceX

;-----------------------------------------------------------------------------

; Description: Implement the Euclidean algorithm for the Greatest Common
; Divisor (GCD) as described in
; https://en.wikipedia.org/wiki/Euclidean_algorithm:
;
; function gcd(a, b)
;    while b != 0
;        t := b
;        b := a mod b
;        a := t
;    return a
;
; TODO: To reduce code size and programming time, this uses the TI-OS floating
; point operations to calculate (a mod b). It would probably be a LOT faster to
; use native Z-80 assembly to implement the (a mod b). However, that requires
; writing an integer division routine that takes a 32-bit and a 16-bit
; arguments, producing a 32-bit result. It's probably available somewhere on
; the internet, but I'm going to punt on that for now.
mGcdHandler:
    call closeInputBuf
    call validatePosIntGcdLcm
    call gcdOp1Op2 ; OP1 = gcd()
    jp replaceXY ; X = OP1

; Description: Validate that X and Y are positive (> 0) integers. Calls
; ErrDomain exception upon failure.
; Output:
;   - OP1 = Y
;   - OP2 = X
validatePosIntGcdLcm:
    call rclX
    bcall(_CkOP1FP0)
    jr z, validatePosIntGcdLcmError
    bcall(_CkPosInt) ; if OP1 >= 0: ZF=1
    jr nz, validatePosIntGcdLcmError
    bcall(_OP1ToOP2) ; OP2=X=b
    call rclY ; OP1=Y=a
    bcall(_CkOP1FP0) ; if OP1 >= 0: ZF=1
    jr z, validatePosIntGcdLcmError
    bcall(_CkPosInt)
    ret z
validatePosIntGcdLcmError:
    bjump(_ErrDomain) ; throw exception

; Description: Calculate the Great Common Divisor.
; Input: OP1, OP2
; Output: OP1 = GCD(OP1, OP2)
; Destroys: OP1, OP2, OP3
gcdOp1Op2:
    bcall(_CkOP2FP0) ; while b != 0
    ret z
    bcall(_PushRealO2) ; t = b
    call modOp1Op2 ; (a mod b)
    bcall(_OP1ToOP2) ; b = (a mod b)
    bcall(_PopRealO1) ; a = t
    jr gcdOp1Op2

; Description: Calculate the Lowest Common Multiple using the following:
; LCM(Y, X) = Y * X / GCD(Y, X)
;           = Y * (X / GCD(Y,X))
mLcmHandler:
    call closeInputBuf
    call validatePosIntGcdLcm

    bcall(_PushRealO1) ; FPS = OP1 = Y
    bcall(_PushRealO2) ; FPS = OP2 = X
    call gcdOp1Op2 ; OP1 = gcd()
    bcall(_OP1ToOP2) ; OP2 = gcd()
    bcall(_PopRealO1) ; OP1 = X
    bcall(_FPDiv) ; OP1 = X / gcd
    bcall(_PopRealO2) ; OP2 = Y
    bcall(_FPMult) ; OP1 = Y * (X / gcd)

    jp replaceXY ; X = lcm(X, Y)

;-----------------------------------------------------------------------------

; Description: Determine if the integer in X is a prime number and returns 1 if
; prime, or 0 if not a prime. X must be in the range of [2, 2^32-1]. The TI-OS
; floating point operations can probably handle larger integers, but
; restricting the range to be < 2^32 will make it easier to rewrite the
; algorithm using Z-80 integer operations in the future.
;
; This algorithm uses the fact that every prime above 3 is of the form (6n-1)
; or (6n+1), where n=1,2,3,... It checks candidate divisors from 5 to sqrt(X),
; in steps of 6, checking whether (6n-1) or (6n+1) divides into X. If the
; candidate divides into X, X is *not* a prime. If the loop reaches the end of
; the iteration, then no prime factor was found, so X is a prime.
;
; Benchmarks:
;   - 4001*4001: 15 seconds
;   - 10007*10009: 36 seconds
;   - 19997*19997: 72 seconds
; In other words, about 280 candidate-interval / second.
;
; TODO: Rewrite this using integer operations instead of floating point
; operations to make it a LOT faster.
;
; Input: X: Number to check
; Output:
;   - X=1 if prime
;   - X=prime factor, if not a prime
mPrimeHandler:
    call closeInputAndRecallX
mPrimeHandlerCheckZero:
    bcall(_CkOP1FP0)
    jp z, mPrimeHandlerError
mPrimeHandlerCheckPosInt:
    bcall(_CkPosInt) ; if OP1 >= 0: ZF=1
    jp nz, mPrimeHandlerError
mPrimeHandlerCheck32Bits:
    call op2Set2Pow32 ; if OP1 >= 2^32: CF=0
    bcall(_CpOP1OP2)
    jp nc, mPrimeHandlerError
mPrimeHandlerCheckOne:
    bcall(_OP2Set1) ; OP2 = 1
    bcall(_CpOP1OP2) ; if OP1==1: ZF=1
    jp z, mPrimeHandlerError
    bcall(_OP1ToOP4) ; save OP4 = X
mPrimeHandlerCheckTwo:
    bcall(_OP2Set2) ; OP2 = 2
    bcall(_CpOP1OP2) ; if OP1==2: ZF=1
    jp z, mPrimeHandlerYes
mPrimeHandlerCheckDivTwo:
    call mPrimeHandlerCheckDiv
    jr z, mPrimeHandlerNo
mPrimeHandlerCheckThree:
    bcall(_OP4ToOP1) ; OP1 = X
    bcall(_OP2Set3) ; OP2 = 3
    bcall(_CpOP1OP2) ; if OP1==3: ZF=1
    jr z, mPrimeHandlerYes
mPrimeHandlerCheckDivThree:
    call mPrimeHandlerCheckDiv
    jr z, mPrimeHandlerNo
mPrimeHandlerLoopSetup:
    ; start with candidate=5, first of the form (6k +/- 1)
    ; OP1=OP4=original X
    ; OP5=limit
    ; OP6=candidate
    bcall(_OP4ToOP1)
    bcall(_SqRoot) ; OP1 = sqrt(X)
    bcall(_RndGuard)
    bcall(_Trunc) ; OP1 = trunc(sqrt(X))
    bcall(_OP1ToOP5) ; OP5=limit
    bcall(_OP2Set5)
    bcall(_OP2ToOP6) ; OP6=candidate=5
    bcall(_RunIndicOn) ; enable run indicator
mPrimeHandlerLoop:
    ; Check if loop limit reached
    bcall(_OP6ToOP2) ; OP2=candidate
    bcall(_OP5ToOP1) ; OP1=limit
    bcall(_CpOP1OP2) ; if limit < candidate: CF=1
    jr c, mPrimeHandlerYes
    ; Check for ON/Break
    bit onInterrupt, (IY+onFlags)
    jr nz, mPrimeHandlerBreak
    ; Check (6n-1)
    bcall(_OP4ToOP1) ; OP1 = X
    bcall(_OP6ToOP2) ; OP2 = candidate
    call mPrimeHandlerCheckDiv
    jr z, mPrimeHandlerNo
    ; Check (6n+1)
    bcall(_OP6ToOP1) ; OP1 = candidate
    bcall(_OP2Set2)
    bcall(_FPAdd) ; OP1+=2
    bcall(_OP1ToOP6) ; candidate+=2
    bcall(_OP1ToOP2) ; OP2=candidate
    bcall(_OP4ToOP1) ; OP1=X
    call mPrimeHandlerCheckDiv
    jr z, mPrimeHandlerNo
    ; OP6 += 4
    bcall(_OP6ToOP1) ; OP1 = candidate
    bcall(_OP2Set4) ; OP2=4
    bcall(_FPAdd) ; OP1+=4
    bcall(_OP1ToOP6) ; candidate+=4
    jr mPrimeHandlerLoop
mPrimeHandlerNo:
    bcall(_OP2ToOP1)
    jr mPrimeHandlerEnd
mPrimeHandlerYes:
    bcall(_OP1Set1)
mPrimeHandlerEnd:
    bcall(_RunIndicOff) ; disable run indicator
    jp replaceX

; Description: Determine if OP2 is an integer factor of OP1.
; Output: ZF=1 if OP2 is a factor, 0 if not
; Destroys: OP1
mPrimeHandlerCheckDiv:
    bcall(_FPDiv) ; OP1 = OP1/OP2
    bcall(_Frac) ; convert to frac part, preserving sign
    bcall(_CkOP1FP0) ; if OP1 == 0: ZF=1
    ret

mPrimeHandlerError:
    bjump(_ErrDomain) ; throw exception

mPrimeHandlerBreak:
    bcall(_RunIndicOff) ; disable run indicator
    res onInterrupt, (IY+onFlags)
    bjump(_ErrBreak) ; throw exception

;-----------------------------------------------------------------------------

; mAbsHandler(X) -> Abs(X)
mAbsHandler:
    call closeInputAndRecallX
    bcall(_ClrOP1S) ; clear sign bit of OP1
    jp replaceX

; mSignHandler(X) -> Sign(X)
mSignHandler:
    call closeInputAndRecallX
    bcall(_CkOP1FP0) ; check OP1 is float 0
    jr z, mSignHandlerSetZero
    bcall(_CkOP1Pos) ; check OP1 > 0
    jr z, mSignHandlerSetOne
mSignHandlerSetNegOne:
    bcall(_OP1Set1)
    bcall(_InvOP1S)
    jr mSignHandlerStoX
mSignHandlerSetOne:
    bcall(_OP1Set1)
    jr mSignHandlerStoX
mSignHandlerSetZero:
    bcall(_OP1Set0)
mSignHandlerStoX:
    jp replaceX

; Description: Calculate (Y mod X), where Y and X could be floating point
; numbers. There does not seem to be a built-in function to calculator this, so
; it is implemented as (Y mod X) = Y - X*floor(Y/X).
; Destroys: OP1, OP2, OP3
mModHandler:
    call closeInputAndRecallXY ; OP2 = X; OP1 = Y
    call modOp1Op2 ; OP1 = (OP1 mod OP2)
    jp replaceXY

; Description: Internal helper routine to calculate OP1 = (OP1 mod OP2) = OP1 -
; OP2 * floor(OP1/OP2). Used by mModHandler and mGcdHandler. There does not
; seem to be a built-in function to calculate this.
; Destroys: OP1, OP2, OP3
modOp1Op2:
    bcall(_PushRealO1) ; FPS = OP1
    bcall(_PushRealO2) ; FPS = OP2
    bcall(_FPDiv) ; OP1 = OP1/OP2
    bcall(_Intgr) ; OP1 = floor(OP1/OP2)
    bcall(_PopRealO2) ; OP2 = OP2
    bcall(_FPMult) ; OP1 = floor(OP1/OP2) * OP2
    bcall(_OP1ToOP2) ; OP2 = floor(OP1/OP2) * OP2
    bcall(_PopRealO1) ; OP1 = OP1
    bcall(_FPSub) ; OP1 = OP1 - floor(OP1/OP2) * OP2
    bcall(_RndGuard) ; force integer results if OP1 and OP2 were integers
    ret

mMinHandler:
    call closeInputAndRecallXY
    bcall(_Min)
    jp replaceXY

mMaxHandler:
    call closeInputAndRecallXY
    bcall(_Max)
    jp replaceXY

;-----------------------------------------------------------------------------

mIntPartHandler:
    call closeInputAndRecallX
    bcall(_Trunc) ; convert to int part, truncating towards 0.0, preserving sign
    jp replaceX

mFracPartHandler:
    call closeInputAndRecallX
    bcall(_Frac) ; convert to frac part, preserving sign
    jp replaceX

mFloorHandler:
    call closeInputAndRecallX
    bcall(_Intgr) ; convert to integer towards -Infinity
    jp replaceX

mCeilHandler:
    call closeInputAndRecallX
    bcall(_InvOP1S) ; invert sign
    bcall(_Intgr) ; convert to integer towards -Infinity
    bcall(_InvOP1S) ; invert sign
    jp replaceX

mNearHandler:
    call closeInputAndRecallX
    bcall(_Int) ; round to nearest integer, irrespective of sign
    jp replaceX

;-----------------------------------------------------------------------------
; Children nodes of PROB menu.
;-----------------------------------------------------------------------------

; Calculate the Permutation function:
; P(y, x) = P(n, r) = n!/(n-r)! = n(n-1)...(n-r+1)
;
; TODO: (n,r) are limited to [0.255]. It should be relatively easy to extended
; the range to [0,65535].
mPermHandler:
    call closeInputBuf
    call validatePermComb

    ; Do the calculation. Set initial Result to 1 so that P(N, 0) = 1.
    bcall(_OP1Set1)
    ld a, e ; A = X
    or a
    jr z, mPermHandlerEnd
    ; Loop x times, multiple by (y-i)
    ld b, a ; B = X, C = Y
mPermHandlerLoop:
    push bc
    ld l, c ; L = C = Y
    ld h, 0 ; HL = Y
    bcall(_SetXXXXOP2)
    bcall(_FPMult)
    pop bc
    dec c
    djnz mPermHandlerLoop
mPermHandlerEnd:
    jp replaceXY

;-----------------------------------------------------------------------------

; Calculate the Combintation function:
; C(y, x) = C(n, r) = n!/(n-r)!/r! = n(n-1)...(n-r+1)/(r)(r-1)...(1).
;
; TODO: (n,r) are limited to [0.255]. It should be relatively easy to extended
; the range to [0,65535].
;
; TODO: This algorithm below is a variation of the algorithm used for P(n,r)
; above, with a division operation inside the loop that corresponds to each
; term of the `r!` divisor. However, the division can cause intermediate result
; to be non-integral. Eventually the final answer will be an integer, but
; that's not guaranteed until the end of the loop. I think it should be
; possible to rearrange the order of these divisions so that the intermediate
; results are always integral.
mCombHandler:
    call closeInputBuf
    call validatePermComb

    ; Do the calculation. Set initial Result to 1 C(N, 0) = 1.
    bcall(_OP1Set1)
    ld a, e ; A = X
    or a
    jr z, mCombHandlerEnd
    ; Loop X times, multiple by (Y-i), divide by i.
    ld b, a ; B = X, C = Y
mCombHandlerLoop:
    push bc
    ld l, c ; L = C = Y
    ld h, 0 ; HL = Y
    bcall(_SetXXXXOP2) ; OP2 = Y
    bcall(_FPMult)
    pop bc
    push bc
    ld l, b ; L = B = X
    ld h, 0 ; HL = X
    bcall(_SetXXXXOP2) ; OP2 = X
    bcall(_FPDiv)
    pop bc
    dec c
    djnz mCombHandlerLoop
mCombHandlerEnd:
    jp replaceXY

;-----------------------------------------------------------------------------

; Validate the n and r parameters of P(n,r) and C(n,r):
;   - n, r are integers in the range of [0,255]
;   - n >= r
; Output:
;   - C: Y
;   - E: X
; Destroys: A, BC, DE, HL
validatePermComb:
    ; Validate X
    call rclX
    call validatePermCombParam
    ; Validate Y
    call rclY
    call validatePermCombParam

    ; Convert X and Y into integers
    bcall(_ConvOP1) ; OP1 = Y
    push de ; save Y
    bcall(_OP1ToOP2) ; OP2 = Y
    call rclX
    bcall(_ConvOP1) ; E = X
    pop bc ; C = Y
    ; Check that Y >= X
    ld a, c ; A = Y
    cp e ; Y - X
    jr c, validatePermCombError
    ret

; Validate OP1 is an integer in the range of [0, 255].
validatePermCombParam:
    bcall(_CkPosInt) ; if OP1 >= 0: ZF=1
    jr nz, validatePermCombError
    ld hl, 256
    bcall(_SetXXXXOP2) ; OP2=256
    bcall(_CpOP1OP2)
    ret c ; ok if OP1 < 255
validatePermCombError:
    bjump(_ErrDomain) ; throw exception

;-----------------------------------------------------------------------------

; mFactorialHandler(X) -> X!
; Description: Calculate the factorial of X.
mFactorialHandler:
    call closeInputAndRecallX
    bcall(_Factorial)
    jp replaceX

;-----------------------------------------------------------------------------

; mRandomHandler() -> rand()
; Description: Generate a random number [0,1) into the X register.
mRandomHandler:
    call closeInputBuf
    bcall(_Random)
    call liftStackNonEmpty
    call stoX
    ret

;-----------------------------------------------------------------------------

; mRandomSeedHandler(X) -> None
; Description: Set X as the Random() seed.
mRandomSeedHandler:
    call closeInputAndRecallX
    bcall(_StoRand)
    ret

;-----------------------------------------------------------------------------
; Children nodes of UNIT menu.
;-----------------------------------------------------------------------------

mFToCHandler:
    call closeInputAndRecallX
    ld a, 32
    bcall(_SetXXOP2) ; OP2 = 32
    bcall(_FPSub) ; OP1 = X - 32
    ld a, $18
    bcall(_OP2SetA) ; OP2 = 1.8
    bcall(_FPDiv) ; OP1 = (X - 32) / 1.8
    jp replaceX

mCToFHandler:
    call closeInputAndRecallX
    ld a, $18
    bcall(_OP2SetA) ; OP2 = 1.8
    bcall(_FPMult) ; OP1 = X * 1.8
    ld a, 32
    bcall(_SetXXOP2) ; OP2 = 32
    bcall(_FPAdd) ; OP1 = 1.8*X + 32
    jp replaceX

mInhgToHpaHandler:
    call closeInputAndRecallX
    call op2SetHpaPerInhg
    bcall(_FPMult)
    jp replaceX

mHpaToInhgHandler:
    call closeInputAndRecallX
    call op2SetHpaPerInhg
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------

mMiToKmHandler:
    call closeInputAndRecallX
    call op2SetKmPerMi
    bcall(_FPMult)
    jp replaceX

mKmToMiHandler:
    call closeInputAndRecallX
    call op2SetKmPerMi
    bcall(_FPDiv)
    jp replaceX

mFtToMHandler:
    call closeInputAndRecallX
    call op2SetMPerFt
    bcall(_FPMult)
    jp replaceX

mMToFtHandler:
    call closeInputAndRecallX
    call op2SetMPerFt
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------

mInToCmHandler:
    call closeInputAndRecallX
    call op2SetCmPerIn
    bcall(_FPMult)
    jp replaceX

mCmToInHandler:
    call closeInputAndRecallX
    call op2SetCmPerIn
    bcall(_FPDiv)
    jp replaceX

mMilToMicronHandler:
    call closeInputAndRecallX
    call op2SetCmPerIn
    bcall(_FPMult)
    call op2Set10
    bcall(_FPMult)
    jp replaceX

mMicronToMilHandler:
    call closeInputAndRecallX
    call op2SetCmPerIn
    bcall(_FPDiv)
    call op2Set10
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------

mLbsToKgHandler:
    call closeInputAndRecallX
    call op2SetKgPerLbs
    bcall(_FPMult)
    jp replaceX

mKgToLbsHandler:
    call closeInputAndRecallX
    call op2SetKgPerLbs
    bcall(_FPDiv)
    jp replaceX

mOzToGHandler:
    call closeInputAndRecallX
    call op2SetGPerOz
    bcall(_FPMult)
    jp replaceX

mGToOzHandler:
    call closeInputAndRecallX
    call op2SetGPerOz
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------

mGalToLHandler:
    call closeInputAndRecallX
    call op2SetLPerGal
    bcall(_FPMult)
    jp replaceX

mLToGalHandler:
    call closeInputAndRecallX
    call op2SetLPerGal
    bcall(_FPDiv)
    jp replaceX

mFlozToMlHandler:
    call closeInputAndRecallX
    call op2SetMlPerFloz
    bcall(_FPMult)
    jp replaceX

mMlToFlozHandler:
    call closeInputAndRecallX
    call op2SetMlPerFloz
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------

mCalToKjHandler:
    call closeInputAndRecallX
    call op2SetKjPerKcal
    bcall(_FPMult)
    jp replaceX

mKjToCalHandler:
    call closeInputAndRecallX
    call op2SetKjPerKcal
    bcall(_FPDiv)
    jp replaceX

mHpToKwHandler:
    call closeInputAndRecallX
    call op2SetKwPerHp
    bcall(_FPMult)
    jp replaceX

mKwToHpHandler:
    call closeInputAndRecallX
    call op2SetKwPerHp
    bcall(_FPDiv)
    jp replaceX

;-----------------------------------------------------------------------------
; Children nodes of CONV menu.
;-----------------------------------------------------------------------------

mRToDHandler:
    call closeInputAndRecallX
    bcall(_RToD) ; RAD to DEG
    jp replaceX

mDToRHandler:
    call closeInputAndRecallX
    bcall(_DToR) ; DEG to RAD
    jp replaceX

; Polar to Rectangular
; Input:
;   - stY: r
;   - stX: theta
; Output:
;   - stY: x
;   - stX: y
mPToRHandler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2 = stX = theta
    call rclY ; OP1 = stY = r
    bcall(_PToR) ; OP1 = x; OP2 = y (?)
    jp replaceXYWithOP2OP1 ; stX=OP2=y; stY=OP1=x

; Rectangular to Polar
; Input:
;   - stY: x
;   - stX: y
; Output:
;   - stY: r
;   - stX: theta
mRtoPHandler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2 = stX = y
    call rclY ; OP1 = stY = x
    bcall(_RToP) ; OP1 = r; OP2 = theta (?)
    jp replaceXYWithOP2OP1 ; stX=OP2=theta; stY=OP1=r

;-----------------------------------------------------------------------------

; HR(hh.mmss) = int(hh.mmss) + int(mm.ss)/60 + int(ss.nnnn)/3600
; Destroys: OP1, OP2, OP3, OP4 (temp)
mHmsToHrHandler:
    call closeInputAndRecallX

    ; Sometimes, the internal floating point value is slightly different than
    ; the displayed value due to rounding errors. For example, a value
    ; displayed as `10` (e.g. `e^(ln(10))`) could actually be `9.9999999999xxx`
    ; internally due to rounding errors. This routine parses out the digits
    ; after the decimal point and interprets them as minutes (mm) and seconds
    ; (ss) components. Any rounding errors will cause incorrect results. To
    ; mitigate this, we round the X value to 10 digits to make sure that the
    ; internal value matches the displayed value.
    bcall(_RndGuard)

    ; Extract the whole 'hh' and push it into the FPS.
    bcall(_OP1ToOP4) ; OP4 = hh.mmss (save in temp)
    bcall(_Trunc) ; OP1 = int(hh.mmss)
    bcall(_PushRealO1) ; FPS = hh

    ; Extract the 'mm' and push it into the FPS.
    bcall(_OP4ToOP1) ; OP1 = hh.mmss
    bcall(_Frac) ; OP1 = .mmss
    call op2Set100
    bcall(_FPMult) ; OP1 = mm.ss
    bcall(_OP1ToOP4) ; OP4 = mm.ss
    bcall(_Trunc) ; OP1 = mm
    bcall(_PushRealO1) ; FPS = mm

    ; Extract the 'ss.nnn' part
    bcall(_OP4ToOP1) ; OP1 = mm.ssnnn
    bcall(_Frac) ; OP1 = .ssnnn
    call op2Set100
    bcall(_FPMult) ; OP1 = ss.nnn

    ; Reassemble in the form of `hh.nnn`.
    ; Extract ss.nnn/60
    bcall(_OP2Set60) ; OP2 = 60
    bcall(_FPDiv) ; OP1 = ss.nnn/60
    ; Extract mm/60
    bcall(_PopRealO2) ; OP1 = mm
    bcall(_FPAdd) ; OP1 = mm + ss.nnn/60
    bcall(_OP2Set60) ; OP2 = 60
    bcall(_FPDiv) ; OP1 = (mm + ss.nnn/60) / 60
    ; Extract the hh.
    bcall(_PopRealO2) ; OP1 = hh
    bcall(_FPAdd) ; OP1 = hh + (mm + ss.nnn/60) / 60

    jp replaceX

; HMS(hh.nnn) = int(hh + (mm + ss.nnn/100)/100 where
;   - mm = int(.nnn* 60)
;   - ss.nnn = frac(.nnn*60)*60
; Destroys: OP1, OP2, OP3, OP4 (temp)
mHrToHmsHandler:
    call closeInputAndRecallX

    ; Extract the whole hh.
    bcall(_OP1ToOP4) ; OP4 = hh.nnn (save in temp)
    bcall(_Trunc) ; OP1 = int(hh.nnn)
    bcall(_PushRealO1) ; FPS = hh

    ; Extract the 'mm' and push it into the FPS
    bcall(_OP4ToOP1) ; OP1 = hh.nnn
    bcall(_Frac) ; OP1 = .nnn
    bcall(_OP2Set60) ; OP2 = 60
    bcall(_FPMult) ; OP1 = mm.nnn
    bcall(_OP1ToOP4) ; OP4 = mm.nnn
    bcall(_Trunc) ; OP1 = mm
    bcall(_PushRealO1) ; FPS = mm

    ; Extract the 'ss.nnn' part
    bcall(_OP4ToOP1) ; OP1 = mm.nnn
    bcall(_Frac) ; OP1 = .nnn
    bcall(_OP2Set60) ; OP2 = 60
    bcall(_FPMult) ; OP1 = ss.nnn

    ; Reassemble in the form of `hh.mmssnnn`.
    ; Extract ss.nnn/100
    call op2Set100
    bcall(_FPDiv) ; OP1 = ss.nnn/100
    ; Extract mm/100
    bcall(_PopRealO2) ; OP1 = mm
    bcall(_FPAdd) ; OP1 = mm + ss.nnn/100
    call op2Set100
    bcall(_FPDiv) ; OP1 = (mm + ss.nnn/100) / 100
    ; Extract the hh.
    bcall(_PopRealO2) ; OP1 = hh
    bcall(_FPAdd) ; OP1 = hh + (mm + ss.nnn/100) / 100

    jp replaceX

;-----------------------------------------------------------------------------
; Children nodes of MODE menu.
;-----------------------------------------------------------------------------

mFixHandler:
    call closeInputBuf
    ld hl, mFixCallback
    ld (argHandler), hl
    ld hl, mFixName
    jr enableArgMode
mFixCallback:
    res fmtExponent, (iy + fmtFlags)
    res fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

mSciHandler:
    call closeInputBuf
    ld hl, mSciCallback
    ld (argHandler), hl
    ld hl, mSciName
    jr enableArgMode
mSciCallback:
    set fmtExponent, (iy + fmtFlags)
    res fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

mEngHandler:
    call closeInputBuf
    ld hl, mEngCallback
    ld (argHandler), hl
    ld hl, mEngName
    jr enableArgMode
mEngCallback:
    set fmtExponent, (iy + fmtFlags)
    set fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

; Description: Enter command argument input mode by prompting the user
; for a numerical parameter.
; Input: HL: argPrompt
; Output:
;   - argPrompt set with C string
;   - argBufSize set to 0
;   - rpnFlagsArgMode set
;   - dirtyFlagsInput set
; Destroys: A, HL
enableArgMode:
    ld (argPrompt), hl
    xor a
    ld (argBufSize), a
    set rpnFlagsArgMode, (iy + rpnFlags)
    set dirtyFlagsInput, (iy + dirtyFlags)
    set dirtyFlagsXLabel, (iy + dirtyFlags)
    ret

; Description: Save the (argValue) to (fmtDigits).
; Output:
;   - dirtyFlagsStack set
;   - dirtyFlagsFloatMode set
;   - fmtDigits updated
; Destroys: A
saveFormatDigits:
    set dirtyFlagsStack, (iy + dirtyFlags)
    set dirtyFlagsFloatMode, (iy + dirtyFlags)
    ld a, (argValue)
    cp 10
    jr c, saveFormatDigitsContinue
    ld a, $FF ; variable number of digits, not fixed
saveFormatDigitsContinue:
    ld (fmtDigits), a
    ret

;-----------------------------------------------------------------------------

mRadHandler:
    res trigDeg, (iy + trigFlags)
    set dirtyFlagsTrigMode, (iy + dirtyFlags)
    ret

mDegHandler:
    set trigDeg, (iy + trigFlags)
    set dirtyFlagsTrigMode, (iy + dirtyFlags)
    ret

;-----------------------------------------------------------------------------
; Children nodes of HYP menu.
;-----------------------------------------------------------------------------

mSinhHandler:
    call closeInputAndRecallX
    bcall(_SinH)
    jp replaceX

mCoshHandler:
    call closeInputAndRecallX
    bcall(_CosH)
    jp replaceX

mTanhHandler:
    call closeInputAndRecallX
    bcall(_TanH)
    jp replaceX

mAsinhHandler:
    call closeInputAndRecallX
    bcall(_ASinH)
    jp replaceX

mAcoshHandler:
    call closeInputAndRecallX
    bcall(_ACosH)
    jp replaceX

mAtanhHandler:
    call closeInputAndRecallX
    bcall(_ATanH)
    jp replaceX

;-----------------------------------------------------------------------------
; Children nodes of BASE menu.
;-----------------------------------------------------------------------------

mHexHandler:
    call closeInputBuf
    ld a, 16
    jr setBaseMode

mDecHandler:
    call closeInputBuf
    ld a, 10
    jr setBaseMode

mOctHandler:
    call closeInputBuf
    ld a, 8
    jr setBaseMode

mBinHandler:
    call closeInputBuf
    ld a, 2
    ; [[fallthrough]]

; Description: Set the (baseMode) to the value in A. Set dirty flag.
; Destroys: none
setBaseMode:
    set dirtyFlagsBaseMode, (iy + dirtyFlags)
    set dirtyFlagsStack, (iy + dirtyFlags)
    ld (baseMode), a
    ret

;-----------------------------------------------------------------------------

mBitwiseAndHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    call andU32U32 ; OP4 = OP4 AND OP3

    call convertU32ToOP1 ; OP1 = float(OP4)
    jp replaceXY

;-----------------------------------------------------------------------------

mBitwiseOrHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    call orU32U32 ; OP4 = OP4 OR OP3

    call convertU32ToOP1 ; OP1 = float(OP4)
    jp replaceXY

;-----------------------------------------------------------------------------

mBitwiseXorHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    call xorU32U32 ; OP4 = OP4 XOR OP3

    call convertU32ToOP1 ; OP1 = float(OP4)
    jp replaceXY

;-----------------------------------------------------------------------------

mBitwiseNotHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call notU32 ; OP3 = NOT(OP3)
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

;-----------------------------------------------------------------------------

mBitwiseNegHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call negU32 ; OP3 = NEG(OP3), 2's complement negation
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

;-----------------------------------------------------------------------------

mShiftLeftHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call shiftLeftU32 ; OP3 = (OP3 << 1)
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

mShiftRightHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call shiftRightLogicalU32 ; OP3 = (OP3 >> 1)
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

mRotateLeftHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call rotateLeftCircularU32; OP3 = rotLeftCircular(OP3)
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

mRotateRightHandler:
    call closeInputAndRecallX ; OP1=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(X)
    call rotateRightCircularU32; OP3 = rotRightCircular(OP3)
    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceX

;-----------------------------------------------------------------------------

mBitwiseAddHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    call addU32U32 ; OP4(X) += OP3(Y)

    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceXY

;-----------------------------------------------------------------------------

mBitwiseSubtHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    ex de, hl
    call subU32U32 ; OP3(Y) -= OP4(X)

    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceXY

;-----------------------------------------------------------------------------

mBitwiseMultHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)

    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)

    ld de, OP3
    call multU32U32 ; OP4(X) *= OP3(Y)

    call convertU32ToOP1 ; OP1 = float(OP3)
    jp replaceXY

;-----------------------------------------------------------------------------

; Description: Calculate bitwise x/y.
; Output:
;   - X=quotient
;   - remainder thrown away
mBitwiseDivHandler:
    call divHandlerCommon ; HL=quotient, BC=remainder
    call convertU32ToOP1 ; OP1 = quotient
    jp replaceXY

; Description: Calculate bitwise div(x, y) -> (y/x, y % x).
; Output:
;   - X=remainder
;   - Y=quotient
mBitwiseDiv2Handler:
    call divHandlerCommon ; HL=quotient, BC=remainder
    ; convert remainder into OP2
    push hl
    ld l, c
    ld h, b
    call convertU32ToOP1 ; OP1=remainder
    bcall(_OP1ToOP2) ; OP2=remainder
    ; convert quotient into OP1
    pop hl
    call convertU32ToOP1 ; OP1 = quotient
    ;
    jp replaceXYWithOP2OP1 ; Y=quotient, X=remainder

divHandlerCommon:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)
    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)
    call testU32
    jr nz,  divHandlerContinue
divHandlerDivByZero:
    bcall(_ErrDivBy0) ; throw 'Div By 0' exception
divHandlerContinue:
    ld hl, OP3 ; HL=dividend (Y)
    ld de, OP4 ; DE=divisor (X)
    ld bc, OP5 ; BC=remainder
    jp divU32U32 ; HL=OP3=quotient, DE=OP4=divisor, BC=OP5=remainder

;-----------------------------------------------------------------------------
; Children nodes of STK menu group (stack functions).
;-----------------------------------------------------------------------------

mStackRotUpHandler:
    call closeInputBuf
    jp rotUpStack

mStackRotDownHandler:
    jp handleKeyRotDown

mStackExchangeXYHandler:
    jp handleKeyExchangeXY

;-----------------------------------------------------------------------------
; Children nodes of CLR menu group (clear functions).
;-----------------------------------------------------------------------------

mClearRegsHandler:
    call closeInputBuf
    call clearRegs
    ld a, errorCodeRegsCleared
    jp setHandlerCode

mClearStackHandler:
    call closeInputBuf
    jp clearStack

mClearXHandler:
    call closeInputBuf
    res rpnFlagsLiftEnabled, (iy + rpnFlags) ; disable stack lift
    bcall(_OP1Set0)
    jp stoX
