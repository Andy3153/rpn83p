;-----------------------------------------------------------------------------
; MIT License
; Copyright (c) 2023 Brian T. Park
;
; Handlers for menu items.
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Predefined Menu handlers.
;-----------------------------------------------------------------------------

; Description: Null item handler. Does nothing.
; Input:
;   HL: pointer to MenuNode that was activated (ignored)
mNullHandler:
    ret

; Description: Handler for menu item which has not been implemented. Prints an
; "Err: Not Yet" error message.
; Input:
;   HL: pointer to MenuNode that was activated (ignored)
mNotYetHandler:
    ld a, errorCodeNotYet
    ld (handlerCode), a
    ret

; Description: Default handler for MenuGroup nodes. This handler currently does
; nothing. The 'chdir' functionality is now handled by dispatchMenuNode()
; because it needs to send an 'onExit' to the current node, and an 'onEnter'
; event to the selected node.
; Input:
;   HL: pointer to MenuNode that was activated (ignored)
;   CF: 0 indicates on 'onEnter' event into group; 1 indicates onExit event
;   from group
mGroupHandler:
    ret

;-----------------------------------------------------------------------------
; Handlers for the various menu nodes generated by compilemenu.py.
;-----------------------------------------------------------------------------

; Description: Show Help pages.
mHelpHandler:
    bcall(_ProcessHelp) ; use bcall() to invoke HELP handler on Flash Page 1
    ret

;-----------------------------------------------------------------------------
; Children nodes of MATH menu.
;-----------------------------------------------------------------------------

; Description: Calculate X^3.
mCubeHandler:
    call closeInputAndRecallUniversalX
    call universalCube
    jp replaceX

; Description: Calculate the cubic root of X, X^(1/3).
mCubeRootHandler:
    call closeInputAndRecallUniversalX
    call universalCubeRoot
    jp replaceX

; Description: Calculate the X root of Y, Y^(1/X).
mXRootYHandler:
    call closeInputAndRecallUniversalXY ; OP1=Y; OP2=X
    call universalXRootY
    jp replaceXY

; Description: Calculate the angle of the (X, Y) number in complex plane.
; Use bcall(_RToP) instead of bcall(_ATan2) because ATan2 does not seem produce
; the correct results. There must be something wrong with the documentation.
;
; (It turns out that the documentation does not describe the necessary values
; of the D register which must be set before calling this. Apparently the D
; register should be set to 0. See
; https://wikiti.brandonw.net/index.php?title=83Plus:BCALLs:40D8 for more
; details. Although that page refers to ATan2Rad(), I suspect a similar thing
; is happening for ATan2().)
;
; The imaginary part (i.e. y-axis) must be entered first, then the real part
; (i.e. x-axis). This order is consistent with the the `>POL` conversion
; function.
mAtan2Handler:
    call closeInputAndRecallXY ; OP1=Y=imaginary; OP2=X=real
    call op1ExOp2 ; OP1=X=real; OP2=Y=imaginary
    bcall(_RToP) ; complex to polar
    bcall(_OP2ToOP1) ; OP2 contains the angle with range of (-pi, pi]
    jp replaceXY

;-----------------------------------------------------------------------------

; Calculate e^x-1 without round off errors around x=0.
mExpMinusOneHandler:
    call closeInputAndRecallX
    bcall(_ExpMinusOne)
    jp replaceX

; Calculate ln(1+x) without round off errors around x=0.
mLnOnePlusHandler:
    call closeInputAndRecallX
    bcall(_LnOnePlus)
    jp replaceX

; Alog2(X) = 2^X
mAlog2Handler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2 = X
    bcall(_OP1Set2) ; OP1 = 2
    bcall(_YToX) ; OP1 = 2^X
    jp replaceX

; Log2(X) = log_base_2(X) = log(X)/log(2)
mLog2Handler:
    call closeInputAndRecallX
    bcall(_LnX) ; OP1 = ln(X)
    bcall(_PushRealO1) ; FPS=[ln(x)]
    bcall(_OP1Set2) ; OP1=2.0
    bcall(_LnX) ; OP1=ln(2.0)
    call op1ToOp2 ; OP2=ln(2.0)
    bcall(_PopRealO1) ; FPS=[]; OP1=ln(x)
    bcall(_FPDiv) ; OP1=ln(x)/ln(2)
    jp replaceX

; LogBase(Y, X) = log_base_X(Y)=log(Y)/log(X)
mLogBaseHandler:
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    bcall(_PushRealO2) ; FPS=[X]
    bcall(_LnX) ; OP1=ln(Y)
    call exchangeFPSOP1; FPS=[ln(Y)]; OP1=X
    bcall(_LnX) ; OP1=ln(X)
    call op1ToOp2 ; OP2=ln(X)
    bcall(_PopRealO1) ; FPS=[]; OP1=ln(Y)
    bcall(_FPDiv) ; OP1=ln(Y)/ln(X)
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
    call closeInputAndRecallXY ; OP1=Y; OP2=X
    bcall(_PushRealO1) ; FPS=[Y]
    bcall(_InvSub) ; OP1=X-Y
    bcall(_PopRealO2) ; FPS=[]; OP2=Y
    bcall(_FPDiv) ; OP1=(X-Y)/Y
    call op2Set100
    bcall(_FPMult) ; OP1=100*(X-Y)/Y
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
    call closeInputAndRecallXY
    call validatePosIntGcdLcm
    call gcdOp1Op2 ; OP1=gcd(OP1,OP2)
    jp replaceXY

; Description: Validate that X and Y are positive (> 0) integers. Calls
; ErrDomain exception upon failure.
; Input: OP1=Y; OP2=X
; Output: OP1=Y; OP2=X
validatePosIntGcdLcm:
    call op1ExOp2
    call validatePosIntGcdLcmCommon ; neat trick, calls the tail of itself
    call op1ExOp2
validatePosIntGcdLcmCommon:
    bcall(_CkOP1FP0) ; if OP1 >= 0: ZF=1
    jr z, validatePosIntGcdLcmError
    bcall(_CkPosInt)
    ret z
validatePosIntGcdLcmError:
    bcall(_ErrDomain) ; throw exception

; Description: Calculate the Great Common Divisor.
; Input: OP1, OP2
; Output: OP1 = GCD(OP1, OP2)
; Destroys: OP1, OP2, OP3
gcdOp1Op2:
    bcall(_CkOP2FP0) ; while b != 0
    ret z
    bcall(_PushRealO2) ; FPS=[b]; (t = b)
    call modOp1Op2 ; (a mod b)
    bcall(_OP1ToOP2) ; b = (a mod b)
    bcall(_PopRealO1) ; FPS=[]; (a = t)
    jr gcdOp1Op2

; Description: Calculate the Lowest Common Multiple using the following:
; LCM(Y, X) = Y * X / GCD(Y, X)
;           = Y * (X / GCD(Y,X))
mLcmHandler:
    call closeInputAndRecallXY
    call validatePosIntGcdLcm
    call lcdOp1Op2 ; OP1=lcd(OP1,OP2)
    jp replaceXY ; X = lcm(X, Y)

lcdOp1Op2:
    bcall(_PushRealO1) ; FPS=[Y]
    bcall(_PushRealO2) ; FPS=[Y,X]
    call gcdOp1Op2 ; OP1 = gcd()
    bcall(_OP1ToOP2) ; OP2 = gcd()
    bcall(_PopRealO1) ; FPS=[Y]; OP1 = X
    bcall(_FPDiv) ; OP1 = X / gcd
    bcall(_PopRealO2) ; FPS=[]; OP2 = Y
    bcall(_FPMult) ; OP1 = Y * (X / gcd)
    ret

;-----------------------------------------------------------------------------

; Description: Determine if the integer in X is a prime number and returns 1 if
; prime, or the lowest prime factor (>1) if not a prime. X must be in the range
; of [2, 2^32-1].
;
; Input: X: Number to check
; Output:
;   - stack lifted
;   - Y=original X
;   - X=1 if prime
;   - X=prime factor, if not a prime
;   - "Err: Domain" if X is not an integer in the range of [2, 2^32).
mPrimeHandler:
    call closeInputAndRecallX
    ; Check 0
    bcall(_CkOP1FP0)
    jp z, mPrimeHandlerError
    ; Check 1
    bcall(_OP2Set1) ; OP2 = 1
    bcall(_CpOP1OP2) ; if OP1==1: ZF=1
    jp z, mPrimeHandlerError
    bcall(_OP1ToOP4) ; save OP4 = X
    ; Check integer >= 0
    bcall(_CkPosInt) ; if OP1 >= 0: ZF=1
    jp nz, mPrimeHandlerError
    ; Check unsigned 32-bit integer, i.e. < 2^32.
    call op2Set2Pow32 ; if OP1 >= 2^32: CF=0
    bcall(_CpOP1OP2)
    jp nc, mPrimeHandlerError

    ; Choose one of the various primeFactorXXX() routines.
    ; OP1=1 if prime, or its smallest prime factor (>1) otherwise
#ifdef USE_PRIME_FACTOR_FLOAT
    call primeFactorFloat
#else
    #ifdef USE_PRIME_FACTOR_INT
        call primeFactorInt
    #else
        call primeFactorMod
    #endif
#endif
    bcall(_RunIndicOff) ; disable run indicator

    ; Instead of replacing the original X, push the prime factor into the RPN
    ; stack. This allows the user to press '/' to get the next candidate prime
    ; factor, which can be processed through 'PRIM` again. Running through this
    ; multiple times until a '1' is returns allows all prime factors to be
    ; discovered.
    jp pushX

mPrimeHandlerError:
    bcall(_ErrDomain) ; throw exception

;-----------------------------------------------------------------------------

#ifdef ENABLE_PRIME_MOD
; Description: Test modU32ByDE().
; Uses:
;   - OP1=Y
;   - OP2=X
;   - OP3=u32(Y)
;   - OP4=u32(X)
mPrimeModHandler:
    call closeInputAndRecallXY ; OP2 = X; OP1 = Y
    ld hl, OP3
    call convertOP1ToU32 ; OP3=u32(Y)
    bcall(_OP2ToOP1)
    ld hl, OP4
    call convertOP1ToU32 ; OP4=u32(X)
    ;
    ld e, (hl)
    inc hl
    ld d, (hl) ; DE=u16(X)
    ;
    ld hl, OP3
    call modU32ByDE ; BC=remainder=Y mod X
    ld hl, OP3
    call setU32ToBC ; u32(OP3)=BC
    call convertU32ToOP1 ; OP1=float(OP3)
    jp replaceXY
#endif

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
    bcall(_PushRealO1) ; FPS=[OP1]
    bcall(_PushRealO2) ; FPS=[OP1,OP2]
    bcall(_FPDiv) ; OP1 = OP1/OP2
    bcall(_Intgr) ; OP1 = floor(OP1/OP2)
    bcall(_PopRealO2) ; FPS=[OP1]; OP2 = OP2
    bcall(_FPMult) ; OP1 = floor(OP1/OP2) * OP2
    bcall(_OP1ToOP2) ; OP2 = floor(OP1/OP2) * OP2
    bcall(_PopRealO1) ; FPS=[]; OP1 = OP1
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
; P(Y, X) = P(n, r) = n!/(n-r)! = n(n-1)...(n-r+1)
mPermHandler:
    call closeInputAndRecallXY ; OP1=Y=n; OP2=X=r
    bcall(_ProbPerm)
    jp replaceXY

;-----------------------------------------------------------------------------

; Calculate the Combintation function:
; C(Y, X) = C(n, r) = n!/(n-r)!/r! = n(n-1)...(n-r+1)/(r)(r-1)...(1).
mCombHandler:
    call closeInputAndRecallXY ; OP1=Y=n; OP2=X=r
    bcall(_ProbComb)
    jp replaceXY

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
    call closeInputAndRecallX
    bcall(_Random)
    jp pushX

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

; Polar to Rectangular. The order of arguments is intended to be consistent
; with the HP-42S.
; Input:
;   - Y: theta
;   - X: r
; Output:
;   - Y: y
;   - X: x
mPToRHandler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2=X=r
    call rclY ; OP1 =Y=theta
    call op1ExOp2  ; OP1=r; OP2=theta
    bcall(_PToR) ; OP1=x; OP2=y
    call op1ExOp2  ; OP1=y; OP2=x
    jp replaceXYWithOP1OP2 ; Y=OP2=y; X=OP1=x

; Rectangular to Polar. The order of arguments is intended to be consistent
; with the HP-42S.
; Input:
;   - Y: y
;   - X: x
; Output:
;   - Y: theta
;   - X: r
mRtoPHandler:
    call closeInputAndRecallX
    bcall(_OP1ToOP2) ; OP2=X=x
    call rclY ; OP1=Y=y
    call op1ExOp2  ; OP1=x; OP2=y
    bcall(_RToP) ; OP1=r; OP2=theta
    call op1ExOp2  ; OP1=theta; OP2=r
    jp replaceXYWithOP1OP2 ; Y=OP1=theta; X=OP2=r

;-----------------------------------------------------------------------------

; Description: Convert "hh.mmss" to "hh.ddddd".
; Destroys: OP1, OP2, OP3, OP4 (temp)
mHmsToHrHandler:
    call closeInputAndRecallX
    bcall(_HmsToHr)
    jp replaceX

; Description: Convert "hh.dddd" to "hh.mmss".
; Destroys: OP1, OP2, OP3, OP4 (temp)
mHrToHmsHandler:
    call closeInputAndRecallX
    bcall(_HmsFromHr)
    jp replaceX

;-----------------------------------------------------------------------------
; Children nodes of MODE menu.
;-----------------------------------------------------------------------------

mFixHandler:
    call closeInputAndRecallNone
    ld hl, msgFixPrompt
    call startArgParser
    call processArgCommands
    ret nc ; do nothing if canceled
    res fmtExponent, (iy + fmtFlags)
    res fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

mSciHandler:
    call closeInputAndRecallNone
    ld hl, msgSciPrompt
    call startArgParser
    call processArgCommands
    ret nc ; do nothing if canceled
    set fmtExponent, (iy + fmtFlags)
    res fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

mEngHandler:
    call closeInputAndRecallNone
    ld hl, msgEngPrompt
    call startArgParser
    call processArgCommands
    ret nc ; do nothing if canceled
    set fmtExponent, (iy + fmtFlags)
    set fmtEng, (iy + fmtFlags)
    jr saveFormatDigits

msgFixPrompt:
    .db "FIX", 0
msgSciPrompt:
    .db "SCI", 0
msgEngPrompt:
    .db "ENG", 0

; Description: Save the (argValue) to (fmtDigits).
; Output:
;   - dirtyFlagsStack set
;   - dirtyFlagsFloatMode set
;   - fmtDigits updated
; Destroys: A
saveFormatDigits:
    set dirtyFlagsStack, (iy + dirtyFlags)
    set dirtyFlagsStatus, (iy + dirtyFlags)
    set dirtyFlagsMenu, (iy + dirtyFlags)
    ld a, (argValue)
    cp 10
    jr c, saveFormatDigitsContinue
    ld a, $FF ; variable number of digits, not fixed
saveFormatDigitsContinue:
    ld (fmtDigits), a
    ret

;-----------------------------------------------------------------------------

; Description: Select the display name of 'FIX' menu.
; Input:
;   - A: nameId
;   - C: altNameId
;   - HL: pointer to MenuNode
; Output:
;   - A: either A or C
mFixNameSelector:
    bit fmtExponent, (iy + fmtFlags)
    ret nz
    ld a, c
    ret

; Description: Select the display name of 'SCI' menu.
; Input:
;   - A: nameId
;   - C: altNameId
;   - HL: pointer to MenuNode
; Output:
;   - A: either A or C
mSciNameSelector:
    bit fmtExponent, (iy + fmtFlags)
    ret z
mSciNameSelectorMaybeOn:
    bit fmtEng, (iy + fmtFlags)
    ret nz
    ld a, c
    ret

; Description: Select the display name of 'ENG' menu.
; Input:
;   - A: nameId
;   - C: altNameId
;   - HL: pointer to MenuNode
; Output:
;   - A: either A or C
mEngNameSelector:
    bit fmtExponent, (iy + fmtFlags)
    ret z
mEngNameSelectorMaybeOn:
    bit fmtEng, (iy + fmtFlags)
    ret z
    ld a, c
    ret

;-----------------------------------------------------------------------------

mRadHandler:
    res trigDeg, (iy + trigFlags)
    set dirtyFlagsStatus, (iy + dirtyFlags)
    set dirtyFlagsMenu, (iy + dirtyFlags)
    ret

mDegHandler:
    set trigDeg, (iy + trigFlags)
    set dirtyFlagsStatus, (iy + dirtyFlags)
    set dirtyFlagsMenu, (iy + dirtyFlags)
    ret

; Description: Select the display name of 'RAD' menu.
; Input:
;   - A: nameId
;   - C: altNameId
;   - HL: pointer to MenuNode
; Output:
;   - A: either A or C
mRadNameSelector:
    bit trigDeg, (iy + trigFlags)
    ret nz
    ld a, c
    ret

; Description: Select the display name of 'DEG' menu.
; Input:
;   - A: nameId
;   - C: altNameId
;   - HL: pointer to MenuNode
; Output:
;   - A: either A or C
mDegNameSelector:
    bit trigDeg, (iy + trigFlags)
    ret z
    ld a, c
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
; Children nodes of STK menu group (stack functions).
;-----------------------------------------------------------------------------

mStackRollUpHandler:
    call closeInputAndRecallNone
    jp rollUpStack

mStackRollDownHandler:
    jp handleKeyRollDown

mStackExchangeXYHandler:
    jp handleKeyExchangeXY

;-----------------------------------------------------------------------------
; Children nodes of CLR menu group (clear functions).
;-----------------------------------------------------------------------------

mClearRegsHandler:
    call closeInputAndRecallNone
    call clearRegs
    ld a, errorCodeRegsCleared
    ld (handlerCode), a
    ret

mClearStackHandler:
    call closeInputAndRecallNone
    jp clearStack

mClearXHandler:
    call closeInputAndRecallNone
    res rpnFlagsLiftEnabled, (iy + rpnFlags) ; disable stack lift
    bcall(_OP1Set0)
    jp stoX

mClearStatHandler:
    jp mStatClearHandler

mClearTvmHandler:
    jp mTvmClearHandler
