;-----------------------------------------------------------------------------
; Handlers for the various menu nodes generated by compilemenu.py.
;-----------------------------------------------------------------------------

mHelpHandler:
mDispHandler:
mModeHandler:
mHyperbolicHandler:
mUnitHandler:
    ret

;-----------------------------------------------------------------------------
; Children nodes of NUM menu.
;-----------------------------------------------------------------------------

; mCubeHandler(X) -> X^3
; Description: Calculate X^3.
mCubeHandler:
    call closeInputBuf
    call rclX
    bcall(_Cube)
    call stoX
    ret

; mCubeRootHandler(X) -> X^(1/3)
; Description: Calculate the cubic root of X. The SDK documentation has the OP1
; and OP2 flipped.
mCubeRootHandler:
    call closeInputBuf
    call rclX
    bcall(_OP1ToOP2)
    bcall(_OP1Set3)
    bcall(_XRootY)
    call stoX
    ret

; mAtan2Handler(Y, X) -> atan2(Y + Xi)
;
; Description: Calculate the angle of the (Y, X) number in complex plane.
; Use bcall(_RToP) instead of bcall(_ATan2) because ATan2 does not seem produce
; the correct results. There must be something wrong with the documentation, or
; it is buggy and no one has bothered to fix it because I don't think this
; function is exposed to the user through the normal TI-OS.
;
; The real part (i.e. x-axis) is assumed to be entered first, then the
; imaginary part (i.e. y-axis). They becomes stored in the RPN stack variables
; with X and Y flipped, which is bit confusing.
mAtan2Handler:
    call closeInputBuf
    call rclX ; imaginary
    bcall(_OP1ToOP2)
    call rclY ; OP1=Y (real), OP2=X (imaginary)
    bcall(_RToP) ; complex to polar
    call dropStack
    bcall(_OP2ToOP1) ; OP2 contains the angle with range of (-pi, pi]
    call stoX
    ret

; mPercentHandler(Y, X) -> (Y, Y*(X/100))
; Description: Calculate the X percent of Y.
mPercentHandler:
    call closeInputBuf
    call rclX
    ld hl, constHundred
    bcall(_Mov9ToOP2)
    bcall(_FPDiv)
    bcall(_OP1ToOP2)
    call rclY
    bcall(_FPMult)
    call stoX
    ret

;-----------------------------------------------------------------------------
; Children nodes of PROB menu.
;-----------------------------------------------------------------------------

mPermHandler:
mCombHandler:
    ret

; mFactorialHandler(X) -> X!
; Description: Calculate the factorial of X.
mFactorialHandler:
    call closeInputBuf
    call rclX
    bcall(_Factorial)
    bcall(_CkValidNum)
    call stoX
    ret

; mRandomHandler() -> rand()
; Description: Generate a random number [0,1) into the X register.
mRandomHandler:
    call closeInputBuf
    bcall(_Random)
    call liftStackNonEmpty
    call stoX
    ret

; mRandomSeedHandler(X) -> None
; Description: Set X as the Random() seed.
mRandomSeedHandler:
    call closeInputBuf
    call rclX
    bcall(_StoRand)
    ret

mAbsHandler:
mSignHandler:
mModHandler:
mLcmHandler:
mGcdHandler:
    ret

;-----------------------------------------------------------------------------
; Children nodes of MODE menu.
;-----------------------------------------------------------------------------

mRadHandler:
    res trigDeg, (iy + trigFlags)
    set rpnFlagsTrigDirty, (iy + rpnFlags)
    ret

mDegHandler:
    set trigDeg, (iy + trigFlags)
    set rpnFlagsTrigDirty, (iy + rpnFlags)
    ret
