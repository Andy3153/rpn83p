;-----------------------------------------------------------------------------
; Menu hierarchy definitions, generated from menusample.txt.
; See menu.asm for the equivalent C struct declaration.
;
; The following symbols are reserved and pre-generated by the compilemenu.py
; script:
;   - mNull
;   - mNullId
;   - mNullName
;   - mNullNameId
;   - mNullHandler
;   - mGroupHandler
;
; The following symbols are not reserved, but they recommended to be used
; for the root menu:
;   - mRoot
;   - mRootId
;   - mRootNameId
;
; DO NOT EDIT: This file was autogenerated.
;-----------------------------------------------------------------------------

mMenuTable:
mNull:
mNullId equ 0
    .db mNullId ; id
    .db mNullId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler
mRoot:
mRootId equ 1
    .db mRootId ; id
    .db mNullId ; parentId
    .db mRootNameId ; nameId
    .db 2 ; numStrips
    .db mNumId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
; MenuGroup root: children
; MenuGroup root: children: strip 0
mNum:
mNumId equ 2
    .db mNumId ; id
    .db mRootId ; parentId
    .db mNumNameId ; nameId
    .db 2 ; numStrips
    .db mCubeId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mProb:
mProbId equ 3
    .db mProbId ; id
    .db mRootId ; parentId
    .db mProbNameId ; nameId
    .db 1 ; numStrips
    .db mPermId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mBlank004:
mBlank004Id equ 4
    .db mBlank004Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank005:
mBlank005Id equ 5
    .db mBlank005Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mHelp:
mHelpId equ 6
    .db mHelpId ; id
    .db mRootId ; parentId
    .db mHelpNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHelpHandler ; handler (to be implemented)
; MenuGroup root: children: strip 1
mDisp:
mDispId equ 7
    .db mDispId ; id
    .db mRootId ; parentId
    .db mDispNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDispHandler ; handler (to be implemented)
mMode:
mModeId equ 8
    .db mModeId ; id
    .db mRootId ; parentId
    .db mModeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mModeHandler ; handler (to be implemented)
mHyperbolic:
mHyperbolicId equ 9
    .db mHyperbolicId ; id
    .db mRootId ; parentId
    .db mHyperbolicNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHyperbolicHandler ; handler (to be implemented)
mUnit:
mUnitId equ 10
    .db mUnitId ; id
    .db mRootId ; parentId
    .db mUnitNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mUnitHandler ; handler (to be implemented)
mBlank011:
mBlank011Id equ 11
    .db mBlank011Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup NUM: children
; MenuGroup NUM: children: strip 0
mCube:
mCubeId equ 12
    .db mCubeId ; id
    .db mNumId ; parentId
    .db mCubeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeHandler ; handler (to be implemented)
mCubeRoot:
mCubeRootId equ 13
    .db mCubeRootId ; id
    .db mNumId ; parentId
    .db mCubeRootNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeRootHandler ; handler (to be implemented)
mAtan2:
mAtan2Id equ 14
    .db mAtan2Id ; id
    .db mNumId ; parentId
    .db mAtan2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAtan2Handler ; handler (to be implemented)
mPercent:
mPercentId equ 15
    .db mPercentId ; id
    .db mNumId ; parentId
    .db mPercentNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPercentHandler ; handler (to be implemented)
mBlank016:
mBlank016Id equ 16
    .db mBlank016Id ; id
    .db mNumId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup NUM: children: strip 1
mAbs:
mAbsId equ 17
    .db mAbsId ; id
    .db mNumId ; parentId
    .db mAbsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAbsHandler ; handler (to be implemented)
mSign:
mSignId equ 18
    .db mSignId ; id
    .db mNumId ; parentId
    .db mSignNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSignHandler ; handler (to be implemented)
mMod:
mModId equ 19
    .db mModId ; id
    .db mNumId ; parentId
    .db mModNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mModHandler ; handler (to be implemented)
mLcm:
mLcmId equ 20
    .db mLcmId ; id
    .db mNumId ; parentId
    .db mLcmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLcmHandler ; handler (to be implemented)
mGcd:
mGcdId equ 21
    .db mGcdId ; id
    .db mNumId ; parentId
    .db mGcdNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGcdHandler ; handler (to be implemented)
; MenuGroup PROB: children
; MenuGroup PROB: children: strip 0
mPerm:
mPermId equ 22
    .db mPermId ; id
    .db mProbId ; parentId
    .db mPermNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPermHandler ; handler (to be implemented)
mComb:
mCombId equ 23
    .db mCombId ; id
    .db mProbId ; parentId
    .db mCombNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCombHandler ; handler (to be implemented)
mFactorial:
mFactorialId equ 24
    .db mFactorialId ; id
    .db mProbId ; parentId
    .db mFactorialNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFactorialHandler ; handler (to be implemented)
mRandom:
mRandomId equ 25
    .db mRandomId ; id
    .db mProbId ; parentId
    .db mRandomNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomHandler ; handler (to be implemented)
mRandomSeed:
mRandomSeedId equ 26
    .db mRandomSeedId ; id
    .db mProbId ; parentId
    .db mRandomSeedNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomSeedHandler ; handler (to be implemented)

; Table of 2-byte pointers to names in the pool of strings below.
mMenuNameTable:
mNullNameId equ 0
    .dw mNullName
mRootNameId equ 1
    .dw mRootName
mNumNameId equ 2
    .dw mNumName
mProbNameId equ 3
    .dw mProbName
mHelpNameId equ 4
    .dw mHelpName
mDispNameId equ 5
    .dw mDispName
mModeNameId equ 6
    .dw mModeName
mHyperbolicNameId equ 7
    .dw mHyperbolicName
mUnitNameId equ 8
    .dw mUnitName
mCubeNameId equ 9
    .dw mCubeName
mCubeRootNameId equ 10
    .dw mCubeRootName
mAtan2NameId equ 11
    .dw mAtan2Name
mPercentNameId equ 12
    .dw mPercentName
mAbsNameId equ 13
    .dw mAbsName
mSignNameId equ 14
    .dw mSignName
mModNameId equ 15
    .dw mModName
mLcmNameId equ 16
    .dw mLcmName
mGcdNameId equ 17
    .dw mGcdName
mPermNameId equ 18
    .dw mPermName
mCombNameId equ 19
    .dw mCombName
mFactorialNameId equ 20
    .dw mFactorialName
mRandomNameId equ 21
    .dw mRandomName
mRandomSeedNameId equ 22
    .dw mRandomSeedName

; Table of names as NUL terminated C strings.
mNullName:
    .db 0
mRootName:
    .db "root", 0
mNumName:
    .db "NUM", 0
mProbName:
    .db "PROB", 0
mHelpName:
    .db "HELP", 0
mDispName:
    .db "DISP", 0
mModeName:
    .db "MODE", 0
mHyperbolicName:
    .db "HYP", 0
mUnitName:
    .db "UNIT", 0
mCubeName:
    .db Scaret, '3', 0
mCubeRootName:
    .db "CBRT", 0
mAtan2Name:
    .db "ATN2", 0
mPercentName:
    .db Spercent, 0
mAbsName:
    .db "ABS", 0
mSignName:
    .db "SIGN", 0
mModName:
    .db "MOD", 0
mLcmName:
    .db "LCM", 0
mGcdName:
    .db "GCD", 0
mPermName:
    .db "PERM", 0
mCombName:
    .db "COMB", 0
mFactorialName:
    .db 'N', Sexclam, 0
mRandomName:
    .db "RAND", 0
mRandomSeedName:
    .db "SEED", 0
