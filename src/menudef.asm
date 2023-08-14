;-----------------------------------------------------------------------------
; Menu hierarchy definitions, generated from menudef.txt.
; See menu.asm for the equivalent C struct declaration.
;
; The following symbols are reserved and pre-generated by the compilemenu.py
; script:
;   - mNull
;   - mNullId
;   - mNullName
;   - mNullNameId
;   - mNullHandler
;
; The following symbols are not reserved, but the root menu group is
; recommended to use the 'mRoot' label, which then generates the following
; for the root menu group:
;   - mRoot
;   - mRootId
;   - mRootNameId
;
; The following is the recommended configuration of the 'GroupHandler'
; directive inside a 'MenuConfig':
;   - GroupHandler mGroupHandler
;
; The following are the recommended configurations for a blank menu item:
;   - ItemName mNullName
;   - ItemNameId mNullNameId
;   - ItemHandler mNullHandler
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
    .db 3 ; numStrips
    .db mMathId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
; MenuGroup root: children
; MenuGroup root: children: strip 0
mMath:
mMathId equ 2
    .db mMathId ; id
    .db mRootId ; parentId
    .db mMathNameId ; nameId
    .db 2 ; numStrips
    .db mCubeId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mNum:
mNumId equ 3
    .db mNumId ; id
    .db mRootId ; parentId
    .db mNumNameId ; nameId
    .db 3 ; numStrips
    .db mPercentId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mProb:
mProbId equ 4
    .db mProbId ; id
    .db mRootId ; parentId
    .db mProbNameId ; nameId
    .db 1 ; numStrips
    .db mCombId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mConv:
mConvId equ 5
    .db mConvId ; id
    .db mRootId ; parentId
    .db mConvNameId ; nameId
    .db 2 ; numStrips
    .db mRToDId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mHelp:
mHelpId equ 6
    .db mHelpId ; id
    .db mRootId ; parentId
    .db mHelpNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHelpHandler ; handler (to be implemented)
; MenuGroup root: children: strip 1
mMode:
mModeId equ 7
    .db mModeId ; id
    .db mRootId ; parentId
    .db mModeNameId ; nameId
    .db 1 ; numStrips
    .db mFixId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mHyperbolic:
mHyperbolicId equ 8
    .db mHyperbolicId ; id
    .db mRootId ; parentId
    .db mHyperbolicNameId ; nameId
    .db 2 ; numStrips
    .db mBlank062Id ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mUnit:
mUnitId equ 9
    .db mUnitId ; id
    .db mRootId ; parentId
    .db mUnitNameId ; nameId
    .db 6 ; numStrips
    .db mFToCId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mBase:
mBaseId equ 10
    .db mBaseId ; id
    .db mRootId ; parentId
    .db mBaseNameId ; nameId
    .db 2 ; numStrips
    .db mDecId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mStack:
mStackId equ 11
    .db mStackId ; id
    .db mRootId ; parentId
    .db mStackNameId ; nameId
    .db 1 ; numStrips
    .db mBlank112Id ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
; MenuGroup root: children: strip 2
mClear:
mClearId equ 12
    .db mClearId ; id
    .db mRootId ; parentId
    .db mClearNameId ; nameId
    .db 1 ; numStrips
    .db mClearXId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mBlank013:
mBlank013Id equ 13
    .db mBlank013Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank014:
mBlank014Id equ 14
    .db mBlank014Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank015:
mBlank015Id equ 15
    .db mBlank015Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank016:
mBlank016Id equ 16
    .db mBlank016Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup MATH: children
; MenuGroup MATH: children: strip 0
mCube:
mCubeId equ 17
    .db mCubeId ; id
    .db mMathId ; parentId
    .db mCubeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeHandler ; handler (to be implemented)
mCubeRoot:
mCubeRootId equ 18
    .db mCubeRootId ; id
    .db mMathId ; parentId
    .db mCubeRootNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeRootHandler ; handler (to be implemented)
mAtan2:
mAtan2Id equ 19
    .db mAtan2Id ; id
    .db mMathId ; parentId
    .db mAtan2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAtan2Handler ; handler (to be implemented)
mBlank020:
mBlank020Id equ 20
    .db mBlank020Id ; id
    .db mMathId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank021:
mBlank021Id equ 21
    .db mBlank021Id ; id
    .db mMathId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup MATH: children: strip 1
mAlog2:
mAlog2Id equ 22
    .db mAlog2Id ; id
    .db mMathId ; parentId
    .db mAlog2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAlog2Handler ; handler (to be implemented)
mLog2:
mLog2Id equ 23
    .db mLog2Id ; id
    .db mMathId ; parentId
    .db mLog2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLog2Handler ; handler (to be implemented)
mLogBase:
mLogBaseId equ 24
    .db mLogBaseId ; id
    .db mMathId ; parentId
    .db mLogBaseNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLogBaseHandler ; handler (to be implemented)
mBlank025:
mBlank025Id equ 25
    .db mBlank025Id ; id
    .db mMathId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank026:
mBlank026Id equ 26
    .db mBlank026Id ; id
    .db mMathId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup NUM: children
; MenuGroup NUM: children: strip 0
mPercent:
mPercentId equ 27
    .db mPercentId ; id
    .db mNumId ; parentId
    .db mPercentNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPercentHandler ; handler (to be implemented)
mPercentChange:
mPercentChangeId equ 28
    .db mPercentChangeId ; id
    .db mNumId ; parentId
    .db mPercentChangeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPercentChangeHandler ; handler (to be implemented)
mGcd:
mGcdId equ 29
    .db mGcdId ; id
    .db mNumId ; parentId
    .db mGcdNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGcdHandler ; handler (to be implemented)
mLcm:
mLcmId equ 30
    .db mLcmId ; id
    .db mNumId ; parentId
    .db mLcmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLcmHandler ; handler (to be implemented)
mPrime:
mPrimeId equ 31
    .db mPrimeId ; id
    .db mNumId ; parentId
    .db mPrimeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPrimeHandler ; handler (to be implemented)
; MenuGroup NUM: children: strip 1
mIntPart:
mIntPartId equ 32
    .db mIntPartId ; id
    .db mNumId ; parentId
    .db mIntPartNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mIntPartHandler ; handler (to be implemented)
mFracPart:
mFracPartId equ 33
    .db mFracPartId ; id
    .db mNumId ; parentId
    .db mFracPartNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFracPartHandler ; handler (to be implemented)
mFloor:
mFloorId equ 34
    .db mFloorId ; id
    .db mNumId ; parentId
    .db mFloorNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFloorHandler ; handler (to be implemented)
mCeil:
mCeilId equ 35
    .db mCeilId ; id
    .db mNumId ; parentId
    .db mCeilNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCeilHandler ; handler (to be implemented)
mNear:
mNearId equ 36
    .db mNearId ; id
    .db mNumId ; parentId
    .db mNearNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNearHandler ; handler (to be implemented)
; MenuGroup NUM: children: strip 2
mAbs:
mAbsId equ 37
    .db mAbsId ; id
    .db mNumId ; parentId
    .db mAbsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAbsHandler ; handler (to be implemented)
mSign:
mSignId equ 38
    .db mSignId ; id
    .db mNumId ; parentId
    .db mSignNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSignHandler ; handler (to be implemented)
mMod:
mModId equ 39
    .db mModId ; id
    .db mNumId ; parentId
    .db mModNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mModHandler ; handler (to be implemented)
mMin:
mMinId equ 40
    .db mMinId ; id
    .db mNumId ; parentId
    .db mMinNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMinHandler ; handler (to be implemented)
mMax:
mMaxId equ 41
    .db mMaxId ; id
    .db mNumId ; parentId
    .db mMaxNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMaxHandler ; handler (to be implemented)
; MenuGroup PROB: children
; MenuGroup PROB: children: strip 0
mComb:
mCombId equ 42
    .db mCombId ; id
    .db mProbId ; parentId
    .db mCombNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCombHandler ; handler (to be implemented)
mPerm:
mPermId equ 43
    .db mPermId ; id
    .db mProbId ; parentId
    .db mPermNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPermHandler ; handler (to be implemented)
mFactorial:
mFactorialId equ 44
    .db mFactorialId ; id
    .db mProbId ; parentId
    .db mFactorialNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFactorialHandler ; handler (to be implemented)
mRandom:
mRandomId equ 45
    .db mRandomId ; id
    .db mProbId ; parentId
    .db mRandomNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomHandler ; handler (to be implemented)
mRandomSeed:
mRandomSeedId equ 46
    .db mRandomSeedId ; id
    .db mProbId ; parentId
    .db mRandomSeedNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomSeedHandler ; handler (to be implemented)
; MenuGroup CONV: children
; MenuGroup CONV: children: strip 0
mRToD:
mRToDId equ 47
    .db mRToDId ; id
    .db mConvId ; parentId
    .db mRToDNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRToDHandler ; handler (to be implemented)
mDToR:
mDToRId equ 48
    .db mDToRId ; id
    .db mConvId ; parentId
    .db mDToRNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDToRHandler ; handler (to be implemented)
mBlank049:
mBlank049Id equ 49
    .db mBlank049Id ; id
    .db mConvId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mPToR:
mPToRId equ 50
    .db mPToRId ; id
    .db mConvId ; parentId
    .db mPToRNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPToRHandler ; handler (to be implemented)
mRToP:
mRToPId equ 51
    .db mRToPId ; id
    .db mConvId ; parentId
    .db mRToPNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRToPHandler ; handler (to be implemented)
; MenuGroup CONV: children: strip 1
mHmsToHr:
mHmsToHrId equ 52
    .db mHmsToHrId ; id
    .db mConvId ; parentId
    .db mHmsToHrNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHmsToHrHandler ; handler (to be implemented)
mHrToHms:
mHrToHmsId equ 53
    .db mHrToHmsId ; id
    .db mConvId ; parentId
    .db mHrToHmsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHrToHmsHandler ; handler (to be implemented)
mBlank054:
mBlank054Id equ 54
    .db mBlank054Id ; id
    .db mConvId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank055:
mBlank055Id equ 55
    .db mBlank055Id ; id
    .db mConvId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank056:
mBlank056Id equ 56
    .db mBlank056Id ; id
    .db mConvId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup MODE: children
; MenuGroup MODE: children: strip 0
mFix:
mFixId equ 57
    .db mFixId ; id
    .db mModeId ; parentId
    .db mFixNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFixHandler ; handler (to be implemented)
mSci:
mSciId equ 58
    .db mSciId ; id
    .db mModeId ; parentId
    .db mSciNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSciHandler ; handler (to be implemented)
mEng:
mEngId equ 59
    .db mEngId ; id
    .db mModeId ; parentId
    .db mEngNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mEngHandler ; handler (to be implemented)
mRad:
mRadId equ 60
    .db mRadId ; id
    .db mModeId ; parentId
    .db mRadNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRadHandler ; handler (to be implemented)
mDeg:
mDegId equ 61
    .db mDegId ; id
    .db mModeId ; parentId
    .db mDegNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDegHandler ; handler (to be implemented)
; MenuGroup HYP: children
; MenuGroup HYP: children: strip 0
mBlank062:
mBlank062Id equ 62
    .db mBlank062Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mSinh:
mSinhId equ 63
    .db mSinhId ; id
    .db mHyperbolicId ; parentId
    .db mSinhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSinhHandler ; handler (to be implemented)
mCosh:
mCoshId equ 64
    .db mCoshId ; id
    .db mHyperbolicId ; parentId
    .db mCoshNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCoshHandler ; handler (to be implemented)
mTanh:
mTanhId equ 65
    .db mTanhId ; id
    .db mHyperbolicId ; parentId
    .db mTanhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mTanhHandler ; handler (to be implemented)
mBlank066:
mBlank066Id equ 66
    .db mBlank066Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup HYP: children: strip 1
mBlank067:
mBlank067Id equ 67
    .db mBlank067Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mAsinh:
mAsinhId equ 68
    .db mAsinhId ; id
    .db mHyperbolicId ; parentId
    .db mAsinhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAsinhHandler ; handler (to be implemented)
mAcosh:
mAcoshId equ 69
    .db mAcoshId ; id
    .db mHyperbolicId ; parentId
    .db mAcoshNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAcoshHandler ; handler (to be implemented)
mAtanh:
mAtanhId equ 70
    .db mAtanhId ; id
    .db mHyperbolicId ; parentId
    .db mAtanhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAtanhHandler ; handler (to be implemented)
mBlank071:
mBlank071Id equ 71
    .db mBlank071Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup UNIT: children
; MenuGroup UNIT: children: strip 0
mFToC:
mFToCId equ 72
    .db mFToCId ; id
    .db mUnitId ; parentId
    .db mFToCNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFToCHandler ; handler (to be implemented)
mCToF:
mCToFId equ 73
    .db mCToFId ; id
    .db mUnitId ; parentId
    .db mCToFNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCToFHandler ; handler (to be implemented)
mBlank074:
mBlank074Id equ 74
    .db mBlank074Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mInhgToHpa:
mInhgToHpaId equ 75
    .db mInhgToHpaId ; id
    .db mUnitId ; parentId
    .db mInhgToHpaNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mInhgToHpaHandler ; handler (to be implemented)
mHpaToInhg:
mHpaToInhgId equ 76
    .db mHpaToInhgId ; id
    .db mUnitId ; parentId
    .db mHpaToInhgNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHpaToInhgHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 1
mMiToKm:
mMiToKmId equ 77
    .db mMiToKmId ; id
    .db mUnitId ; parentId
    .db mMiToKmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMiToKmHandler ; handler (to be implemented)
mKmToMi:
mKmToMiId equ 78
    .db mKmToMiId ; id
    .db mUnitId ; parentId
    .db mKmToMiNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKmToMiHandler ; handler (to be implemented)
mBlank079:
mBlank079Id equ 79
    .db mBlank079Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mFtToM:
mFtToMId equ 80
    .db mFtToMId ; id
    .db mUnitId ; parentId
    .db mFtToMNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFtToMHandler ; handler (to be implemented)
mMToFt:
mMToFtId equ 81
    .db mMToFtId ; id
    .db mUnitId ; parentId
    .db mMToFtNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMToFtHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 2
mInToCm:
mInToCmId equ 82
    .db mInToCmId ; id
    .db mUnitId ; parentId
    .db mInToCmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mInToCmHandler ; handler (to be implemented)
mCmToIn:
mCmToInId equ 83
    .db mCmToInId ; id
    .db mUnitId ; parentId
    .db mCmToInNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCmToInHandler ; handler (to be implemented)
mBlank084:
mBlank084Id equ 84
    .db mBlank084Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mMilToMicron:
mMilToMicronId equ 85
    .db mMilToMicronId ; id
    .db mUnitId ; parentId
    .db mMilToMicronNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMilToMicronHandler ; handler (to be implemented)
mMicronToMil:
mMicronToMilId equ 86
    .db mMicronToMilId ; id
    .db mUnitId ; parentId
    .db mMicronToMilNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMicronToMilHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 3
mLbsToKg:
mLbsToKgId equ 87
    .db mLbsToKgId ; id
    .db mUnitId ; parentId
    .db mLbsToKgNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLbsToKgHandler ; handler (to be implemented)
mKgToLbs:
mKgToLbsId equ 88
    .db mKgToLbsId ; id
    .db mUnitId ; parentId
    .db mKgToLbsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKgToLbsHandler ; handler (to be implemented)
mBlank089:
mBlank089Id equ 89
    .db mBlank089Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mOzToG:
mOzToGId equ 90
    .db mOzToGId ; id
    .db mUnitId ; parentId
    .db mOzToGNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mOzToGHandler ; handler (to be implemented)
mGToOz:
mGToOzId equ 91
    .db mGToOzId ; id
    .db mUnitId ; parentId
    .db mGToOzNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGToOzHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 4
mGalToL:
mGalToLId equ 92
    .db mGalToLId ; id
    .db mUnitId ; parentId
    .db mGalToLNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGalToLHandler ; handler (to be implemented)
mLToGal:
mLToGalId equ 93
    .db mLToGalId ; id
    .db mUnitId ; parentId
    .db mLToGalNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLToGalHandler ; handler (to be implemented)
mBlank094:
mBlank094Id equ 94
    .db mBlank094Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mFlozToMl:
mFlozToMlId equ 95
    .db mFlozToMlId ; id
    .db mUnitId ; parentId
    .db mFlozToMlNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFlozToMlHandler ; handler (to be implemented)
mMlToFloz:
mMlToFlozId equ 96
    .db mMlToFlozId ; id
    .db mUnitId ; parentId
    .db mMlToFlozNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMlToFlozHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 5
mCalToKj:
mCalToKjId equ 97
    .db mCalToKjId ; id
    .db mUnitId ; parentId
    .db mCalToKjNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCalToKjHandler ; handler (to be implemented)
mKjToCal:
mKjToCalId equ 98
    .db mKjToCalId ; id
    .db mUnitId ; parentId
    .db mKjToCalNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKjToCalHandler ; handler (to be implemented)
mBlank099:
mBlank099Id equ 99
    .db mBlank099Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mHpToKw:
mHpToKwId equ 100
    .db mHpToKwId ; id
    .db mUnitId ; parentId
    .db mHpToKwNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHpToKwHandler ; handler (to be implemented)
mKwToHp:
mKwToHpId equ 101
    .db mKwToHpId ; id
    .db mUnitId ; parentId
    .db mKwToHpNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKwToHpHandler ; handler (to be implemented)
; MenuGroup BASE: children
; MenuGroup BASE: children: strip 0
mDec:
mDecId equ 102
    .db mDecId ; id
    .db mBaseId ; parentId
    .db mDecNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDecHandler ; handler (to be implemented)
mHex:
mHexId equ 103
    .db mHexId ; id
    .db mBaseId ; parentId
    .db mHexNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHexHandler ; handler (to be implemented)
mOct:
mOctId equ 104
    .db mOctId ; id
    .db mBaseId ; parentId
    .db mOctNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mOctHandler ; handler (to be implemented)
mBin:
mBinId equ 105
    .db mBinId ; id
    .db mBaseId ; parentId
    .db mBinNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBinHandler ; handler (to be implemented)
mBlank106:
mBlank106Id equ 106
    .db mBlank106Id ; id
    .db mBaseId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup BASE: children: strip 1
mBitwiseAnd:
mBitwiseAndId equ 107
    .db mBitwiseAndId ; id
    .db mBaseId ; parentId
    .db mBitwiseAndNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBitwiseAndHandler ; handler (to be implemented)
mBitwiseOr:
mBitwiseOrId equ 108
    .db mBitwiseOrId ; id
    .db mBaseId ; parentId
    .db mBitwiseOrNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBitwiseOrHandler ; handler (to be implemented)
mBitwiseXor:
mBitwiseXorId equ 109
    .db mBitwiseXorId ; id
    .db mBaseId ; parentId
    .db mBitwiseXorNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBitwiseXorHandler ; handler (to be implemented)
mBitwiseNot:
mBitwiseNotId equ 110
    .db mBitwiseNotId ; id
    .db mBaseId ; parentId
    .db mBitwiseNotNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBitwiseNotHandler ; handler (to be implemented)
mBitwiseNeg:
mBitwiseNegId equ 111
    .db mBitwiseNegId ; id
    .db mBaseId ; parentId
    .db mBitwiseNegNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mBitwiseNegHandler ; handler (to be implemented)
; MenuGroup STK: children
; MenuGroup STK: children: strip 0
mBlank112:
mBlank112Id equ 112
    .db mBlank112Id ; id
    .db mStackId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mStackRotUp:
mStackRotUpId equ 113
    .db mStackRotUpId ; id
    .db mStackId ; parentId
    .db mStackRotUpNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mStackRotUpHandler ; handler (to be implemented)
mStackRotDown:
mStackRotDownId equ 114
    .db mStackRotDownId ; id
    .db mStackId ; parentId
    .db mStackRotDownNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mStackRotDownHandler ; handler (to be implemented)
mStackExchangeXY:
mStackExchangeXYId equ 115
    .db mStackExchangeXYId ; id
    .db mStackId ; parentId
    .db mStackExchangeXYNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mStackExchangeXYHandler ; handler (to be implemented)
mBlank116:
mBlank116Id equ 116
    .db mBlank116Id ; id
    .db mStackId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup CLR: children
; MenuGroup CLR: children: strip 0
mClearX:
mClearXId equ 117
    .db mClearXId ; id
    .db mClearId ; parentId
    .db mClearXNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mClearXHandler ; handler (to be implemented)
mClearStack:
mClearStackId equ 118
    .db mClearStackId ; id
    .db mClearId ; parentId
    .db mClearStackNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mClearStackHandler ; handler (to be implemented)
mClearRegs:
mClearRegsId equ 119
    .db mClearRegsId ; id
    .db mClearId ; parentId
    .db mClearRegsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mClearRegsHandler ; handler (to be implemented)
mBlank120:
mBlank120Id equ 120
    .db mBlank120Id ; id
    .db mClearId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank121:
mBlank121Id equ 121
    .db mBlank121Id ; id
    .db mClearId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)

; Table of 2-byte pointers to names in the pool of strings below.
mMenuNameTable:
mNullNameId equ 0
    .dw mNullName
mRootNameId equ 1
    .dw mRootName
mMathNameId equ 2
    .dw mMathName
mNumNameId equ 3
    .dw mNumName
mProbNameId equ 4
    .dw mProbName
mConvNameId equ 5
    .dw mConvName
mHelpNameId equ 6
    .dw mHelpName
mModeNameId equ 7
    .dw mModeName
mHyperbolicNameId equ 8
    .dw mHyperbolicName
mUnitNameId equ 9
    .dw mUnitName
mBaseNameId equ 10
    .dw mBaseName
mStackNameId equ 11
    .dw mStackName
mClearNameId equ 12
    .dw mClearName
mCubeNameId equ 13
    .dw mCubeName
mCubeRootNameId equ 14
    .dw mCubeRootName
mAtan2NameId equ 15
    .dw mAtan2Name
mAlog2NameId equ 16
    .dw mAlog2Name
mLog2NameId equ 17
    .dw mLog2Name
mLogBaseNameId equ 18
    .dw mLogBaseName
mPercentNameId equ 19
    .dw mPercentName
mPercentChangeNameId equ 20
    .dw mPercentChangeName
mGcdNameId equ 21
    .dw mGcdName
mLcmNameId equ 22
    .dw mLcmName
mPrimeNameId equ 23
    .dw mPrimeName
mIntPartNameId equ 24
    .dw mIntPartName
mFracPartNameId equ 25
    .dw mFracPartName
mFloorNameId equ 26
    .dw mFloorName
mCeilNameId equ 27
    .dw mCeilName
mNearNameId equ 28
    .dw mNearName
mAbsNameId equ 29
    .dw mAbsName
mSignNameId equ 30
    .dw mSignName
mModNameId equ 31
    .dw mModName
mMinNameId equ 32
    .dw mMinName
mMaxNameId equ 33
    .dw mMaxName
mCombNameId equ 34
    .dw mCombName
mPermNameId equ 35
    .dw mPermName
mFactorialNameId equ 36
    .dw mFactorialName
mRandomNameId equ 37
    .dw mRandomName
mRandomSeedNameId equ 38
    .dw mRandomSeedName
mRToDNameId equ 39
    .dw mRToDName
mDToRNameId equ 40
    .dw mDToRName
mPToRNameId equ 41
    .dw mPToRName
mRToPNameId equ 42
    .dw mRToPName
mHmsToHrNameId equ 43
    .dw mHmsToHrName
mHrToHmsNameId equ 44
    .dw mHrToHmsName
mFixNameId equ 45
    .dw mFixName
mSciNameId equ 46
    .dw mSciName
mEngNameId equ 47
    .dw mEngName
mRadNameId equ 48
    .dw mRadName
mDegNameId equ 49
    .dw mDegName
mSinhNameId equ 50
    .dw mSinhName
mCoshNameId equ 51
    .dw mCoshName
mTanhNameId equ 52
    .dw mTanhName
mAsinhNameId equ 53
    .dw mAsinhName
mAcoshNameId equ 54
    .dw mAcoshName
mAtanhNameId equ 55
    .dw mAtanhName
mFToCNameId equ 56
    .dw mFToCName
mCToFNameId equ 57
    .dw mCToFName
mInhgToHpaNameId equ 58
    .dw mInhgToHpaName
mHpaToInhgNameId equ 59
    .dw mHpaToInhgName
mMiToKmNameId equ 60
    .dw mMiToKmName
mKmToMiNameId equ 61
    .dw mKmToMiName
mFtToMNameId equ 62
    .dw mFtToMName
mMToFtNameId equ 63
    .dw mMToFtName
mInToCmNameId equ 64
    .dw mInToCmName
mCmToInNameId equ 65
    .dw mCmToInName
mMilToMicronNameId equ 66
    .dw mMilToMicronName
mMicronToMilNameId equ 67
    .dw mMicronToMilName
mLbsToKgNameId equ 68
    .dw mLbsToKgName
mKgToLbsNameId equ 69
    .dw mKgToLbsName
mOzToGNameId equ 70
    .dw mOzToGName
mGToOzNameId equ 71
    .dw mGToOzName
mGalToLNameId equ 72
    .dw mGalToLName
mLToGalNameId equ 73
    .dw mLToGalName
mFlozToMlNameId equ 74
    .dw mFlozToMlName
mMlToFlozNameId equ 75
    .dw mMlToFlozName
mCalToKjNameId equ 76
    .dw mCalToKjName
mKjToCalNameId equ 77
    .dw mKjToCalName
mHpToKwNameId equ 78
    .dw mHpToKwName
mKwToHpNameId equ 79
    .dw mKwToHpName
mDecNameId equ 80
    .dw mDecName
mHexNameId equ 81
    .dw mHexName
mOctNameId equ 82
    .dw mOctName
mBinNameId equ 83
    .dw mBinName
mBitwiseAndNameId equ 84
    .dw mBitwiseAndName
mBitwiseOrNameId equ 85
    .dw mBitwiseOrName
mBitwiseXorNameId equ 86
    .dw mBitwiseXorName
mBitwiseNotNameId equ 87
    .dw mBitwiseNotName
mBitwiseNegNameId equ 88
    .dw mBitwiseNegName
mStackRotUpNameId equ 89
    .dw mStackRotUpName
mStackRotDownNameId equ 90
    .dw mStackRotDownName
mStackExchangeXYNameId equ 91
    .dw mStackExchangeXYName
mClearXNameId equ 92
    .dw mClearXName
mClearStackNameId equ 93
    .dw mClearStackName
mClearRegsNameId equ 94
    .dw mClearRegsName

; Table of names as NUL terminated C strings.
mNullName:
    .db 0
mRootName:
    .db "root", 0
mMathName:
    .db "MATH", 0
mNumName:
    .db "NUM", 0
mProbName:
    .db "PROB", 0
mConvName:
    .db "CONV", 0
mHelpName:
    .db "HELP", 0
mModeName:
    .db "MODE", 0
mHyperbolicName:
    .db "HYP", 0
mUnitName:
    .db "UNIT", 0
mBaseName:
    .db "BASE", 0
mStackName:
    .db "STK", 0
mClearName:
    .db "CLR", 0
mCubeName:
    .db 'X', Scaret, '3', 0
mCubeRootName:
    .db ScubeR, Sroot, 'X', 0
mAtan2Name:
    .db "ATN2", 0
mAlog2Name:
    .db '2', Scaret, 'X', 0
mLog2Name:
    .db "LOG2", 0
mLogBaseName:
    .db "LOGB", 0
mPercentName:
    .db Spercent, 0
mPercentChangeName:
    .db Spercent, 'C', 'H', 0
mGcdName:
    .db "GCD", 0
mLcmName:
    .db "LCM", 0
mPrimeName:
    .db "PRIM", 0
mIntPartName:
    .db "IP", 0
mFracPartName:
    .db "FP", 0
mFloorName:
    .db "FLR", 0
mCeilName:
    .db "CEIL", 0
mNearName:
    .db "NEAR", 0
mAbsName:
    .db "ABS", 0
mSignName:
    .db "SIGN", 0
mModName:
    .db "MOD", 0
mMinName:
    .db "MIN", 0
mMaxName:
    .db "MAX", 0
mCombName:
    .db "COMB", 0
mPermName:
    .db "PERM", 0
mFactorialName:
    .db 'N', Sexclam, 0
mRandomName:
    .db "RAND", 0
mRandomSeedName:
    .db "SEED", 0
mRToDName:
    .db Sconvert, 'D', 'E', 'G', 0
mDToRName:
    .db Sconvert, 'R', 'A', 'D', 0
mPToRName:
    .db 'P', Sconvert, 'R', 0
mRToPName:
    .db 'R', Sconvert, 'P', 0
mHmsToHrName:
    .db Sconvert, 'H', 'R', 0
mHrToHmsName:
    .db Sconvert, 'H', 'M', 'S', 0
mFixName:
    .db "FIX", 0
mSciName:
    .db "SCI", 0
mEngName:
    .db "ENG", 0
mRadName:
    .db "RAD", 0
mDegName:
    .db "DEG", 0
mSinhName:
    .db "SINH", 0
mCoshName:
    .db "COSH", 0
mTanhName:
    .db "TANH", 0
mAsinhName:
    .db "ASNH", 0
mAcoshName:
    .db "ACSH", 0
mAtanhName:
    .db "ATNH", 0
mFToCName:
    .db Sconvert, Stemp, 'C', 0
mCToFName:
    .db Sconvert, Stemp, 'F', 0
mInhgToHpaName:
    .db Sconvert, 'h', 'P', 'a', 0
mHpaToInhgName:
    .db Sconvert, 'i', 'H', 'g', 0
mMiToKmName:
    .db Sconvert, 'k', 'm', 0
mKmToMiName:
    .db Sconvert, 'm', 'i', 0
mFtToMName:
    .db Sconvert, 'm', 0
mMToFtName:
    .db Sconvert, 'f', 't', 0
mInToCmName:
    .db Sconvert, 'c', 'm', 0
mCmToInName:
    .db Sconvert, 'i', 'n', 0
mMilToMicronName:
    .db Sconvert, Smu, 'm', 0
mMicronToMilName:
    .db Sconvert, 'm', 'i', 'l', 0
mLbsToKgName:
    .db Sconvert, 'k', 'g', 0
mKgToLbsName:
    .db Sconvert, 'l', 'b', 's', 0
mOzToGName:
    .db Sconvert, 'g', 0
mGToOzName:
    .db Sconvert, 'o', 'z', 0
mGalToLName:
    .db Sconvert, 'L', 0
mLToGalName:
    .db Sconvert, 'g', 'a', 'l', 0
mFlozToMlName:
    .db Sconvert, 'm', 'L', 0
mMlToFlozName:
    .db Sconvert, 'f', 'o', 'z', 0
mCalToKjName:
    .db Sconvert, 'k', 'J', 0
mKjToCalName:
    .db Sconvert, 'c', 'a', 'l', 0
mHpToKwName:
    .db Sconvert, 'k', 'W', 0
mKwToHpName:
    .db Sconvert, 'h', 'p', 0
mDecName:
    .db "DEC", 0
mHexName:
    .db "HEX", 0
mOctName:
    .db "OCT", 0
mBinName:
    .db "BIN", 0
mBitwiseAndName:
    .db "AND", 0
mBitwiseOrName:
    .db "OR", 0
mBitwiseXorName:
    .db "XOR", 0
mBitwiseNotName:
    .db "NOT", 0
mBitwiseNegName:
    .db "NEG", 0
mStackRotUpName:
    .db 'R', SupArrow, 0
mStackRotDownName:
    .db 'R', SdownArrow, 0
mStackExchangeXYName:
    .db 'X', Sleft, Sconvert, 'Y', 0
mClearXName:
    .db "CLX", 0
mClearStackName:
    .db "CLST", 0
mClearRegsName:
    .db "CLRG", 0
