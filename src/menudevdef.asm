;-----------------------------------------------------------------------------
; Menu hierarchy definitions, generated from menudevdef.txt.
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
    .db 4 ; numStrips
    .db mPercentId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mProb:
mProbId equ 3
    .db mProbId ; id
    .db mRootId ; parentId
    .db mProbNameId ; nameId
    .db 1 ; numStrips
    .db mCombId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mConv:
mConvId equ 4
    .db mConvId ; id
    .db mRootId ; parentId
    .db mConvNameId ; nameId
    .db 1 ; numStrips
    .db mRToDId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mUnit:
mUnitId equ 5
    .db mUnitId ; id
    .db mRootId ; parentId
    .db mUnitNameId ; nameId
    .db 4 ; numStrips
    .db mFToCId ; stripBeginId
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
mDisp:
mDispId equ 7
    .db mDispId ; id
    .db mRootId ; parentId
    .db mDispNameId ; nameId
    .db 1 ; numStrips
    .db mFixId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mMode:
mModeId equ 8
    .db mModeId ; id
    .db mRootId ; parentId
    .db mModeNameId ; nameId
    .db 1 ; numStrips
    .db mRadId ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mHyperbolic:
mHyperbolicId equ 9
    .db mHyperbolicId ; id
    .db mRootId ; parentId
    .db mHyperbolicNameId ; nameId
    .db 2 ; numStrips
    .db mBlank072Id ; stripBeginId
    .dw mGroupHandler ; handler (predefined)
mBlank010:
mBlank010Id equ 10
    .db mBlank010Id ; id
    .db mRootId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
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
mPercent:
mPercentId equ 12
    .db mPercentId ; id
    .db mNumId ; parentId
    .db mPercentNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPercentHandler ; handler (to be implemented)
mDeltaPercent:
mDeltaPercentId equ 13
    .db mDeltaPercentId ; id
    .db mNumId ; parentId
    .db mDeltaPercentNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDeltaPercentHandler ; handler (to be implemented)
mCube:
mCubeId equ 14
    .db mCubeId ; id
    .db mNumId ; parentId
    .db mCubeNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeHandler ; handler (to be implemented)
mCubeRoot:
mCubeRootId equ 15
    .db mCubeRootId ; id
    .db mNumId ; parentId
    .db mCubeRootNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCubeRootHandler ; handler (to be implemented)
mBlank016:
mBlank016Id equ 16
    .db mBlank016Id ; id
    .db mNumId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup NUM: children: strip 1
mAlog2:
mAlog2Id equ 17
    .db mAlog2Id ; id
    .db mNumId ; parentId
    .db mAlog2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAlog2Handler ; handler (to be implemented)
mLog2:
mLog2Id equ 18
    .db mLog2Id ; id
    .db mNumId ; parentId
    .db mLog2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLog2Handler ; handler (to be implemented)
mLogBase:
mLogBaseId equ 19
    .db mLogBaseId ; id
    .db mNumId ; parentId
    .db mLogBaseNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLogBaseHandler ; handler (to be implemented)
mAtan2:
mAtan2Id equ 20
    .db mAtan2Id ; id
    .db mNumId ; parentId
    .db mAtan2NameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAtan2Handler ; handler (to be implemented)
mBlank021:
mBlank021Id equ 21
    .db mBlank021Id ; id
    .db mNumId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup NUM: children: strip 2
mAbs:
mAbsId equ 22
    .db mAbsId ; id
    .db mNumId ; parentId
    .db mAbsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAbsHandler ; handler (to be implemented)
mSign:
mSignId equ 23
    .db mSignId ; id
    .db mNumId ; parentId
    .db mSignNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSignHandler ; handler (to be implemented)
mMod:
mModId equ 24
    .db mModId ; id
    .db mNumId ; parentId
    .db mModNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mModHandler ; handler (to be implemented)
mMin:
mMinId equ 25
    .db mMinId ; id
    .db mNumId ; parentId
    .db mMinNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMinHandler ; handler (to be implemented)
mMax:
mMaxId equ 26
    .db mMaxId ; id
    .db mNumId ; parentId
    .db mMaxNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMaxHandler ; handler (to be implemented)
; MenuGroup NUM: children: strip 3
mIntPart:
mIntPartId equ 27
    .db mIntPartId ; id
    .db mNumId ; parentId
    .db mIntPartNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mIntPartHandler ; handler (to be implemented)
mFracPart:
mFracPartId equ 28
    .db mFracPartId ; id
    .db mNumId ; parentId
    .db mFracPartNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFracPartHandler ; handler (to be implemented)
mFloor:
mFloorId equ 29
    .db mFloorId ; id
    .db mNumId ; parentId
    .db mFloorNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFloorHandler ; handler (to be implemented)
mCeil:
mCeilId equ 30
    .db mCeilId ; id
    .db mNumId ; parentId
    .db mCeilNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCeilHandler ; handler (to be implemented)
mNear:
mNearId equ 31
    .db mNearId ; id
    .db mNumId ; parentId
    .db mNearNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNearHandler ; handler (to be implemented)
; MenuGroup PROB: children
; MenuGroup PROB: children: strip 0
mComb:
mCombId equ 32
    .db mCombId ; id
    .db mProbId ; parentId
    .db mCombNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCombHandler ; handler (to be implemented)
mPerm:
mPermId equ 33
    .db mPermId ; id
    .db mProbId ; parentId
    .db mPermNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mPermHandler ; handler (to be implemented)
mFactorial:
mFactorialId equ 34
    .db mFactorialId ; id
    .db mProbId ; parentId
    .db mFactorialNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFactorialHandler ; handler (to be implemented)
mRandom:
mRandomId equ 35
    .db mRandomId ; id
    .db mProbId ; parentId
    .db mRandomNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomHandler ; handler (to be implemented)
mRandomSeed:
mRandomSeedId equ 36
    .db mRandomSeedId ; id
    .db mProbId ; parentId
    .db mRandomSeedNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRandomSeedHandler ; handler (to be implemented)
; MenuGroup CONV: children
; MenuGroup CONV: children: strip 0
mRToD:
mRToDId equ 37
    .db mRToDId ; id
    .db mConvId ; parentId
    .db mRToDNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRToDHandler ; handler (to be implemented)
mDToR:
mDToRId equ 38
    .db mDToRId ; id
    .db mConvId ; parentId
    .db mDToRNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDToRHandler ; handler (to be implemented)
mBlank039:
mBlank039Id equ 39
    .db mBlank039Id ; id
    .db mConvId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mHmsToHr:
mHmsToHrId equ 40
    .db mHmsToHrId ; id
    .db mConvId ; parentId
    .db mHmsToHrNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHmsToHrHandler ; handler (to be implemented)
mHrToHms:
mHrToHmsId equ 41
    .db mHrToHmsId ; id
    .db mConvId ; parentId
    .db mHrToHmsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mHrToHmsHandler ; handler (to be implemented)
; MenuGroup UNIT: children
; MenuGroup UNIT: children: strip 0
mFToC:
mFToCId equ 42
    .db mFToCId ; id
    .db mUnitId ; parentId
    .db mFToCNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFToCHandler ; handler (to be implemented)
mCToF:
mCToFId equ 43
    .db mCToFId ; id
    .db mUnitId ; parentId
    .db mCToFNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCToFHandler ; handler (to be implemented)
mBlank044:
mBlank044Id equ 44
    .db mBlank044Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mMiToKm:
mMiToKmId equ 45
    .db mMiToKmId ; id
    .db mUnitId ; parentId
    .db mMiToKmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMiToKmHandler ; handler (to be implemented)
mKmToMi:
mKmToMiId equ 46
    .db mKmToMiId ; id
    .db mUnitId ; parentId
    .db mKmToMiNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKmToMiHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 1
mFtToM:
mFtToMId equ 47
    .db mFtToMId ; id
    .db mUnitId ; parentId
    .db mFtToMNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFtToMHandler ; handler (to be implemented)
mMToFt:
mMToFtId equ 48
    .db mMToFtId ; id
    .db mUnitId ; parentId
    .db mMToFtNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMToFtHandler ; handler (to be implemented)
mBlank049:
mBlank049Id equ 49
    .db mBlank049Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mInToCm:
mInToCmId equ 50
    .db mInToCmId ; id
    .db mUnitId ; parentId
    .db mInToCmNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mInToCmHandler ; handler (to be implemented)
mCmToIn:
mCmToInId equ 51
    .db mCmToInId ; id
    .db mUnitId ; parentId
    .db mCmToInNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCmToInHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 2
mLbsToKg:
mLbsToKgId equ 52
    .db mLbsToKgId ; id
    .db mUnitId ; parentId
    .db mLbsToKgNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLbsToKgHandler ; handler (to be implemented)
mKgToLbs:
mKgToLbsId equ 53
    .db mKgToLbsId ; id
    .db mUnitId ; parentId
    .db mKgToLbsNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mKgToLbsHandler ; handler (to be implemented)
mBlank054:
mBlank054Id equ 54
    .db mBlank054Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mOzToG:
mOzToGId equ 55
    .db mOzToGId ; id
    .db mUnitId ; parentId
    .db mOzToGNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mOzToGHandler ; handler (to be implemented)
mGToOz:
mGToOzId equ 56
    .db mGToOzId ; id
    .db mUnitId ; parentId
    .db mGToOzNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGToOzHandler ; handler (to be implemented)
; MenuGroup UNIT: children: strip 3
mGalToL:
mGalToLId equ 57
    .db mGalToLId ; id
    .db mUnitId ; parentId
    .db mGalToLNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mGalToLHandler ; handler (to be implemented)
mLToGal:
mLToGalId equ 58
    .db mLToGalId ; id
    .db mUnitId ; parentId
    .db mLToGalNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mLToGalHandler ; handler (to be implemented)
mBlank059:
mBlank059Id equ 59
    .db mBlank059Id ; id
    .db mUnitId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mFlozToMl:
mFlozToMlId equ 60
    .db mFlozToMlId ; id
    .db mUnitId ; parentId
    .db mFlozToMlNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFlozToMlHandler ; handler (to be implemented)
mMlToFloz:
mMlToFlozId equ 61
    .db mMlToFlozId ; id
    .db mUnitId ; parentId
    .db mMlToFlozNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mMlToFlozHandler ; handler (to be implemented)
; MenuGroup DISP: children
; MenuGroup DISP: children: strip 0
mFix:
mFixId equ 62
    .db mFixId ; id
    .db mDispId ; parentId
    .db mFixNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mFixHandler ; handler (to be implemented)
mSci:
mSciId equ 63
    .db mSciId ; id
    .db mDispId ; parentId
    .db mSciNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSciHandler ; handler (to be implemented)
mEng:
mEngId equ 64
    .db mEngId ; id
    .db mDispId ; parentId
    .db mEngNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mEngHandler ; handler (to be implemented)
mBlank065:
mBlank065Id equ 65
    .db mBlank065Id ; id
    .db mDispId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank066:
mBlank066Id equ 66
    .db mBlank066Id ; id
    .db mDispId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup MODE: children
; MenuGroup MODE: children: strip 0
mRad:
mRadId equ 67
    .db mRadId ; id
    .db mModeId ; parentId
    .db mRadNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mRadHandler ; handler (to be implemented)
mDeg:
mDegId equ 68
    .db mDegId ; id
    .db mModeId ; parentId
    .db mDegNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mDegHandler ; handler (to be implemented)
mBlank069:
mBlank069Id equ 69
    .db mBlank069Id ; id
    .db mModeId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank070:
mBlank070Id equ 70
    .db mBlank070Id ; id
    .db mModeId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mBlank071:
mBlank071Id equ 71
    .db mBlank071Id ; id
    .db mModeId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup HYP: children
; MenuGroup HYP: children: strip 0
mBlank072:
mBlank072Id equ 72
    .db mBlank072Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mSinh:
mSinhId equ 73
    .db mSinhId ; id
    .db mHyperbolicId ; parentId
    .db mSinhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mSinhHandler ; handler (to be implemented)
mCosh:
mCoshId equ 74
    .db mCoshId ; id
    .db mHyperbolicId ; parentId
    .db mCoshNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mCoshHandler ; handler (to be implemented)
mTanh:
mTanhId equ 75
    .db mTanhId ; id
    .db mHyperbolicId ; parentId
    .db mTanhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mTanhHandler ; handler (to be implemented)
mBlank076:
mBlank076Id equ 76
    .db mBlank076Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
; MenuGroup HYP: children: strip 1
mBlank077:
mBlank077Id equ 77
    .db mBlank077Id ; id
    .db mHyperbolicId ; parentId
    .db mNullNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mNullHandler ; handler (predefined)
mAsinh:
mAsinhId equ 78
    .db mAsinhId ; id
    .db mHyperbolicId ; parentId
    .db mAsinhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAsinhHandler ; handler (to be implemented)
mAcosh:
mAcoshId equ 79
    .db mAcoshId ; id
    .db mHyperbolicId ; parentId
    .db mAcoshNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAcoshHandler ; handler (to be implemented)
mAtanh:
mAtanhId equ 80
    .db mAtanhId ; id
    .db mHyperbolicId ; parentId
    .db mAtanhNameId ; nameId
    .db 0 ; numStrips
    .db 0 ; stripBeginId
    .dw mAtanhHandler ; handler (to be implemented)
mBlank081:
mBlank081Id equ 81
    .db mBlank081Id ; id
    .db mHyperbolicId ; parentId
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
mNumNameId equ 2
    .dw mNumName
mProbNameId equ 3
    .dw mProbName
mConvNameId equ 4
    .dw mConvName
mUnitNameId equ 5
    .dw mUnitName
mHelpNameId equ 6
    .dw mHelpName
mDispNameId equ 7
    .dw mDispName
mModeNameId equ 8
    .dw mModeName
mHyperbolicNameId equ 9
    .dw mHyperbolicName
mPercentNameId equ 10
    .dw mPercentName
mDeltaPercentNameId equ 11
    .dw mDeltaPercentName
mCubeNameId equ 12
    .dw mCubeName
mCubeRootNameId equ 13
    .dw mCubeRootName
mAlog2NameId equ 14
    .dw mAlog2Name
mLog2NameId equ 15
    .dw mLog2Name
mLogBaseNameId equ 16
    .dw mLogBaseName
mAtan2NameId equ 17
    .dw mAtan2Name
mAbsNameId equ 18
    .dw mAbsName
mSignNameId equ 19
    .dw mSignName
mModNameId equ 20
    .dw mModName
mMinNameId equ 21
    .dw mMinName
mMaxNameId equ 22
    .dw mMaxName
mIntPartNameId equ 23
    .dw mIntPartName
mFracPartNameId equ 24
    .dw mFracPartName
mFloorNameId equ 25
    .dw mFloorName
mCeilNameId equ 26
    .dw mCeilName
mNearNameId equ 27
    .dw mNearName
mCombNameId equ 28
    .dw mCombName
mPermNameId equ 29
    .dw mPermName
mFactorialNameId equ 30
    .dw mFactorialName
mRandomNameId equ 31
    .dw mRandomName
mRandomSeedNameId equ 32
    .dw mRandomSeedName
mRToDNameId equ 33
    .dw mRToDName
mDToRNameId equ 34
    .dw mDToRName
mHmsToHrNameId equ 35
    .dw mHmsToHrName
mHrToHmsNameId equ 36
    .dw mHrToHmsName
mFToCNameId equ 37
    .dw mFToCName
mCToFNameId equ 38
    .dw mCToFName
mMiToKmNameId equ 39
    .dw mMiToKmName
mKmToMiNameId equ 40
    .dw mKmToMiName
mFtToMNameId equ 41
    .dw mFtToMName
mMToFtNameId equ 42
    .dw mMToFtName
mInToCmNameId equ 43
    .dw mInToCmName
mCmToInNameId equ 44
    .dw mCmToInName
mLbsToKgNameId equ 45
    .dw mLbsToKgName
mKgToLbsNameId equ 46
    .dw mKgToLbsName
mOzToGNameId equ 47
    .dw mOzToGName
mGToOzNameId equ 48
    .dw mGToOzName
mGalToLNameId equ 49
    .dw mGalToLName
mLToGalNameId equ 50
    .dw mLToGalName
mFlozToMlNameId equ 51
    .dw mFlozToMlName
mMlToFlozNameId equ 52
    .dw mMlToFlozName
mFixNameId equ 53
    .dw mFixName
mSciNameId equ 54
    .dw mSciName
mEngNameId equ 55
    .dw mEngName
mRadNameId equ 56
    .dw mRadName
mDegNameId equ 57
    .dw mDegName
mSinhNameId equ 58
    .dw mSinhName
mCoshNameId equ 59
    .dw mCoshName
mTanhNameId equ 60
    .dw mTanhName
mAsinhNameId equ 61
    .dw mAsinhName
mAcoshNameId equ 62
    .dw mAcoshName
mAtanhNameId equ 63
    .dw mAtanhName

; Table of names as NUL terminated C strings.
mNullName:
    .db 0
mRootName:
    .db "root", 0
mNumName:
    .db "NUM", 0
mProbName:
    .db "PROB", 0
mConvName:
    .db "CONV", 0
mUnitName:
    .db "UNIT", 0
mHelpName:
    .db "HELP", 0
mDispName:
    .db "DISP", 0
mModeName:
    .db "MODE", 0
mHyperbolicName:
    .db "HYP", 0
mPercentName:
    .db Spercent, 0
mDeltaPercentName:
    .db ScapDelta, Spercent, 0
mCubeName:
    .db 'X', Scaret, '3', 0
mCubeRootName:
    .db ScubeR, Sroot, 'X', 0
mAlog2Name:
    .db '2', Scaret, 'X', 0
mLog2Name:
    .db "LOG2", 0
mLogBaseName:
    .db "LOGB", 0
mAtan2Name:
    .db "ATN2", 0
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
mHmsToHrName:
    .db Sconvert, 'H', 'R', 0
mHrToHmsName:
    .db Sconvert, 'H', 'M', 'S', 0
mFToCName:
    .db Sconvert, Stemp, 'C', 0
mCToFName:
    .db Sconvert, Stemp, 'F', 0
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
