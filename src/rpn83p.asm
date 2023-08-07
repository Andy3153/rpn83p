; RPN calculator for the TI-83 Plus and TI-84 Plus calculators. Inspired
; by the HP-42S calculator.
;
; See Appendix A ("Creating Flash Applications with SPASM") of the book ("TI-83
; ASM For the Absolute Beginner") at
; (https://www.ticalc.org/archives/files/fileinfo/437/43784.html) regarding the
; "app.inc" include, the defpage() macro, and the validate() macro.
;
; This needs to be compiled using spasm-ng into a *.8xk file.

.nolist
#include "ti83plus.inc"

#ifdef FLASHAPP
#include "app.inc"
; Define single page flash app
defpage(0, "RPN83P")
#endif

.list

;-----------------------------------------------------------------------------

; Define the Cursor character
cursorChar equ LcurI
cursorCharAlt equ LcurO
signChar equ Lneg ; different from '-' which is LDash

; Menu keys, left to right.
keyMenu1 equ kYequ
keyMenu2 equ kWindow
keyMenu3 equ kZoom
keyMenu4 equ kTrace
keyMenu5 equ kGraph

; Flags that indicate the need to re-draw the display. These are intended to
; be optimization purposes. In theory, we could eliminate these dirty flags
; without affecting the correctness of the rest of the RPN83P app.
dirtyFlags equ asm_Flag1
dirtyFlagsInput equ 0 ; set if the input buffer is dirty
dirtyFlagsStack equ 1 ; set if the stack is dirty
dirtyFlagsMenu equ 2 ; set if the menu selection is dirty
dirtyFlagsTrigMode equ 3 ; set if the trig status is dirty
dirtyFlagsFloatMode equ 4 ; set if the floating mode is dirty
dirtyFlagsBaseMode equ 5 ; set if the base mode is dirty
dirtyFlagsErrorCode equ 6 ; set if the error code is dirty
; The dirtyFlagsXLabel flag is set if the 'X:' label is dirty due to the
; command arg mode. The 6x8 cell occupied by the 'X:' label is rendered in
; small-font, which means that it actually uses only 7 rows of pixels from the
; top. During the command arg mode, the cell is replaced by the first letter of
; the label (i.e. "FIX", "SCI", or "ENG"), which is rendered using large font,
; so that first letter consumes 8 rows of pixels. When the command arg mode
; exits, the bottom row of pixels contains artifacts of the command arg mode
; label. The easy solution is to write a large-font space (Lspace) into that
; first cell, then write the "X:" label in small font. But doing this for every
; refresh of that line (e.g. when entering the digits of a number) would cause
; flickering of the "X:" label on every redraw. This flag allows us to optimize
; the redraw algorithm so that the Lspace character *only* when transitioning
; from the command arg mode to normal mode. That would cause a redraw of the
; entire X line, so the slight flicker of the "X:" label should be completely
; imperceptible.
dirtyFlagsXLabel equ 7

; Flags for RPN stack modes. Offset from IY register.
rpnFlags equ asm_Flag2
rpnFlagsEditing equ 0 ; set if in edit mode
rpnFlagsArgMode equ 1 ; set if in command argument mode
rpnFlagsLiftEnabled equ 2 ; set if stack lift is enabled (ENTER disables it)

; Flags for the inputBuf. Offset from IY register.
inputBufFlags equ asm_Flag3
inputBufFlagsDecPnt equ 1 ; set if decimal point exists
inputBufFlagsEE equ 2 ; set if EE symbol exists
inputBufFlagsClosedEmpty equ 3 ; inputBuf empty when closeInputBuf() called
inputBufFlagsExpSign equ 4 ; exponent sign bit detected during parsing

;-----------------------------------------------------------------------------
; Application variables and buffers.
;-----------------------------------------------------------------------------

; Begin RPN83P variables.
rpnVarsBegin equ tempSwapArea

; The result code after the execution of each handler. Success is code 0. If a
; TI-OS exception is thrown (through a `bjump(ErrXxx)`), the exception handler
; places a system error code into here. Before calling a handler, set this to 0
; because vast majority of handlers will not explicitly set handlerCode to 0
; upon success. (This makes coding easier because a successful handler can
; simply do a `ret` or a conditional `ret`.) A few handlers will set a custom,
; non-zero code to indicate an error.
handlerCode equ rpnVarsBegin

; The errorCode is displayed on the LCD screen if non-zero. This is set to the
; value of handlerCode after every execution of a handler. Inside a handler,
; the errorCode will be the handlerCode of the previous handler. This is useful
; for the CLEAR handler which will simply clear the displayed errorCode if
; non-zero.
errorCode equ handlerCode + 1

; Current base mode. Allowed values are: 2, 8, 10, 16. Anything else is
; interpreted as 10.
baseMode equ errorCode + 1

; String buffer for keyboard entry. This is a Pascal-style with a single size
; byte at the start. It does not include the cursor displayed at the end of the
; string. The equilvalent C struct is:
;
;   struct inputBuf {
;       uint8_t size;
;       char buf[14];
;   };
inputBuf equ baseMode + 1
inputBufSize equ inputBuf ; size byte of the pascal string
inputBufBuf equ inputBuf + 1
inputBufMax equ 14 ; maximum size of buffer, not including appended cursor
inputBufSizeOf equ inputBufMax + 1

; When the inputBuf is used as a command argBuf, the maximum number of
; characters in the buffer is 2.
argBuf equ inputBuf
argBufSize equ inputBufSize
argBufMax equ inputBufMax
argBufSizeMax equ 2

; Location (offset index) of the one past the 'E' symbol if it exists. Zero
; indicates that 'E' does NOT exist.
inputBufEEPos equ inputBuf + inputBufSizeOf
; Length of EE digits. Maximum of 2.
inputBufEELen equ inputBufEEPos + 1
; Max number of digits allowed for exponent.
inputBufEELenMax equ 2

; Temporary buffer for parsing keyboard input into a floating point number. This
; is a pascal string that contains the normalized floating point number, one
; character per digit. It's a stepping stone before converting this into the
; packed floating point number format used by TI-OS. The equivalent C struct
; is:
;
;   struct parseBuf {
;       uint8_t size; // number of digits in mantissa, 0 for 0.0
;       char man[14];  // mantissa, implicit starting decimal point
;   }
;
; A TI-OS floating number can have a mantissa of a maximum 14 digits.
parseBuf equ inputBufEELen + 1
parseBufSize equ parseBuf ; size byte of the pascal string
parseBufMan equ parseBufSize + 1
parseBufMax equ 14
parseBufSizeOf equ parseBufMax + 1

; Floating point number buffer, 9 bytes for TI-OS:
;
;   struct floatBuf {
;       uint8_t type;
;       uint8_t exp;
;       uint8_t man[7];
;   }
floatBuf equ parseBuf + parseBufSizeOf
floatBufType equ floatBuf ; type
floatBufExp equ floatBufType + 1 ; exponent, shifted by $80
floatBufMan equ floatBufExp + 1 ; mantissa, 2 digits per byte
floatBufSizeOf equ 9

; Menu variables. The C equivalent is:
;
;   struct menu {
;     uint8_t groupId; // id of the current menu group
;     uint8_t stripIndex; // menu strip, groups of 5
;   }
menuGroupId equ floatBuf + floatBufSizeOf
menuStripIndex equ menuGroupId + 1

; Menu name, copied here as a Pascal string.
;
;   struct menuName {
;       uint8_t size;
;       char buf[5];
;   }
menuName equ menuStripIndex + 1
menuNameSize equ menuName
menuNameBuf equ menuName + 1
menuNameBufMax equ 5
menuNameSizeOf equ 6

; Pointer to a C string that prompts before the argBuf.
; void *argPrompt;
argPrompt equ menuName + menuNameSizeOf
argPromptSizeOf equ 2

; Pointer to the command handler waiting for the arg.
argHandler equ argPrompt + argPromptSizeOf
argHandlerSizeOf equ 2

; Parsed value of argBuf.
argValue equ argHandler + argHandlerSizeOf
argValueSizeOf equ 1

; End RPN83P variables. Total size of vars = rpnVarsEnd - rpnVarsBegin.
rpnVarsEnd equ argValue + argValueSizeOf

;-----------------------------------------------------------------------------

#ifndef FLASHAPP
.org userMem - 2
.db t2ByteTok, tAsmCmp
#endif

main:
    bcall(_RunIndicOff)
    res appAutoScroll, (iy + appFlags) ; disable auto scroll
    res appTextSave, (iy + appFlags) ; disable shawdow text
    res lwrCaseActive, (iy + appLwrCaseFlag) ; disable ALPHA-ALPHA lowercase
    bcall(_ClrLCDFull)

    call initBase
    call initErrorCode
    call initInputBuf
    call initArgBuf
    call initStack
    call initMenu
    call initDisplay
    ; [[fall through]]

; The main event/read loop. Read button and dispatch to the appropriate
; handler. Some of the functionalities are:
;
;   - 2ND-QUIT: quit app
;   - 0-9: add to input buffer
;   - . and (-): add to input buffer
;   - , or 2nd-EE: add scientific notation 'E'
;   - DEL: removes the last character in the input buffer
;   - CLEAR: remove RPN stack X register, or clear the input buffer
;
; See 83pa28d/week2/day12.
readLoop:
    ; call debugFlags
    call displayAll

    ; Set the handler code initially to 0.
    xor a
    ld (handlerCode), a

    ; Get the key code, and reset the ON flag right after. See TI-83 Plus SDK
    ; guide, p. 69. If this flag is not reset, then the next bcall(_DispHL)
    ; causes subsequent bcall(_GetKey) to always return 0. Interestingly, if
    ; the flag is not reset, but the next call is another bcall(_GetKey), then
    ; it sort of seems to work. Except that upon exiting, the TI-OS displays an
    ; Quit/Goto error message.
    bcall(_GetKey)
    res onInterrupt, (iy + onFlags)

    ; Check for 2nd-Quit to Quit. ON (0) triggers the handleKeyMenuBack() to
    ; emulate the ON/EXIT key on the HP 42S which exits nested menus on that
    ; calculator.
    ; TODO: The LeftArrow is also bound to hanldeKeyMenuBack(), and that seems
    ; convenient because LeftArrow is in close proximity to UpArrow and
    ; DownArrow. Maybe on the TI-83/TI-84 calculators, the ON button should
    ; just do nothing.
    cp kQuit
    jr z, mainExit

    ; Install error handler
    ld hl, readLoopException
    call APP_PUSH_ERRORH
    ; Handle the normal buttons or the menu F1-F5 buttons.
    call lookupKey
    ; Uninstall error handler
    call APP_POP_ERRORH

    ; Transfer the handler code to errorCode.
    ld a, (handlerCode)
readLoopSetErrorCode:
    call setErrorCode
    jr readLoop

readLoopException:
    ; Handle system exception. Register A contains the system error code.
    call setHandlerCodeToSystemCode
    jr readLoopSetErrorCode

; Clean up and exit app.
mainExit:
    set appAutoScroll, (iy + appFlags)
    bcall(_ClrLCDFull)
    bcall(_HomeUp)
#ifdef FLASHAPP
    bjump(_JForceCmdNoChar)
#else
    ret
#endif

;-----------------------------------------------------------------------------

#include "vars.asm"
#include "handlers.asm"
#include "pstring.asm"
#include "input.asm"
#include "display.asm"
#include "errorcode.asm"
#include "base.asm"
#include "menu.asm"
#include "menuhandlers.asm"

#ifdef DEBUG
#include "debug.asm"
#endif

#include "common.asm"
#include "const.asm"

; Place data files at the end, because the TI-OS prevents execution of assembly
; code if it spills over to page $C000. The limitation does not apply to data.
#include "handlertab.asm"
#include "menudef.asm"

.end

;-----------------------------------------------------------------------------

#ifdef FLASHAPP
; Not sure if this needs to go before or after the `.end`.
validate()
#endif
