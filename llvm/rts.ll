; Basic types

%Word = type i64

%Sp = type %Word*
%BoxesSp = type %Stk*

%Base = type %Sp
%BoxesBase = type %BoxesSp

%Stk = type { %Sp, %Base, %BoxesSp, %BoxesBase }

; Global locations

@base = global %Base undef
@boxessp = global %BoxesSp undef
@boxesbase = global %BoxesBase undef

@metaStack = global [16 x %Stk] undef

@msp = global i64 0

; Foreign imports

declare %Sp @malloc(i64)
declare void @free(%Sp)
declare void @memcpy(%Sp, %Sp, i64)
declare void @print(i64)

; Meta-stack management

define fastcc %Stk @newStack() alwaysinline {
    %base = call %Sp @malloc(i64 8800)
    %boxesbase.0 = getelementptr %Word, %Word* %base, %Word 1000
    %boxesbase = bitcast %Sp %boxesbase.0 to %BoxesSp

    %stk.0 = insertvalue %Stk undef, %Sp %base, 0
    %stk.1 = insertvalue %Stk %stk.0, %Sp %base, 1
    %stk.2 = insertvalue %Stk %stk.1, %BoxesSp %boxesbase, 2
    %stk = insertvalue %Stk %stk.2, %BoxesSp %boxesbase, 3

    ret %Stk %stk
}

; Generated type-specialized internal functions

%Cnt1 = type void (%Sp, i64)*

define fastcc %Cnt1 @loadCnt1(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %Cnt1*
    %newsptocnt = getelementptr %Cnt1, %Cnt1* %sptocnt, i64 -1
    %val   = load %Cnt1, %Cnt1* %newsptocnt
    %newsp = bitcast %Cnt1* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %Cnt1 %val
}

; RTS initialization

%ToplevelCnt = type void (%Sp, i64)*

define fastcc void @toplevel(%Sp %sp, i64 %res) {
    ; TODO drop last meta cont and clean up
    ret void
}

define fastcc void @storeToplevel(%Sp* %spp) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %ToplevelCnt*
    store %ToplevelCnt @toplevel, %ToplevelCnt* %sptocnt
    %newsptocnt = getelementptr %ToplevelCnt, %ToplevelCnt* %sptocnt, i64 1
    %newsp = bitcast %ToplevelCnt* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

define fastcc %Sp @initializeRts() alwaysinline {
    %stk = call fastcc %Stk @newStack()

    %sp = extractvalue %Stk %stk, 0
    %base = extractvalue %Stk %stk, 1
    %boxessp = extractvalue %Stk %stk, 2
    %boxesbase = extractvalue %Stk %stk, 3

    %spp = alloca %Sp
    store %Sp %sp, %Sp* %spp
    store %Sp %base, %Sp* @base
    store %BoxesSp %boxessp, %BoxesSp* @boxessp
    store %BoxesSp %boxesbase, %BoxesSp* @boxesbase

    call fastcc void @storeToplevel(%Sp* %spp)

    %newsp = load %Sp, %Sp* %spp
    ret %Sp %newsp
}

