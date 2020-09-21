; Basic types

%Word = type i64

%Sp = type %Word*
%BoxesSp = type %Stk*

%Base = type %Sp
%BoxesBase = type %BoxesSp

%Stk = type { %Sp, %Base, %BoxesSp, %BoxesBase }

; Global locations

@base = private global %Base undef
@boxessp = private global %BoxesSp undef
@boxesbase = private global %BoxesBase undef

@metaStack = private global [16 x %Stk] undef

@msp = private global i64 0

; Foreign imports

declare i8* @malloc(i64)
declare void @free(i8*)
declare void @memcpy(i8*, i8*, i64)
declare void @print(i64)

; Meta-stack management

define fastcc %Stk @newStack() alwaysinline {
    %area = call i8* @malloc(i64 8800)
    %base = bitcast i8* %area to %Sp
    %boxesbase.0 = getelementptr %Word, %Word* %base, %Word 1000
    %boxesbase = bitcast %Sp %boxesbase.0 to %BoxesSp

    %stk.0 = insertvalue %Stk undef, %Sp %base, 0
    %stk.1 = insertvalue %Stk %stk.0, %Sp %base, 1
    %stk.2 = insertvalue %Stk %stk.1, %BoxesSp %boxesbase, 2
    %stk = insertvalue %Stk %stk.2, %BoxesSp %boxesbase, 3

    ret %Stk %stk
}

; RTS initialization

define fastcc void @topLevel(%Sp noalias %sp, i64 %res) {
    ; TODO properly clean up
    %area = bitcast %Sp %sp to i8*
    call void @free(i8* %area)
    ret void
}



define fastcc noalias %Sp @initializeRts() alwaysinline {
    %stk = call fastcc %Stk @newStack()

    %sp = extractvalue %Stk %stk, 0
    %base = extractvalue %Stk %stk, 1
    %boxessp = extractvalue %Stk %stk, 2
    %boxesbase = extractvalue %Stk %stk, 3

    store %Sp %base, %Sp* @base
    store %BoxesSp %boxessp, %BoxesSp* @boxessp
    store %BoxesSp %boxesbase, %BoxesSp* @boxesbase

    ret %Sp %sp
}

; Generated type-specialized internal functions
; TODO generate

%Int = type i64

define fastcc %Int @loadInt(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %newsp = getelementptr %Int, %Int* %sp, i64 -1
    %val   = load %Int, %Int* %newsp

    store %Sp %newsp, %Sp* %spp
    ret %Int %val
}

define fastcc void @storeInt(%Sp* %spp, %Int %val) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    store %Int %val, %Int* %sp
    %newsp = getelementptr %Int, %Int* %sp, i64 1

    store %Sp %newsp, %Sp* %spp
    ret void
}

%Boolean = type i1

define fastcc %Boolean @loadBoolean(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptoval = bitcast %Sp %sp to %Boolean*
    %newsptoval = getelementptr %Boolean, %Boolean* %sptoval, i64 -1
    %val   = load %Boolean, %Boolean* %newsptoval
    %newsp = bitcast %Boolean* %newsptoval to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %Boolean %val
}

define fastcc void @storeBoolean(%Sp* %spp, %Boolean %val) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptoval = bitcast %Sp %sp to %Boolean*
    store %Boolean %val, %Boolean* %sptoval
    %newsptoval = getelementptr %Boolean, %Boolean* %sptoval, i64 1
    %newsp = bitcast %Boolean* %newsptoval to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

%Unit = type i64

define fastcc %Unit @loadUnit(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptoval = bitcast %Sp %sp to %Unit*
    %newsptoval = getelementptr %Unit, %Unit* %sptoval, i64 -1
    %val   = load %Unit, %Unit* %newsptoval
    %newsp = bitcast %Unit* %newsptoval to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %Unit %val
}

define fastcc void @storeUnit(%Sp* %spp, %Unit %val) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptoval = bitcast %Sp %sp to %Unit*
    store %Unit %val, %Unit* %sptoval
    %newsptoval = getelementptr %Unit, %Unit* %sptoval, i64 1
    %newsp = bitcast %Unit* %newsptoval to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

%CntInt = type void (%Sp, %Int)*

define fastcc %CntInt @loadCntInt(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntInt*
    %newsptocnt = getelementptr %CntInt, %CntInt* %sptocnt, i64 -1
    %cnt   = load %CntInt, %CntInt* %newsptocnt
    %newsp = bitcast %CntInt* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %CntInt %cnt
}

define fastcc void @storeCntInt(%Sp* %spp, %CntInt %cnt) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntInt*
    store %CntInt %cnt, %CntInt* %sptocnt
    %newsptocnt = getelementptr %CntInt, %CntInt* %sptocnt, i64 1
    %newsp = bitcast %CntInt* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

%CntBoolean = type void (%Sp, %Boolean)*

define fastcc %CntBoolean @loadCntBoolean(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntBoolean*
    %newsptocnt = getelementptr %CntBoolean, %CntBoolean* %sptocnt, i64 -1
    %val   = load %CntBoolean, %CntBoolean* %newsptocnt
    %newsp = bitcast %CntBoolean* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %CntBoolean %val
}

define fastcc void @storeCntBoolean(%Sp* %spp, %CntBoolean %val) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntBoolean*
    store %CntBoolean %val, %CntBoolean* %sptocnt
    %newsptocnt = getelementptr %CntBoolean, %CntBoolean* %sptocnt, i64 1
    %newsp = bitcast %CntBoolean* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

%CntUnit = type void (%Sp, %Unit)*

define fastcc %CntUnit @loadCntUnit(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntUnit*
    %newsptocnt = getelementptr %CntUnit, %CntUnit* %sptocnt, i64 -1
    %val   = load %CntUnit, %CntUnit* %newsptocnt
    %newsp = bitcast %CntUnit* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %CntUnit %val
}

define fastcc void @storeCntUnit(%Sp* %spp, %CntUnit %val) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %CntUnit*
    store %CntUnit %val, %CntUnit* %sptocnt
    %newsptocnt = getelementptr %CntUnit, %CntUnit* %sptocnt, i64 1
    %newsp = bitcast %CntUnit* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}


