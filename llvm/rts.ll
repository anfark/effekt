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

define fastcc void @pushStack(%Sp* %spp, %Stk %stk) alwaysinline {
    %newsp = extractvalue %Stk %stk, 0
    %newbase = extractvalue %Stk %stk, 1
    %newboxessp = extractvalue %Stk %stk, 2
    %newboxesbase = extractvalue %Stk %stk, 3
    %oldsp = load %Sp, %Sp* %spp
    %oldbase = load %Sp, %Sp* @base
    %oldboxessp = load %BoxesSp, %BoxesSp* @boxessp
    %oldboxesbase = load %BoxesSp, %BoxesSp* @boxesbase

    %msp = load i64, i64* @msp
    %mspsp = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %msp, i32 0
    %mspbase = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %msp, i32 1
    %mspboxessp = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %msp, i32 2
    %mspboxesbase = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %msp, i32 3
    %newmsp = add i64 %msp, 1

    store i64 %newmsp, i64* @msp
    store %Sp %newsp, %Sp* %spp
    store %Sp %newbase, %Sp* @base
    store %BoxesSp %newboxessp, %BoxesSp* @boxessp
    store %BoxesSp %newboxesbase, %BoxesSp* @boxesbase
    store %Sp %oldsp, %Sp* %mspsp
    store %Sp %oldbase, %Sp* %mspbase
    store %BoxesSp %oldboxessp, %BoxesSp* %mspboxessp
    store %BoxesSp %oldboxesbase, %BoxesSp* %mspboxesbase
    ret void
}

define fastcc %Stk @popStack(%Sp* %spp) alwaysinline {
    %msp = load i64, i64* @msp
    %newmsp = add i64 %msp, -1
    %mspsp = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %newmsp, i32 0
    %mspbase = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %newmsp, i32 1
    %mspboxessp = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %newmsp, i32 2
    %mspboxesbase = getelementptr [16 x %Stk], [16 x %Stk]* @metaStack, i64 0, i64 %newmsp, i32 3

    %oldsp = load %Sp, %Sp* %spp
    %oldbase = load %Sp, %Sp* @base
    %oldboxessp = load %BoxesSp, %BoxesSp* @boxessp
    %oldboxesbase = load %BoxesSp, %BoxesSp* @boxesbase
    %newsp = load %Sp, %Sp* %mspsp
    %newbase = load %Sp, %Sp* %mspbase
    %newboxessp = load %BoxesSp, %BoxesSp* %mspboxessp
    %newboxesbase = load %BoxesSp, %BoxesSp* %mspboxesbase

    store i64 %newmsp, i64* @msp
    store %Sp %newsp, %Sp* %spp
    store %Sp %newbase, %Sp* @base
    store %BoxesSp %newboxessp, %BoxesSp* @boxessp
    store %BoxesSp %newboxesbase, %BoxesSp* @boxesbase

    %result.0 = insertvalue %Stk undef, %Sp %oldsp, 0
    %result.1 = insertvalue %Stk %result.0, %Sp %oldbase, 1
    %result.2 = insertvalue %Stk %result.1, %BoxesSp %oldboxessp, 2
    %result.3 = insertvalue %Stk %result.2, %BoxesSp %oldboxesbase, 3

    ret %Stk %result.3
}

define fastcc %Stk @copyStk(%Stk %stk) alwaysinline {
entry:
    %sp = extractvalue %Stk %stk, 0
    %base = extractvalue %Stk %stk, 1
    %boxessp = extractvalue %Stk %stk, 2
    %boxesbase = extractvalue %Stk %stk, 3

    %newstk = call fastcc %Stk @newStack()
    %newsp = extractvalue %Stk %newstk, 0
    %newbase = extractvalue %Stk %newstk, 1
    %newboxessp = extractvalue %Stk %newstk, 2
    %newboxesbase = extractvalue %Stk %newstk, 3

    %intsp = ptrtoint %Sp %sp to i64
    %intbase = ptrtoint %Sp %base to i64
    %size = sub i64 %intsp, %intbase
    %baseptr = bitcast %Sp %base to i8*
    %newbaseptr = bitcast %Sp %newbase to i8*
    call void @memcpy(i8* %newbaseptr, i8* %baseptr, i64 %size)
    %intnewbase = ptrtoint %Sp %newbase to i64
    %intnewsp = add i64 %intnewbase, %size
    %newsp.1 = inttoptr i64 %intnewsp to %Sp
    br label %comp
comp:
    %currentboxessp = phi %BoxesSp [%boxesbase, %entry], [%nextboxessp, %loop]
    %currentnewboxessp = phi %BoxesSp [%newboxessp, %entry], [%nextnewboxessp, %loop]
    %atend = icmp eq %BoxesSp %currentboxessp, %boxessp
    br i1 %atend, label %done, label %loop
loop:
    %currentstk = load %Stk, %Stk* %boxessp
    %currentnewstk = call fastcc %Stk @copyStk(%Stk %currentstk)
    store %Stk %currentnewstk, %Stk* %currentnewboxessp
    %nextboxessp = getelementptr %Stk, %Stk* %currentboxessp, i64 1
    %nextnewboxessp = getelementptr %Stk, %Stk* %currentnewboxessp, i64 1
    br label %comp
done:
    %newstk.1 = insertvalue %Stk %newstk, %Sp %newsp.1, 0
    %newstk.2 = insertvalue %Stk %newstk.1, %BoxesSp %currentnewboxessp, 2
    ret %Stk %newstk.2
}

define fastcc void @eraseStk(%Stk %stk) alwaysinline {
entry:
    %base = extractvalue %Stk %stk, 1
    %boxessp = extractvalue %Stk %stk, 2
    %boxesbase = extractvalue %Stk %stk, 3

    br label %comp
comp:
    %currentboxessp = phi %BoxesSp [%boxessp, %entry], [%newboxessp, %loop]
    %atbase = icmp eq %BoxesSp %boxessp, %boxesbase
    br i1 %atbase, label %done, label %loop
loop:
    %newboxessp = getelementptr %Stk, %Stk* %boxessp, i64 -1
    %val = load %Stk, %Stk* %newboxessp
    call fastcc void @eraseStk(%Stk %val)
    br label %comp
done:
    %baseptr = bitcast %Sp %base to i8*
    call void @free(i8* %baseptr)
    ret void
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

%Cnt = type void (%Sp)*

define fastcc %Cnt @loadCnt(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %Cnt*
    %newsptocnt = getelementptr %Cnt, %Cnt* %sptocnt, i64 -1
    %cnt   = load %Cnt, %Cnt* %newsptocnt
    %newsp = bitcast %Cnt* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %Cnt %cnt
}

define fastcc void @storeCnt(%Sp* %spp, %Cnt %cnt) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %Cnt*
    store %Cnt %cnt, %Cnt* %sptocnt
    %newsptocnt = getelementptr %Cnt, %Cnt* %sptocnt, i64 1
    %newsp = bitcast %Cnt* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

define fastcc %Stk @loadStk(%Sp* %spp) alwaysinline {
    %sp    = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %Stk*
    %newsptocnt = getelementptr %Stk, %Stk* %sptocnt, i64 -1
    %cnt   = load %Stk, %Stk* %newsptocnt
    %newsp = bitcast %Stk* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret %Stk %cnt
}

define fastcc void @storeStk(%Sp* %spp, %Stk %cnt) alwaysinline {
    %sp   = load %Sp, %Sp* %spp

    %sptocnt = bitcast %Sp %sp to %Stk*
    store %Stk %cnt, %Stk* %sptocnt
    %newsptocnt = getelementptr %Stk, %Stk* %sptocnt, i64 1
    %newsp = bitcast %Stk* %newsptocnt to %Sp

    store %Sp %newsp, %Sp* %spp
    ret void
}

