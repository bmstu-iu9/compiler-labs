PROGRAM BTPC;
{$IFDEF WIN32}{$APPTYPE CONSOLE}{$ENDIF}

CONST MaxCode=65536;
      MaxIdent=512;
      MaxType=32;
      MaxList=10;
      MaxAlfa=20;

      OPNone=-1;
      OPAdd=0;
      OPNeg=1;
      OPMul=2;
      OPDivD=3;
      OPRemD=4;
      OPDiv2=5;
      OPRem2=6;
      OPEqlI=7;
      OPNEqI=8;
      OPLssI=9;
      OPLeqI=10;
      OPGtrI=11;
      OPGEqI=12;
      OPDupl=13;
      OPSwap=14;
      OPAndB=15;
      OPOrB=16;
      OPLoad=17;
      OPStore=18;
      OPHalt=19;
      OPWrI=20;
      OPWrC=21;
      OPWrL=22;
      OPRdI=23;
      OPRdC=24;
      OPRdL=25;
      OPEOF=26;
      OPEOL=27;
      OPLdC=28;
      OPLdA=29;
      OPLdLA=30;
      OPLdL=31;
      OPLdG=32;
      OPStL=33;
      OPStG=34;
      OPMove=35;
      OPCopy=36;
      OPAddC=37;
      OPMulC=38;
      OPJmp=39;
      OPJZ=40;
      OPCall=41;
      OPAdjS=42;
      OPExit=43;

      TokIdent=0;
      TokNumber=1;
      TokStrC=2;
      TokPlus=3;
      TokMinus=4;
      TokMul=5;
      TokLBracket=6;
      TokRBracket=7;
      TokColon=8;
      TokEql=9;
      TokNEq=10;
      TokLss=11;
      TokLEq=12;
      TokGtr=13;
      TokGEq=14;
      TokLParent=15;
      TokRParent=16;
      TokComma=17;
      TokSemi=18;
      TokPeriod=19;
      TokAssign=20;
      SymBEGIN=21;
      SymEND=22;
      SymIF=23;
      SymTHEN=24;
      SymELSE=25;
      SymWHILE=26;
      SymDO=27;
      SymCASE=28;
      SymREPEAT=29;
      SymUNTIL=30;
      SymFOR=31;
      SymTO=32;
      SymDOWNTO=33;
      SymNOT=34;
      SymDIV=35;
      SymMOD=36;
      SymAND=37;
      SymOR=38;
      SymCONST=39;
      SymVAR=40;
      SymTYPE=41;
      SymARRAY=42;
      SymOF=43;
      SymPACKED=44;
      SymRECORD=45;
      SymPROGRAM=46;
      SymFORWARD=47;
      SymHALT=48;
      SymFUNC=49;
      SymPROC=50;

      IdCONST=0;
      IdVAR=1;
      IdFIELD=2;
      IdTYPE=3;
      IdFUNC=4;

      KindSIMPLE=0;
      KindARRAY=1;
      KindRECORD=2;

      TypeINT=1;
      TypeBOOL=2;
      TypeCHAR=3;
      TypeSTR=4;

      FunCHR=0;
      FunORD=1;
      FunWRITE=2;
      FunWRITELN=3;
      FunREAD=4;
      FunREADLN=5;
      FunEOF=6;
      FunEOFLN=7;

TYPE TAlfa=ARRAY[1..MaxAlfa] OF CHAR;

     TIdent=RECORD
      Name:TAlfa;
      Link:INTEGER;
      TypeDef:INTEGER;
      Kind:INTEGER;
      Value:INTEGER;
      VLevel:INTEGER;
      VAdr:INTEGER;
      RefPar:BOOLEAN;
      Offset:INTEGER;
      FLevel:INTEGER;
      FAdr:INTEGER;
      LastParameter:INTEGER;
      ReturnAddress:INTEGER;
      Inside:BOOLEAN;
     END;

     TType=RECORD
      Size:INTEGER;
      Kind:INTEGER;
      StartIndex:INTEGER;
      EndIndex:INTEGER;
      SubType:INTEGER;
      Fields:INTEGER;
     END;

VAR CurChar:CHAR;
    LinePos:INTEGER;
    LineNum:INTEGER;
    CurSym:INTEGER;
    CurID:TAlfa;
    CurNum:INTEGER;
    CurStr:ARRAY[1..255] OF CHAR;
    CurStrLen:INTEGER;
    FuncDecl:INTEGER;
    Keywords:ARRAY[SymBEGIN..SymPROC] OF TAlfa;
    LastOpcode:INTEGER;
    CurLevel:INTEGER;
    IsLabeled:BOOLEAN;
    SymNameList:ARRAY[-1..MaxList] OF INTEGER;
    IdentPos:INTEGER;
    TypePos:INTEGER;
    IdentTab:ARRAY[0..MaxIdent] OF TIdent;
    TypeTab:ARRAY[1..MaxType] OF TType;
    Code:ARRAY[0..MaxCode] OF INTEGER;
    CodePos:INTEGER;
    StackPos:INTEGER;

FUNCTION StrCmp(VAR S1,S2:TAlfa):BOOLEAN;
VAR F:BOOLEAN;
    I:INTEGER;
BEGIN
 F:=TRUE;
 I:=1;
 WHILE F AND (I<=MaxAlfa) DO BEGIN
  F:=(S1[I]=S2[I]);
  I:=I+1;
 END;
 StrCmp:=F;
END;

PROCEDURE StrCpy(VAR Dest:TAlfa;Src:TAlfa);
BEGIN
 Dest:=Src;
END;

PROCEDURE Error(N:INTEGER);
BEGIN
 WRITELN('Error ',N:1,' detected at line ',LineNum:1,' at column ',LinePos:1);
 HALT;
END;

PROCEDURE ReadChar;
BEGIN
 IF NOT EOF THEN BEGIN
  READ(CurChar);
  LinePos:=LinePos+1;
  IF CurChar=CHR(10) THEN BEGIN
   LineNum:=LineNum+1;
   LinePos:=0;
  END;
 END ELSE BEGIN
  CurChar:=CHR(0);
 END;
END;

FUNCTION ReadNumber:INTEGER;
VAR Num:INTEGER;
BEGIN
 Num:=0;
 IF ('0'<=CurChar) AND (CurChar<='9') THEN BEGIN
  WHILE ('0'<=CurChar) AND (CurChar<='9') DO BEGIN
   Num:=(Num*10)+(ORD(CurChar)-ORD('0'));
   ReadChar;
  END;
 END ELSE IF CurChar='$' THEN BEGIN
  ReadChar;
  WHILE (('0'<=CurChar) AND (CurChar<='9')) OR
        (('A'<=CurChar) AND (CurChar<='F')) DO BEGIN
   IF ('0'<=CurChar) AND (CurChar<='9') THEN BEGIN
    Num:=(Num*16)+(ORD(CurChar)-ORD('0'));
   END ELSE IF ('A'<=CurChar) AND (CurChar<='F') THEN BEGIN
    Num:=(Num*16)+(ORD(CurChar)-ORD('A')+10);
   END;
   ReadChar;
  END;
 END;
 ReadNumber:=Num;
END;

PROCEDURE GetSymbol;
VAR K,S:INTEGER;
    StrEnd,InStr:BOOLEAN;
    LastChar:CHAR;
BEGIN
 WHILE (CurChar>CHR(0)) AND (CurChar<=' ') DO ReadChar;
 IF (('a'<=CurChar) AND (CurChar<='z')) OR (('A'<=CurChar) AND (CurChar<='Z')) THEN BEGIN
  K:=0;
  WHILE (('a'<=CurChar) AND (CurChar<='z')) OR (('A'<=CurChar) AND (CurChar<='Z')) OR (('0'<=CurChar) AND (CurChar<='9')) DO BEGIN
   IF K<>MaxAlfa THEN BEGIN
    K:=K+1;
    IF ('a'<=CurChar) AND (CurChar<='z') THEN CurChar:=CHR(ORD(CurChar)-32);
    CurID[K]:=CurChar;
   END;
   ReadChar;
  END;
  WHILE K<>MaxAlfa DO BEGIN
   K:=K+1;
   CurID[K]:=' ';
  END;
  CurSym:=TokIdent;
  S:=SymBEGIN;
  WHILE S<=SymPROC DO BEGIN
   IF StrCmp(Keywords[S],CurID) THEN CurSym:=S;
   S:=S+1;
  END;
 END ELSE IF (('0'<=CurChar) AND (CurChar<='9')) OR (CurChar='$') THEN BEGIN
  CurSym:=TokNumber;
  CurNum:=ReadNumber;
 END ELSE IF CurChar=':' THEN BEGIN
  ReadChar;
  IF CurChar='=' THEN BEGIN
   ReadChar;
   CurSym:=TokAssign;
  END ELSE BEGIN
   CurSym:=TokColon;
  END;
 END ELSE IF CurChar='>' THEN BEGIN
  ReadChar;
  IF CurChar='=' THEN BEGIN
   ReadChar;
   CurSym:=TokGEq;
  END ELSE BEGIN
   CurSym:=TokGtr;
  END;
 END ELSE IF CurChar='<' THEN BEGIN
  ReadChar;
  IF CurChar='=' THEN BEGIN
   ReadChar;
   CurSym:=TokLEq;
  END ELSE IF CurChar='>' THEN BEGIN
   ReadChar;
   CurSym:=TokNEq;
  END ELSE BEGIN
   CurSym:=TokLss;
  END;
 END ELSE IF CurChar='.' THEN BEGIN
  ReadChar;
  IF CurChar='.' THEN BEGIN
   ReadChar;
   CurSym:=TokColon;
  END ELSE BEGIN
   CurSym:=TokPeriod
  END;
 END ELSE IF (CurChar='''') OR (CurChar='#') THEN BEGIN
  CurStrLen:=0;
  StrEnd:=FALSE;
  InStr:=FALSE;
  CurSym:=TokStrC;
  WHILE NOT StrEnd DO BEGIN
   IF InStr THEN BEGIN
    IF CurChar='''' THEN BEGIN
     ReadChar;
     IF CurChar='''' THEN BEGIN
      CurStrLen:=CurStrLen+1;
      CurStr[CurStrLen]:=CurChar;
      ReadChar;
     END ELSE BEGIN
      InStr:=FALSE;
     END;
    END ELSE IF (CurChar=CHR(13)) OR (CurChar=CHR(10)) THEN BEGIN
     Error(100);
     StrEnd:=TRUE;
    END ELSE BEGIN
     CurStrLen:=CurStrLen+1;
     CurStr[CurStrLen]:=CurChar;
     ReadChar;
    END;
   END ELSE BEGIN
    IF CurChar='''' THEN BEGIN
     InStr:=TRUE;
     ReadChar;
    END ELSE IF CurChar='#' THEN BEGIN
     ReadChar;
     CurStrLen:=CurStrLen+1;
     CurStr[CurStrLen]:=CHR(ReadNumber);
    END ELSE BEGIN
     StrEnd:=TRUE;
    END;
   END;
  END;
  IF CurStrLen=0 THEN Error(101);
 END ELSE IF CurChar='+' THEN BEGIN
  ReadChar;
  CurSym:=TokPlus;
 END ELSE IF CurChar='-' THEN BEGIN
  ReadChar;
  CurSym:=TokMinus;
 END ELSE IF CurChar='*' THEN BEGIN
  ReadChar;
  CurSym:=TokMul;
 END ELSE IF CurChar='(' THEN BEGIN
  ReadChar;
  IF CurChar='*' THEN BEGIN
   ReadChar;
   LastChar:='-';
   WHILE NOT ((CurChar=')') AND (LastChar='*')) DO BEGIN
    LastChar:=CurChar;
    ReadChar;
   END;
   ReadChar;
   GetSymbol;
  END ELSE BEGIN
   CurSym:=TokLParent;
  END;
 END ELSE IF CurChar=')' THEN BEGIN
  ReadChar;
  CurSym:=TokRParent;
 END ELSE IF CurChar='[' THEN BEGIN
  ReadChar;
  CurSym:=TokLBracket;
 END ELSE IF CurChar=']' THEN BEGIN
  ReadChar;
  CurSym:=TokRBracket;
 END ELSE IF CurChar='=' THEN BEGIN
  ReadChar;
  CurSym:=TokEql;
 END ELSE IF CurChar=',' THEN BEGIN
  ReadChar;
  CurSym:=TokComma;
 END ELSE IF CurChar=';' THEN BEGIN
  ReadChar;
  CurSym:=TokSemi;
 END ELSE IF CurChar='{' THEN BEGIN
  WHILE CurChar<>'}' DO ReadChar;
  ReadChar;
  GetSymbol;
 END ELSE BEGIN
  Error(102);
 END;
END;

PROCEDURE Check(S:INTEGER);
BEGIN
 IF CurSym<>S THEN Error(S);
END;

PROCEDURE Expect(S:INTEGER);
BEGIN
 Check(S);
 GetSymbol;
END;

PROCEDURE EnterSymbol(CurID:TAlfa;K,T:INTEGER);
VAR J:INTEGER;
BEGIN
 IF IdentPos=MaxIdent THEN Error(103);
 IdentPos:=IdentPos+1;
 IdentTab[0].Name:=CurID;
 J:=SymNameList[CurLevel];
 WHILE NOT StrCmp(IdentTab[J].Name,CurID) DO J:=IdentTab[J].Link;
 IF J<>0 THEN BEGIN
  IF IdentTab[J].Kind<>IdFUNC THEN Error(104);
  IF (Code[IdentTab[J].FAdr]<>OPJmp) OR (Code[IdentTab[J].FAdr+1]>0) THEN Error(105);
  IdentTab[J].Name[1]:='$';
  Code[IdentTab[J].FAdr+1]:=CodePos;
  FuncDecl:=J;
 END;
 IdentTab[IdentPos].Name:=CurID;
 IdentTab[IdentPos].Link:=SymNameList[CurLevel];
 IdentTab[IdentPos].TypeDef:=T;
 IdentTab[IdentPos].Kind:=K;
 SymNameList[CurLevel]:=IdentPos;
END;

FUNCTION Position:INTEGER;
VAR I,J:INTEGER;
BEGIN
 IdentTab[0].Name:=CurID;
 I:=CurLevel;
 REPEAT
  J:=SymNameList[I];
  WHILE NOT StrCmp(IdentTab[J].Name,CurID) DO J:=IdentTab[J].Link;
  I:=I-1;
 UNTIL (I<-1) OR (J<>0);
 IF J=0 THEN Error(106);
 Position:=J;
END;

PROCEDURE GenOp(Opcode,A:INTEGER);
BEGIN
 CASE Opcode OF
  OPDupl,OPEOF,OPEOL,OPLdC,OPLdA,OPLdLA,OPLdL,OPLdG:StackPos:=StackPos-4;
  OPNeg,OPDiv2,OPRem2,OPSwap,OPLoad,OPHalt,OPWrL,OPRdL,OpAddC,OPMulC,
  OPJmp,OPCall,OPExit:BEGIN
  END;
  OPAdd,OPMul,OPDivD,OPRemD,OPEqlI,OPNEqI,OPLssI,OPLeqI,OPGtrI,OPGEqI,OPAndB,
  OPOrB,OPWrC,OPRdI,OPRdC,OPStL,OPStG,OPJZ:StackPos:=StackPos+4;
  OPStore,OPWrI,OPMove:StackPos:=StackPos+8;
  OPCopy:StackPos:=StackPos-(A-4);
  OPAdjS:StackPos:=StackPos+A;
 END;
 IF NOT ((((Opcode=OPAddC) OR (Opcode=OPAdjS)) AND (A=0)) OR ((Opcode=OPMulC) AND (A=1))) THEN BEGIN
  IF IsLabeled THEN BEGIN
   Code[CodePos]:=Opcode;
   CodePos:=CodePos+1;
   IF Opcode>=OPLdC THEN BEGIN
    Code[CodePos]:=A;
    CodePos:=CodePos+1;
   END;
   IsLabeled:=FALSE;
  END ELSE IF (LastOpcode=OPLdC) AND (Opcode=OPAdd) THEN BEGIN
   Code[CodePos-2]:=OPAddC;
  END ELSE IF (LastOpcode=OPLdC) AND (Opcode=OPMul) THEN BEGIN
   Code[CodePos-2]:=OPMulC;
  END ELSE IF (LastOpcode=OPLdC) AND (Opcode=OPNeg) THEN BEGIN
   Code[CodePos-1]:=-Code[CodePos-1];
   Opcode:=LastOpcode;
  END ELSE IF (LastOpcode=OPLdC) AND (Code[CodePos-1]=2) AND (Opcode=OPDivD) THEN BEGIN
   Code[CodePos-2]:=OPDiv2;
   CodePos:=CodePos-1;
  END ELSE IF (LastOpcode=OPLdC) AND (Code[CodePos-1]=2) AND (Opcode=OPRemD) THEN BEGIN
   Code[CodePos-2]:=OPRem2;
   CodePos:=CodePos-1;
  END ELSE IF (LastOpcode=OPLdA) AND (Opcode=OPStore) THEN BEGIN
   Code[CodePos-2]:=OPStG;
  END ELSE IF (LastOpcode=OPLdA) AND (Opcode=OPLoad) THEN BEGIN
   Code[CodePos-2]:=OPLdG;
  END ELSE IF (LastOpcode=OPLdLA) AND (Opcode=OPStore) THEN BEGIN
   Code[CodePos-2]:=OPStL;
  END ELSE IF (LastOpcode=OPLdLA) AND (Opcode=OPLoad) THEN BEGIN
   Code[CodePos-2]:=OPLdL;
  END ELSE BEGIN
   Code[CodePos]:=Opcode;
   CodePos:=CodePos+1;
   IF Opcode>=OPLdC THEN BEGIN
    Code[CodePos]:=A;
    CodePos:=CodePos+1;
   END;
  END;
  LastOpcode:=Opcode;
 END;
END;

PROCEDURE GenOp2(Opcode:INTEGER);
BEGIN
 GenOp(Opcode,0);
END;

FUNCTION CodeLabel:INTEGER;
BEGIN
 CodeLabel:=CodePos;
 IsLabeled:=TRUE;
END;

PROCEDURE GenAddress(Level,Address:INTEGER);
BEGIN
 IF Level=0 THEN BEGIN
  GenOp(OPLdA,Address);
 END ELSE IF Level=CurLevel THEN BEGIN
  GenOp(OPLdLA,Address-StackPos);
 END ELSE BEGIN
  GenOp(OPLdL,-StackPos);
  WHILE Level+1<>CurLevel DO BEGIN
   GenOp2(OPLoad);
   Level:=Level+1;
  END;
  GenOp(OPAddC,Address);
 END;
END;

PROCEDURE GenAddressVar(IdentNr:INTEGER);
BEGIN
 GenAddress(IdentTab[IdentNr].VLevel,IdentTab[IdentNr].VAdr);
 IF IdentTab[IdentNr].RefPar THEN GenOp2(OPLoad);
END;

PROCEDURE MustBe(X,Y:INTEGER);
BEGIN
 IF X<>Y THEN BEGIN
  IF (TypeTab[X].Kind=KindARRAY) AND (TypeTab[Y].Kind=KindARRAY) AND (TypeTab[X].StartIndex=TypeTab[Y].StartIndex) AND (TypeTab[X].EndIndex=TypeTab[Y].EndIndex) THEN BEGIN
   MustBe(TypeTab[X].SubType,TypeTab[Y].SubType);
  END ELSE BEGIN
   Error(107);
  END;
 END;
END;

PROCEDURE Expression(VAR X:INTEGER); FORWARD;

PROCEDURE Selector(VAR T,IdentNr:INTEGER);
VAR J,X:INTEGER;
BEGIN
 T:=IdentTab[IdentNr].TypeDef;
 GetSymbol;
 IF (CurSym=TokPeriod) OR (CurSym=TokLBracket) THEN BEGIN
  GenAddressVar(IdentNr);
  IdentNr:=0;
  WHILE (CurSym=TokPeriod) OR (CurSym=TokLBracket) DO BEGIN
   CASE CurSym OF
    TokPeriod:BEGIN
     IF TypeTab[T].Kind<>KindRECORD THEN Error(108);
     GetSymbol;
     Check(TokIdent);
     J:=TypeTab[T].Fields;
     IdentTab[0].Name:=CurID;
     WHILE NOT StrCmp(IdentTab[J].Name,CurID) DO J:=IdentTab[J].Link;
     IF J=0 THEN Error(109);
     GenOp(OPAddC,IdentTab[J].Offset);
     T:=IdentTab[J].TypeDef;
     GetSymbol;
    END;
    TokLBracket:BEGIN
     REPEAT
      IF TypeTab[T].Kind<>KindARRAY THEN Error(110);
      GetSymbol;
      Expression(X);
      MustBe(TypeINT,X);
      GenOp(OPAddC,-TypeTab[T].StartIndex);
      T:=TypeTab[T].SubType;
      GenOp(OPMulC,TypeTab[T].Size);
      GenOp2(OPAdd);
     UNTIL CurSym<>TokComma;
     Expect(TokRBracket)
    END;
   END;
  END;
 END;
END;

PROCEDURE VarPar(VAR T:INTEGER);
VAR J:INTEGER;
BEGIN
 Check(TokIdent);
 J:=Position;
 Selector(T,J);
 IF J<>0 THEN GenAddressVar(J);
END;

PROCEDURE InternalFunction(N:INTEGER);
VAR X:INTEGER;
BEGIN
 CASE N OF
  FunCHR:BEGIN
   Expect(TokLParent);
   Expression(X);
   MustBe(TypeINT,X);
   Expect(TokRParent)
  END;
  FunORD:BEGIN
   Expect(TokLParent);
   Expression(X);
   IF X<>TypeBOOL THEN MustBe(TypeCHAR,X);
   Expect(TokRParent);
  END;
  FunWRITE,FunWRITELN:BEGIN
   IF N=FunWRITE THEN Check(TokLParent);
   IF CurSym=TokLParent THEN BEGIN
    REPEAT
     GetSymbol;
     IF CurSym=TokStrC THEN BEGIN
      X:=1;
      WHILE X<=CurStrLen DO BEGIN
       GenOp(OPLdC,ORD(CurStr[X]));
       GenOp2(OPWrC);
       X:=X+1;
      END;
      GetSymbol;
     END ELSE BEGIN
      Expression(X);
      IF CurSym=TokColon THEN BEGIN
       MustBe(TypeINT,X);
       GetSymbol;
       Expression(X);
       MustBe(TypeINT,X);
       GenOp2(OPWrI);
      END ELSE IF X=TypeINT THEN BEGIN
       GenOp(OPLdC,1);
       GenOp2(OPWrI);
      END ELSE IF X=TypeCHAR THEN BEGIN
       GenOp2(OPWrC);
      END ELSE BEGIN
       Error(111);
      END;
     END;
    UNTIL CurSym<>TokComma;
    Expect(TokRParent)
   END;
   IF N=FunWRITELN THEN GenOp2(OPWrL);
  END;
  FunREAD,FunREADLN:BEGIN
   IF N=FunREAD THEN Check(TokLParent);
   IF CurSym=TokLParent THEN BEGIN
    REPEAT
     GetSymbol;
     VarPar(X);
     IF X=TypeINT THEN BEGIN
      GenOp2(OPRdI);
     END ELSE IF X=TypeCHAR THEN BEGIN
      GenOp2(OPRdC);
     END ELSE BEGIN
      Error(112);
     END;
    UNTIL CurSym<>TokComma;
    Expect(TokRParent);
   END;
   IF N=FunREADLN THEN GenOp2(OPRdL);
  END;
  FunEOF:GenOp2(OPEOF);
  FunEOFLN:GenOp2(OPEOL);
 END;
END;

PROCEDURE FunctionCall(I:INTEGER);
VAR OldStackPos,P,X:INTEGER;
BEGIN
 GetSymbol;
 IF IdentTab[I].FLevel<0 THEN BEGIN
  InternalFunction(IdentTab[I].FAdr);
 END ELSE BEGIN
  IF IdentTab[I].TypeDef<>0 THEN GenOp(OPLdC,0);
  P:=I;
  OldStackPos:=StackPos;
  IF CurSym=TokLParent THEN BEGIN
   REPEAT
    GetSymbol;
    IF P=IdentTab[I].LastParameter THEN Error(113);
    P:=P+1;
    IF IdentTab[P].RefPar THEN BEGIN
     VarPar(X);
    END ELSE BEGIN
     Expression(X);
     IF TypeTab[X].Kind<>KindSIMPLE THEN GenOp(OPCopy,TypeTab[X].Size);
    END;
    IF X=TypeSTR THEN BEGIN
     IF IdentTab[P].RefPar THEN Error(114);
     IF TypeTab[IdentTab[P].TypeDef].Kind<>KindARRAY THEN Error(115);
     IF TypeTab[IdentTab[P].TypeDef].SubType<>TypeCHAR THEN Error(116);
     IF ((TypeTab[IdentTab[P].TypeDef].EndIndex-TypeTab[IdentTab[P].TypeDef].StartIndex)+1)<>CurStrLen THEN Error(117);
    END ELSE BEGIN
     MustBe(IdentTab[P].TypeDef,X);
    END;
   UNTIL CurSym<>TokComma;
   Expect(TokRParent);
  END;
  IF P<>IdentTab[I].LastParameter THEN Error(118);
  IF IdentTab[I].FLevel<>0 THEN GenAddress(IdentTab[I].FLevel,0);
  GenOp(OPCall,IdentTab[I].FAdr);
  StackPos:=OldStackPos;
 END;
END;

PROCEDURE Factor(VAR T:INTEGER);
VAR I:INTEGER;
BEGIN
 IF CurSym=TokIdent THEN BEGIN
  I:=Position;
  T:=IdentTab[I].TypeDef;
  CASE IdentTab[I].Kind OF
   IdCONST:BEGIN
    GetSymbol;
    GenOp(OPLdC,IdentTab[I].Value);
   END;
   IdVAR:BEGIN
    Selector(T,I);
    IF I<>0 THEN GenAddressVar(I);
    IF TypeTab[T].Kind=KindSIMPLE THEN GenOp2(OPLoad);
   END;
   IdFUNC:BEGIN
    IF T=0 THEN BEGIN
     Error(119);
    END ELSE BEGIN
     FunctionCall(I);
    END;
   END;
   IdTYPE:Error(120);
  END;
 END ELSE IF CurSym=TokNumber THEN BEGIN
  GenOp(OPLdC,CurNum);
  T:=TypeINT;
  GetSymbol;
 END  ELSE IF CurSym=TokStrC THEN BEGIN
  I:=CurStrLen;
  WHILE I>=1 DO BEGIN
   GenOp(OPLdC,ORD(CurStr[I]));
   I:=I-1;
  END;
  T:=TypeCHAR;
  IF CurStrLen<>1 THEN T:=TypeSTR;
  GetSymbol;
 END ELSE IF CurSym=TokLParent THEN BEGIN
  GetSymbol;
  Expression(T);
  Expect(TokRParent);
 END ELSE IF CurSym=SymNOT THEN BEGIN
  GetSymbol;
  Factor(T);
  MustBe(TypeBOOL,T);
  GenOp2(OPNeg);
  GenOp(OPAddC,1);
 END ELSE BEGIN
  Error(121);
 END;
END;

PROCEDURE Term(VAR X:INTEGER);
VAR Y:INTEGER;
BEGIN
 Factor(X);
 WHILE (CurSym=SymAND) OR (CurSym=TokMul) OR (CurSym=SymDIV) OR (CurSym=SymMOD) DO BEGIN
  IF CurSym=SymAND THEN BEGIN
   MustBe(TypeBOOL,X);
  END ELSE BEGIN
   MustBe(TypeINT,X);
  END;
  CASE CurSym OF
   TokMul:BEGIN
    GetSymbol;
    Factor(Y);
    GenOp2(OPMul);
   END;
   SymDIV:BEGIN
    GetSymbol;
    Factor(Y);
    GenOp2(OPDivD);
   END;
   SymMOD:BEGIN
    GetSymbol;
    Factor(Y);
    GenOp2(OPRemD);
   END;
   SymAND:BEGIN
    GetSymbol;
    Factor(Y);
    GenOp2(OPAndB);
   END;
  END;
  MustBe(X,Y);
 END;
END;

PROCEDURE SimpleExpression(VAR X:INTEGER);
VAR Y:INTEGER;
BEGIN
 IF CurSym=TokPlus THEN BEGIN
  GetSymbol;
  Term(X);
  MustBe(TypeINT,X);
 END ELSE IF CurSym=TokMinus THEN BEGIN
  GetSymbol;
  Term(X);
  MustBe(TypeINT,X);
  GenOp2(OPNeg);
 END ELSE BEGIN
  Term(X);
 END;
 WHILE (CurSym=SymOR) OR (CurSym=TokPlus) OR (CurSym=TokMinus) DO BEGIN
  IF CurSym=SymOR THEN BEGIN
   MustBe(TypeBOOL,X);
  END ELSE BEGIN
   MustBe(TypeINT,X);
  END;
  CASE CurSym OF
   TokPlus:BEGIN
    GetSymbol;
    Term(Y);
    GenOp2(OPAdd);
   END;
   TokMinus:BEGIN
    GetSymbol;
    Term(Y);
    GenOp2(OPNeg);
    GenOp2(OPAdd);
   END;
   SymOR:BEGIN
    GetSymbol;
    Term(Y);
    GenOp2(OPOrB);
   END;
  END;
  MustBe(X,Y);
 END;
END;

PROCEDURE Expression(VAR X:INTEGER);
VAR O,Y:INTEGER;
BEGIN
 SimpleExpression(X);
 IF (CurSym=TokEql) OR (CurSym=TokNEq) OR (CurSym=TokLss) OR (CurSym=TokLEq) OR (CurSym=TokGtr) OR (CurSym=TokGEq) THEN BEGIN
  IF (X=TypeSTR) OR (TypeTab[X].Kind<>KindSIMPLE) THEN Error(121);
  O:=CurSym;
  GetSymbol;
  SimpleExpression(Y);
  MustBe(X,Y);
  CASE O OF
   TokEql:GenOp2(OPEqlI);
   TokNEq:GenOp2(OPNEqI);
   TokLss:GenOp2(OPLssI);
   TokLEq:GenOp2(OPLeqI);
   TokGtr:GenOp2(OPGtrI);
   TokGEq:GenOp2(OPGEqI);
  END;
  X:=TypeBOOL;
 END;
END;

PROCEDURE Statement;
VAR L:ARRAY[1..64] OF INTEGER;
    M,N,I,J,T,X,R,OldStackPos:INTEGER;
BEGIN
 IF CurSym=TokIdent THEN BEGIN
  I:=Position;
  CASE IdentTab[I].Kind OF
   IdVAR:BEGIN
    Selector(T,I);
    Expect(TokAssign);
    Expression(X);
    MustBe(T,X);
    IF I=0 THEN BEGIN
     GenOp2(OPSwap);
    END ELSE BEGIN
     GenAddressVar(I);
    END;
    IF TypeTab[T].Kind=KindSIMPLE THEN BEGIN
     GenOp2(OPStore);
    END ELSE BEGIN
     GenOp(OPMove,TypeTab[T].Size);
    END;
   END;
   IdFUNC:BEGIN
    IF IdentTab[I].TypeDef=0 THEN BEGIN
     FunctionCall(I);
    END ELSE BEGIN
     IF NOT IdentTab[I].Inside THEN Error(122);
     GetSymbol;
     Expect(TokAssign);
     Expression(X);
     MustBe(IdentTab[I].TypeDef,X);
     GenAddress(IdentTab[I].FLevel+1,IdentTab[I].ReturnAddress);
     GenOp2(OPStore);
    END;
   END;
   IdCONST,IdFIELD,IdTYPE:Error(123);
  END;
 END ELSE IF CurSym=SymIF THEN BEGIN
  GetSymbol;
  Expression(T);
  MustBe(TypeBOOL,T);
  Expect(SymTHEN);
  I:=CodeLabel;
  GenOp(OPJZ,0);
  Statement;
  IF CurSym=SymELSE THEN BEGIN
   GetSymbol;
   J:=CodeLabel;
   GenOp(OPJmp,0);
   Code[I+1]:=CodeLabel;
   I:=J;
   Statement;
  END;
  Code[I+1]:=CodeLabel;
 END ELSE IF CurSym=SymCASE THEN BEGIN
  GetSymbol;
  Expression(T);
  MustBe(TypeINT,T);
  Expect(SymOF);
  J:=0;
  M:=0;
  REPEAT
   IF J<>0 THEN Code[J+1]:=CodeLabel;
   N:=M;
   REPEAT
    IF N<>M THEN GetSymbol;
    GenOp2(OPDupl);
    IF CurSym=TokIdent THEN BEGIN
     I:=Position;
     IF IdentTab[I].Kind<>IdCONST THEN Error(124);
     GenOp(OPLdC,IdentTab[I].Value);
    END ELSE IF CurSym=TokNumber THEN BEGIN
     GenOp(OPLdC,CurNum);
    END ELSE IF (CurSym=TokStrC) AND (CurStrLen=1) THEN BEGIN
     GenOp(OPLdC,ORD(CurStr[1]));
    END ELSE BEGIN
     Error(125);
    END;
    GenOp2(OPNEqI);
    N:=N+1;
    L[N]:=CodeLabel;
    GenOp(OPJZ,0);
    GetSymbol;
   UNTIL CurSym<>TokComma;
   IF CurSym<>TokColon THEN Error(126);
   J:=CodeLabel;
   GenOp(OPJmp,0);
   REPEAT
    Code[L[N]+1]:=CodeLabel;
    N:=N-1;
   UNTIL N=M;
   GetSymbol;
   Statement;
   M:=M+1;
   L[M]:=CodeLabel;
   GenOp(OPJmp,0);
   IF CurSym=TokSemi THEN GetSymbol;
  UNTIL CurSym=SymEND;
  Code[J+1]:=CodeLabel;
  REPEAT
   Code[L[M]+1]:=CodeLabel;
   M:=M-1;
  UNTIL M=0;
  GenOp(OPAdjS,4);
  GetSymbol;
 END ELSE IF CurSym=SymFOR THEN BEGIN
  GetSymbol;
  IF CurSym=TokIdent THEN BEGIN
   OldStackPos:=StackPos;

   I:=Position;
   IF IdentTab[I].Kind<>IdVAR THEN Error(127);
   Selector(T,I);
   Expect(TokAssign);
   Expression(X);
   MustBe(T,X);
   IF I=0 THEN BEGIN
    GenOp2(OPSwap);
   END ELSE BEGIN
    GenAddressVar(I);
   END;
   IF TypeTab[T].Kind<>KindSIMPLE THEN Error(128);
   GenOp2(OPStore);

   R:=1;
   IF CurSym=SymTO THEN BEGIN
    Expect(SymTO);
   END ELSE IF CurSym=SymDOWNTO THEN BEGIN
    Expect(SymDOWNTO);
    R:=-1;
   END ELSE BEGIN
    Error(129);
   END;

   J:=CodeLabel;
   IF I=0 THEN BEGIN
    GenOp2(OPSwap);
   END ELSE BEGIN
    GenAddressVar(I);
   END;
   GenOp2(OPLoad);
   Expression(X);
   MustBe(T,X);
   IF R>0 THEN BEGIN
    GenOp2(OPLeqI);
   END ELSE BEGIN
    GenOp2(OPGeqI);
   END;
   N:=CodeLabel;
   GenOp(OPJZ,0);

   Expect(SymDO);

   Statement;

   IF I=0 THEN BEGIN
    GenOp2(OPSwap);
   END ELSE BEGIN
    GenAddressVar(I);
   END;
   GenOp2(OPLoad);

   GenOp(OPAddC,R);

   IF I=0 THEN BEGIN
    GenOp2(OPSwap);
   END ELSE BEGIN
    GenAddressVar(I);
   END;
   GenOp2(OPStore);

   GenOp(OPJmp,J);
   Code[N+1]:=CodeLabel;

   GenOp(OPAdjS,OldStackPos-StackPos);

  END ELSE BEGIN
   Expect(TokIdent);
  END;
 END ELSE IF CurSym=SymWHILE THEN BEGIN
  GetSymbol;
  I:=CodeLabel;
  Expression(T);
  MustBe(TypeBOOL,T);
  Expect(SymDO);
  J:=CodeLabel;
  GenOp(OPJZ,0);
  Statement;
  GenOp(OPJmp,I);
  Code[J+1]:=CodeLabel;
 END ELSE IF CurSym=SymREPEAT THEN BEGIN
  I:=CodeLabel;
  REPEAT
   GetSymbol;
   Statement;
  UNTIL CurSym<>TokSemi;
  Expect(SymUNTIL);
  Expression(T);
  MustBe(TypeBOOL,T);
  GenOp(OPJZ,I);
 END ELSE IF CurSym=SymBEGIN THEN BEGIN
  REPEAT
   GetSymbol;
   Statement;
  UNTIL CurSym<>TokSemi;
  Expect(SymEND);
 END ELSE IF CurSym=SymHALT THEN BEGIN
  GenOp2(OPHalt);
  GetSymbol;
 END;
END;

PROCEDURE Block(L:INTEGER); FORWARD;

PROCEDURE Constant(VAR C,T:INTEGER);
VAR I,S:INTEGER;
BEGIN
 IF (CurSym=TokStrC) AND (CurStrLen=1) THEN BEGIN
  C:=ORD(CurStr[1]);
  T:=TypeCHAR;
 END ELSE BEGIN
  IF CurSym=TokPlus THEN BEGIN
   GetSymbol;
   S:=1;
  END  ELSE IF CurSym=TokMinus THEN BEGIN
   GetSymbol;
   S:=-1;
  END ELSE BEGIN
   S:=0;
  END;
  IF CurSym=TokIdent THEN BEGIN
   I:=Position;
   IF IdentTab[I].Kind<>IdCONST THEN Error(130);
   C:=IdentTab[I].Value;
   T:=IdentTab[I].TypeDef;
  END ELSE IF CurSym=TokNumber THEN BEGIN
   C:=CurNum;
   T:=TypeINT;
  END ELSE BEGIN
   Error(131);
  END;
  IF S<>0 THEN BEGIN
   MustBe(T,TypeINT);
   C:=C*S;
  END;
 END;
 GetSymbol;
END;

PROCEDURE ConstDeclaration;
VAR A:TAlfa;
    T,C:INTEGER;
BEGIN
 A:=CurID;
 GetSymbol;
 Expect(TokEql);
 Constant(C,T);
 Expect(TokSemi);
 EnterSymbol(A,IdCONST,T);
 IdentTab[IdentPos].Value:=C;
END;

PROCEDURE TypeDef(VAR T:INTEGER); FORWARD;

PROCEDURE ArrayType(VAR T:INTEGER);
VAR X:INTEGER;
BEGIN
 TypeTab[T].Kind:=KindARRAY;
 GetSymbol;
 Constant(TypeTab[T].StartIndex,X);
 MustBe(TypeINT,X);
 Expect(TokColon);
 Constant(TypeTab[T].EndIndex,X);
 MustBe(TypeINT,X);
 IF TypeTab[T].StartIndex>TypeTab[T].EndIndex THEN Error(132);
 IF CurSym=TokComma THEN BEGIN
  ArrayType(TypeTab[T].SubType);
 END ELSE BEGIN
  Expect(TokRBracket);
  Expect(SymOF);
  TypeDef(TypeTab[T].SubType);
 END;
 TypeTab[T].Size:=(TypeTab[T].EndIndex-TypeTab[T].StartIndex+1)*TypeTab[TypeTab[T].SubType].Size;
END;

PROCEDURE TypeDef(VAR T:INTEGER);
VAR I,J,SZ,FT:INTEGER;
BEGIN
 IF CurSym=SymPACKED THEN GetSymbol;
 IF CurSym=TokIdent THEN BEGIN
  I:=Position;
  IF IdentTab[I].Kind<>IdTYPE THEN Error(133);
  T:=IdentTab[I].TypeDef;
  GetSymbol;
 END ELSE BEGIN
  IF TypePos=MaxType THEN Error(134);
  TypePos:=TypePos+1;
  T:=TypePos;
  IF CurSym=SymARRAY THEN BEGIN
   GetSymbol;
   Check(TokLBracket);
   ArrayType(T);
  END ELSE BEGIN
   Expect(SymRECORD);
   IF CurLevel=MaxList THEN Error(135);
   CurLevel:=CurLevel+1;
   SymNameList[CurLevel]:=0;
   Check(TokIdent);
   SZ:=0;
   REPEAT
    EnterSymbol(CurID,IdFIELD,0);
    I:=IdentPos;
    GetSymbol;
    WHILE CurSym=TokComma DO BEGIN
     GetSymbol;
     Check(TokIdent);
     EnterSymbol(CurID,IdFIELD,0);
     GetSymbol;
    END;
    J:=IdentPos;
    Expect(TokColon);
    TypeDef(FT);
    REPEAT
     IdentTab[I].TypeDef:=FT;
     IdentTab[I].Offset:=SZ;
     SZ:=SZ+TypeTab[FT].Size;
     I:=I+1;
    UNTIL I>J;
    IF CurSym=TokSemi THEN BEGIN
     GetSymbol;
    END ELSE BEGIN
     Check(SymEND);
    END;
   UNTIL CurSym<>TokIdent;
   TypeTab[T].Size:=SZ;
   TypeTab[T].Kind:=KindRECORD;
   TypeTab[T].Fields:=SymNameList[CurLevel];
   CurLevel:=CurLevel-1;
   Expect(SymEND);
  END;
 END;
END;

PROCEDURE TypeDeclaration;
VAR A:TAlfa;
    T:INTEGER;
BEGIN
 A:=CurID;
 GetSymbol;
 Expect(TokEql);
 TypeDef(T);
 Expect(TokSemi);
 EnterSymbol(A,IdTYPE,T);
END;

PROCEDURE VarDeclaration;
VAR P,Q,T:INTEGER;
BEGIN
 EnterSymbol(CurID,IdVAR,0);
 P:=IdentPos;
 GetSymbol;
 WHILE CurSym=TokComma DO BEGIN
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurID,IdVAR,0);
  GetSymbol;
 END;
 Q:=IdentPos;
 Expect(TokColon);
 TypeDef(T);
 Expect(TokSemi);
 REPEAT
  IdentTab[P].VLevel:=CurLevel;
  StackPos:=StackPos-TypeTab[T].Size;
  IdentTab[P].TypeDef:=T;
  IdentTab[P].VAdr:=StackPos;
  IdentTab[P].RefPar:=FALSE;
  P:=P+1;
 UNTIL P>Q;
END;

PROCEDURE NewParameter(VAR P,PS:INTEGER);
VAR R:BOOLEAN;
    T:INTEGER;
BEGIN
 IF CurSym=SymVAR THEN BEGIN
  R:=TRUE;
  GetSymbol;
 END ELSE BEGIN
  R:=FALSE;
 END;
 Check(TokIdent);
 P:=IdentPos;
 EnterSymbol(CurID,IdVAR,0);
 GetSymbol;
 WHILE CurSym=TokComma DO BEGIN
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurID,IdVAR,0);
  GetSymbol;
 END;
 Expect(TokColon);
 Check(TokIdent);
 TypeDef(T);
 WHILE P<IdentPos DO BEGIN
  P:=P+1;
  IdentTab[P].TypeDef:=T;
  IdentTab[P].RefPar:=R;
  IF R THEN BEGIN
   PS:=PS+4;
  END ELSE BEGIN
   PS:=PS+TypeTab[T].Size;
  END;
 END;
END;

PROCEDURE FunctionDeclaration(IstFunktion:BOOLEAN);
VAR F,P,PS,P1,P2,OldStackPos:INTEGER;
BEGIN
 GetSymbol;
 Check(TokIdent);
 FuncDecl:=-1;
 EnterSymbol(CurID,IdFUNC,0);
 GetSymbol;
 F:=IdentPos;
 IdentTab[F].FLevel:=CurLevel;
 IdentTab[F].FAdr:=CodeLabel;
 GenOp(OPJmp,0);
 IF CurLevel=MaxList THEN Error(136);
 CurLevel:=CurLevel+1;
 SymNameList[CurLevel]:=0;
 PS:=4;
 OldStackPos:=StackPos;
 IF CurSym=TokLParent THEN BEGIN
  REPEAT
   GetSymbol;
   NewParameter(P,PS);
  UNTIL CurSym<>TokSemi;
  Expect(TokRParent);
 END;
 IF CurLevel>1 THEN BEGIN
  StackPos:=-4;
 END ELSE BEGIN
  StackPos:=0;
 END;
 IdentTab[F].ReturnAddress:=PS;
 P:=F;
 WHILE P<IdentPos DO BEGIN
  P:=P+1;
  IF IdentTab[P].RefPar THEN BEGIN
   PS:=PS-4;
  END ELSE BEGIN
   PS:=PS-TypeTab[IdentTab[P].TypeDef].Size;
  END;
  IdentTab[P].VLevel:=CurLevel;
  IdentTab[P].VAdr:=PS;
 END;
 IF IstFunktion THEN BEGIN
  Expect(TokColon);
  Check(TokIdent);
  TypeDef(IdentTab[F].TypeDef);
  IF TypeTab[IdentTab[F].TypeDef].Kind<>KindSIMPLE THEN Error(137);
 END;
 Expect(TokSemi);
 IdentTab[F].LastParameter:=IdentPos;
 IF CurSym<>SymFORWARD THEN BEGIN
  IF FuncDecl>=0 THEN BEGIN
   P1:=FuncDecl+1;
   P2:=F+1;
   WHILE P1<=IdentTab[FuncDecl].LastParameter DO BEGIN
    IF P2>IdentTab[F].LastParameter THEN Error(138);
    IF NOT StrCmp(IdentTab[P1].Name,IdentTab[P2].Name) THEN Error(139);
    IF IdentTab[P1].TypeDef<>IdentTab[P2].TypeDef THEN Error(140);
    IF IdentTab[P1].RefPar<>IdentTab[P2].RefPar THEN Error(141);
    P1:=P1+1;
    P2:=P2+1;
   END;
   IF P2<=IdentTab[F].LastParameter THEN Error(142);
  END;
  IdentTab[F].Inside:=TRUE;
  Block(IdentTab[F].FAdr);
  IdentTab[F].Inside:=FALSE;
  GenOp(OPExit,IdentTab[F].ReturnAddress-StackPos);
 END ELSE BEGIN
  IF FuncDecl>=0 THEN Error(143);
  GetSymbol;
 END;
 CurLevel:=CurLevel-1;
 StackPos:=OldStackPos;
 Expect(TokSemi);
END;

PROCEDURE Block(L:INTEGER);
VAR I,D,OldStackPos,OldIdentPos:INTEGER;
BEGIN
 OldStackPos:=StackPos;
 OldIdentPos:=IdentPos;
 WHILE (CurSym=SymCONST) OR (CurSym=SymTYPE) OR (CurSym=SymVAR) OR (CurSym=SymFUNC) OR (CurSym=SymPROC) DO BEGIN
  IF CurSym=SymCONST THEN BEGIN
   GetSymbol;
   Check(TokIdent);
   WHILE CurSym=TokIdent DO ConstDeclaration;
  END ELSE IF CurSym=SymTYPE THEN BEGIN
   GetSymbol;
   Check(TokIdent);
   WHILE CurSym=TokIdent DO TypeDeclaration;
  END ELSE IF CurSym=SymVAR THEN BEGIN
   GetSymbol;
   Check(TokIdent);
   WHILE CurSym=TokIdent DO VarDeclaration;
  END ELSE IF (CurSym=SymFUNC) OR (CurSym=SymPROC) THEN BEGIN
   FunctionDeclaration(CurSym=SymFUNC);
  END;
 END;
 IF L+1=CodeLabel THEN BEGIN
  CodePos:=CodePos-1;
 END ELSE BEGiN
  Code[L+1]:=CodeLabel;
 END;
 IF CurLevel=0 THEN BEGIN
  GenOp(OPAdjS,StackPos);
 END ELSE BEGIN
  D:=StackPos-OldStackPos;
  StackPos:=OldStackPos;
  GenOp(OPAdjS,D);
 END;
 Statement;
 IF CurLevel<>0 THEN GenOp(OPAdjS,OldStackPos-StackPos);
 I:=OldIdentPos+1;
 WHILE I<=IdentPos DO BEGIN
  IF IdentTab[I].Kind=IdFUNC THEN IF (Code[IdentTab[I].FAdr]=OPJmp) AND (Code[IdentTab[I].FAdr+1]=0) THEN Error(144);
  I:=I+1;
 END;
 IdentPos:=OldIdentPos;
END;

VAR OCTab:ARRAY[1..262144] OF CHAR;
    OCP:INTEGER;

PROCEDURE OCC(C:CHAR);
BEGIN
 IF OCP<262143 THEN BEGIN
  OCP:=OCP+1;
  OCTab[OCP]:=C;
 END;
END;

PROCEDURE OC(B:INTEGER);
BEGIN
 OCC(CHR(B));
END;

PROCEDURE OCW(I:INTEGER);
BEGIN
 IF I>=0 THEN BEGIN
  OC(I MOD 256);
  OC((I DIV 256) MOD 256);
 END ELSE BEGIN
  I:=-(I+1);
  OC(255-(I MOD 256));
  OC(255-((I DIV 256) MOD 256));
 END;
END;

PROCEDURE OCI(I:INTEGER);
BEGIN
 IF I>=0 THEN BEGIN
  OC(I MOD 256);
  OC((I DIV 256) MOD 256);
  OC((I DIV 65536) MOD 256);
  OC(I DIV 16777216);
 END ELSE BEGIN
  I:=-(I+1);        
  OC(255-(I MOD 256));
  OC(255-((I DIV 256) MOD 256));
  OC(255-((I DIV 65536) MOD 256));
  OC(255-(I DIV 16777216));
 END;
END;

FUNCTION OCGI(O:INTEGER):INTEGER;
BEGIN
 IF ORD(OCTab[O+3])<$80 THEN BEGIN
  OCGI:=ORD(OCTab[O])+(ORD(OCTab[O+1])*256)+(ORD(OCTab[O+2])*65536)+(ORD(OCTab[O+3])*16777216);
 END ELSE BEGIN
  OCGI:=-(((255-ORD(OCTab[O]))+((255-ORD(OCTab[O+1]))*256)+((255-ORD(OCTab[O+2]))*65536)+((255-ORD(OCTab[O+3]))*16777216))+1);
 END;
END;

PROCEDURE OCPI(O,I:INTEGER);
BEGIN
 IF I>=0 THEN BEGIN
  OCTab[O]:=CHR(I MOD 256);
  OCTab[O+1]:=CHR((I DIV 256) MOD 256);
  OCTab[O+2]:=CHR((I DIV 65536) MOD 256);
  OCTab[O+3]:=CHR(I DIV 16777216);
 END ELSE BEGIN
  I:=-(I+1);
  OCTab[O]:=CHR(255-(I MOD 256));
  OCTab[O+1]:=CHR(255-((I DIV 256) MOD 256));
  OCTab[O+2]:=CHR(255-((I DIV 65536) MOD 256));
  OCTab[O+3]:=CHR(255-(I DIV 16777216));
 END;
END;

PROCEDURE WOC;
VAR I:INTEGER;
BEGIN
 FOR I:=1 TO OCP DO BEGIN
  WRITE(OCTab[I]);
 END;
END;

TYPE TOCS=ARRAY[1..51] OF CHAR;

PROCEDURE OCS(S:TOCS);
VAR I:INTEGER;
BEGIN
 FOR I:=1 TO 51 DO OCC(S[I]);
END;

PROCEDURE OCASC;
BEGIN
 OCP:=0;  

 OCS('MZR'#195'BeRo^fr'#0'PE'#0#0'L'#1#1#0#0#0#0#0#0#0#0#0#0#0#0#0#224#0#15#3#11#1#0#0#139#3#0#0#0#0#0#0#0#0#0);
 OCS(#0#196#16#0#0#0#16#0#0#12#0#0#0#0#0'@'#0#0#16#0#0#0#2#0#0#4#0#0#0#0#0#0#0#4#0#0#0#0#0#0#0#0' '#0#0#0#2#0#0#0#0);
 OCS(#0#0#3#0#0#0#0#0#16#0#0' '#0#0#0#0#16#0#0' '#0#0#0#0#0#0#16#0#0#0#0#0#0#0#0#0#0#0#0#16#0#0#196#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#16#0#0#0#16#0#0#139#3#0#0#0#2#0#0#0#0#0#0#0#0#0#0#0#0#0#0' '#0#0#224#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OCS(#0#0#0#0#0#0#0#0#0#0#255#255#255#255'('#16#0#0'5'#16#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0'kernel32.');
 OCS('dll'#0'W'#16#0#0'e'#16#0#0't'#16#0#0#133#16#0#0#145#16#0#0#156#16#0#0#173#16#0#0#185#16#0#0#0#0#0#0'ExitProcess');
 OCS(#0#0#0'GetStdHandle'#0#0#0'SetConsoleMode'#0#0#0'WriteFile'#0#0#0'Read');
 OCS('File'#0#0#0'GetProcessHeap'#0#0#0'HeapAlloc'#0#0#0'HeapFree'#0#233'b'#2#0#0' ');
 OCS('Compiled by: BeRoTinyPascal - (C) Copyright 2006, B');
 OCS('enjamin ''BeRo'' Rosseaux '#144#141'@'#0'V'#141't$'#8'`j'#0'h'#21#17'@'#0'j'#1'V'#255'5'''#19'@'#0#255);
 OCS(#21'A'#16'@'#0'a^'#194#4#0#144#141'@'#0#144#141'@'#0#144#141'@'#0'V'#139'\$'#8#139'D$'#12#131#248#0'}'#10#247#216'Kj-'#232#187#255#255#255'3'#201'PS'#133);
 OCS(#192't'#12'A'#187#10#0#0#0'3'#210#247#251#235#240#133#201#15#148#210#10#202'[X+'#217#131#251#0'~'#12'Qj '#232#143#255#255#255'Ku'#246'Y'#141#185'9'#17'@'#0'Q'#190);
 OCS(#10#0#0#0'3'#210#247#254#141'Z0'#136#31'O'#226#239'Yj'#0'h'#21#17'@'#0'Qh:'#17'@'#0#255'5'''#19'@'#0#255#21'A'#16'@'#0'^'#194#8#0'j'#13#232'N'#255);
 OCS(#255#255'j'#10#232'G'#255#255#255#195#144#144#141'@'#0'`j'#0'h'#212#17'@'#0'j'#1'h'#211#17'@'#0#255'5#'#19'@'#0#255#21'E'#16'@'#0#133#192#15#148#208#8#5#186#18);
 OCS('@'#0#131'='#212#17'@'#0#0#15#148#208#8#5#186#18'@'#0'a'#195#0#128'='#16#18'@'#0#0'u'#12#232#185#255#255#255#198#5#16#18'@'#0#1#195#232#229#255#255#255#15#182#5);
 OCS(#211#17'@'#0#232#160#255#255#255#195#232#211#255#255#255'`3'#192#141'H'#1#128'='#186#18'@'#0#0'uH'#128'='#211#17'@'#0#0't'#16#128'='#211#17'@'#0' w'#7#232't'#255);
 OCS(#255#255#235#222#128'='#211#17'@'#0'-u'#7#247#217#232'b'#255#255#255#15#182#29#211#17'@'#0#128#251'0r'#19#128#251'9w'#14'k'#192#10#141'D'#24#208#232'E'#255#255#255#235#225);
 OCS(#247#233'a'#195#232's'#255#255#255#128'='#186#18'@'#0#0'u'#18#138#29#211#17'@'#0#128#251#10't'#7#232'!'#255#255#255#235#224#195#0#15#182#5#186#18'@'#0#195#128'='#211#17'@');
 OCS(#0#10#15#148#210#195#144#141'@'#0#139'%'#206#18'@'#0#255'5'#31#19'@'#0'h'#0#0#0#0#255'5'#27#19'@'#0#255#21'Q'#16'@'#0'j'#0#255#21'5'#16'@'#0#210#18'@'#0);
 OCS(#25#17'@'#0'F'#17'@'#0#196#17'@'#0#39#18'@'#0'9'#18'@'#0#153#18'@'#0#187#18'@'#0#195#18'@'#0#144#141'@'#0#144#141'@'#0#144#141'@'#0#144#141'@'#0'j'#246#255);
 OCS(#21'9'#16'@'#0#163'#'#19'@'#0'j'#5'P'#255#21'='#16'@'#0'j'#245#255#21'9'#16'@'#0#163''''#19'@'#0'j'#3'P'#255#21'='#16'@'#0#137'%'#206#18'@'#0#255#21'I'#16);
 OCS('@'#0#163#27#19'@'#0'h'#28#0'@'#0'h'#12#0#1#0'P'#255#21'M'#16'@'#0#163#31#19'@'#0#141#160#0#0'@'#0#139#236#190#247#18'@'#0'         ');
 OCP:=1419;
END;

CONST locNone=0;
      locPushEAX=1;
      locPopEAX=2;
      locPopEBX=3;
      locIMulEBX=4;
      locXorEDXEDX=5;
      locIDivEBX=6;
      locPushEDX=7;
      locCmpEAXEBX=8;
      locMovzxEAXAL=9;
      locMovDWordPtrESPEAX=10;
      locJNZJNE0x03=11;
      locMovDWordPtrEBXEAX=12;
      locJmpDWordPtrESIOfs=13;
      locCallDWordPtrESIOfs=14;
      locXChgEDXESI=15;
      locPopESI=16;
      locMovECXImm=17;
      locCLD=18;
      locREPMOVSB=19;
      locTestEAXEAX=20;
      locNegDWordPtrESP=21;
      locMovEAXDWordPtrESP=22;
      locMovEBXDWordPtrFORStateCurrentValue=23;
      locCmpDWordPtrEBXEAX=24;
      locMovEAXDWordPtrFORStateDestValue=25;

VAR LOCV,PC:INTEGER;

PROCEDURE OCPushEAX;
BEGIN
 IF LOCV=locPopEAX THEN BEGIN
  IF Code[PC]=OCP THEN BEGIN
   Code[PC]:=Code[PC]-1;
  END;
  IF OCP>0 THEN OCP:=OCP-1;
  LOCV:=locNone;
 END ELSE BEGIN
  OC($50);
  LOCV:=locPushEAX;
 END;
END;

PROCEDURE OCPopEAX;
BEGIN
 IF LOCV=locPushEAX THEN BEGIN
  IF Code[PC]=OCP THEN BEGIN
   Code[PC]:=Code[PC]-1;
  END;
  IF OCP>0 THEN OCP:=OCP-1;
  LOCV:=locNone;
 END ELSE BEGIN
  OC($58);
  LOCV:=locPopEAX;
 END;
END;

PROCEDURE OCPopEBX;
BEGIN
 OC($5B);
 LOCV:=locPopEBX;
END;

PROCEDURE OCIMulEBX;
BEGIN
 OC($F7); OC($EB);
 LOCV:=locIMulEBX;
END;

PROCEDURE OCXorEDXEDX;
BEGIN
 OC($33); OC($D2);
 LOCV:=locXorEDXEDX;
END;

PROCEDURE OCIDIVEBX;
BEGIN
 OC($F7); OC($FB);
 LOCV:=locIDivEBX;
END;

PROCEDURE OCPushEDX;
BEGIN
 OC($52);
 LOCV:=locPushEDX;
END;

PROCEDURE OCCmpEAXEBX;
BEGIN
 OC($3B); OC($C3);
 LOCV:=locCmpEAXEBX;
END;

PROCEDURE OCMovzxEAXAL;
BEGIN
 OC($0F); OC($B6); OC($C0);
 LOCV:=locMovzxEAXAL;
END;

PROCEDURE OCJNZJNE0x03;
BEGIN
 OC($75); OC($03);
 LOCV:=locJNZJNE0x03;
END;

PROCEDURE OCMovDWordPtrESPEAX;
BEGIN
 OC($89); OC($04); OC($24);
 LOCV:=locMovDWordPtrESPEAX;
END;

PROCEDURE OCMovDWordPtrEBXEAX;
BEGIN
 OC($89); OC($03);
 LOCV:=locMovDWordPtrEBXEAX;
END;

PROCEDURE OCJmpDWordPtrESIOfs(Ofs:INTEGER);
BEGIN
 OC($FF); OC($66); OC(Ofs);
 LOCV:=locJmpDWordPtrESIOfs;
END;

PROCEDURE OCCallDWordPtrESIOfs(Ofs:INTEGER);
BEGIN
 OC($FF); OC($56); OC(Ofs);
 LOCV:=locCallDWordPtrESIOfs;
END;

PROCEDURE OCXChgEDXESI;
BEGIN
 OC($87); OC($D6);
 LOCV:=locXChgEDXESI;
END;

PROCEDURE OCPopESI;
BEGIN
 OC($5E);
 LOCV:=locPopESI;
END;

PROCEDURE OCMovECXImm(Value:INTEGER);
BEGIN
 OC($B9); OCI(Value);
 LOCV:=locMovECXImm;
END;

PROCEDURE OCCLD;
BEGIN
 OC($FC);
 LOCV:=locCLD;
END;

PROCEDURE OCREPMOVSB;
BEGIN
 OC($F3); OC($A4);
 LOCV:=locREPMOVSB;
END;

PROCEDURE OCTestEAXEAX;
BEGIN
 OC($85); OC($C0); { TEST EAX,EAX }
 LOCV:=locTestEAXEAX;
END;

PROCEDURE OCNegDWordPtrESP;
BEGIN
 OC($F7); OC($1C); OC($24); { NEG DWORD PTR [ESP] }
 LOCV:=locNegDWordPtrESP;
END;

PROCEDURE OCMovEAXDWordPtrESP;
BEGIN
 OC($8B); OC($04); OC($24); { MOV EAX,DWORD PTR [ESP] }
 LOCV:=locMovEAXDWordPtrESP;
END;

PROCEDURE OCMovEBXDWordPtrFORStateCurrentValue;
BEGIN
 OC($8B); OC($5D); OC($04);
 LOCV:=locMovEBXDWordPtrFORStateCurrentValue;
END;

PROCEDURE OCCmpDWordPtrEBXEAX;
BEGIN
 OC($39); OC($03);
 LOCV:=locCmpDWordPtrEBXEAX;
END;

PROCEDURE OCMovEAXDWordPtrFORStateDestValue;
BEGIN
 OC($8B); OC($45); OC($08);
 LOCV:=locMovEAXDWordPtrFORStateDestValue;
END;

VAR JCPT:ARRAY[1..MaxCode] OF INTEGER;

PROCEDURE AssembleAndLink;
VAR JCPC,I,D,CS,SA,SOCP:INTEGER;
BEGIN
 OCASC;
 SOCP:=OCP;
 LOCV:=locNone;
 PC:=0;
 JCPC:=0;
 WHILE PC<CodePos DO BEGIN
  I:=Code[PC];
  D:=Code[PC+1];
  Code[PC]:=OCP;
  CASE I OF
   OPAdd:BEGIN
    OCPopEAX;
    OC($01); OC($04); OC($24); { ADD DWORD PTR [ESP],EAX }
    LOCV:=locNone;
   END;
   OPNeg:BEGIN
    OCNegDWordPtrESP;
   END;
   OPMul:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCIMulEBX;
    OCPushEAX;
   END;
   OPDivD:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCXorEDXEDX;
    OCIDIVEBX;
    OCPushEAX;
   END;
   OPRemD:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCXorEDXEDX;
    OCIDIVEBX;
    OCPushEDX;
   END;
   OPDiv2:BEGIN
    OC($D1); OC($3C); OC($24); { SAR DWORD PTR [ESP],1 }
    LOCV:=locNone;
   END;
   OPRem2:BEGIN
    OCPopEAX;  
    OC($8B); OC($D8); { MOV EBX,EAX }
    OC($25); OC($01); OC($00); OC($00); OC($80); { AND EAX,$80000001 }
    OC($79); OC($05); { JNS +$05 }
    OC($48); { DEC EAX }
    OC($83); OC($C8); OC($FE); { OR EAX,BYTE -$02 }
    OC($40); { INC EAX }
    LOCV:=locNone;
    OCIMulEBX;
    OCPushEAX;
   END;
   OPEqlI:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($94); OC($D0); { SETE AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPNEqI:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($95); OC($D0); { SETNE AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPLssI:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($9C); OC($D0); { SETL AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPLeqI:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($9E); OC($D0); { SETLE AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPGtrI:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($9F); OC($D0); { SETG AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPGEqi:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    OC($0F); OC($9D); OC($D0); { SETGE AL }
    LOCV:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   END;
   OPDupl:BEGIN
    OC($FF); OC($34); OC($24); { PUSH DWORD PTR [ESP] }
    LOCV:=locNone;
   END;
   OPSwap:BEGIN
    OCPopEBX;
    OCPopEAX;
    OC($53); { PUSH EBX }
    LOCV:=locNone;
    OCPushEAX;
   END;
   OPAndB:BEGIN
    OCPopEAX;
    OCTestEAXEAX;
    OCJNZJNE0x03;
    OCMovDWordPtrESPEAX;
    LOCV:=locNone;
   END;
   OPOrB:BEGIN
    OCPopEAX;
    OC($83); OC($F8); OC($01);  { CMP EAX,1 }
    LOCV:=locNone;
    OCJNZJNE0x03;
    OCMovDWordPtrESPEAX;
    LOCV:=locNone;
   END;
   OPLoad:BEGIN
    OCPopEAX;
    OC($FF); OC($30); { PUSH DWORD PTR [EAX] }
    LOCV:=locNone;
   END;
   OPStore:BEGIN
    OCPopEBX;
    OCPopEAX;
    OCMovDWordPtrEBXEAX;
   END;
   OPHalt:BEGIN
    OCJmpDWordPtrESIOfs(0);
   END;
   OPWrI:BEGIN
    OCCallDWordPtrESIOfs(8);
   END;
   OPWrC:BEGIN
    OCCallDWordPtrESIOfs(4);
   END;
   OPWrL:BEGIN
    OCCallDWordPtrESIOfs(12);
   END;
   OPRdI:BEGIN
    OCPopEBX;
    OCCallDWordPtrESIOfs(20);
    OCMovDWordPtrEBXEAX;
   END;
   OPRdC:BEGIN
    OCPopEBX;
    OCCallDWordPtrESIOfs(16);
    OCMovzxEAXAL;
    OCMovDWordPtrEBXEAX;
   END;
   OPRdL:BEGIN
    OCCallDWordPtrESIOfs(24);
   END;
   OPEOF:BEGIN
    OCCallDWordPtrESIOfs(28);
    OCPushEAX;
   END;
   OPEOL:BEGIN
    OCCallDWordPtrESIOfs(32);
    OCPushEAX;
   END;
   OPLdC:BEGIN
    IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($6A); OC(D); { PUSH BYTE D }
    END ELSE BEGIN
     OC($68); OCI(D); { PUSH DWORD D }
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPLdA:BEGIN
    IF D=0 THEN BEGIN
     OC($8B); OC($C5); { MOV EAX,EBP }
    END ELSE IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($8D); OC($45); OC(D); { LEA EAX,[EBP+BYTE D] }
    END ELSE BEGIN
     OC($8D); OC($85); OCI(D); { LEA EAX,[EBP+DWORD D] }
    END;
    LOCV:=locNone;
    OCPushEAX;
    PC:=PC+1;
   END;
   OPLdLA:BEGIN
    IF D=0 THEN BEGIN
     OC($8B); OC($C4); { MOV EAX,ESP }
    END ELSE IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($8D); OC($44); OC($24); OC(D); { LEA EAX,[ESP+BYTE D] }
    END ELSE BEGIN
     OC($8D); OC($84); OC($24); OCI(D); { LEA EAX,[ESP+DWORD D] }
    END;
    LOCV:=locNone;
    OCPushEAX;
    PC:=PC+1;
   END;
   OPLdL:BEGIN
    IF D=0 THEN BEGIN
     OCMovEAXDWordPtrESP;
    END ELSE IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($8B); OC($44); OC($24); OC(D); { MOV EAX,DWORD PTR [ESP+BYTE D] }
    END ELSE BEGIN
     OC($8B); OC($84); OC($24); OCI(D); { MOV EAX,DWORD PTR [ESP+DWORD D] }
    END;
    OCPushEAX;
    PC:=PC+1;
   END;
   OPLdG:BEGIN
    IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($8B); OC($45); OC(D); { MOV EAX,DWORD PTR [EBP+BYTE D] }
    END ELSE BEGIN
     OC($8B); OC($85); OCI(D); { MOV EAX,DWORD PTR [EBP+DWORD D] }
    END;
    LOCV:=locNone;
    OCPushEAX;
    PC:=PC+1;
   END;
   OPStL:BEGIN
    OCPopEAX;
    D:=D-4;
    IF D=0 THEN BEGIN
     OC($89); OC($04); OC($24); { MOV DWORD PTR [ESP],EAX }
    END ELSE IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($89); OC($44); OC($24); OC(D); { MOV DWORD PTR [ESP+BYTE D],EAX }
    END ELSE BEGIN
     OC($89); OC($84); OC($24); OCI(D); { MOV EAX,DWORD PTR [ESP+DWORD D] }
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPStG:BEGIN
    OCPopEAX;
    IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($89); OC($45); OC(D); { MOV DWORD PTR [EBP+BYTE D],EAX }
    END ELSE BEGIN
     OC($89); OC($85); OCI(D); { MOV EAX,DWORD PTR [EBP+DWORD D] }
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPMove:BEGIN
    OCXChgEDXESI;
    OC($5F); { POP EDI }
    LOCV:=locNone;
    OCPopESI;
    OCMovECXImm(D);
    OCCLD;
    OCREPMOVSB;
    OCXChgEDXESI;
    PC:=PC+1;
   END;
   OPCopy:BEGIN
    OCXChgEDXESI;
    OCPopESI;
    OCMovECXImm(D);
    OC($2B); OC($E1); { SUB ESP,ECX }
    OC($8B); OC($FC); { MOV EDI,ESP }
    LOCV:=locNone;
    OCCLD;
    OCREPMOVSB;
    OCXChgEDXESI;
    PC:=PC+1;
   END;
   OPAddC:BEGIN
    IF (D>=-128) AND (D<=127) THEN BEGIN
     OC($83); OC($04); OC($24); OC(D); { ADD DWORD PTR [ESP],BYTE D }
    END ELSE BEGIN
     OC($81); OC($04); OC($24); OCI(D); { ADD DWORD PTR [ESP],DWORD D }
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPMulC:BEGIN
    IF D=(-1) THEN BEGIN
     OCNegDWordPtrESP;
    END ELSE IF (D>=-128) AND (D<=127) THEN BEGIN
     OCPopEAX;
     OC($6B); OC($C0); OC(D); { IMUL EAX,BYTE S }
     LOCV:=locNone;
     OCPushEAX;
    END ELSE BEGIN
     OCPopEAX;
     OC($69); OC($C0); OCI(D); { IMUL EAX,DWORD S }
     LOCV:=locNone;
     OCPushEAX;
    END;
    PC:=PC+1;
   END;
   OPJmp:BEGIN
    IF D<>(PC+2) THEN BEGIN
     JCPC:=JCPC+1;
     OC($E9); { JMP D }
     JCPT[JCPC]:=OCP+1;
     OCI(D);
    END;
    PC:=PC+1;
    LOCV:=locNone;
   END;
   OPJZ:BEGIN
    JCPC:=JCPC+1;
    OCPopEAX;
    OCTestEAXEAX;
    OC($0F); OC($84); { JZ D }
    JCPT[JCPC]:=OCP+1;
    OCI(D);
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPCall:BEGIN
    JCPC:=JCPC+1;
    OC($E8); { CALL D }
    JCPT[JCPC]:=OCP+1;
    OCI(D);
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPAdjS:BEGIN
    IF D>0 THEN BEGIN
     IF (D>=-128) AND (D<=127) THEN BEGIN
      OC($83); OC($C4); OC(D); { ADD ESP,BYTE D }
     END ELSE BEGIN
      OC($81); OC($C4); OCI(D); { ADD ESP,DWORD D }
     END;
    END ELSE IF D<0 THEN BEGIN
     D:=-D;
     IF (D>=-128) AND (D<=127) THEN BEGIN
      OC($83); OC($EC); OC(D); { SUB ESP,BYTE D }
     END ELSE BEGIN
      OC($81); OC($EC); OCI(D); { SUB ESP,DWORD D }
     END;
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
   OPExit:BEGIN
    D:=D-4;
    IF D>0 THEN BEGIN
     OC($C2); OCW(D); { RET D }
    END ELSE IF D=0 THEN BEGIN
     OC($C3); { RET }
    END;
    LOCV:=locNone;
    PC:=PC+1;
   END;
  END;
  PC:=PC+1;
 END;

 { Patch jumps + calls }
 FOR I:=1 TO JCPC DO BEGIN
  D:=JCPT[I];
  OCPI(D,((Code[OCGI(D)]-D)-3));
 END;

 { Size Of Code }
 CS:=OCGI($29)+(OCP-SOCP);
 OCPI($29,CS);

 { Get section alignment }
 SA:=OCGI($45);

 { Calculate and patch section virtual size }
 I:=CS;
 IF SA<>0 THEN BEGIN
  D:=I MOD SA;
  IF D<>0 THEN BEGIN
   I:=I+(SA-D);
  END;
 END;
 OCPI($10D,I);

 { Calculate and patch image size }
 OCPI($5D,I+OCGI($39));

 { Patch section raw size }
 OCPI($115,OCGI($115)+(OCP-SOCP));

 WOC;
END;

BEGIN
 StrCpy(Keywords[SymBEGIN],'BEGIN               ');
 StrCpy(Keywords[SymEND],'END                 ');
 StrCpy(Keywords[SymIF],'IF                  ');
 StrCpy(Keywords[SymTHEN],'THEN                ');
 StrCpy(Keywords[SymELSE],'ELSE                ');
 StrCpy(Keywords[SymWHILE],'WHILE               ');
 StrCpy(Keywords[SymDO],'DO                  ');
 StrCpy(Keywords[SymCASE],'CASE                ');
 StrCpy(Keywords[SymREPEAT],'REPEAT              ');
 StrCpy(Keywords[SymUNTIL],'UNTIL               ');
 StrCpy(Keywords[SymFOR],'FOR                 ');
 StrCpy(Keywords[SymTO],'TO                  ');
 StrCpy(Keywords[SymDOWNTO],'DOWNTO              ');
 StrCpy(Keywords[SymNOT],'NOT                 ');
 StrCpy(Keywords[SymDIV],'DIV                 ');
 StrCpy(Keywords[SymMOD],'MOD                 ');
 StrCpy(Keywords[SymAND],'AND                 ');
 StrCpy(Keywords[SymOR],'OR                  ');
 StrCpy(Keywords[SymCONST],'CONST               ');
 StrCpy(Keywords[SymVAR],'VAR                 ');
 StrCpy(Keywords[SymTYPE],'TYPE                ');
 StrCpy(Keywords[SymARRAY],'ARRAY               ');
 StrCpy(Keywords[SymOF],'OF                  ');
 StrCpy(Keywords[SymPACKED],'PACKED              ');
 StrCpy(Keywords[SymRECORD],'RECORD              ');
 StrCpy(Keywords[SymPROGRAM],'PROGRAM             ');
 StrCpy(Keywords[SymFORWARD],'FORWARD             ');
 StrCpy(Keywords[SymHALT],'HALT                ');
 StrCpy(Keywords[SymFUNC],'FUNCTION            ');
 StrCpy(Keywords[SymPROC],'PROCEDURE           ');

 TypeTab[TypeINT].Size:=4;
 TypeTab[TypeINT].Kind:=KindSIMPLE;
 TypeTab[TypeCHAR].Size:=4;
 TypeTab[TypeCHAR].Kind:=KindSIMPLE;
 TypeTab[TypeBOOL].Size:=4;
 TypeTab[TypeBOOL].Kind:=KindSIMPLE;
 TypeTab[TypeSTR].Size:=0;
 TypeTab[TypeSTR].Kind:=KindSIMPLE;
 TypePos:=4;

 SymNameList[-1]:=0;
 CurLevel:=-1;
 IdentPos:=0;

 EnterSymbol('FALSE               ',IdCONST,TypeBOOL);
 IdentTab[IdentPos].Value:=ORD(FALSE);

 EnterSymbol('TRUE                ',IdCONST,TypeBOOL);
 IdentTab[IdentPos].Value:=ORD(TRUE);

 EnterSymbol('MAXINT              ',IdCONST,TypeINT);
 IdentTab[IdentPos].Value:=2147483647;

 EnterSymbol('INTEGER             ',IdTYPE,TypeINT);
 EnterSymbol('CHAR                ',IdTYPE,TypeCHAR);
 EnterSymbol('BOOLEAN             ',IdTYPE,TypeBOOL);

 EnterSymbol('CHR                 ',IdFUNC,TypeCHAR);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunCHR;
 IdentTab[IdentPos].Inside:=FALSE;

 EnterSymbol('ORD                 ',IdFUNC,TypeINT);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunORD;
 IdentTab[IdentPos].Inside:=FALSE;

 EnterSymbol('WRITE               ',IdFUNC,0);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunWRITE;

 EnterSymbol('WRITELN             ',IdFUNC,0);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunWRITELN;

 EnterSymbol('READ                ',IdFUNC,0);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunREAD;

 EnterSymbol('READLN              ',IdFUNC,0);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunREADLN;

 EnterSymbol('EOF                 ',IdFUNC,TypeBOOL);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunEOF;
 IdentTab[IdentPos].Inside:=FALSE;

 EnterSymbol('EOLN                ',IdFUNC,TypeBOOL);
 IdentTab[IdentPos].FLevel:=-1;
 IdentTab[IdentPos].FAdr:=FunEOFLN;
 IdentTab[IdentPos].Inside:=FALSE;

 SymNameList[0]:=0;
 CurLevel:=0;

 LineNum:=1;
 LinePos:=0;

 ReadChar;
 GetSymbol;
 IsLabeled:=TRUE;
 CodePos:=0;
 LastOpcode:=-1;
 StackPos:=4;
 Expect(SymPROGRAM);
 Expect(TokIdent);
 Expect(TokSemi);
 GenOp(OPJmp,0);
 Block(0);
 GenOp2(OPHalt);
 Check(TokPeriod);
 AssembleAndLink;
END.
