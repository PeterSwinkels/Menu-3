DEFINT A-Z

DECLARE SUB AskMenuDir ()
DECLARE SUB AskPrgInfo (IsNewPrg)
DECLARE SUB Center (x, Text$)
DECLARE SUB DisplayBackground ()
DECLARE SUB DisplayHelp ()
DECLARE SUB DisplayMenu (Menu, MenuCurX)
DECLARE SUB DisplayPrgms ()
DECLARE SUB DisplayWait ()
DECLARE SUB DrawBox (x, y, Wdth, Height, Shadow)
DECLARE SUB MainMenu ()
DECLARE SUB LoadActvGrpName ()
DECLARE SUB LoadGrpsAndMenu ()
DECLARE SUB LoadMenuDir ()
DECLARE SUB LoadPlldwnMnTxt ()
DECLARE SUB MakeNewGroup ()
DECLARE SUB PlldwnMenus ()
DECLARE SUB Remove (PrgDir$, PrgCmd$, PrgGrpName$, IsGroup)
DECLARE SUB SavePrgInfo (PrgDir$, PrgCmd$, PrgName$, IsNewPrg)
DECLARE SUB SaveSettings ()
DECLARE SUB SelectGroup ()
OPTION BASE 1
ON ERROR GOTO ErrorTrap
DIM SHARED PrgDir$(20, 15), PrgCmd$(20, 15), PrgName$(20, 15), Group$(10)
DIM SHARED MenuRwCnt(3), MenuWdth(3), MenuText$(3, 3), MenuY(3)
DIM SHARED ActvGrp$, MenuDir$, Page, Row
SCREEN 0: WIDTH 40, 25: WIDTH 80, 25: COLOR 7, 0: CLS
LoadMenuDir
LoadActvGrpName
LoadPlldwnMnTxt
 DO
  LoadGrpsAndMenu
  CALL MainMenu
 LOOP

ErrorTrap:
PCOPY 0, 1
e = ERR
 IF e = 7 OR e = 14 THEN
  Bericht$ = "Not enough memory."
 ELSEIF e = 53 THEN
  Bericht$ = "Cannot find file."
 ELSEIF e = 61 THEN
  Bericht$ = "The disk is full."
 ELSEIF e = 62 THEN
  Bericht$ = "Error while reading file."
 ELSEIF e = 71 THEN
  Bericht$ = "There is no disk in diskdrive."
 ELSEIF e = 76 THEN
  Bericht$ = "Cannot find directory."
 ELSE
  Bericht$ = "Unexpected error."
 END IF
Bericht$ = " " + Bericht$ + " "
l = LEN(Bericht$)
COLOR 0, 7
Center 5, Bericht$
Center 6, SPACE$(l)
COLOR 7, 0
Center 6, "  Ok  "
Center 7, STRING$(l, "ß")
Center 8, " Escape = Quit  F3 = Ignore. "
 DO
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
  IF Key$ = CHR$(0) + "=" THEN PCOPY 1, 0: RESUME NEXT
  IF Key$ = CHR$(27) THEN COLOR 7, 0: CLS : SYSTEM
 LOOP

MenuData:
DATA " Quit ","*"
DATA " New Program "," Remove Program "," Select Group ","*"
DATA " Help ","*"

SUB AskMenuDir
COLOR 0, 7
NwMenuDir$ = MenuDir$
DrawBox 4, 18, 42, 2, -1
 DO
  LOCATE 5, 20: PRINT "Where is Menu 3 located?"
  LOCATE , 20: PRINT "> "; NwMenuDir$; " "
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
   IF Key$ = CHR$(8) THEN
    IF LEN(NwMenuDir$) > 0 THEN NwMenuDir$ = LEFT$(NwMenuDir$, LEN(NwMenuDir$) - 1)
   ELSEIF Key$ = CHR$(13) THEN
     IF NOT RIGHT$(NwMenuDir$, 1) = "\" THEN NwMenuDir$ = NwMenuDir$ + "\"
    MenuDir$ = NwMenuDir$
    EXIT SUB
   ELSEIF Key$ = CHR$(27) THEN
    EXIT SUB
   ELSEIF ASC(Key$) > 31 THEN
    IF LEN(NwMenuDir$) < 38 THEN NwMenuDir$ = NwMenuDir$ + Key$
   END IF
 LOOP
END SUB

SUB AskPrgInfo (IsNewPrg)
 IF NOT IsNewPrg THEN
  PrgName$ = PrgName$(Page, Row)
  PrgDir$ = PrgDir$(Page, Row)
  PrgCmd$ = PrgCmd$(Page, Row)
 END IF
CursorX = 6
COLOR 0, 7: DrawBox 5, 19, 40, 3, -1
 DO
   IF CursorX = 6 THEN COLOR 7, 0 ELSE COLOR 0, 7
  LOCATE 6, 20: PRINT "       Name: "; PrgName$; SPC(27 - LEN(PrgName$));
   IF CursorX = 7 THEN COLOR 7, 0 ELSE COLOR 0, 7
  LOCATE 7, 20
   IF LEN(PrgDir$) < 22 THEN
    PRINT "  Directory: "; PrgDir$; SPC(27 - LEN(PrgDir$));
   ELSE
    PRINT "  Directory: "; RIGHT$(PrgDir$, 21); SPC(6);
   END IF
   IF CursorX = 8 THEN COLOR 7, 0 ELSE COLOR 0, 7
  LOCATE 8, 20: PRINT "    Command: "; PrgCmd$; SPC(27 - LEN(PrgCmd$));
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
   IF Key$ = CHR$(0) + "H" THEN
    IF CursorX = 6 THEN CursorX = 8 ELSE CursorX = CursorX - 1
   ELSEIF Key$ = CHR$(0) + "P" THEN
    IF CursorX = 8 THEN CursorX = 6 ELSE CursorX = CursorX + 1
   ELSEIF Key$ = CHR$(13) THEN
     IF PrgName$ = "" OR PrgDir$ = "" OR PrgCmd$ = "" THEN EXIT SUB
    SavePrgInfo PrgName$, PrgDir$, PrgCmd$, IsNewPrg
    EXIT SUB
   ELSEIF Key$ = CHR$(27) THEN
    EXIT SUB
   ELSEIF Key$ = CHR$(8) THEN
    IF CursorX = 6 THEN
     IF LEN(PrgName$) > 0 THEN PrgName$ = LEFT$(PrgName$, LEN(PrgName$) - 1)
    ELSEIF CursorX = 7 THEN
     IF LEN(PrgDir$) > 0 THEN PrgDir$ = LEFT$(PrgDir$, LEN(PrgDir$) - 1)
    ELSEIF CursorX = 8 THEN
     IF LEN(PrgCmd$) > 0 THEN PrgCmd$ = LEFT$(PrgCmd$, LEN(PrgCmd$) - 1)
    END IF
   ELSEIF ASC(Key$) > 31 THEN
    IF CursorX = 6 THEN
     IF LEN(PrgName$) < 21 THEN PrgName$ = PrgName$ + Key$
    ELSEIF CursorX = 7 THEN
     IF LEN(PrgDir$) < 100 THEN PrgDir$ = PrgDir$ + Key$
    ELSEIF CursorX = 8 THEN
     IF LEN(PrgCmd$) < 21 THEN PrgCmd$ = PrgCmd$ + Key$
    END IF
   END IF
 LOOP
END SUB

SUB Center (x, Text$)
LOCATE x, 40 - (LEN(Text$) / 2): PRINT Text$
END SUB

SUB DisplayBackground
COLOR 7, 0: DrawBox 1, 1, 78, 21, 0
LOCATE 1, 1
COLOR 7, 0: PRINT "Ú"; STRING$(38 - (CINT(LEN(ActvGrp$) / 2)) - 1, "Ä"); "´";
COLOR 0, 7: PRINT " "; ActvGrp$; " ";
COLOR 7, 0: PRINT "Ã"; STRING$(38 - (LEN(ActvGrp$) / 2) - 1, "Ä"); "¿"
LOCATE 2, 2: PRINT USING " F1 = Help   [###### bytes of memory available.] "; (FRE(-1) + FRE(-2) + FRE(""))
LOCATE 20, 4: PRINT "ÛÛÛÛÛ²²²²²±±±±±°° Menu 3, By: Peter Swinkels, ***1996*** °°±±±±±²²²²²ÛÛÛÛÛ"
LOCATE , 1: PRINT "Ã"; STRING$(78, "Ä"); "´"
LOCATE , 5
COLOR 0, 7: PRINT USING " Page: ### Row: ## "; Page; Row;
COLOR 7, 0: PRINT , , "Date: "; DATE$; "    Time: "; TIME$
DisplayPrgms
END SUB

SUB DisplayHelp
COLOR 0, 7
DrawBox 5, 15, 50, 9, -1
LOCATE 6
OPEN MenuDir$ + "Menu3.hlp" FOR INPUT AS 1
  DO
   LINE INPUT #1, HelpText$
   LOCATE , 16: PRINT HelpText$
  LOOP UNTIL EOF(1)
CLOSE 1
COLOR 7, 0
LOCATE 17, 16: PRINT "Press any key to exit help."
 DO: LOOP WHILE INKEY$ = ""
END SUB

SUB DisplayMenu (Menu, MenuCurX)
COLOR 0, 7
DrawBox 2, MenuY(Menu), MenuWdth(Menu), MenuRwCnt(Menu), -1
LOCATE 3
 FOR MenuRow = 1 TO MenuRwCnt(Menu)
   IF MenuRow = MenuCurX THEN COLOR 7, 0 ELSE COLOR 0, 7
  LOCATE , MenuY(Menu) + 1
  PRINT MenuText$(Menu, MenuRow); SPACE$(MenuWdth(Menu) - LEN(MenuText$(Menu, MenuRow)))
 NEXT MenuRow
END SUB

SUB DisplayPrgms
LOCATE 5
 FOR Program = 1 TO 15
  LOCATE , 10
  PRINT " "; PrgName$(Page, Program); SPACE$(21 - LEN(PrgName$(Page, Program)))
 NEXT Program
END SUB

SUB DisplayWait
COLOR 0, 7: DrawBox 5, 31, 16, 1, -1
LOCATE 6, 33: PRINT "Please wait"; : COLOR 16: PRINT "..."
COLOR 7, 0
END SUB

SUB DrawBox (x, y, Wdth, Height, Shadow)
LOCATE x, y: PRINT "Ú"; STRING$(Wdth, "Ä"); "¿"
 FOR BoxRow = 1 TO Height
  LOCATE , y: PRINT "³"; SPC(Wdth); "³";
   IF Shadow THEN PRINT "²²" ELSE PRINT
 NEXT BoxRow
LOCATE , y: PRINT "À"; STRING$(Wdth, "Ä"); "Ù";
 IF Shadow THEN PRINT "²²" ELSE PRINT
 IF Shadow THEN LOCATE , y + 2: PRINT STRING$(Wdth + 2, "²")
END SUB

SUB LoadActvGrpName
OPEN "ActveGrp.dat" FOR BINARY AS 1
 ActvGrp$ = INPUT$(LOF(1), 1)
CLOSE 1
 IF ActvGrp$ = "" THEN ActvGrp$ = "Main"
END SUB

SUB LoadGrpsAndMenu
DisplayWait

ERASE PrgDir$, PrgCmd$, PrgName$, Group$

OPEN MenuDir$ + "Groups.dat" FOR BINARY AS 1
 IF LOF(1) = 0 THEN
  CLOSE 1
  OPEN MenuDir$ + "Groups.dat" FOR OUTPUT AS 1
   PRINT #1, CHR$(10); "Main";
 ELSE
  FOR Grp = 1 TO 10
   l = ASC(INPUT$(1, 1)): Group$(Grp) = INPUT$(l, 1)
    IF LOC(1) = LOF(1) THEN EXIT FOR
  NEXT Grp
 END IF
CLOSE 1

FileRow = 1: FilePage = 1
OPEN MenuDir$ + "Menu3.dat" FOR BINARY AS 1
 DO UNTIL LOC(1) = LOF(1) OR FilePage = 21
  l = ASC(INPUT$(1, 1)): PrgDir$(FilePage, FileRow) = INPUT$(l, 1)
  l = ASC(INPUT$(1, 1)): PrgCmd$(FilePage, FileRow) = INPUT$(l, 1)
  l = ASC(INPUT$(1, 1)): PrgName$(FilePage, FileRow) = INPUT$(l, 1)
  l = ASC(INPUT$(1, 1)): Grp$ = INPUT$(l, 1)
   IF Grp$ = ActvGrp$ THEN
    IF FileRow = 15 THEN
     FilePage = FilePage + 1
     FileRow = 1
    ELSE
     FileRow = FileRow + 1
    END IF
   ELSE
    PrgDir$(FilePage, FileRow) = ""
    PrgCmd$(FilePage, FileRow) = ""
    PrgName$(FilePage, FileRow) = ""
   END IF
 LOOP
CLOSE 1
END SUB

SUB LoadMenuDir
OPEN "Menu3.loc" FOR BINARY AS 1
 IF LOF(1) = 0 THEN
  AskMenuDir
 ELSE
  MenuDir$ = INPUT$(LOF(1), 1)
 END IF
CLOSE 1
 IF NOT RIGHT$(MenuDir$, 1) = "\" THEN MenuDir$ = MenuDir$ + "\"
END SUB

SUB LoadPlldwnMnTxt
RESTORE MenuData
 FOR Menu = 1 TO 3
  FOR MenuRow = 1 TO 5
   READ MenuText$
    IF MenuText$ = "*" THEN EXIT FOR ELSE MenuText$(Menu, MenuRow) = MenuText$
  NEXT MenuRow
 NEXT Menu
END SUB

SUB MainMenu
Row = 1: Page = 1

COLOR 7, 0: CLS
DisplayBackground
 DO
  DisplayPrgms
  COLOR 0, 7
  LOCATE Row + 4, 10: PRINT " "; PrgName$(Page, Row); SPACE$(21 - LEN(PrgName$(Page, Row)))
   DO
    COLOR 0, 7: LOCATE 22, 5: PRINT USING " Page: ### Row: ## "; Page; Row;
    COLOR 7, 0: PRINT , , " Date: "; DATE$; "    Time: "; TIME$
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
  COLOR 7, 0: LOCATE Row + 4, 10: PRINT " "; PrgName$(Page, Row); SPACE$(21 - LEN(PrgName$(Page, Row)))
   IF Key$ = CHR$(0) + "H" THEN
    IF Row > 1 THEN
     Row = Row - 1
    ELSE
     IF Page > 1 THEN Page = Page - 1: Row = 15
    END IF
   ELSEIF Key$ = CHR$(0) + "P" THEN
    IF Row < 15 THEN
     Row = Row + 1
    ELSE
     IF Page < 20 THEN Page = Page + 1: Row = 1
    END IF
   ELSEIF Key$ = CHR$(0) + "I" THEN
    IF Page > 1 THEN Page = Page - 1: Row = 15
   ELSEIF Key$ = CHR$(0) + "Q" THEN
    IF Page < 20 THEN Page = Page + 1: Row = 1
   ELSEIF Key$ = CHR$(7) THEN
    SelectGroup
    EXIT SUB
   ELSEIF Key$ = CHR$(17) THEN
    COLOR 7, 0: CLS : SaveSettings: SYSTEM
   ELSEIF Key$ = CHR$(0) + "S" THEN
    Remove PrgDir$(Page, Row), PrgCmd$(Page, Row), PrgName$(Page, Row), 0
    EXIT SUB
   ELSEIF Key$ = CHR$(14) THEN
    AskPrgInfo -1
    EXIT SUB
   ELSEIF Key$ = CHR$(9) THEN
    AskPrgInfo 0
    EXIT SUB
   ELSEIF Key$ = CHR$(13) THEN
     IF MID$(PrgDir$(Page, Row), 2, 1) = ":" THEN SHELL LEFT$(PrgDir$(Page, Row), 2)
    CHDIR PrgDir$(Page, Row)
    PrgFile$ = PrgCmd$(Page, Row)
     IF INSTR(PrgFile$, " ") THEN PrgFile$ = LEFT$(PrgFile$, INSTR(PrgFile$, " ") - 1)
    OPEN PrgFile$ FOR INPUT AS 1: CLOSE 1
    COLOR 7, 0: CLS
    SHELL PrgCmd$(Page, Row)
    SCREEN 0: WIDTH 40, 25: WIDTH 80, 25
     IF MID$(MenuDir$, 2, 1) = ":" THEN SHELL LEFT$(MenuDir$, 2)
    CHDIR LEFT$(MenuDir$, LEN(MenuDir$) - 1)
    EXIT SUB
   ELSEIF Key$ = CHR$(0) + ";" THEN
    DisplayHelp
    EXIT SUB
   ELSEIF Key$ = CHR$(0) + "<" THEN
    AskMenuDir
    EXIT SUB
   ELSEIF Key$ = CHR$(27) THEN
    CALL PlldwnMenus
    EXIT SUB
   END IF
 LOOP
END SUB

SUB MakeNewGroup
COLOR 0, 7
DrawBox 5, 28, 22, 1, -1
 DO
  LOCATE 6, 30: PRINT "> "; NwGrp$; " "
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
   IF Key$ = CHR$(27) THEN
    EXIT SUB
   ELSEIF Key$ = CHR$(13) THEN
     IF NwGrp$ = "" THEN EXIT SUB
    OPEN MenuDir$ + "Groups.dat" FOR APPEND AS 1
     PRINT #1, CHR$(LEN(NwGrp$)); NwGrp$;
    CLOSE 1
    EXIT SUB
   ELSEIF Key$ = CHR$(8) THEN
    IF LEN(NwGrp$) > 0 THEN NwGrp$ = LEFT$(NwGrp$, LEN(NwGrp$) - 1)
   ELSEIF ASC(Key$) > 31 THEN
    IF LEN(NwGrp$) < 18 THEN NwGrp$ = NwGrp$ + Key$
   END IF
 LOOP
END SUB

SUB PlldwnMenus
CursorX = 1
Menu = 1
MenuY(1) = 3: MenuWdth(1) = 6: MenuRwCnt(1) = 1
MenuY(2) = 13: MenuWdth(2) = 16: MenuRwCnt(2) = 3
MenuY(3) = 23: MenuWdth(3) = 6: MenuRwCnt(3) = 1
 DO
  COLOR 0, 7: LOCATE 1, 1: PRINT "   Program   Menu      Help"; SPC(53);
  COLOR 16, 7: LOCATE 1, MenuY(Menu): PRINT "[": LOCATE 1, MenuY(Menu) + 8: PRINT "]"
  DisplayMenu Menu, CursorX
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
  IF Key$ = CHR$(0) + "H" THEN
   IF CursorX = 1 THEN CursorX = MenuRwCnt(Menu) ELSE CursorX = CursorX - 1
  ELSEIF Key$ = CHR$(0) + "P" THEN
   IF CursorX = MenuRwCnt(Menu) THEN CursorX = 1 ELSE CursorX = CursorX + 1
  ELSEIF Key$ = CHR$(0) + "K" THEN
   DisplayBackground
   CursorX = 1
    IF Menu = 1 THEN Menu = 3 ELSE Menu = Menu - 1
  ELSEIF Key$ = CHR$(0) + "M" THEN
   DisplayBackground
   CursorX = 1
    IF Menu = 3 THEN Menu = 1 ELSE Menu = Menu + 1
  ELSEIF Key$ = CHR$(27) THEN
   EXIT SUB
  ELSEIF Key$ = CHR$(13) THEN
    IF Menu = 1 THEN
     SaveSettings
     COLOR 7, 0: CLS : SYSTEM
    ELSEIF Menu = 2 THEN
     IF CursorX = 1 THEN AskPrgInfo -1
     IF CursorX = 2 THEN Remove PrgDir$(Page, Row), PrgCmd$(Page, Row), PrgName$(Page, Row), 0
     IF CursorX = 3 THEN SelectGroup
    ELSEIF Menu = 3 THEN
     DisplayHelp
    END IF
   LoadGrpsAndMenu
   DisplayBackground
  END IF
 LOOP
END SUB

SUB Remove (PrgDir$, PrgCmd$, PrgGrpName$, IsGroup)
COLOR 0, 7
DrawBox 5, 23, 32, 2, -1
 DO
  LOCATE 6, 35: PRINT "Remove:"
  LOCATE , 25: PRINT PrgGrpName$; " Y/N?"
  Key$ = UCASE$(INPUT$(1))
   IF Key$ = "Y" THEN
    DisplayWait
     IF IsGroup THEN
      OPEN MenuDir$ + "Groups.dat" FOR BINARY AS 1
      OPEN MenuDir$ + "Groups.tmp" FOR OUTPUT AS 2
       DO UNTIL LOC(1) = LOF(1)
        l = ASC(INPUT$(1, 1)): GrpFile$ = INPUT$(l, 1)
         IF NOT GrpFile$ = PrgGrpName$ THEN PRINT #2, CHR$(LEN(GrpFile$)); GrpFile$;
       LOOP
      CLOSE 1, 2
       IF ActvGrp$ = PrgGrpName$ THEN ActvGrp$ = "Main"
      KILL "Groups.dat": NAME "Groups.tmp" AS "Groups.dat"
     END IF
    AllRdyRmvd = 0
    OPEN MenuDir$ + "Menu3.dat" FOR BINARY AS 1
    OPEN MenuDir$ + "Menu3.tmp" FOR OUTPUT AS 2
     DO UNTIL LOC(1) = LOF(1)
      l = ASC(INPUT$(1, 1)): PrgDirFile$ = INPUT$(l, 1)
      l = ASC(INPUT$(1, 1)): PrgCmdFile$ = INPUT$(l, 1)
      l = ASC(INPUT$(1, 1)): PrgNameFile$ = INPUT$(l, 1)
      l = ASC(INPUT$(1, 1)): GrpFile$ = INPUT$(l, 1)
       IF NOT IsGroup THEN
        IF (PrgDirFile$ = PrgDir$ AND PrgCmdFile$ = PrgCmd$ AND PrgNameFile$ = PrgGrpName$) AND NOT AllRdyRmvd THEN
         AllRdyRmvd = -1
        ELSE
         PRINT #2, CHR$(LEN(PrgDirFile$)); PrgDirFile$;
         PRINT #2, CHR$(LEN(PrgCmdFile$)); PrgCmdFile$;
         PRINT #2, CHR$(LEN(PrgNameFile$)); PrgNameFile$;
         PRINT #2, CHR$(LEN(GrpFile$)); GrpFile$;
        END IF
       ELSEIF IsGroup THEN
        IF NOT GrpFile$ = PrgName$ THEN
         PRINT #2, CHR$(LEN(PrgDirFile$)); PrgDirFile$;
         PRINT #2, CHR$(LEN(PrgCmdFile$)); PrgCmdFile$;
         PRINT #2, CHR$(LEN(PrgNameFile$)); PrgNameFile$;
         PRINT #2, CHR$(LEN(GrpFile$)); GrpFile$;
        END IF
       END IF
     LOOP
    CLOSE 1, 2
    KILL "Menu3.dat": NAME "Menu3.tmp" AS "Menu3.dat"
    EXIT SUB
   ELSEIF Key$ = "N" THEN
    EXIT SUB
   END IF
 LOOP
END SUB

SUB SavePrgInfo (PrgDir$, PrgCmd$, PrgName$, IsNewPrg)
DisplayWait

 IF LEN(PrgDir$) > 3 AND RIGHT$(PrgDir$, 1) = "\" THEN
  PrgDir$ = LEFT$(PrgDir$, LEN(PrgDir$) - 1)
 END IF

 IF IsNewPrg THEN
  OPEN MenuDir$ + "Menu3.dat" FOR APPEND AS 1
   PRINT #1, CHR$(LEN(PrgCmd$)); PrgCmd$;
   PRINT #1, CHR$(LEN(PrgName$)); PrgName$;
   PRINT #1, CHR$(LEN(PrgDir$)); PrgDir$;
   PRINT #1, CHR$(LEN(ActvGrp$)); ActvGrp$;
  CLOSE 1
 ELSE
  FileRow = 1: FilePage = 1
  OPEN MenuDir$ + "Menu3.dat" FOR BINARY AS 1
  OPEN MenuDir$ + "Menu3.tmp" FOR OUTPUT AS 2
   DO UNTIL LOC(1) = LOF(1)
    l = ASC(INPUT$(1, 1)): FilePrgDir$ = INPUT$(l, 1)
    l = ASC(INPUT$(1, 1)): FilePrgCmd$ = INPUT$(l, 1)
    l = ASC(INPUT$(1, 1)): FilePrgName$ = INPUT$(l, 1)
    l = ASC(INPUT$(1, 1)): FileGrp$ = INPUT$(l, 1)
     IF FileGrp$ = ActvGrp$ THEN
      IF FileRow = 16 THEN FileRow = 1: FilePage = FilePage + 1 ELSE FileRow = FileRow + 1
     END IF
     IF FileRow = Row + 1 AND FilePage = Page THEN
      PRINT #2, CHR$(LEN(PrgCmd$)); PrgCmd$;
      PRINT #2, CHR$(LEN(PrgName$)); PrgName$;
      PRINT #2, CHR$(LEN(PrgDir$)); PrgDir$;
      PRINT #2, CHR$(LEN(ActvGrp$)); ActvGrp$;
     ELSE
      PRINT #2, CHR$(LEN(FilePrgDir$)); FilePrgDir$;
      PRINT #2, CHR$(LEN(FilePrgCmd$)); FilePrgCmd$;
      PRINT #2, CHR$(LEN(FilePrgName$)); FilePrgName$;
      PRINT #2, CHR$(LEN(FileGrp$)); FileGrp$;
     END IF
   LOOP
  CLOSE 1, 2
  KILL "Menu3.dat": NAME "Menu3.tmp" AS "Menu3.dat"
 END IF
END SUB

SUB SaveSettings
OPEN "ActveGrp.dat" FOR OUTPUT AS 1
 PRINT #1, ActvGrp$;
CLOSE 1

OPEN "Menu3.loc" FOR OUTPUT AS 1
 PRINT #1, MenuDir$;
CLOSE 1
END SUB

SUB SelectGroup
SelGrp = 1
COLOR 0, 7
DrawBox 5, 29, 20, 10, -1
 DO
  LOCATE 6
   FOR Grp = 1 TO 10
     IF Grp = SelGrp THEN COLOR 7, 0 ELSE COLOR 0, 7
    LOCATE , 30: PRINT " "; Group$(Grp); SPACE$(19 - LEN(Group$(Grp)))
   NEXT Grp
    DO
     Key$ = INKEY$
    LOOP WHILE Key$ = ""
   IF Key$ = CHR$(0) + "H" THEN
    IF SelGrp > 1 THEN SelGrp = SelGrp - 1
   ELSEIF Key$ = CHR$(0) + "P" THEN
    IF SelGrp < 10 THEN SelGrp = SelGrp + 1
   ELSEIF Key$ = CHR$(13) THEN
     IF NOT Group$(SelGrp) = "" THEN ActvGrp$ = Group$(SelGrp)
    EXIT SUB
   ELSEIF Key$ = CHR$(27) THEN
    EXIT SUB
   ELSEIF Key$ = CHR$(0) + "S" THEN
    PrgName$ = Group$(SelGrp)
    Remove "", "", PrgName$, -1
    LoadGrpsAndMenu
    EXIT SUB
   ELSEIF Key$ = CHR$(14) THEN
    MakeNewGroup
    EXIT SUB
   END IF
 LOOP
END SUB

