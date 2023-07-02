DEFINT A-Z

DECLARE FUNCTION Choice$ (Choices AS STRING)
DECLARE FUNCTION GetInput$ (x, y, Default AS STRING, MaximumLength, Wdth)
DECLARE FUNCTION GetProgramDirectory$ ()
DECLARE FUNCTION GroupExists (GroupName AS STRING)
DECLARE FUNCTION LoadCurrentGroup$ ()
DECLARE FUNCTION RequestNewGroupName$ (ProgramDirectory AS STRING)
DECLARE FUNCTION RequestProgramDirectory$ (ProgramDirectory AS STRING)
DECLARE FUNCTION SelectGroup$ (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB AddGroup (NewGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB Center (x, Text AS STRING)
DECLARE SUB ChangeDirectory (Directory AS STRING)
DECLARE SUB DisplayBackground (SelectedProgram, CurrentGroup AS STRING)
DECLARE SUB DisplayDateTime ()
DECLARE SUB DisplayHelp (ProgramDirectory AS STRING)
DECLARE SUB DisplayMenu (Menu, Selection, MenuY, MenuWidth(), MenuItemCount(), MenuText() AS STRING)
DECLARE SUB DisplayPrograms (SelectedProgram)
DECLARE SUB DisplaySelection (SelectedProgram)
DECLARE SUB DisplayWait ()
DECLARE SUB DrawBox (x, y, Wdth, Height, Shadow)
DECLARE SUB ExecuteProgram (MenuItemDirectory AS STRING, MenuItemCommand AS STRING, ProgramDirectory AS STRING)
DECLARE SUB GetPulldownMenus (MenuText() AS STRING, MenuWidth(), MenuItemCount())
DECLARE SUB Initialize ()
DECLARE SUB LoadGroups (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB LoadMenu (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB Main ()
DECLARE SUB PulldownMenus (SelectedProgram, CurrentGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB Quit ()
DECLARE SUB ReadMenuItem (FileH, MenuItemName AS STRING, MenuItemDirectory AS STRING, MenuItemCommand AS STRING, Group AS STRING)
DECLARE SUB RemoveMenuItem (MenuItemDirectory AS STRING, MenuItemCommand AS STRING, RemovedItemName AS STRING, CurrentGroup AS STRING, ProgramDirectory AS STRING, IsGroup)
DECLARE SUB RequestItemInformation (SelectedProgram, CurrentGroup AS STRING, ProgramDirectory AS STRING, IsNew)
DECLARE SUB SaveMenuItem (NewProgramName AS STRING, NewProgramDirectory AS STRING, NewProgramCommand AS STRING, CurrentGroup AS STRING, ProgramDirectory AS STRING, SelectedProgram, IsNew)
DECLARE SUB SaveSettings (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DECLARE SUB WriteMenuItem (FileH, MenuItemName AS STRING, MenuItemDirectory AS STRING, MenuItemCommand AS STRING, Group AS STRING)

CONST DEFAULTGROUP$ = "Main"
CONST FALSE = 0
CONST TRUE = -1

OPTION BASE 0
ON ERROR GOTO ErrorTrap
DIM SHARED Groups(0 TO 9) AS STRING
DIM SHARED MenuItemCommands(0 TO 299) AS STRING
DIM SHARED MenuItemDirectories(0 TO 299) AS STRING
DIM SHARED MenuItemNames(0 TO 299) AS STRING

CALL Main
Quit

ErrorTrap:
DIM ErrorCode AS INTEGER
DIM KeyStroke AS STRING
DIM Length AS LONG
DIM Message AS STRING

 ErrorCode = ERR
 PCOPY 0, 1
 SELECT CASE ErrorCode
  CASE 7, 14
   Message = "Not enough memory."
  CASE 52
   Message = "Could not open file."
  CASE 53
   Message = "Cannot find file."
  CASE 61
   Message = "The disk is full."
  CASE 62
   Message = "Error while reading file."
  CASE 71
   Message = "There is no disk in diskdrive."
  CASE 76
   Message = "Cannot find directory."
  CASE ELSE
   Message = "Unexpected error."
 END SELECT
 Length = LEN(Message)
 COLOR 0, 7
 DrawBox 8, 40 - ((Length / 2) + 2), CINT(Length) + 2, 2, FALSE
 Center 9, Message
 Center 0, "Error code:" + STR$(ErrorCode)
 COLOR 7, 0
 Center 12, " Escape = Quit  F3 = Ignore. "
 DO
  DO
   KeyStroke = INKEY$
   DisplayDateTime
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(0) + "="
    PCOPY 1, 0
    RESUME NEXT
   CASE CHR$(27)
    Quit
  END SELECT
 LOOP

MenuDimensions:
DATA 11, 2, 16, 4, 6, 1

MenuItems:
DATA "Directory", "Quit"
DATA "New Program","Change Program","Remove Program","Select Group"
DATA "Help"

SUB AddGroup (NewGroup AS STRING, ProgramDirectory AS STRING)
DIM FileH AS INTEGER

 IF NOT NewGroup = "" THEN
  IF NOT GroupExists(NewGroup) THEN
   FileH = FREEFILE
   OPEN ProgramDirectory + "Groups.dat" FOR APPEND LOCK READ WRITE AS FileH
    IF FREEFILE > FileH THEN
     PRINT #FileH, CHR$(LEN(NewGroup)); NewGroup;
     CLOSE FileH
    END IF
  END IF
 END IF
END SUB

SUB Center (x, Text AS STRING)
 IF x > 0 THEN LOCATE x
 LOCATE , 40 - (LEN(Text) / 2)
 PRINT Text
END SUB

SUB ChangeDirectory (Directory AS STRING)
DIM Position AS LONG

 Position = INSTR(Directory, ":")
 IF Position > 0 THEN SHELL LEFT$(Directory, Position)
 IF NOT Directory = "" THEN CHDIR Directory
END SUB

FUNCTION Choice$ (Choices AS STRING)
DIM KeyStroke AS STRING

 DO
  KeyStroke = INKEY$
  DisplayDateTime
 LOOP UNTIL INSTR(Choices, KeyStroke) > 0

 Choice$ = KeyStroke
END FUNCTION

SUB DisplayBackground (SelectedProgram, CurrentGroup AS STRING)
 COLOR 7, 0: DrawBox 1, 1, 78, 21, FALSE
 LOCATE 1, 1: PRINT "Ú"; STRING$(38 - (CINT(LEN(CurrentGroup) / 2)) - 1, "Ä"); "´";
 COLOR 0, 7: PRINT " "; CurrentGroup; " ";
 COLOR 7, 0: PRINT "Ã"; STRING$(38 - (LEN(CurrentGroup) / 2) - 1, "Ä"); "¿"
 LOCATE 2, 2: PRINT USING " F1 = Help   [###### bytes of memory available.] "; (FRE(-1) + FRE(-2) + FRE(""))
 LOCATE 20, 5: PRINT "ÛÛÛÛ²²²²²±±±±±°° Menu 3, By: Peter Swinkels, ***1996*** °°±±±±±²²²²²ÛÛÛÛ"
 LOCATE , 1: PRINT "Ã"; STRING$(78, "Ä"); "´"
 LOCATE , 5

 DisplaySelection SelectedProgram
 DisplayPrograms SelectedProgram
END SUB

SUB DisplayDateTime
 COLOR 7, 0
 LOCATE 22, 42
 PRINT " Date: "; DATE$; "    Time: "; TIME$;
END SUB

SUB DisplayHelp (ProgramDirectory AS STRING)
DIM FileH AS INTEGER
DIM HelpText AS STRING

 COLOR 0, 7
 DrawBox 4, 15, 50, 10, TRUE
 LOCATE 5
 FileH = FREEFILE
 OPEN ProgramDirectory + "Menu3.hlp" FOR INPUT LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   DO
    LINE INPUT #FileH, HelpText
    LOCATE , 16: PRINT HelpText
   LOOP UNTIL EOF(FileH)
   CLOSE FileH
  END IF
 LOCATE 17, 16
 COLOR 7, 0: PRINT "Press any key to exit help."
 DO: LOOP WHILE INKEY$ = ""
END SUB

SUB DisplayMenu (Menu, Selection, MenuY, MenuWidth(), MenuItemCount(), MenuText() AS STRING)
DIM MenuItem AS INTEGER

 COLOR 0, 7
 DrawBox 2, MenuY, MenuWidth(Menu), MenuItemCount(Menu), TRUE
 LOCATE 3
 FOR MenuItem = 0 TO MenuItemCount(Menu) - 1
  IF MenuItem = Selection THEN COLOR 7, 0 ELSE COLOR 0, 7
  LOCATE , MenuY + 1
  PRINT " "; MenuText(Menu, MenuItem); SPACE$(MenuWidth(Menu) - LEN(MenuText(Menu, MenuItem)) - 1)
 NEXT MenuItem
END SUB

SUB DisplayPrograms (SelectedProgram)
DIM Page AS INTEGER
DIM Program AS INTEGER

 Page = SelectedProgram \ 15

 COLOR 7, 0
 LOCATE 4
 FOR Program = Page * 15 TO (Page * 15) + 15
  LOCATE , 10
  PRINT " "; MenuItemNames(Program); SPACE$(21 - LEN(MenuItemNames(Program)))
 NEXT Program
END SUB

SUB DisplaySelection (SelectedProgram)
DIM Page AS INTEGER
DIM Row AS INTEGER

 Page = SelectedProgram \ 15
 Row = SelectedProgram - (Page * 15)

 LOCATE 22, 5
 COLOR 0, 7
 PRINT USING " Page: ### Row: ## "; Page + 1; Row + 1;
END SUB

SUB DisplayWait
 COLOR 0, 7
 DrawBox 7, 31, 16, 1, TRUE
 LOCATE 8, 33
 PRINT "Please wait";
 COLOR 16
 PRINT "...";
END SUB

SUB DrawBox (x, y, Wdth, Height, Shadow)
DIM Row AS INTEGER

 LOCATE x, y: PRINT "Ú"; STRING$(Wdth, "Ä"); "¿"
 FOR Row = 1 TO Height
  LOCATE , y: PRINT "³"; SPACE$(Wdth); "³";
  IF Shadow THEN PRINT "±±" ELSE PRINT
 NEXT Row
 LOCATE , y: PRINT "À"; STRING$(Wdth, "Ä"); "Ù";
 IF Shadow THEN PRINT "±±"
 IF Shadow THEN LOCATE , y + 2: PRINT STRING$(Wdth + 2, "±")
END SUB

SUB ExecuteProgram (MenuItemDirectory AS STRING, MenuItemCommand AS STRING, ProgramDirectory AS STRING)
DIM FileH AS INTEGER
DIM MenuItemExecutable AS STRING

 IF NOT MenuItemCommand = "" THEN
  ChangeDirectory MenuItemDirectory
  MenuItemExecutable = MenuItemCommand
  IF INSTR(MenuItemExecutable, " ") > 0 THEN
   MenuItemExecutable = LEFT$(MenuItemExecutable, INSTR(MenuItemExecutable, " ") - 1)
  END IF
  Initialize
  FileH = FREEFILE
  OPEN MenuItemDirectory + MenuItemExecutable FOR INPUT LOCK READ WRITE AS FileH
   IF FREEFILE > FileH THEN
    CLOSE FileH
    SHELL MenuItemCommand
   END IF
  Initialize
  ChangeDirectory ProgramDirectory
 END IF
END SUB

FUNCTION GetInput$ (x, y, Default AS STRING, MaximumLength, Wdth)
DIM KeyStroke AS STRING
DIM Text AS STRING

 Text = Default
 DO
  LOCATE x, y
  COLOR 0, 7: PRINT RIGHT$(Text, Wdth - 2);
  COLOR 16, 7: PRINT "_ ";
  DO
   KeyStroke = INKEY$
   DisplayDateTime
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(8)
    IF LEN(Text) > 0 THEN Text = LEFT$(Text, LEN(Text) - 1)
   CASE CHR$(13)
    EXIT DO
   CASE CHR$(27)
    Text = Default
    EXIT DO
   CASE IS >= " "
    IF LEN(Text) < MaximumLength THEN Text = Text + KeyStroke
  END SELECT
 LOOP

 GetInput$ = Text
END FUNCTION

FUNCTION GetProgramDirectory$
DIM Directory AS STRING
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Menu3.loc" FOR BINARY LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   IF LOF(FileH) > 0 THEN
    Directory = INPUT$(LOF(FileH), FileH)
   END IF
  END IF
 CLOSE FileH

 IF NOT Directory = "" AND NOT RIGHT$(Directory, 1) = "\" THEN
  Directory = Directory + "\"
 END IF

 GetProgramDirectory$ = Directory
END FUNCTION

SUB GetPulldownMenus (MenuText() AS STRING, MenuWidth(), MenuItemCount())
DIM Menu AS INTEGER
DIM MenuItem AS INTEGER

 RESTORE MenuDimensions
 FOR Menu = LBOUND(MenuText, 1) TO UBOUND(MenuText, 1)
  READ MenuWidth(Menu), MenuItemCount(Menu)
 NEXT Menu

 RESTORE MenuItems
 FOR Menu = LBOUND(MenuText, 1) TO UBOUND(MenuText, 1)
  FOR MenuItem = 0 TO MenuItemCount(Menu) - 1
   READ MenuText(Menu, MenuItem)
  NEXT MenuItem
 NEXT Menu
END SUB

FUNCTION GroupExists (GroupName AS STRING)
DIM Exists AS INTEGER
DIM Group AS INTEGER

 Exists = FALSE
 FOR Group = LBOUND(Groups) TO UBOUND(Groups)
  IF Groups(Group) = GroupName THEN
   Exists = TRUE
   EXIT FOR
  END IF
 NEXT Group

 GroupExists = Exists
END FUNCTION

SUB Initialize
 SCREEN 0
 WIDTH 40, 25
 WIDTH 80, 25
 PALETTE
 COLOR 7, 0
 CLS
END SUB

FUNCTION LoadCurrentGroup$
DIM FileH AS INTEGER
DIM NewCurrentGroup AS STRING

 FileH = FREEFILE
 OPEN "CrrntGrp.dat" FOR BINARY LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   NewCurrentGroup = INPUT$(LOF(FileH), FileH)
   CLOSE FileH
  END IF

 IF NewCurrentGroup = "" THEN
  NewCurrentGroup = DEFAULTGROUP$
 END IF

 LoadCurrentGroup$ = NewCurrentGroup
END FUNCTION

SUB LoadGroups (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DIM FileH AS INTEGER
DIM Group AS INTEGER

 DisplayWait

 ERASE Groups

 FileH = FREEFILE
 OPEN ProgramDirectory + "Groups.dat" FOR BINARY LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   IF LOF(FileH) = 0 THEN
    CLOSE FileH
    OPEN ProgramDirectory + "Groups.dat" FOR OUTPUT LOCK READ WRITE AS FileH
     PRINT #FileH, CHR$(LEN(DEFAULTGROUP$)); DEFAULTGROUP$;
     Groups(UBOUND(Groups)) = DEFAULTGROUP$
   ELSE
    FOR Group = LBOUND(Groups) TO UBOUND(Groups)
     Groups(Group) = INPUT$(ASC(INPUT$(1, FileH)), FileH)
     IF LOC(FileH) = LOF(FileH) THEN EXIT FOR
    NEXT Group
   END IF
   CLOSE FileH
  END IF
END SUB

SUB LoadMenu (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DIM FileGroup AS STRING
DIM FileH AS INTEGER
DIM FileItemDirectory AS STRING
DIM FileItemCommand AS STRING
DIM FileItemName AS STRING
DIM Program AS INTEGER

 ERASE MenuItemCommands
 ERASE MenuItemDirectories
 ERASE MenuItemNames

 Program = 0
 FileH = FREEFILE
 OPEN ProgramDirectory + "Menu3.dat" FOR BINARY LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   DO UNTIL LOC(FileH) = LOF(FileH) OR Program > UBOUND(MenuItemNames)
    ReadMenuItem FileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
    IF FileGroup = CurrentGroup THEN
     MenuItemNames(Program) = FileItemName
     MenuItemDirectories(Program) = FileItemDirectory
     MenuItemCommands(Program) = FileItemCommand
     Program = Program + 1
    END IF
   LOOP
  CLOSE FileH
 END IF
END SUB

SUB Main
DIM CurrentGroup AS STRING
DIM KeyStroke AS STRING
DIM Page AS INTEGER
DIM ProgramDirectory AS STRING
DIM Row AS INTEGER
DIM SelectedProgram AS INTEGER

 Initialize

 CurrentGroup = DEFAULTGROUP$
 Page = 0
 Row = 0
 SelectedProgram = (Page * 15) + Row
 DisplayBackground SelectedProgram, CurrentGroup
 
 ProgramDirectory = GetProgramDirectory$
 IF ProgramDirectory = "" THEN ProgramDirectory = RequestProgramDirectory$(ProgramDirectory)
 SaveSettings CurrentGroup, ProgramDirectory
 ChangeDirectory ProgramDirectory

 CurrentGroup = LoadCurrentGroup$

 DO
  LoadGroups CurrentGroup, ProgramDirectory
  LoadMenu CurrentGroup, ProgramDirectory
  DisplayBackground SelectedProgram, CurrentGroup

  DO
   SelectedProgram = (Page * 15) + Row
   DisplayPrograms SelectedProgram
   DisplaySelection SelectedProgram
   LOCATE Row + 4, 10
   COLOR 0, 7: PRINT " "; MenuItemNames(SelectedProgram); SPACE$(21 - LEN(MenuItemNames(SelectedProgram)))
   DO
    KeyStroke = INKEY$
    DisplayDateTime
   LOOP WHILE KeyStroke = ""
   COLOR 7, 0: LOCATE Row + 4, 10: PRINT " "; MenuItemNames(SelectedProgram); SPACE$(21 - LEN(MenuItemNames(SelectedProgram)))
   SELECT CASE KeyStroke
    CASE CHR$(0) + "H"
     IF Row > 0 THEN
      Row = Row - 1
     ELSE
      IF Page > 0 THEN Page = Page - 1: Row = 14
     END IF
    CASE CHR$(0) + "P"
     IF Row < 14 THEN
      Row = Row + 1
     ELSE
      IF Page < 19 THEN Page = Page + 1: Row = 0
     END IF
    CASE CHR$(0) + "I"
     IF Page > 0 THEN Page = Page - 1: Row = 14
    CASE CHR$(0) + "Q"
     IF Page < 19 THEN Page = Page + 1: Row = 0
    CASE CHR$(0) + ";"
     DisplayHelp ProgramDirectory
     EXIT DO
    CASE CHR$(0) + "<"
     ProgramDirectory = RequestProgramDirectory$(ProgramDirectory)
     SaveSettings CurrentGroup, ProgramDirectory
     EXIT DO
    CASE CHR$(7)
     CurrentGroup = SelectGroup$(CurrentGroup, ProgramDirectory)
     EXIT DO
    CASE CHR$(14)
     RequestItemInformation SelectedProgram, CurrentGroup, ProgramDirectory, TRUE
     EXIT DO
    CASE CHR$(17)
     SaveSettings CurrentGroup, ProgramDirectory
     Quit
    CASE CHR$(27)
     PulldownMenus SelectedProgram, CurrentGroup, ProgramDirectory
     EXIT DO
    CASE ELSE
     IF NOT MenuItemNames(SelectedProgram) = "" THEN
      SELECT CASE KeyStroke
       CASE CHR$(0) + "S"
        RemoveMenuItem MenuItemDirectories(SelectedProgram), MenuItemCommands(SelectedProgram), MenuItemNames(SelectedProgram), CurrentGroup, ProgramDirectory, FALSE
        EXIT DO
       CASE CHR$(9)
        RequestItemInformation SelectedProgram, CurrentGroup, ProgramDirectory, FALSE
        EXIT DO
       CASE CHR$(13)
        ExecuteProgram MenuItemDirectories(SelectedProgram), MenuItemCommands(SelectedProgram), ProgramDirectory
        EXIT DO
      END SELECT
     END IF
   END SELECT
  LOOP
 LOOP
END SUB

SUB PulldownMenus (SelectedProgram, CurrentGroup AS STRING, ProgramDirectory AS STRING)
DIM KeyStroke AS STRING
DIM Menu AS INTEGER
DIM MenuItemCount(0 TO 2) AS INTEGER
DIM MenuText(0 TO 2, 0 TO 3) AS STRING
DIM MenuWidth(0 TO 2) AS INTEGER
DIM MenuY AS INTEGER
DIM Selection AS INTEGER

 GetPulldownMenus MenuText(), MenuWidth(), MenuItemCount()

 Selection = 0
 Menu = LBOUND(MenuText, 1)
 DO
  MenuY = (Menu * 10) + 3
  LOCATE 1, 1
  COLOR 0, 7: PRINT "   Program   Menu      Help"; SPACE$(53);
  LOCATE 1, MenuY
  COLOR 16, 7: PRINT "[": LOCATE 1, MenuY + 8: PRINT "]"
  DisplayMenu Menu, Selection, MenuY, MenuWidth(), MenuItemCount(), MenuText()
  DO
   KeyStroke = INKEY$
   DisplayDateTime
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Selection = 0 THEN Selection = MenuItemCount(Menu) - 1 ELSE Selection = Selection - 1
   CASE CHR$(0) + "P"
    IF Selection = MenuItemCount(Menu) - 1 THEN Selection = 0 ELSE Selection = Selection + 1
   CASE CHR$(0) + "K"
    DisplayBackground SelectedProgram, CurrentGroup
    Selection = 0
    IF Menu = LBOUND(MenuText, 1) THEN Menu = UBOUND(MenuText, 1) ELSE Menu = Menu - 1
   CASE CHR$(0) + "M"
    DisplayBackground SelectedProgram, CurrentGroup
    Selection = 0
    IF Menu = UBOUND(MenuText, 1) THEN Menu = LBOUND(MenuText, 1) ELSE Menu = Menu + 1
   CASE CHR$(13)
    SELECT CASE Menu
     CASE 0
      SELECT CASE Selection
       CASE 0
        ProgramDirectory = RequestProgramDirectory$(ProgramDirectory)
        SaveSettings CurrentGroup, ProgramDirectory
        ChangeDirectory ProgramDirectory
       CASE 1
        SaveSettings CurrentGroup, ProgramDirectory
        Quit
      END SELECT
     CASE 1
      SELECT CASE Selection
       CASE 0
        RequestItemInformation SelectedProgram, CurrentGroup, ProgramDirectory, TRUE
       CASE 1
        IF NOT MenuItemNames(SelectedProgram) = "" THEN
         RequestItemInformation SelectedProgram, CurrentGroup, ProgramDirectory, FALSE
        END IF
       CASE 2
        RemoveMenuItem MenuItemDirectories(SelectedProgram), MenuItemCommands(SelectedProgram), MenuItemNames(SelectedProgram), CurrentGroup, ProgramDirectory, FALSE
       CASE 3
        CurrentGroup = SelectGroup$(CurrentGroup, ProgramDirectory)
      END SELECT
     CASE 2
      SELECT CASE Selection
       CASE 0
        DisplayHelp ProgramDirectory
      END SELECT
    END SELECT

    LoadGroups CurrentGroup, ProgramDirectory
    LoadMenu CurrentGroup, ProgramDirectory
   
    DisplayBackground SelectedProgram, CurrentGroup
   CASE CHR$(27)
    EXIT DO
  END SELECT
 LOOP
END SUB

SUB Quit
 Initialize
 SYSTEM
END SUB

SUB ReadMenuItem (FileH, MenuItemName AS STRING, MenuItemDirectory AS STRING, MenuItemCommand AS STRING, Group AS STRING)
 MenuItemName = INPUT$(ASC(INPUT$(1, FileH)), FileH)
 MenuItemDirectory = INPUT$(ASC(INPUT$(1, FileH)), FileH)
 MenuItemCommand = INPUT$(ASC(INPUT$(1, FileH)), FileH)
 Group = INPUT$(ASC(INPUT$(1, FileH)), FileH)
END SUB

SUB RemoveMenuItem (MenuItemDirectory AS STRING, MenuItemCommand AS STRING, RemovedItemName AS STRING, CurrentGroup AS STRING, ProgramDirectory AS STRING, IsGroup)
DIM FileItemCommand AS STRING
DIM FileItemDirectory AS STRING
DIM FileGroup AS STRING
DIM FileItemName AS STRING
DIM InFileH AS INTEGER
DIM OutFileH AS INTEGER
DIM ProgramRemoved AS INTEGER

 IF NOT RemovedItemName = "" THEN
  COLOR 0, 7
  DrawBox 5, 23, 32, 2, TRUE
  DO
   LOCATE 6, 35
   COLOR 0, 7: PRINT "Remove:"
   LOCATE , 25: PRINT CHR$(34); RemovedItemName; CHR$(34); " Y/N?"
   SELECT CASE UCASE$(Choice$("YNyn"))
    CASE "N"
     EXIT DO
    CASE "Y"
     DisplayWait
     IF IsGroup THEN
      InFileH = FREEFILE
      OPEN ProgramDirectory + "Groups.dat" FOR BINARY LOCK READ WRITE AS InFileH
       IF FREEFILE > InFileH THEN
        OutFileH = FREEFILE
         OPEN ProgramDirectory + "Groups.tmp" FOR OUTPUT LOCK READ WRITE AS OutFileH
          DO UNTIL LOC(InFileH) = LOF(InFileH)
          FileGroup = INPUT$(ASC(INPUT$(1, InFileH)), InFileH)
          IF NOT FileGroup = RemovedItemName THEN
           PRINT #OutFileH, CHR$(LEN(FileGroup)); FileGroup;
          END IF
         LOOP
        CLOSE OutFileH
       CLOSE InFileH
       IF CurrentGroup = RemovedItemName THEN CurrentGroup = DEFAULTGROUP$
       KILL ProgramDirectory + "Groups.dat"
       NAME ProgramDirectory + "Groups.tmp" AS ProgramDirectory + "Groups.dat"
      END IF
     END IF

     ProgramRemoved = FALSE
     InFileH = FREEFILE
     OPEN ProgramDirectory + "Menu3.dat" FOR BINARY LOCK READ WRITE AS InFileH
      IF FREEFILE > InFileH THEN
       OutFileH = FREEFILE
       OPEN ProgramDirectory + "Menu3.tmp" FOR OUTPUT LOCK READ WRITE AS OutFileH
        DO UNTIL LOC(InFileH) = LOF(InFileH)
         ReadMenuItem InFileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
         IF IsGroup THEN
          IF NOT FileGroup = RemovedItemName THEN
           WriteMenuItem OutFileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
          END IF
         ELSEIF NOT IsGroup THEN
          IF (FileItemDirectory = MenuItemDirectory AND FileItemCommand = MenuItemCommand AND FileItemName = RemovedItemName) AND NOT ProgramRemoved THEN
           ProgramRemoved = TRUE
          ELSE
           WriteMenuItem OutFileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
          END IF
         END IF
        LOOP
       CLOSE OutFileH
      CLOSE InFileH
      KILL ProgramDirectory + "Menu3.dat"
      NAME ProgramDirectory + "Menu3.tmp" AS ProgramDirectory + "Menu3.dat"
      EXIT DO
     END IF
   END SELECT
  LOOP
 END IF
END SUB

SUB RequestItemInformation (SelectedProgram, CurrentGroup AS STRING, ProgramDirectory AS STRING, IsNew)
DIM DataItem AS INTEGER
DIM KeyStroke AS STRING
DIM Labels(0 TO 2) AS STRING
DIM MaximumLength AS INTEGER
DIM MenuItemData(0 TO 2) AS STRING
DIM Selection AS INTEGER

 IF NOT IsNew THEN
  MenuItemData(0) = MenuItemNames(SelectedProgram)
  MenuItemData(1) = MenuItemDirectories(SelectedProgram)
  MenuItemData(2) = MenuItemCommands(SelectedProgram)
 END IF

 Labels(0) = "Name"
 Labels(1) = "Directory"
 Labels(2) = "Command"

 Selection = 0
 COLOR 0, 7: DrawBox 5, 19, 40, 3, TRUE
 DO
  LOCATE 6
  FOR DataItem = LBOUND(MenuItemData) TO UBOUND(MenuItemData)
   IF DataItem = Selection THEN COLOR 7, 0 ELSE COLOR 0, 7
   LOCATE , 20: PRINT SPACE$(10 - LEN(Labels(DataItem))); Labels(DataItem); ": "; RIGHT$(MenuItemData(DataItem), 25);
   IF DataItem = Selection THEN
    COLOR 23
    PRINT "_ "; SPACE$(26 - LEN(RIGHT$(MenuItemData(DataItem), 26)))
   ELSE
    PRINT SPACE$(28 - LEN(RIGHT$(MenuItemData(DataItem), 27)))
   END IF
  NEXT DataItem

  DO
   KeyStroke = INKEY$
   DisplayDateTime
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Selection = 0 THEN Selection = 2 ELSE Selection = Selection - 1
   CASE CHR$(0) + "P"
    IF Selection = 2 THEN Selection = 0 ELSE Selection = Selection + 1
   CASE CHR$(8)
    IF LEN(MenuItemData(Selection)) > 0 THEN MenuItemData(Selection) = LEFT$(MenuItemData(Selection), LEN(MenuItemData(Selection)) - 1)
   CASE CHR$(13)
    IF NOT (MenuItemData(0) = "" OR MenuItemData(1) = "" OR MenuItemData(2) = "") THEN
     SaveMenuItem MenuItemData(0), MenuItemData(1), MenuItemData(2), CurrentGroup, ProgramDirectory, SelectedProgram, IsNew
    END IF
    EXIT DO
   CASE CHR$(27)
    EXIT DO
   CASE IS >= " "
    SELECT CASE Selecion
     CASE 0
      MaximumLength = 20
     CASE 1, 2
      MaximumLength = 255
    END SELECT

    IF LEN(MenuItemData(Selection)) < MaximumLength THEN MenuItemData(Selection) = MenuItemData(Selection) + KeyStroke
  END SELECT
 LOOP
END SUB

FUNCTION RequestNewGroupName$ (ProgramDirectory AS STRING)
DIM KeyStroke AS STRING
DIM SelectedGroup AS STRING

 PCOPY 0, 2
 COLOR 0, 7
 DrawBox 5, 23, 32, 1, TRUE
 COLOR 0, 7
 LOCATE 6, 25: PRINT "New group: ";
 SelectedGroup = GetInput$(6, 36, "", 18, 20)
 IF NOT SelectedGroup = "" THEN AddGroup SelectedGroup, ProgramDirectory
 PCOPY 2, 0

 RequestNewGroupName$ = SelectedGroup
END FUNCTION

FUNCTION RequestProgramDirectory$ (ProgramDirectory AS STRING)
DIM KeyStroke AS STRING
DIM NewDirectory AS STRING

 COLOR 0, 7
 NewDirectory = ProgramDirectory
 DrawBox 4, 18, 42, 2, TRUE
 LOCATE 5, 20
 COLOR 0, 7: PRINT "Where is Menu 3 located?"
 LOCATE , 20: PRINT "> ";
 NewDirectory = GetInput(6, 22, NewDirectory, 255, 39)
 IF NOT NewDirectory = "" AND NOT RIGHT$(NewDirectory, 1) = "\" THEN NewDirectory = NewDirectory + "\"

 RequestProgramDirectory$ = NewDirectory
END FUNCTION

SUB SaveMenuItem (NewProgramName AS STRING, NewProgramDirectory AS STRING, NewProgramCommand AS STRING, CurrentGroup AS STRING, ProgramDirectory AS STRING, SelectedProgram, IsNew)
DIM FileGroup AS STRING
DIM FileH AS INTEGER
DIM FileIndex AS INTEGER
DIM FileItemCommand AS STRING
DIM FileItemDirectory AS STRING
DIM FileItemName AS STRING
DIM InFileH AS INTEGER
DIM OutFileH AS INTEGER

 DisplayWait

 IF NOT RIGHT$(NewProgramDirectory, 1) = "\" THEN
  NewProgramDirectory = NewProgramDirectory + "\"
 END IF

 IF IsNew THEN
  FileH = FREEFILE
  OPEN ProgramDirectory + "Menu3.dat" FOR APPEND LOCK READ WRITE AS FileH
   IF FREEFILE > FileH THEN
    WriteMenuItem FileH, NewProgramName, NewProgramDirectory, NewProgramCommand, CurrentGroup
    CLOSE FileH
   END IF
 ELSEIF NOT IsNew THEN
  FileIndex = 0
  InFileH = FREEFILE
  OPEN ProgramDirectory + "Menu3.dat" FOR BINARY LOCK READ WRITE AS InFileH
   IF FREEFILE > InFileH THEN
    OutFileH = FREEFILE
    OPEN ProgramDirectory + "Menu3.tmp" FOR OUTPUT LOCK READ WRITE AS OutFileH
     DO UNTIL LOC(InFileH) = LOF(InFileH)
      ReadMenuItem InFileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
      IF FileIndex = SelectedProgram THEN
       WriteMenuItem OutFileH, NewProgramName, NewProgramDirectory, NewProgramCommand, CurrentGroup
      ELSE
       WriteMenuItem OutFileH, FileItemName, FileItemDirectory, FileItemCommand, FileGroup
      END IF
      IF FileGroup = CurrentGroup THEN FileIndex = FileIndex + 1
     LOOP
    CLOSE OutFileH
   CLOSE InFileH
   KILL ProgramDirectory + "Menu3.dat"
   NAME ProgramDirectory + "Menu3.tmp" AS ProgramDirectory + "Menu3.dat"
  END IF
 END IF
END SUB

SUB SaveSettings (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "CrrntGrp.dat" FOR OUTPUT LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   PRINT #FileH, CurrentGroup;
   CLOSE FileH
  END IF

 FileH = FREEFILE
 OPEN "Menu3.loc" FOR OUTPUT LOCK READ WRITE AS FileH
  IF FREEFILE > FileH THEN
   PRINT #FileH, ProgramDirectory;
   CLOSE FileH
  END IF
END SUB

FUNCTION SelectGroup$ (CurrentGroup AS STRING, ProgramDirectory AS STRING)
DIM Group AS INTEGER
DIM KeyStroke AS STRING
DIM ProgramName AS STRING
DIM SelectedGroup AS STRING
DIM Selection AS INTEGER

 COLOR 0, 7
 DrawBox 5, 29, 20, 10, TRUE
 Selection = LBOUND(Groups)
 DO
  LOCATE 6
  FOR Group = LBOUND(Groups) TO UBOUND(Groups)
   IF Group = Selection THEN COLOR 7, 0 ELSE COLOR 0, 7
   LOCATE , 30: PRINT " "; Groups(Group); SPACE$(19 - LEN(Groups(Group)))
  NEXT Group
  DO
   KeyStroke = INKEY$
   DisplayDateTime
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF Selection > LBOUND(Groups) THEN Selection = Selection - 1
   CASE CHR$(0) + "P"
    IF Selection < UBOUND(Groups) THEN Selection = Selection + 1
   CASE CHR$(14)
    SelectedGroup = RequestNewGroupName(ProgramDirectory)
    IF NOT SelectedGroup = "" THEN
     CurrentGroup = SelectedGroup
     EXIT DO
    END IF
   CASE CHR$(27)
    SelectedGroup = CurrentGroup
    EXIT DO
   CASE ELSE
    IF NOT Groups(Selection) = "" THEN
     SELECT CASE KeyStroke
      CASE CHR$(0) + "S"
       ProgramName = Groups(Selection)
       RemoveMenuItem "", "", ProgramName, CurrentGroup, ProgramDirectory, TRUE
       SelectedGroup = CurrentGroup
       LoadGroups CurrentGroup, ProgramDirectory
       LoadMenu CurrentGroup, ProgramDirectory
       EXIT DO
      CASE CHR$(13)
       IF NOT Groups(Selection) = "" THEN SelectedGroup = Groups(Selection)
       EXIT DO
     END SELECT
    END IF
  END SELECT
 LOOP

 SelectGroup$ = SelectedGroup
END FUNCTION

SUB WriteMenuItem (FileH, MenuItemName AS STRING, MenuItemDirectory AS STRING, MenuItemCommand AS STRING, Group AS STRING)
 PRINT #FileH, CHR$(LEN(MenuItemName)); MenuItemName;
 PRINT #FileH, CHR$(LEN(MenuItemDirectory)); MenuItemDirectory;
 PRINT #FileH, CHR$(LEN(MenuItemCommand)); MenuItemCommand;
 PRINT #FileH, CHR$(LEN(Group)); Group;
END SUB

