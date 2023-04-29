DEFINT A-Z
OPTION BASE 0

TYPE CardFileStr
 CardCount AS INTEGER
 CurrentCard AS INTEGER
 CurrentRow AS INTEGER
END TYPE

DECLARE FUNCTION FileExists (FileName AS STRING)
DECLARE FUNCTION GetChoice$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Choices AS STRING, AllowStatus AS INTEGER)
DECLARE FUNCTION GetInput$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Default AS STRING, MaximumLength AS INTEGER, IsFileDialog AS INTEGER, IsNumeric AS INTEGER, AllowStatus AS INTEGER)
DECLARE FUNCTION GetState$ (StateOn AS INTEGER)
DECLARE FUNCTION LoadFile$ (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE FUNCTION LoadHelpTopic (HelpText() AS STRING, TopicNumber AS INTEGER)
DECLARE FUNCTION MenuBar$ ()
DECLARE FUNCTION RemoveInvalidCharacters$ (Text AS STRING)
DECLARE FUNCTION SaveFileAs$ (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB AddNameToList (FileName AS STRING)
DECLARE SUB Center (x AS INTEGER, Text AS STRING)
DECLARE SUB DisplayCards (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB DisplayFileList ()
DECLARE SUB DisplayHelpMenu ()
DECLARE SUB DisplayHelpTopic (TopicNumber AS INTEGER)
DECLARE SUB DisplayInformation ()
DECLARE SUB DisplayPullDownMenus (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB DisplayStatus ()
DECLARE SUB DisplayStatusPane ()
DECLARE SUB DisplayTitle ()
DECLARE SUB DrawBox (x AS INTEGER, y AS INTEGER, BoxWidth AS INTEGER, BoxHeight AS INTEGER, Shadow AS INTEGER)
DECLARE SUB Initialize ()
DECLARE SUB InitializeNameList ()
DECLARE SUB InitializePullDownMenuItems (MenuItems() AS STRING)
DECLARE SUB Main ()
DECLARE SUB NewCardFile (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB PrintCards (CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB ProcessChoice (SelectedMenu AS INTEGER, SelectedMenuItem AS INTEGER, MenuItems() AS STRING, CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB Quit ()
DECLARE SUB RemoveFile ()
DECLARE SUB SaveFile (FileName AS STRING, CardCaptions() AS STRING, CardTexts() AS STRING)
DECLARE SUB SetCardCaption (Card AS INTEGER, CardCaptions() AS STRING, CardTexts() AS STRING)
ON ERROR GOTO ErrorTrap

Initialize
DisplayTitle
CALL Main

PullDownMenuData:
DATA 13, 6, 12, 2, 17, 3, 14, 2

ErrorTrap:
DIM ErrorCode AS INTEGER
DIM KeyStroke AS STRING
DIM Row AS INTEGER

 DO
  ErrorCode = ERR
  COLOR 7, 0
  LOCATE 4
  FOR Row = 1 TO 6
   Center 0, SPACE$(26)
  NEXT Row
  COLOR 0, 7
  LOCATE 5
  SELECT CASE ErrorCode
   CASE 7, 14
    Center 0, " Not enough memory. "
    Center 0, " Error: 1           "
   CASE 52, 64
    Center 0, " Bad file name. "
    Center 0, " Error: 2       "
   CASE 53
    Center 0, " Cannot find file. "
    Center 0, " Error: 3          "
   CASE 61
    Center 0, " The disk is full. "
    Center 0, " Error: 4          "
   CASE 62
    Center 0, " Error while   "
    Center 0, " reading file. "
    Center 0, " Error: 5      "
   CASE 70
    Center 0, " Disk/file is     "
    Center 0, " write protected. "
    Center 0, " Error: 6         "
   CASE 71
    Center 0, " No disk in diskdrive. "
    Center 0, " Error: 7              "
   CASE 72
    Center 0, " The disk is damaged. "
    Center 0, " Error: 8             "
   CASE 76
    Center 0, " Cannot find "
    Center 0, " directory.  "
    Center 0, " Error: 9    "
   CASE ELSE
    Center 0, " Unexpected error. "
    Center 0, " Error: 0       "
  END SELECT
  PRINT
  COLOR 7, 0
  Center 0, " Enter = Retry  Spacebar = Ignore  Escape = Quit "
  Center 0, " F1 = Help "
  DO
   DO
    KeyStroke = INKEY$
   LOOP WHILE KeyStroke = ""
   SELECT CASE KeyStroke
    CASE CHR$(0) + ";"
     DisplayHelpTopic 2
     EXIT DO
    CASE CHR$(13)
     RESUME
    CASE CHR$(27)
     Quit
    CASE " "
     RESUME NEXT
   END SELECT
  LOOP
 LOOP

SUB AddNameToList (FileName AS STRING)
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Names.lst" FOR APPEND LOCK READ WRITE AS FileH
  PRINT #FileH, UCASE$(FileName); ","; TIME$; ","; DATE$
 CLOSE FileH
END SUB

SUB Center (x AS INTEGER, Text AS STRING)
 IF x > 0 THEN LOCATE x
 LOCATE , INT(40 - (LEN(Text) / 2))
 PRINT Text
END SUB

SUB DisplayCards (CardCaptions() AS STRING, CardTexts() AS STRING)
DIM Card AS INTEGER
DIM CardX AS INTEGER
DIM CardY AS INTEGER
DIM Row AS INTEGER
DIM Text AS STRING
SHARED CurrentCardFile AS CardFileStr

 CardX = 5
 CardY = 30
 IF CurrentCardFile.CurrentCard + 1 > CurrentCardFile.CardCount - 5 THEN
  CardX = CardX + ((5 - (CurrentCardFile.CardCount - CurrentCardFile.CurrentCard)) * 2)
  CardY = CardY - ((5 - (CurrentCardFile.CardCount - CurrentCardFile.CurrentCard)) * 2)
 END IF

 COLOR 7, 0
 LOCATE 3, 1
 PRINT SPACE$(1400);

 COLOR 0, 7
 FOR Card = 4 TO 0 STEP -1
  IF CurrentCardFile.CurrentCard + Card < CurrentCardFile.CardCount THEN
   LOCATE CardX - 2, CardY
   PRINT "Ú"; STRING$(17, "Ä"); "¿"
   LOCATE CardX - 1, CardY
   PRINT USING "³ \             \ ³"; CardCaptions(CurrentCardFile.CurrentCard + Card);
   DrawBox CardX, CardY, 25, 5, 0
   LOCATE CardX, CardY
   PRINT "Ã"; STRING$(17, "Ä"); "Á"
   CardX = CardX + 2
   CardY = CardY - 2
  END IF
 NEXT Card

 FOR Row = LBOUND(CardTexts, 2) TO UBOUND(CardTexts, 2)
  LOCATE Row + 14, 24
  PRINT CardTexts(CurrentCardFile.CurrentCard, Row);
 NEXT Row
END SUB

SUB DisplayFileList
DIM FileH AS INTEGER
DIM KeyStroke AS STRING
DIM ListedFile AS STRING
DIM ListFile AS INTEGER
DIM SaveDate AS STRING
DIM SaveTime AS STRING

 COLOR 7, 0
 CLS
 COLOR 0, 7
 Center 1, " File List "
 PRINT " Name:        Time:        Date: "; SPACE$(46);
 COLOR 7, 0
 LOCATE 22, 1
 PRINT STRING$(80, "Ä");
 
 FileH = FREEFILE
 OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS FileH
  IF EOF(FileH) THEN
   COLOR 7, 0
   LOCATE 2
   PRINT "There are no saved files.";
   KeyStroke = INPUT$(1)
  ELSE
   DO UNTIL EOF(FileH) OR KeyStroke = CHR$(27)
    COLOR 7, 0
    LOCATE 3
    FOR ListFile = 0 TO 18
     IF EOF(FileH) THEN EXIT FOR
     INPUT #FileH, ListedFile, SaveTime, SaveDate
     LOCATE , 1
     PRINT USING " \          \ \          \ \          \ "; ListedFile; SaveTime$; SaveDate$
    NEXT ListFile
    COLOR 0, 7
    LOCATE 23, 5
    PRINT " Press any key to continue or the escape key to exit... "
    KeyStroke = INPUT$(1)
   LOOP
  END IF
 CLOSE FileH
 
 COLOR 7, 0
 CLS
END SUB

SUB DisplayHelpMenu
DIM Cancel AS INTEGER
DIM KeyStroke AS STRING
DIM OptionIndex AS INTEGER
DIM Options(0 TO 3) AS STRING
DIM SelectedOption AS INTEGER

 Options(0) = "Keys"
 Options(1) = "Pull down menus"
 Options(2) = "Error messages"
 Options(3) = "Back"

 Cancel = 0
 DO UNTIL Cancel
  SelectedOption = 0
  COLOR 7, 0
  CLS
  COLOR 0, 7
  DrawBox 1, 1, 78, 21, 0: Center 1, "´ Help Ã"
  DO
   FOR OptionIndex = LBOUND(Options) TO UBOUND(Options)
    IF OptionIndex = SelectedOption THEN
     COLOR 7, 0
    ELSE
     COLOR 0, 7
    END IF
    LOCATE OptionIndex + 6, 10
    PRINT USING " \             \ "; Options(OptionIndex);
   NEXT OptionIndex
   DO
    KeyStroke = INKEY$
   LOOP WHILE KeyStroke = ""
   SELECT CASE KeyStroke
    CASE CHR$(0) + "H"
     IF SelectedOption = 0 THEN
      SelectedOption = 3
     ELSE
      SelectedOption = SelectedOption - 1
     END IF
    CASE CHR$(0) + "P"
     IF SelectedOption = 3 THEN
      SelectedOption = 0
     ELSE
      SelectedOption = SelectedOption + 1
     END IF
    CASE CHR$(13)
     IF SelectedOption = 3 THEN
      Cancel = -1
     ELSE
      DisplayHelpTopic SelectedOption
     END IF

     EXIT DO
   END SELECT
  LOOP
 LOOP

 COLOR 7, 0
 CLS
END SUB

SUB DisplayHelpTopic (TopicNumber AS INTEGER)
DIM HelpCurrentRow AS INTEGER
DIM HelpRowCount AS INTEGER
DIM HelpText(0 TO 49) AS STRING
DIM HelpTextLine AS STRING
DIM KeyStroke AS STRING
DIM Row AS INTEGER

 HelpRowCount = LoadHelpTopic(HelpText(), TopicNumber)

 COLOR 7, 0
 CLS
 COLOR 0, 7
 DrawBox 1, 1, 78, 21, 0
 Center 1, "´ Help Ã"

 HelpCurrentRow = 0
 LOCATE 21, 1
 PRINT "Ã"; STRING$(78, "Ä"); "´";
 LOCATE 22, 21
 PRINT "/ = Scroll   Control + P = Print Topic"
 DO
  LOCATE 2
  FOR Row = HelpCurrentRow TO HelpCurrentRow + 18
   IF Row > UBOUND(HelpText) THEN EXIT FOR

   SELECT CASE LEFT$(HelpText(Row), 1)
    CASE "*"
     COLOR 7, 0
    CASE "-"
     COLOR 0, 7
   END SELECT

   LOCATE , 2
   PRINT MID$(HelpText(Row), 2); SPACE$(70 - LEN(HelpText(Row)) - 1)
  NEXT Row
  COLOR 7, 0
  LOCATE 22, 3
  PRINT USING " Row: ### "; HelpCurrentRow + 1;
  COLOR 0, 7
  DO
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF HelpCurrentRow > LBOUND(HelpText) THEN HelpCurrentRow = HelpCurrentRow - 1
   CASE CHR$(0) + "P"
    IF HelpCurrentRow < UBOUND(HelpText) THEN HelpCurrentRow = HelpCurrentRow + 1
   CASE CHR$(16)
    FOR Row = 0 TO HelpRowCount - 1
     LPRINT HelpText(Row)
    NEXT Row
   CASE CHR$(27)
    COLOR , 0
    CLS
    EXIT DO
  END SELECT
 LOOP
END SUB

SUB DisplayInformation
DIM KeyStroke AS STRING

 COLOR 0, 7
 DrawBox 7, 27, 24, 3, -1
 Center 7, "´ Information  Ã"
 Center 0, "Cardfile v1.01"
 Center 0, " By: Peter Swinkels"
 Center 0, " ***1996*** "
 KeyStroke = INPUT$(1)
END SUB

SUB DisplayPullDownMenus (CardCaptions() AS STRING, CardTexts() AS STRING)
DIM KeyStroke AS STRING
DIM Menu AS INTEGER
DIM MenuHeights(0 TO 3)  AS INTEGER
DIM MenuItem AS INTEGER
DIM MenuItems(0 TO 3, 0 TO 5) AS STRING
DIM MenuWidths(0 TO 3)  AS INTEGER
DIM MenuY AS INTEGER
DIM SelectedMenu AS INTEGER
DIM SelectedMenuItem AS INTEGER

 SelectedMenuItem = 0
 SelectedMenu = 0

 RESTORE PullDownMenuData
 FOR Menu = LBOUND(MenuItems, 1) TO UBOUND(MenuItems, 1)
  READ MenuWidths(Menu), MenuHeights(Menu)
 NEXT Menu

 InitializePullDownMenuItems MenuItems()

 DO
  MenuY = (SelectedMenu * 10) + 1
  COLOR 0, 7
  LOCATE 1, 1
  PRINT MenuBar$;
  DrawBox 2, MenuY, MenuWidths(SelectedMenu), MenuHeights(SelectedMenu), -1
  COLOR 7, 0
  LOCATE 1, MenuY
  PRINT MID$(MenuBar$, MenuY, 10);
  FOR MenuItem = 0 TO MenuHeights(SelectedMenu) - 1
   IF MenuItem = SelectedMenuItem THEN
    COLOR 7, 0
   ELSE
    COLOR 0, 7
   END IF
   Menu = LEN(MenuItems(SelectedMenu, MenuItem)) + 1
   LOCATE MenuItem + 3, MenuY + 1
   PRINT " "; MenuItems(SelectedMenu, MenuItem); SPACE$(MenuWidths(SelectedMenu) - Menu)
  NEXT MenuItem
  DO
   DisplayStatus
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  COLOR 7, 0
  SELECT CASE KeyStroke
   CASE CHR$(0) + "H"
    IF SelectedMenuItem = 0 THEN
     SelectedMenuItem = MenuHeights(SelectedMenu) - 1
    ELSE
     SelectedMenuItem = SelectedMenuItem - 1
    END IF
   CASE CHR$(0) + "P"
    IF SelectedMenuItem = MenuHeights(SelectedMenu) - 1 THEN
     SelectedMenuItem = 0
    ELSE
     SelectedMenuItem = SelectedMenuItem + 1
    END IF
   CASE CHR$(0) + "K"
    LOCATE 2, 1
    PRINT SPACE$(720);
    SelectedMenuItem = 0
    DisplayCards CardCaptions(), CardTexts()
    IF SelectedMenu = 0 THEN
     SelectedMenu = 3
    ELSE
     SelectedMenu = SelectedMenu - 1
    END IF
   CASE CHR$(0) + "M"
    LOCATE 2, 1
    PRINT SPACE$(720);
    SelectedMenuItem = 0
    DisplayCards CardCaptions(), CardTexts()
    IF SelectedMenu = 3 THEN
     SelectedMenu = 0
    ELSE
     SelectedMenu = SelectedMenu + 1
    END IF
   CASE CHR$(13)
    ProcessChoice SelectedMenu, SelectedMenuItem, MenuItems(), CardCaptions(), CardTexts()
   CASE CHR$(27)
    EXIT DO
  END SELECT
 LOOP
END SUB

SUB DisplayStatus
SHARED CurrentCardFile AS CardFileStr
SHARED CurrentFileName AS STRING
SHARED DisplayDate AS INTEGER
SHARED DisplayTime AS INTEGER

 COLOR 7, 0
 LOCATE 22, 3
 PRINT USING " Card: ###/###"; CurrentCardFile.CurrentCard + 1; CurrentCardFile.CardCount;

 IF DisplayTime THEN
  LOCATE 23, 3
  PRINT USING " Time: \            \ "; TIME$;
 END IF

 IF DisplayDate THEN
  LOCATE 23, 25
  PRINT USING " Date: \            \ "; DATE$;
 END IF

 LOCATE 23, 58
 PRINT " File Name: ";
 IF CurrentFileName = "" THEN
  PRINT "None.";
 ELSE
  PRINT USING "\        \"; CurrentFileName;
 END IF
END SUB

SUB DisplayStatusPane
 COLOR 7, 0
 LOCATE 21, 1
 PRINT STRING$(80, "Ä");

 DisplayStatus
END SUB

SUB DisplayTitle
DIM FileH AS INTEGER
DIM KeyStroke AS STRING
DIM Row AS STRING

 FileH = FREEFILE
 OPEN "Title.dat" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   LINE INPUT #FileH, Row
   Center 0, Row
  LOOP
 CLOSE FileH
 COLOR 0, 7
 Center 12, " Cardfile v1.01, By: Peter Swinkels, ***1996*** "
 Center 14, " Press any key to continue. "

 KeyStroke = INPUT$(1)
END SUB

SUB DrawBox (x AS INTEGER, y AS INTEGER, BoxWidth AS INTEGER, BoxHeight AS INTEGER, Shadow AS INTEGER)
DIM BoxRow AS INTEGER

 LOCATE x, y
 PRINT "Ú"; STRING$(BoxWidth, "Ä"); "¿"

 FOR BoxRow = 1 TO BoxHeight
  LOCATE , y
  PRINT "³"; SPACE$(BoxWidth); "³";
  IF Shadow THEN PRINT "°°" ELSE PRINT
 NEXT BoxRow

 LOCATE , y
 PRINT "À"; STRING$(BoxWidth, "Ä"); "Ù";

 IF Shadow THEN
  PRINT "°°"
  LOCATE , y + 2
  PRINT STRING$(BoxWidth + 2, "°");
 END IF
END SUB

FUNCTION FileExists (FileName AS STRING)
DIM Exists AS INTEGER
DIM FileH AS INTEGER
DIM ListedFile AS STRING
DIM SaveDate AS STRING
DIM SaveTime AS STRING

 Exists = 0
 FileH = FREEFILE
 OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH) OR Exists
   INPUT #FileH, ListedFile, SaveTime, SaveDate
   Exists = (UCASE$(FileName) = ListedFile)
  LOOP
 CLOSE FileH

 FileExists = Exists
END FUNCTION

FUNCTION GetChoice$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Choices AS STRING, AllowStatus AS INTEGER)
DIM Choice AS STRING

 COLOR 7, 0
 LOCATE x, y
 PRINT Prompt;

 DO
  DO
   IF AllowStatus THEN DisplayStatus
   Choice = UCASE$(INKEY$)
  LOOP WHILE Choice = ""
 LOOP UNTIL INSTR(Choices, Choice) > 0

 LOCATE x, y
 PRINT SPACE$(LEN(Prompt))

 GetChoice$ = Choice
END FUNCTION

FUNCTION GetInput$ (x AS INTEGER, y AS INTEGER, Prompt AS STRING, Default AS STRING, MaximumLength AS INTEGER, IsFileDialog AS INTEGER, IsNumeric AS INTEGER, AllowStatus AS INTEGER)
DIM Filter AS STRING
DIM KeyStroke AS STRING
DIM Length AS INTEGER
DIM Text AS STRING

 Filter = ""
 Text = Default
 IF IsFileDialog THEN
  Filter = CHR$(34) + " *+,./<=>?[\]"
 END IF

 DO
  COLOR 7, 0
  IF IsFileDialog THEN
   LOCATE 3, 6
   PRINT "F1 = File List"
  END IF

  LOCATE x, y
  PRINT Prompt; Text;
  COLOR 23
  PRINT "_ "
  DO
   IF AllowStatus THEN DisplayStatus
   KeyStroke = INKEY$
  LOOP WHILE KeyStroke = ""
  Length = LEN(Text)
  SELECT CASE KeyStroke
   CASE CHR$(8)
    IF Length > 0 THEN Text = LEFT$(Text, Length - 1)
   CASE CHR$(13)
    EXIT DO
   CASE CHR$(27)
    Text = ""
    EXIT DO
   CASE CHR$(0) + ";"
    IF IsFileDialog THEN DisplayFileList
   CASE IS >= " "
    IF Length < MaximumLength THEN
     IF IsFileDialog THEN
      IF INSTR(Filter, KeyStroke) = 0 THEN
       Text = Text + KeyStroke
      END IF
     ELSEIF IsNumeric THEN
      IF INSTR("0123456789", KeyStroke) > 0 THEN
       Text = Text + KeyStroke
      END IF
     ELSE
      Text = Text + KeyStroke
     END IF
    END IF
  END SELECT
 LOOP

 LOCATE x, y
 PRINT SPACE$(LEN(Prompt) + Length + 2);

 GetInput$ = Text
END FUNCTION

FUNCTION GetState$ (StateOn AS INTEGER)
DIM State AS STRING
 
 IF StateOn THEN
  State = "On"
 ELSE
  State = "Off"
 END IF

 GetState$ = State
END FUNCTION

SUB Initialize
SHARED DisplayDate AS INTEGER
SHARED DisplayTime AS INTEGER

 SCREEN 0
 PALETTE
 WIDTH 80, 25
 COLOR 7, 0
 CLS

 DisplayDate = -1
 DisplayTime = -1

 InitializeNameList
END SUB

SUB InitializeNameList
DIM FileH AS INTEGER

 FileH = FREEFILE
 OPEN "Names.lst" FOR BINARY LOCK READ WRITE AS FileH
 CLOSE FileH
END SUB

SUB InitializePullDownMenuItems (MenuItems() AS STRING)
SHARED DisplayDate AS INTEGER
SHARED DisplayTime AS INTEGER

 MenuItems(0, 0) = "New File"
 MenuItems(0, 1) = "Load File"
 MenuItems(0, 2) = "Save File"
 MenuItems(0, 3) = "Remove File"
 MenuItems(0, 4) = "Print Cards"
 MenuItems(0, 5) = "Quit"
 MenuItems(1, 0) = "Time = " + GetState$(DisplayTime)
 MenuItems(1, 1) = "Date = " + GetState$(DisplayDate)
 MenuItems(2, 0) = "Number of Cards"
 MenuItems(2, 1) = "Erase Card Text"
 MenuItems(2, 2) = "Card Caption"
 MenuItems(3, 0) = "Display Help"
 MenuItems(3, 1) = "Information"
END SUB

FUNCTION LoadFile$ (CardCaptions() AS STRING, CardTexts() AS STRING)
DIM Card AS INTEGER
DIM FileH AS INTEGER
DIM Length AS INTEGER
DIM NewFileName AS STRING
DIM Row AS INTEGER
SHARED CurrentCardFile AS CardFileStr

 COLOR 7, 0
 CLS
 NewFileName = UCASE$(GetInput$(4, 6, "Load file: ", "", 8, -1, 0, 0))

 IF NOT NewFileName = "" THEN
  CurrentCardFile.CardCount = 10
  CurrentCardFile.CurrentCard = 0
  CurrentCardFile.CurrentRow = 0

  NewCardFile CardCaptions(), CardTexts()

  FileH = FREEFILE
  OPEN NewFileName + ".crd" FOR INPUT LOCK READ WRITE AS FileH
  CLOSE FileH

  OPEN NewFileName + ".crd" FOR BINARY LOCK READ WRITE AS FileH
   FOR Card = LBOUND(CardTexts, 1) TO UBOUND(CardTexts, 1)
    IF LOC(FileH) = LOF(FileH) THEN EXIT FOR

    Length = ASC(INPUT$(1, FileH))
    CardCaptions(Card) = INPUT$(Length, FileH)
    IF LEN(CardCaptions(Card)) > 15 THEN
     CardCaptions(Card) = LEFT$(CardCaptions(Card), 15)
    END IF
    CardCaptions(Card) = RemoveInvalidCharacters$(CardCaptions(Card))

    FOR Row = LBOUND(CardTexts, 2) TO UBOUND(CardTexts, 2)
     IF LOC(FileH) = LOF(FileH) THEN EXIT FOR

     Length = ASC(INPUT$(1, FileH))
     CardTexts(Card, Row) = INPUT$(Length, FileH)
     IF LEN(CardTexts(Card, Row)) > 22 THEN
      CardTexts(Card, Row) = LEFT$(CardTexts(Card, Row), 22)
     END IF
     CardTexts(Card, Row) = RemoveInvalidCharacters$(CardTexts(Card, Row))
    NEXT Row
   NEXT Card
  CLOSE FileH

  CurrentCardFile.CardCount = Card - 1
 END IF

 LoadFile$ = NewFileName
END FUNCTION

FUNCTION LoadHelpTopic (HelpText() AS STRING, TopicNumber AS INTEGER)
DIM ControlCharacter AS STRING
DIM FileH AS INTEGER
DIM HelpRowCount AS INTEGER
DIM HelpTextLine AS STRING

CLS ''

 FileH = FREEFILE
 OPEN "CardFile.hlp" FOR INPUT LOCK READ WRITE AS FileH
  DO UNTIL EOF(FileH)
   LINE INPUT #FileH, HelpTextLine
   ControlCharacter = LEFT$(HelpTextLine, 1)
   IF VAL(ControlCharacter) = TopicNumber THEN
    HelpRowCount = 0
    DO UNTIL EOF(FileH)
     LINE INPUT #FileH, HelpTextLine
     ControlCharacter = LEFT$(HelpTextLine, 1)
     IF ControlCharacter = "x" THEN EXIT DO
     HelpText(HelpRowCount) = HelpTextLine
     HelpRowCount = HelpRowCount + 1
    LOOP
    IF ControlCharacter = "x" THEN EXIT DO
   END IF
  LOOP
 CLOSE FileH

 LoadHelpTopic = HelpRowCount
END FUNCTION

SUB Main
DIM CardCaptions(0 TO 99) AS STRING
DIM CardTexts(0 TO 99, 0 TO 4) AS STRING
DIM ClipboardData AS STRING
DIM KeyStroke AS STRING
DIM Length AS INTEGER
DIM NewCardCaption AS STRING
DIM NewFileName AS STRING
SHARED CurrentCardFile AS CardFileStr
SHARED CurrentFileName AS STRING

 ClipboardData = ""
 NewCardFile CardCaptions(), CardTexts()
 
 DO
  COLOR 7, 0
  CLS
  LOCATE 21, 1
  PRINT STRING$(80, "Ä");
  DisplayCards CardCaptions(), CardTexts()
  COLOR 0, 7
  LOCATE 1, 1
  PRINT MenuBar$;
  COLOR 7, 0

  DO
   COLOR 0, 7
   LOCATE CurrentCardFile.CurrentRow + 14, 24
   PRINT CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow);
   COLOR 16
   PRINT "_ ";
   DO
    DisplayStatus
    KeyStroke = INKEY$
   LOOP WHILE KeyStroke = ""
   COLOR 0, 7
   LOCATE CurrentCardFile.CurrentRow + 14, 24 + Length
   PRINT " ";
   SELECT CASE KeyStroke
    CASE CHR$(0) + "H"
     IF CurrentCardFile.CurrentRow > 0 THEN CurrentCardFile.CurrentRow = CurrentCardFile.CurrentRow - 1
    CASE CHR$(0) + "P"
     IF CurrentCardFile.CurrentRow < 4 THEN CurrentCardFile.CurrentRow = CurrentCardFile.CurrentRow + 1
    CASE CHR$(0) + "K"
     IF CurrentCardFile.CurrentCard > 0 THEN
      CurrentCardFile.CurrentCard = CurrentCardFile.CurrentCard - 1
      CurrentCardFile.CurrentRow = 0
     END IF
     DisplayCards CardCaptions(), CardTexts()
    CASE CHR$(0) + "M"
     IF CurrentCardFile.CurrentCard < CurrentCardFile.CardCount - 1 THEN
      CurrentCardFile.CurrentCard = CurrentCardFile.CurrentCard + 1
      CurrentCardFile.CurrentRow = 0
     END IF
     DisplayCards CardCaptions(), CardTexts()
    CASE CHR$(0) + "R"
     IF CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) = "" THEN
      CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) = ClipboardData
     END IF
    CASE CHR$(0) + "S"
     COLOR 0, 7
     LOCATE CurrentCardFile.CurrentRow + 14, 24
     PRINT SPACE$(22);
     CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) = ""
    CASE CHR$(3)
     IF Length > 0 THEN
      ClipboardData = CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow)
     END IF
    CASE CHR$(8)
     IF Length > 0 THEN
      CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) = LEFT$(CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow), Length - 1)
     END IF
    CASE CHR$(13)
     IF CurrentCardFile.CurrentRow < 5 THEN CurrentCardFile.CurrentRow = CurrentCardFile.CurrentRow + 1
    CASE CHR$(14)
     SetCardCaption CurrentCardFile.CurrentCard, CardCaptions(), CardTexts()
    CASE CHR$(19)
     IF CurrentFileName = "" THEN
      NewFileName = SaveFileAs$(CardCaptions(), CardTexts())
      IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
      EXIT DO
     ELSE
      SaveFile CurrentFileName, CardCaptions(), CardTexts()
     END IF
    CASE CHR$(27)
     DisplayPullDownMenus CardCaptions(), CardTexts()
     EXIT DO
    CASE IS >= " "
     IF Length < 22 THEN
      CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) = CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow) + KeyStroke
     END IF
   END SELECT
   Length = LEN(CardTexts(CurrentCardFile.CurrentCard, CurrentCardFile.CurrentRow))
  LOOP
 LOOP
END SUB

FUNCTION MenuBar$
 MenuBar$ = " File      Options   Card      Help     " + SPACE$(40)
END FUNCTION

SUB NewCardFile (CardCaptions() AS STRING, CardTexts() AS STRING)
SHARED CurrentCardFile AS CardFileStr
SHARED CurrentFileName AS STRING

 CurrentFileName = ""

 CurrentCardFile.CardCount = 10
 CurrentCardFile.CurrentCard = 0
 CurrentCardFile.CurrentRow = 0

 REDIM CardCaptions(0 TO 99) AS STRING
 REDIM CardTexts(0 TO 99, 0 TO 4) AS STRING
END SUB

SUB PrintCards (CardCaptions() AS STRING, CardTexts() AS STRING)
DIM Card AS INTEGER
DIM PrintCount AS INTEGER
DIM Row AS INTEGER
SHARED CurrentCardFile AS CardFileStr

 PrintCount = VAL(GetInput$(20, 4, "How many cards do you want to print? ", RTRIM$(STR$(CurrentCardFile.CardCount)), 3, 0, -1, -1))
 IF PrintCount <= CurrentCardFile.CardCount THEN
  FOR Card = 0 TO PrintCount - 1
   LPRINT "Ú"; STRING$(17, "Ä"); "¿"
   LPRINT USING "³ \             \ ³"; CardCaptions(Card)
   LPRINT "Ã"; STRING$(17, "Ä"); "ÁÄÄÄÄÄÄÄ¿"
   FOR Row = LBOUND(CardTexts, 2) TO UBOUND(CardTexts, 2)
    LPRINT USING "³ \                     \ ³"; CardTexts(Card, Row)
   NEXT Row
   LPRINT "À"; STRING$(25, "Ä"); "Ù"
  NEXT Card
 END IF
END SUB

SUB ProcessChoice (SelectedMenu AS INTEGER, SelectedMenuItem AS INTEGER, MenuItems() AS STRING, CardCaptions() AS STRING, CardTexts() AS STRING)
''Too much stuff in here??

DIM Card AS INTEGER
DIM Choice AS STRING
DIM NewCount AS INTEGER
DIM NewFileName AS STRING
DIM Row AS INTEGER
DIM UserInput AS STRING
SHARED CurrentCardFile AS CardFileStr
SHARED CurrentFileName AS STRING
SHARED DisplayDate AS INTEGER
SHARED DisplayTime AS INTEGER

 SELECT CASE SelectedMenu
  CASE 0
   SELECT CASE SelectedMenuItem
    CASE 0
     COLOR 7, 0
     Choice = GetChoice$(20, 4, "Start with a new cardfile Y/N?", "YN", -1)
     IF Choice = "Y" THEN
      NewCardFile CardCaptions(), CardTexts()
      DisplayCards CardCaptions(), CardTexts()
     END IF
    CASE 1
     NewFileName = LoadFile$(CardCaptions(), CardTexts())
     IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
     COLOR 7, 0
     CLS
     DisplayCards CardCaptions(), CardTexts()
     DisplayStatusPane
    CASE 2
     NewFileName = SaveFileAs$(CardCaptions(), CardTexts())
     IF NOT NewFileName = "" THEN CurrentFileName = NewFileName
     COLOR 7, 0
     CLS
     DisplayCards CardCaptions(), CardTexts()
     DisplayStatusPane
    CASE 3
     RemoveFile
     COLOR 7, 0
     CLS
     DisplayCards CardCaptions(), CardTexts()
     DisplayStatusPane
    CASE 4
     PrintCards CardCaptions(), CardTexts()
    CASE 5
     Choice = GetChoice$(20, 4, "Quit Y/N?", "YN", -1)
     IF Choice = "Y" THEN Quit
   END SELECT
  CASE 1
   SELECT CASE SelectedMenuItem
    CASE 0
     IF DisplayTime THEN DisplayTime = 0 ELSE DisplayTime = -1
     MenuItems(1, 0) = "Time = " + GetState$(DisplayTime)
    CASE 1
     IF DisplayDate THEN DisplayDate = 0 ELSE DisplayDate = -1
     MenuItems(1, 1) = "Date = " + GetState$(DisplayDate)
   END SELECT
  CASE 2
   SELECT CASE SelectedMenuItem
    CASE 0
     UserInput = GetInput$(20, 4, "Number of cards (10-100): ", "10", 3, 0, -1, -1)
     IF NOT UserInput = "" THEN NewCount = VAL(UserInput)
     IF NewCount >= 10 AND NewCount <= 100 THEN
      CurrentCardFile.CardCount = NewCount
     END IF
    CASE 1
     UserInput = GetInput$(20, 4, "Erase card number: ", RTRIM$(STR$(CurrentCardFile.CurrentCard + 1)), 3, 0, -1, -1)
     Card = VAL(UserInput) - 1
     IF Card >= 0 AND Card <= 99 THEN
      Choice = GetChoice$(20, 4, "Erase card number" + STR$(Card + 1) + " Y/N? ", "YN", -1)
      IF Choice = "Y" THEN
       CardCaptions(Card) = ""
       FOR Row = LBOUND(CardTexts, 2) TO UBOUND(CardTexts, 2)
        CardTexts(Card, Row) = ""
       NEXT Row
       DisplayCards CardCaptions(), CardTexts()
      END IF
     END IF
    CASE 2
     SetCardCaption CurrentCardFile.CurrentCard, CardCaptions(), CardTexts()
   END SELECT
  CASE 3
   SELECT CASE SelectedMenuItem
    CASE 0
     DisplayHelpMenu
     DisplayCards CardCaptions(), CardTexts()
     DisplayStatusPane
     COLOR 7, 0
    CASE 1
     DisplayInformation
     DisplayCards CardCaptions(), CardTexts()
   END SELECT
 END SELECT
END SUB

SUB Quit
 SCREEN 0
 PALETTE
 WIDTH 80, 25
 COLOR 7, 0
 CLS
 SYSTEM
END SUB

SUB RemoveFile
DIM Choice AS STRING
DIM FileToDelete AS STRING
DIM ListedDate AS STRING
DIM ListedFile AS STRING
DIM ListedTime AS STRING
DIM SourceFileH AS INTEGER
DIM TargetFileH AS INTEGER

 COLOR 7, 0
 CLS

 FileToDelete = UCASE$(GetInput$(4, 6, "Remove file: ", "", 8, -1, 0, 0))
 IF NOT FileToDelete = "" THEN
  Choice = GetChoice$(4, 5, "Remove: " + FileToDelete + " Y/N? ", "YN", 0)
  IF Choice = "Y" THEN
   SourceFileH = FREEFILE
   OPEN "Names.lst" FOR INPUT LOCK READ WRITE AS SourceFileH
    TargetFileH = FREEFILE
    OPEN "Names.tmp" FOR OUTPUT LOCK READ WRITE AS TargetFileH
     DO UNTIL EOF(SourceFileH)
      INPUT #SourceFileH, ListedFile, ListedTime, ListedDate
      IF NOT UCASE$(FileToDelete) = ListedFile THEN
       PRINT #TargetFileH, ListedFile; ","; ListedTime; ","; ListedDate; ",";
      END IF
     LOOP
    CLOSE TargetFileH
   CLOSE SourceFileH

   KILL "Names.lst"
   NAME "Names.tmp" AS "Names.lst"
   KILL FileToDelete + ".crd"
  END IF
 END IF
END SUB

FUNCTION RemoveInvalidCharacters$ (Text AS STRING)
DIM Character AS STRING
DIM NewText AS STRING
DIM Position AS INTEGER

 NewText = ""
 FOR Position = 1 TO LEN(Text)
  Character = MID$(Text, Position, 1)
  IF Character >= " " THEN
   NewText = NewText + Character
  END IF
 NEXT Position

 RemoveInvalidCharacters$ = NewText
END FUNCTION

SUB SaveFile (FileName AS STRING, CardCaptions() AS STRING, CardTexts() AS STRING)
DIM Card AS INTEGER
DIM FileH AS INTEGER
DIM Row AS INTEGER
SHARED CurrentCardFile AS CardFileStr

 FileH = FREEFILE
 OPEN FileName + ".crd" FOR OUTPUT LOCK READ WRITE AS FileH
  FOR Card = 0 TO CurrentCardFile.CardCount - 1
   PRINT #FileH, CHR$(LEN(CardCaptions(Card))); CardCaptions(Card);
   FOR Row = LBOUND(CardTexts, 2) TO UBOUND(CardTexts, 2)
    PRINT #FileH, CHR$(LEN(CardTexts(Card, Row))); CardTexts(Card, Row);
   NEXT Row
  NEXT Card
 CLOSE FileH
END SUB

FUNCTION SaveFileAs$ (CardCaptions() AS STRING, CardTexts() AS STRING)
DIM Choice AS STRING
DIM NewFileName AS STRING
SHARED CurrentFileName AS STRING

 COLOR 7, 0
 CLS

 NewFileName = UCASE$(GetInput$(4, 6, "Save file as: ", CurrentFileName, 8, -1, 0, 0))
 IF NOT NewFileName = "" THEN
  IF FileExists(NewFileName) THEN
   Choice = GetChoice$(4, 5, NewFileName + " already exists, overwrite Y/N? ", "YN", 0)
   IF Choice = "Y" THEN
    SaveFile NewFileName, CardCaptions(), CardTexts()
   END IF
  ELSE
   AddNameToList NewFileName
   SaveFile NewFileName, CardCaptions(), CardTexts()
  END IF
 END IF

 SaveFileAs$ = NewFileName
END FUNCTION

SUB SetCardCaption (Card AS INTEGER, CardCaptions() AS STRING, CardTexts() AS STRING)
DIM NewCardCaption AS STRING

 NewCardCaption = GetInput$(20, 6, "Card caption: ", CardCaptions(Card), 15, 0, 0, -1)
 IF NOT NewCardCaption = "" THEN
  CardCaptions(Card) = NewCardCaption
  DisplayCards CardCaptions(), CardTexts()
 END IF
END SUB

