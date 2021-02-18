       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULESYSTEM.
       AUTHOR. GROUP1.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01  WS-MENU        PIC A.
           88 A           VALUE 'A', 'a'.
           88 B           VALUE 'B', 'b'.
       01 QUIT            PIC 9  VALUE 0.
       01 WS-BLANK        PIC X(25) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM PARA-MENU WITH TEST BEFORE UNTIL QUIT = 1.
           STOP RUN.
       PARA-MENU.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*   COBOL BANK TRANSACTION SYSTEM    *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   ADMIN LOGIN              *'.
           DISPLAY '*  => [B]   TEACHER LOGIN            *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' WITH NO ADVANCING.  
           ACCEPT WS-MENU.
           IF A
              
           ELSE IF B
               
           ELSE 
               ADD 1 TO QUIT
           END-IF.
