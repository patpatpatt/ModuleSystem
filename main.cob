       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULESYSTEM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FD-TEACHER ASSIGN TO 'C:\COBOL\BANK\TEACHER.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS F-USERNAME
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-STUDENT ASSIGN TO 'C:\COBOL\BANK\STUDENT.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS FD-STUDNUMBER
           FILE STATUS IS WS-FILESTATUS2.

       DATA DIVISION.
       FILE SECTION.
       FD  FD-TEACHER.
       01  F-TEACHERINFO.
           05 F-USERNAME PIC X(10).
           05 F-PASSWORD PIC X(10).
           05 F-TEACHERNAME PIC X(25).
           05 F-SECTION PIC 9(2).

       FD  FD-STUDENT.
       01  F-STUDENTINFO.
           05 FD-STUDNUMBER PIC 9(10).
           05 FD-STUDNAME PIC X(25).
           05 FD-STUDSECT PIC 9(2).
           05 FD-MODULENUMB PIC 9(5).
           05 FD-GRADE PIC 9(3).

       WORKING-STORAGE SECTION.
       01  WS-MENU        PIC A.
           88 A           VALUE 'A', 'a'.
           88 B           VALUE 'B', 'b'.
           88 C           VALUE 'C', 'c'.

       01  QUIT            PIC 9  VALUE 0.
       01  WS-BLANK        PIC X(25) VALUE SPACES.
       01  WS-FILESTATUS PIC 9(2).
       01  WS-FILESTATUS2 PIC 9(2).
       01  WS-FLAG PIC 9.
       01  WS-FLAG2 PIC 9.


       01  WS-ADMINUSERNAME PIC X(10).
       01  WS-ADMINPASSWORD PIC X(10).
       01  WS-PASSWORD-TEMP PIC X(10).

       01  WS-TEACHERINFO.
           05 WS-USERNAME PIC X(10).
           05 WS-PASSWORD PIC X(10).
           05 WS-TEACHERNAME PIC X(25).
           05 WS-SECTION PIC 9(2).

       01  WS-STUDINFO.
           05 WS-STUDNUMBER PIC 9(10).
           05 WS-STUDNAME PIC X(25).
           05 WS-STUDSECT PIC 9(2).
           05 WS-MODULENUMB PIC 9(5).
           05 WS-GRADE PIC 9(3).

       01  WS-MOD1 PIC 9.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM PARA-MENU WITH TEST BEFORE UNTIL QUIT = 1.
           STOP RUN.

       PARA-MENU.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*            MAIN MENU               *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   ADMIN LOGIN              *'.
           DISPLAY '*  => [B]   TEACHER LOGIN            *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
              GO TO PARA-ADMIN
           ELSE IF B
              GO TO PARA-TEACHER
           ELSE
               ADD 1 TO QUIT
           END-IF.


       PARA-ADMIN.
           INITIALIZE WS-ADMINUSERNAME, WS-ADMINPASSWORD.

           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*        ADMINISTRATOR LOGIN         *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  USERNAME: ' .
           ACCEPT WS-ADMINUSERNAME.
           DISPLAY '*  PASSWORD: ' .
           ACCEPT WS-ADMINPASSWORD.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.

           IF WS-ADMINUSERNAME="ADMIN" AND WS-ADMINPASSWORD="ADMIN"
               GO TO PARA-ADMIN-DASHBOARD
           ELSE
               GO TO PARA-ADMIN
           END-IF.

       PARA-ADMIN-DASHBOARD.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*      ADMINISTRATOR DASHBOARD       *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   CREATE TEACHER ACCOUNT   *'.
           DISPLAY '*  => [B]   EDIT TEACHER DATA        *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
              GO TO CREATE-TEACHER
           ELSE IF B
              GO TO EDIT-TEACHER
           ELSE
              GO TO PARA-MENU
           END-IF.


       CREATE-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY "USERNAME: "
           ACCEPT F-USERNAME.
           DISPLAY "PASSWORD: "
           ACCEPT F-PASSWORD.
           DISPLAY "FIRST & LAST NAME: "
           ACCEPT F-TEACHERNAME.
           DISPLAY "SECTION: "
           ACCEPT F-SECTION.

           OPEN OUTPUT FD-TEACHER
               WRITE F-TEACHERINFO
           CLOSE FD-TEACHER.

           DISPLAY "ACCOUNT CREATION SUCCESSFUL."
           GO TO PARA-ADMIN-DASHBOARD.

       EDIT-TEACHER.


       PARA-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*        TEACHER LOGIN PORTAL        *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  USERNAME: ' .
           ACCEPT F-USERNAME.
           DISPLAY '*  PASSWORD: ' .
           ACCEPT WS-PASSWORD-TEMP.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.

           OPEN INPUT FD-TEACHER
           IF WS-FILESTATUS NOT EQUAL TO 35
               READ FD-TEACHER INTO WS-TEACHERINFO
                   KEY IS F-USERNAME
                   NOT INVALID KEY MOVE 1 TO WS-FLAG
               END-READ
           ELSE
               DISPLAY "ACCOUNT DATABASE IS EMPTY."
           END-IF.

           IF WS-FLAG = 1
               IF WS-PASSWORD-TEMP = WS-PASSWORD
                   DISPLAY "LOGGED IN"
               ELSE
                   DISPLAY "ACCOUNT NOT FOUND"
               END-IF
           ELSE
               DISPLAY "ACCOUNT NOT FOUND"
           END-IF.

           CLOSE FD-TEACHER.


       MENU-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*          TEACHERS MENU             *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   INPUT STUDENT DATA       *'.
           DISPLAY '*  => [B]   SEARCH STUDENT           *'.
           DISPLAY '*  => [C]   STUDENT LIST             *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
              GO TO STUDENT-DATA
           ELSE IF B
              STOP RUN
           ELSE IF C
              STOP RUN
           ELSE
              GO TO PARA-MENU
           END-IF.

       STUDENT-DATA.
           DISPLAY "ENTER STUDENT NUMBER".
           ACCEPT WS-STUDNUMBER.
           DISPLAY "ENTER STUDENT NAME".
           ACCEPT WS-STUDNAME.
           DISPLAY "ENTER STUDENT SECTION".
           ACCEPT WS-STUDSECT.
           DISPLAY "ENTER MODULE NUMBER".
           ACCEPT WS-MODULENUMB.
           DISPLAY "ENTER MODULE GRADE".
           ACCEPT WS-GRADE.

           OPEN OUTPUT FD-STUDENT
               WRITE F-STUDENTINFO
           CLOSE FD-STUDENT.

           DISPLAY "STUDENT DATA HAS BEEN RECORDED".
           GO TO MENU-TEACHER.

           
