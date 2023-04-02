' Компилятор графического редактора "Совок"
' (C) 1997 Anton Mamichev // Lotos Software
' All rights reserved.

DECLARE FUNCTION mousex! ()
DECLARE FUNCTION mousey! ()
DECLARE FUNCTION mouseb! ()
        SCREEN 9
        COLOR 1, 63
        TYPE wfile
                fil AS SINGLE
        END TYPE
        DIM disk AS wfile
        DIM savbar(5000), savbar1(1000), pu(numd, 2), pun$(15), pc(5000)
        DIM tcurs(40), mode$(19), cursor(40)
        foncol = 63
        colf = 7
        colt = 4
again: 
        GOSUB file
        GOTO again
'-=2=----------------------------------------------------------------
'--------------------------------------------------------------------
file:
        pun$(1) = "Загрузить файл        "
        pun$(2) = "Записать файл         "
        pun$(3) = "Добавить файл         "
        pun$(4) = "Сделать BASIC-блок    "
        pun$(5) = "Временный выход в DOS "
        pun$(6) = "Выход                 "
       
        uplx = 10
        uply = 4
        ml = 22
        num = 6
        GOSUB menu
        IF stroka = 6 OR stroka > 30 THEN END
        IF stroka = 5 THEN
                SCREEN 0
                CLS
                PRINT "Наберите EXIT для возврата..."
                SHELL
                END IF

        ix = 2: iy = 2: il = 20
       
        IF stroka = 2 THEN
                ext$ = "svk"
                IF nam$ <> "" THEN GOSUB extens
                GOSUB inname
                IF eror = 1 THEN RETURN
                ext$ = "svk"
                GOSUB extens
                OPEN nam$ FOR RANDOM AS #1 LEN = LEN(disk)
                disk.fil = current
                PUT #1, 1, disk.fil
                FOR hhh = 1 TO current
                disk.fil = pc(hhh)
                PUT #1, hhh + 1, disk.fil
                NEXT hhh
                CLOSE #1
                RETURN
END IF
        IF stroka = 1 THEN
                ext$ = "svk"
                IF nam$ <> "" THEN GOSUB extens
                GOSUB inname
                IF eror = 1 THEN GOTO ex1
                ext$ = "svk"
                GOSUB extens
                OPEN nam$ FOR RANDOM AS #1 LEN = LEN(disk)
                GET #1, 1, disk.fil
                current = disk.fil
                FOR hhh = 1 TO current
                GET #1, hhh + 1, disk.fil
                pc(hhh) = disk.fil
                NEXT hhh
                CLOSE #1
ex1:            RETURN
        END IF
        IF stroka = 3 THEN
                ext$ = "svk"
                IF nam$ <> "" THEN GOSUB extens
                GOSUB inname
                IF eror THEN GOTO ex2
                ext$ = "svk"
                GOSUB extens
                OPEN nam$ FOR RANDOM AS #1 LEN = LEN(disk)
                GET #1, 1, disk.fil
                de = current
                current = current + disk.fil - 1
                FOR hhh = 1 TO current
                GET #1, hhh + 1, disk.fil
                pc(hhh + de - 1) = disk.fil
                NEXT hhh
                CLOSE #1
ex2:                RETURN
        END IF
        IF stroka = 4 THEN
                ext$ = "bas"
                IF nam$ <> "" THEN GOSUB extens
        GOSUB inname
        IF eror THEN GOTO ex3
                ext$ = "bas"
                GOSUB extens
        OPEN nam$ FOR OUTPUT AS #1
        PRINT #1, "' SOVOK files viewer by DIGGER from Lotos Software"
        PRINT #1, "SCREEN 9"
        PRINT #1, "COLOR 1, "; foncol; ": CLS"
        PRINT #1, "LINE (0, 0)-(640, 350),"; clcol; ", BF"
        PRINT #1, "RESTORE "; nam$; "1"
        PRINT #1, "x0 = 0 : y0 = 0 'Координата в знакоместах"
        PRINT #1, "x0 = x0 * 8 : y0 = y0 * 14"
        PRINT #1, nam$; ":READ value"
        PRINT #1, "   IF value<>65535 THEN "
        PRINT #1, "      IF value = 1000 THEN READ x, y, c: PSET (x+x0, y+y0), c"
        PRINT #1, "      IF value = 2000 THEN READ x, y, x1, y1, c: LINE (x+x0, y+y0)-(x1+x0, y1+y0), c"
        PRINT #1, "      IF value = 3000 THEN READ x, y, r, c: CIRCLE (x+x0, y+y0), r, c"
        PRINT #1, "      IF value = 4000 THEN READ x, y, c, f: PAINT (x+x0, y+y0), c, f"
        PRINT #1, "      IF value = 5000 THEN READ x, y, r, c: CIRCLE (x+x0, y+y0), r: PAINT (x+x0, y+y0), c, c"
        PRINT #1, "      IF value = 6000 THEN READ x, y, x1, y1, c: LINE (x+x0, y+y0)-(x1+x0, y1+y0), c, B"
        PRINT #1, "      IF value = 7000 THEN READ x, y, x1, y1, c: LINE (x+x0, y+y0)-(x1+x0, y1+y0), c, BF"
        PRINT #1, "      IF value = 8000 THEN "
        PRINT #1, "         READ arx0, ary0, r1, a1, a2, c"
        PRINT #1, "         FOR nast = a1 + 1 TO a2 + 1 STEP SGN(a2 - a1)"
        PRINT #1, "         LINE (arx0 + x0 + r1 * COS(nast * 3.14 / 180), ary0 + y0 + r1 * SIN(nast * 3.14 / 180))-(arx0 + x0 + r1 * COS((nast - 1) * 3.14 / 180), ary0+ y0 + r1 * SIN((nast - 1) * 3.14 / 180)), c"
        PRINT #1, "         NEXT nast"
        PRINT #1, "      END IF"
        PRINT #1, "      IF value = 8500 THEN READ elx, ely, elr1, elr2, c: CIRCLE (elx + x0, ely + y0), elr1, c, , , elr2 / elr1"
        PRINT #1, "      IF value = 9000 THEN"
        PRINT #1, "           READ x, y, c, n"
        PRINT #1, "           COLOR c: LOCATE y + 1, x + 1"
        PRINT #1, "           FOR i = 1 TO n"
        PRINT #1, "           READ a"
        PRINT #1, "           PRINT CHR$(a);"
        PRINT #1, "           NEXT i"
        PRINT #1, "           END IF"
        PRINT #1, "GOTO "; nam$
        PRINT #1, "   END IF"
        PRINT #1, nam$; "1:"
        PRINT #1, "DATA ";
        jjj = current
        kkk = 1
starta:
        ddd$ = STR$(pc(current - jjj + 1))
        PRINT #1, ddd$;
        IF kkk <> 14 THEN PRINT #1, ",";
        jjj = jjj - 1
        kkk = kkk + 1
        IF jjj = 1 THEN GOTO wyhod
        IF kkk = 15 THEN kkk = 1: PRINT #1, "": PRINT #1, "DATA ";
        GOTO starta
wyhod:  IF kkk = 15 THEN PRINT #1, ",";
        PRINT #1, "65535"
CLOSE
ex3:
END IF
        RETURN
menu:
        wincol = 8
        winborcol = 1
        winx = uplx
        winy = uply + 1
        winh = num
        winl = ml
        GOSUB putwindow
        COLOR colf, foncol
        FOR i = 1 TO num
        LOCATE uply + i, uplx
        PRINT (pun$(i))
        NEXT i
        stroka = 1
        COLOR colt: LOCATE uply + stroka, uplx: PRINT pun$(stroka): COLOR colf
        tyr = mousey
rrr:
        GOSUB inke
        IF k = 80 THEN GOTO 100
        IF k = 72 THEN GOTO 200
oiu:    IF a = 13 OR a = 32 THEN GOTO rret
        IF a = 27 THEN stroka = 900: GOTO rret
        GOTO rrr
rret:
        GOSUB killwindow
        RETURN
100 :
        IF stroka = num THEN GOTO rrr
        COLOR colf: LOCATE uply + stroka, uplx: PRINT pun$(stroka)
        stroka = stroka + 1
        COLOR colt: LOCATE uply + stroka, uplx: PRINT pun$(stroka): COLOR colf
        GOTO rrr
200 :
        IF stroka = 1 THEN GOTO rrr
        COLOR colf: LOCATE uply + stroka, uplx: PRINT pun$(stroka)
        stroka = stroka - 1
        COLOR colt: LOCATE uply + stroka, uplx: PRINT pun$(stroka): COLOR colf
        GOTO rrr
putwindow:
        wind = 1
        winx = winx - 1
        winy = winy - 1
        dwinx = winx
        dwiny = winy
        GET (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), savbar
        LINE (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), wincol, BF
        LINE (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), winborcol, B
        winx = winx + 1
        winy = winy + 1
        RETURN
'--------Очистка окна номер 1--------
killwindow:
        wind = 0
        PUT (dwinx * 8 - 3, dwiny * 14 - 3), savbar, PSET
        RETURN
inke:   a$ = INKEY$
        IF a$ = "" THEN GOTO inke
        a = ASC(a$)
        k = ASC(RIGHT$(a$, 1))
        RETURN
'------ Подготовка расширения -------------------------------
' nam$  - name
' ext$  - extension
' digger
extens:
        df$ = ""
        poi = 1
cycl:   IF poi = 9 OR MID$(nam$, poi, 1) = "." OR poi > LEN(nam$) THEN GOTO exut
        df$ = df$ + MID$(nam$, poi, 1)
        poi = poi + 1
        GOTO cycl
exut:   nam$ = df$ + "." + ext$
RETURN
inname:
        eror = 0
        GET (ix * 8 - 3, iy * 14 - 3)-(ix * 8 + (il + 2) * 8 + 2, iy * 14 + 17), savbar
        LOCATE iy + 1, ix + 1
        PRINT SPC(il + 2);
        LINE (ix * 8 - 2, iy * 14 - 2)-(ix * 8 + (il + 2) * 8 + 1, iy * 14 + 14), 4, B
        LINE (ix * 8 - 1, iy * 14 - 1)-(ix * 8 + (il + 2) * 8, iy * 14 + 15), 13, B
        GOTO start1
start:
        GOSUB inke
        IF a <> 0 AND (a > 31 AND a < 128) AND LEN(nam$) < il THEN nam$ = nam$ + CHR$(a)
        IF LEN(nam$) > 0 AND a = 8 THEN nam$ = LEFT$(nam$, LEN(nam$) - 1): LOCATE iy + 1, ix + 1: PRINT nam$; "   "
        IF a = 13 AND LEN(nam$) <> 0 THEN LOCATE iy + 1, ix + 1: PRINT nam$; " ": GOTO xit
        IF a = 27 THEN eror = 1: GOTO xit
start1:
        LOCATE iy + 1, ix + 1
        PRINT nam$; " "
        LINE (ix * 8 + LEN(nam$) * 8, iy * 14)-(ix * 8 + (1 + LEN(nam$)) * 8, iy * 14 + 12), 10, BF
        GOTO start
xit:
        PUT (ix * 8 - 3, iy * 14 - 3), savbar, PSET
        RETURN

FUNCTION mouseb
regs.ax = &H3
CALL interrupt(&H33, regs, regs)
mouseb = regs.bx
END FUNCTION

FUNCTION mousex
regs.ax = &H3
CALL interrupt(&H33, regs, regs)
mousex = regs.cx
END FUNCTION

FUNCTION mousey
regs.ax = &H3
CALL interrupt(&H33, regs, regs)
mousey = regs.dx
END FUNCTION

