DECLARE FUNCTION min! (a!, b!)
DECLARE FUNCTION max! (a!, b!)
DECLARE FUNCTION mousex! ()
DECLARE FUNCTION mousey! ()
DECLARE FUNCTION mouseb! ()
' $INCLUDE: 'p:/qbasic/lib/qb.bi'
' The 'SOVOK' graphics editor, created by DIGGER from Lotos Software (C) 1997
' on 11:14:08 on 29 of july of 1997
' Отредактировано в 01:12:04 30 марта 1998 года
'               A lot of GLUKS left!!!
        li = 8
        numd = 8
        foncol = 63
        clcol = 15
        col = 1
        SCREEN 9: COLOR 1, foncol
        TYPE wfile
                fil AS SINGLE
        END TYPE
        DIM disk AS wfile
        DIM SHARED regs AS RegType
        DIM savbar(5000), savbar1(1000), pu(numd, 2), pun$(15), pc(5000)
        DIM tcurs(40), mode$(19), cursor(40)
        LINE (1, 1)-(8, 14), 1, BF
        GET (1, 1)-(8, 14), tcurs
        FOR c = 1 TO numd
        READ pu(c, 1), pu(c, 2)
        NEXT c
        pereris = 0
        colt = 2
        colf = 4
        cx = mousex
        cy = mousey
        mumu = 0
        smex = 0
        smey = 0
        smexa = 0
        smeya = 0
        grid = 0
        gridcol = 1
        magnify = 1
        mstep = 5
        clx = 0
        cly = 0
        mode = 1
        current = 1
        cmouse = 1
        mmouse = 0
        stepout = 0
        mode$(1) = "Точка.               "
        mode$(2) = "Линия.               "
        mode$(3) = "Круг.                "
        mode$(4) = "Окраска.             "
        mode$(5) = "Дуга.                "
        mode$(6) = "Прямоугольник.       "
        mode$(7) = "Закрашенный прямоуг. "
        mode$(8) = "Непрерывная линия.   "
        mode$(9) = "Эллипс.              "
        lf = 0
        cf = 0
        bfl = 0
        fbfl = 0
        tf = 0
        clfl = 0
        arfl = 0
        elfl = 0
        elx = 0: ely = 0: elr1 = 0: elr2 = 0
        arx1 = 0: ary1 = 0: arx2 = 0: ary2 = 0: arx3 = 0: ary3 = 0
        GOSUB readyscreen
        GOSUB setcursor
opros:
        GOSUB cursor
        IF escf = 1 THEN GOTO quit
        GOSUB space
'        GOSUB killcursor
'        GOSUB setcursor
        GOTO opros
        STOP
' ------------- ПРОЦЕДУРА ВЫХОДА --------------
quit:
        winx = 5
        winy = 5
        winh = 1
        winl = 29
        wincol = 4
        winborcol = 10
        GOSUB putwindow1
        LOCATE winy, winx
        COLOR 1
        PRINT "Вы хотите выйти из программы?"
        pun$(1) = "Да, пожалуйста! "
        pun$(2) = "Нет, спасибо!   "
        uplx = 12
        uply = 6
        ml = 16
        num = 2
        GOSUB menu
        IF stroka = 90 OR stroka = 1 THEN SCREEN 0: PRINT "(C) 1999 by Digger From Lotos Software": END
        GOSUB killwindow1
        GOSUB setcursor
        GOTO opros
' ---------- ПРОЦЕДУРА ДВИЖЕНИЯ КУРСОРА -----------
cursor:
                a$ = ""
                GOSUB inke1
                IF a = 0 AND k <> 0 THEN GOSUB killcursor: GOTO obhod1
                IF a$ = CHR$(27) THEN escf = 1: GOSUB killcursor: RETURN
        IF cmouse = 0 THEN IF a$ = "" THEN GOTO cursor
        IF mousex = cx AND mousey = cy AND a$ = "" THEN GOTO cursor
ou:     GOSUB killcursor
        IF a <> 0 THEN GOTO obhod1
        cax = mousex
        cyx = mousey
        IF cmouse = 1 THEN IF cax <> cx OR cay <> cy THEN cx = mousex: cy = mousey: k = 0
        IF cx < 10 THEN cx = 10
        IF cx > 626 THEN cx = 626
        IF cy > 297 THEN cy = 297
        IF cy < 10 THEN cy = 10
obhod1:
        IF cmouse = 0 THEN
        du = mstep * magnify
        du = mstep
        IF k = 77 THEN IF cx + du < 626 THEN cx = cx + du ELSE cx = 626
        IF k = 75 THEN IF cx - du > 10 THEN cx = cx - du ELSE cx = 10
        IF k = 80 THEN IF cy + du < 297 THEN cy = cy + du ELSE cy = 297
        IF k = 72 THEN IF cy - du > 10 THEN cy = cy - du ELSE cy = 10
        END IF
        IF a = 0 AND (k >= 59 AND k <= 66) THEN punk = k - 58: d$ = a$: GOSUB ddd: a$ = d$
        punk = 0
        IF a$ = "R" OR a$ = "r" OR a$ = "К" OR a$ = "к" THEN clfl = 0
        IF a$ = "П" OR a$ = "п" OR a$ = "G" OR a$ = "g" THEN punk = 1
        IF a$ = "Ф" OR a$ = "ф" OR a$ = "A" OR a$ = "a" THEN punk = 2
        IF a$ = "Г" OR a$ = "г" OR a$ = "U" OR a$ = "u" THEN punk = 3
        IF a$ = "Ц" OR a$ = "ц" OR a$ = "W" OR a$ = "w" THEN punk = 4
        IF a$ = "У" OR a$ = "у" OR a$ = "E" OR a$ = "e" THEN punk = 5
        IF a$ = "Т" OR a$ = "т" OR a$ = "N" OR a$ = "n" THEN punk = 6
        IF a$ = "О" OR a$ = "о" OR a$ = "J" OR a$ = "j" THEN punk = 7
        IF a$ = "Е" OR a$ = "е" OR a$ = "T" OR a$ = "t" THEN punk = 8
        IF punk <> 0 THEN d$ = a$: GOSUB ddd: a$ = d$: punk = 0
        pereris = 1
        IF a$ = "4" THEN smex = smex + 10 * magnify: CLS : GOSUB outall: GOSUB readyscreen
        IF a$ = "6" THEN smex = smex - 10 * magnify: CLS : GOSUB outall: GOSUB readyscreen
        IF a$ = "8" THEN smey = smey + 10 * magnify: CLS : GOSUB outall: GOSUB readyscreen
        IF a$ = "2" THEN smey = smey - 10 * magnify: CLS : GOSUB outall: GOSUB readyscreen
        pereris = 0
        escf = 0
        IF a$ = " " THEN GOSUB statusline: RETURN
ret:
        GOSUB setcursor
        GOTO cursor
' ------------- ПРОЦЕДУРА ОБРАБОТКИ НАЖАТИЯ КЛАВИШИ ПРОБЕЛ --------------
space:
        IF cy > 33 AND mumu = 0 THEN GOTO graphics
        GOSUB oprf
ddd:
        GOSUB killcursor
        IF punk = 1 THEN GOSUB help
        IF punk = 2 THEN GOSUB file
        IF punk = 3 THEN GOSUB graph
        IF punk = 4 THEN GOSUB colors
        IF punk = 5 THEN GOSUB magnify
        IF punk = 6 AND magnify = 1 THEN GOSUB TEXT
        IF punk = 7 THEN GOSUB clearscreen
        IF punk = 8 THEN GOSUB addi
        RETURN
graphics:
        GOSUB killcursor
        ON mode GOSUB 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000
        GOSUB statusline
        GOSUB setcursor
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
        GOSUB inke1
        IF mmouse = 0 THEN
        IF k = 80 THEN GOTO 100
        IF k = 72 THEN GOTO 200
        ELSE
        IF tyr = mousey THEN GOTO oiu
        COLOR colf: LOCATE uply + stroka, uplx: PRINT pun$(stroka)
        stroka = INT((mousey + 1) * (num - 1) / 350 + .5) + 1
        tyr = mousey
        COLOR colt: LOCATE uply + stroka, uplx: PRINT pun$(stroka): COLOR colf
        END IF
       
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
' PSET
1000 :
        pc(current) = 1000
        pc(current + 1) = (cx - smex) / magnify
        pc(current + 2) = (cy - smey) / magnify
        pc(current + 3) = col
        PSET (cx, cy), col
        current = current + 4
        RETURN
' LINE
2000 :
        IF lf = 0 THEN lx = cx: ly = cy: lf = 1: PSET (cx, cy), col: RETURN
        lf = 0
        LINE (cx, cy)-(lx, ly), col
        pc(current) = 2000
        pc(current + 1) = (lx - smex) / magnify
        pc(current + 2) = (ly - smey) / magnify
        pc(current + 3) = (cx - smex) / magnify
        pc(current + 4) = (cy - smey) / magnify
        pc(current + 5) = col
        current = current + 6
        RETURN
'CIRCLE
3000 :
        IF cf = 0 THEN cix = cx: ciy = cy: cf = 1: re1 = POINT(cx, cy): PSET (cx, cy), col: RETURN
        PSET (cix, ciy), re1
        ra = SQR((cix - cx) ^ 2 + (1.37 * (cy - ciy)) ^ 2)
        IF cix - ra < 8 OR ciy - ra < 32 THEN RETURN
        cf = 0
        CIRCLE (cix, ciy), ra, col
        pc(current) = 3000
        pc(current + 1) = (cix - smex) / magnify
        pc(current + 2) = (ciy - smey) / magnify
        pc(current + 3) = ra / magnify
        pc(current + 4) = col
        current = current + 5
        RETURN
' FILL
4000 :
        ttt = col
        winx = 12
        winy = 12
        winl = 22
        winh = 1
        GOSUB putwindow1
        LOCATE winy, winx
        PRINT "Выберите цвет контура?"
          winx = 15
          winy = 15
          winl = 49
          winh = 2
          wincol = 8
          winborcol = 1
          GOSUB cols1
        GOSUB killwindow1
        IF flag <> 0 THEN col = ttt: RETURN
          borcol = col
          winx = 12
          winy = 12
          winl = 23
          winh = 1
         GOSUB putwindow1
         LOCATE winy, winx
         PRINT "Выберите цвет закраски?";
          winx = 15
          winy = 15
          winl = 49
          winh = 2
          wincol = 8
          winborcol = 1
         GOSUB cols1
         GOSUB killwindow1
        IF flag <> 0 THEN col = ttt: RETURN
        PAINT (cx, cy), col, borcol
        pc(current) = 4000
        pc(current + 1) = (cx - smex) / magnify
        pc(current + 2) = (cy - smey) / magnify
        pc(current + 3) = col
        pc(current + 4) = borcol
        current = current + 5
        col = ttt
               pereris = 1
               GOSUB readyscreen
RETURN
' ARC
5000 :
        IF arfl = 0 THEN PSET (cx, cy), col: arx = cx: ary = cy: arfl = 1: RETURN
        IF arfl = 1 THEN PSET (cx, cy), col: arx2 = cx: ary2 = cy: arfl = 2: RETURN
        IF arfl = 2 THEN PSET (cx, cy), col: arx1 = cx: ary1 = cy

a = 2 * (ary - ary2)
b = arx ^ 2 - arx2 ^ 2 + ary ^ 2 - ary2 ^ 2
c = 2 * (arx - arx2)

a1 = 2 * (arx - arx1)
b1 = arx1 ^ 2 - arx ^ 2 + ary1 ^ 2 - ary ^ 2
c1 = 2 * (ary - ary1)

IF c * c1 - a * a1 = 0 THEN arfl = 0: RETURN
IF c1 = 0 THEN arfl = 0: RETURN

arx0 = (b * c1 + a * b1) / (c * c1 - a * a1)
ary0 = -(a1 * arx0 + b1) / c1
       
        r1 = SQR((arx - arx0) ^ 2 + (ary - ary0) ^ 2)
        xa = (arx - arx0): ya = (ary - ary0): GOSUB quarter
            IF xa <> 0 THEN a1 = ATN(ya / xa) * 180 / 3.14 + sme ELSE a1 = 90
        xa = (arx2 - arx0): ya = (ary2 - ary0): GOSUB quarter
            IF xa <> 0 THEN a2 = ATN(ya / xa) * 180 / 3.14 + sme ELSE a2 = 90
        xa = (arx1 - arx0): ya = (ary1 - ary0): GOSUB quarter
            IF xa <> 0 THEN a3 = ATN(ya / xa) * 180 / 3.14 + sme ELSE a3 = 90

        IF a3 < 0 THEN a3 = a3 + 360
        IF a2 > a1 AND a3 > a2 AND a3 > a1 THEN a1 = a1 + 360
        IF a2 < a1 AND a3 > a2 AND a3 > a1 THEN a2 = a2 + 360
'       a1 = -a1 + 360
'        a2 = -a2 + 360
'        a3 = -a3 + 360
        a1 = (a1 MOD 360)
        a2 = (a2 MOD 360)
        a3 = (a3 MOD 360)
'        f1 = min(a1, min(a2, a3))
'        f3 = max(a1, max(a2, a3))
'        f2 = (a1 + a2 + a3) - (f1 + f3)
'        f1 = a1
'        f2 = a2
'        f3 = a3
'        IF a2 > a1 THEN SWAP a2, a1
'        a1 = f1 * 3.14 / 180
'        a2 = f2 * 3.14 / 180
'        a3 = f3 * 3.14 / 180
'        CIRCLE (arx0, ary0), r1, col, a2, a1, 1

FOR nast = a1 + 1 TO a2 + 1 STEP SGN(a2 - a1)
LINE (arx0 + r1 * COS(nast * 3.14 / 180), ary0 + r1 * SIN(nast * 3.14 / 180))-(arx0 + r1 * COS((nast - 1) * 3.14 / 180), ary0 + r1 * SIN((nast - 1) * 3.14 / 180)), col
NEXT nast
        nasx = (arx0 - smex) / magnify
        nasy = (ary0 - smey) / magnify
        nasr = r1 / magnify

        pc(current) = 8000
        pc(current + 1) = nasx + .000000000001#
        pc(current + 2) = nasy + .000000000001#
        pc(current + 3) = nasr + .000000000001#
        pc(current + 4) = a1
        pc(current + 5) = a2
        pc(current + 6) = col
        current = current + 7
        arfl = 0
        RETURN
quarter:
sme = 0
IF xa < 0 AND ya < 0 THEN sme = -180 ' 3 четверть
IF xa > 0 AND ya > 0 THEN sme = 0' 1 четверть
IF xa < 0 AND ya > 0 THEN sme = 180 ' 2 четверть
IF xa > 0 AND ya < 0 THEN sme = 0' 4 четверть
RETURN
' BAR
6000 :
        IF bfl = 0 THEN bx1 = cx: by1 = cy: bfl = 1: PSET (cx, cy), col: RETURN
        bfl = 0
        LINE (bx1, by1)-(cx, cy), col, B
        pc(current) = 6000
        pc(current + 1) = (bx1 - smex) / magnify
        pc(current + 2) = (by1 - smey) / magnify
        pc(current + 3) = (cx - smex) / magnify
        pc(current + 4) = (cy - smey) / magnify
        pc(current + 5) = col
        current = current + 6
        RETURN
' FILLED BAR
7000 :
        IF fbfl = 0 THEN fbx = cx: fby = cy: fbfl = 1: PSET (cx, cy), col: RETURN
        fbfl = 0
        LINE (fbx, fby)-(cx, cy), col, BF
        pc(current) = 7000
        pc(current + 1) = (fbx - smex) / magnify
        pc(current + 2) = (fby - smey) / magnify
        pc(current + 3) = (cx - smex) / magnify
        pc(current + 4) = (cy - smey) / magnify
        pc(current + 5) = col
        current = current + 6
        RETURN
' CONTINOUS LINE
8000 :
        IF clfl = 0 THEN PSET (cx, cy), col: clx = (cx - smex) / magnify: cly = (cy - smey) / magnify: clfl = 1: RETURN' GOTO 1000
        LINE (clx * magnify + smex, cly * magnify + smey)-(cx, cy), col
        pc(current) = 2000
        pc(current + 1) = clx
        pc(current + 2) = cly
        pc(current + 3) = (cx - smex) / magnify
        pc(current + 4) = (cy - smey) / magnify
        pc(current + 5) = col
        current = current + 6
        clx = (cx - smex) / magnify
        cly = (cy - smey) / magnify
        RETURN
9000 :
        IF elfl = 0 THEN elx = cx: ely = cy: re = POINT(cx, cy): PSET (cx, cy), col: elfl = 1: RETURN
        IF elfl = 1 THEN elr1 = SQR((elx - cx) ^ 2 + ((ely - cy)) ^ 2): ress = POINT(cx, cy): adsx = cx: adsy = cy: PSET (cx, cy), col: elfl = 2: RETURN
        PSET (elx, ely), re
        PSET (adsx, adsy), ress
        elr2 = SQR(((elx - cx) ^ 2) + ((ely - cy)) ^ 2)
        CIRCLE (elx, ely), elr1, col, , , elr2 / elr1
        pc(current) = 8500
        pc(current + 1) = (elx - smex) / magnify
        pc(current + 2) = (ely - smey) / magnify
        pc(current + 3) = elr1 / magnify
        pc(current + 4) = elr2 / magnify
        pc(current + 5) = col
        current = current + 6
        elfl = 0
        'pereris = 1
        'GOSUB readyscreen
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
putc:
        LINE (cx, cy)-(cx + 6, cy), 4
        LINE (cx, cy)-(cx, cy + 6), 4
        LINE (cx, cy + 6)-(cx + 6, cy), 4
        LINE (cx + 1, cy + 1)-(cx + 4, cy + 1), 10
        LINE (cx + 2, cy + 2)-(cx + 3, cy + 2), 10
        LINE (cx + 1, cy + 1)-(cx + 1, cy + 4), 10
        PSET (cx + 2, cy + 3), 10
        RETURN
putc1:
        LINE (cx - 3, cy - 3)-(cx + 3, cy + 3), 4, B
        LINE (cx - 4, cy - 4)-(cx + 4, cy + 4), 2, B
        RETURN
putc2:
        CIRCLE (cx, cy), 5, 1
        CIRCLE (cx, cy), 4, 4
        LINE (cx, cy + 5)-(cx, cy + 8), 1
        RETURN
killcursor:
        PUT (cx - 6, cy - 6), cursor, PSET
        RETURN
setcursor:
        IF cx < 10 THEN cx = 10
        IF cx > 626 THEN cx = 626
        IF cy > 297 THEN cy = 297
        IF cy < 10 THEN cy = 10
       
        GET (cx - 6, cy - 6)-(cx + 6, cy + 8), cursor
        IF cy <= 33 THEN GOSUB putc ELSE
        IF mumu = 0 AND cy > 33 THEN GOSUB putc1 ELSE IF cy > 33 THEN GOSUB putc2
        RETURN
'--------------=Процедура вывода всего рисунка=-------------
outall:
        fl = stepout
        numbb = 0
        d = 1
sttr:
        IF d >= current THEN GOTO rtyu
        te = pc(d)
        IF te = 1000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                c = pc(d + 3)
                PSET (x, y), c
                'LINE (x - INT(magnify / 2), y - INT(magnify / 2))-(x + INT(magnify / 2), y + INT(magnify / 2)), c, BF
                d = d + 4
        END IF
        IF te = 2000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                x1 = pc(d + 3) * magnify + smex
                y1 = pc(d + 4) * magnify + smey
                c = pc(d + 5)
                LINE (x, y)-(x1, y1), c
                d = d + 6
        END IF
        IF te = 3000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                r = pc(d + 3) * magnify
                c = pc(d + 4)
                CIRCLE (x, y), r, c
                d = d + 5
        END IF
        IF te = 4000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                c = pc(d + 3)
                c1 = pc(d + 4)
                PAINT (x, y), c, c1
               d = d + 5
        END IF
        IF te = 5000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                c = pc(d + 3)
                c1 = pc(d + 4)
                CIRCLE (x, y), c, c1
                d = d + 5
        END IF
        IF te = 6000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                x1 = pc(d + 3) * magnify + smex
                y1 = pc(d + 4) * magnify + smey
                c = pc(d + 5)
                LINE (x, y)-(x1, y1), c, B
                d = d + 6
        END IF
        IF te = 7000 THEN
                x = pc(d + 1) * magnify + smex
                y = pc(d + 2) * magnify + smey
                x1 = pc(d + 3) * magnify + smex
                y1 = pc(d + 4) * magnify + smey
                c = pc(d + 5)
                LINE (x, y)-(x1, y1), c, BF
                d = d + 6
        END IF
        IF te = 8500 THEN
        elx = pc(d + 1) * magnify + smex
        ely = pc(d + 2) * magnify + smey
        elr1 = pc(d + 3) * magnify
        elr2 = pc(d + 4) * magnify
        c = pc(d + 5)
        d = d + 6
        CIRCLE (elx, ely), elr1, c, , , elr2 / elr1

        END IF
        IF te = 8000 THEN
        arx0 = pc(d + 1) * magnify + smex
        ary0 = pc(d + 2) * magnify + smey
        r1 = pc(d + 3) * magnify
        a1 = pc(d + 4)
        a2 = pc(d + 5)
        c = pc(d + 6)
        d = d + 7
FOR nast = a1 + 1 TO a2 + 1 STEP SGN(a2 - a1)
LINE (arx0 + r1 * COS(nast * 3.14 / 180), ary0 + r1 * SIN(nast * 3.14 / 180))-(arx0 + r1 * COS((nast - 1) * 3.14 / 180), ary0 + r1 * SIN((nast - 1) * 3.14 / 180)), c
NEXT nast
        END IF
        IF te = 9000 THEN
                ttx = pc(d + 1)
                tty = pc(d + 2)
                ttc = pc(d + 3)
                ttn = pc(d + 4)
                d = d + 4
                COLOR ttc
                LOCATE tty + 1, ttx + 1
caca1:
        ttn = ttn - 1
        d = d + 1
        IF magnify = 1 THEN PRINT CHR$(pc(d));
        IF ttn <> 0 THEN GOTO caca1
        d = d + 1
END IF
        numbb = numbb + 1
        sy = 23: sx = 65: st$ = "Элемент:" + STR$(numbb)
        LOCATE sy, sx: PRINT st$
        LINE ((sx - 1) * 8 - 1, (sy - 1) * 14)-((sx - 1 + LEN(st$)) * 8 - 1, (sy) * 14), 4, B
        IF stepout = 1 THEN
                        GOSUB inke
                        IF a = 27 THEN stepout = 0
                        END IF
        GOTO sttr
'--------Вывод сетки----------
rtyu:
        stepout = fl
        IF grid = 1 THEN
           IF magnify <> 1 THEN
                FOR tx.gr = 9 TO 630 STEP magnify
                LINE (tx.gr, 20)-(tx.gr, 300), gridcol
                NEXT tx.gr
               FOR ty.gr = 20 TO 340 STEP magnify
               LINE (9, ty.gr)-(630, ty.gr), gridcol
               NEXT ty.gr
           END IF
        END IF
        pereris = 1
        GOSUB readyscreen
RETURN
coll:
        LINE (winx * 8 + (col - 1) * 25, (winy - 1) * 14 + 3)-(winx * 8 + (col) * 25, (winy + 1) * 14 - 3), col, BF
        LINE (winx * 8 + (col - 1) * 25, (winy - 1) * 14 + 3)-(winx * 8 + (col) * 25, (winy + 1) * 14 - 3), 1, B
        RETURN
curcol:
        LINE (winx * 8 + (col - 1) * 25 + 1, (winy - 1) * 14 + 3 + 1)-(winx * 8 + (col) * 25 - 1, (winy + 1) * 14 - 3 - 1), 1, B
        LINE (winx * 8 + (col - 1) * 25 + 2, (winy - 1) * 14 + 3 + 2)-(winx * 8 + (col) * 25 - 2, (winy + 1) * 14 - 3 - 2), 2, B
        LINE (winx * 8 + (col - 1) * 25 + 3, (winy - 1) * 14 + 3 + 3)-(winx * 8 + (col) * 25 - 3, (winy + 1) * 14 - 3 - 3), 3, B
        LINE (winx * 8 + (col - 1) * 25 + 4, (winy - 1) * 14 + 3 + 4)-(winx * 8 + (col) * 25 - 4, (winy + 1) * 14 - 3 - 4), 4, B
        LINE (winx * 8 + (col - 1) * 25 + 5, (winy - 1) * 14 + 3 + 5)-(winx * 8 + (col) * 25 - 5, (winy + 1) * 14 - 3 - 5), 5, B
        LINE (winx * 8 + (col - 1) * 25 + 6, (winy - 1) * 14 + 3 + 6)-(winx * 8 + (col) * 25 - 6, (winy + 1) * 14 - 3 - 6), 6, B
        RETURN
'------ Подготовка экрана к работе ---------
readyscreen:
        SCREEN 9
        COLOR 1, foncol
                IF pereris = 0 THEN LINE (0, 0)-(640, 350), clcol, BF
        LINE (0, 0)-(9, 347), 10, BF
        LINE (0, 0)-(640, 9), 10, BF
        LINE (640, 0)-(630, 347), 10, BF
        LINE (0, 320)-(640, 350), 10, BF
        LINE (2, 2)-(637, 347), li, B
        LINE (4, 4)-(635, 345), li, B
        LINE (6, 6)-(633, 343), li, B
        LINE (8, 8)-(631, 341), li, B
        LINE (8, 8)-(631, 32), li, BF
        LINE (9, 300)-(630, 340), li, BF
        RESTORE fields
        GOSUB statusline
        COLOR 3
        sy = 2
                st$ = " Помощь ": GOSUB nex
                st$ = " Файлы ": GOSUB nex
                st$ = " Графика ": GOSUB nex
                st$ = " Цвета ": GOSUB nex
                st$ = " Увеличение ": GOSUB nex
                st$ = " Текст ": GOSUB nex
                st$ = " Очистка экрана ": GOSUB nex
                st$ = " Е ": GOSUB nex
        RETURN
nex:
        READ sx, sl
        GOSUB coolprint
        RETURN
fields:
        DATA 3,8,12,7,20,9,30,7,38,12,51,7,59,16,76,3
' --- ПРЦЕДУРА КРУТОГО ВЫВОДА ТЕКСТА ---
coolprint:
        LOCATE sy, sx: PRINT st$
        COLOR 4
        LOCATE sy, sx + 1: PRINT MID$(st$, 2, 1);
        COLOR 3
        LINE ((sx - 1) * 8 - 1, (sy - 1) * 14)-((sx - 1 + LEN(st$)) * 8 - 1, (sy) * 14), 4, B
        RETURN
'------------- ПРОЦЕДУРЫ ОПРОСА КЛАВИШ -----------------
inke:
        a$ = INKEY$
        IF mouseb = 1 THEN
12341 :                  IF mouseb <> 0 THEN GOTO 12341
                        a$ = " "
                        END IF
        IF mouseb = 2 THEN
a12341:                  IF mouseb <> 0 THEN GOTO a12341
                        a$ = CHR$(27)
                        END IF
        IF a$ = "" THEN GOTO inke
        a = ASC(a$)
        k = ASC(RIGHT$(a$, 1))
        RETURN
inke1:
        a = 0: k = 0: a$ = ""
        a$ = INKEY$
        IF mouseb = 1 THEN
1234 :                  IF mouseb <> 0 THEN GOTO 1234
                        a$ = " "
                        END IF
        IF mouseb = 2 THEN
a1234:                  IF mouseb <> 0 THEN GOTO a1234
                        a$ = CHR$(27)
                        END IF
        IF a$ <> "" THEN
                a = ASC(a$)
                IF a = 0 THEN y$ = MID$(a$, 2, 1): k = ASC(y$)
                END IF
        RETURN
' ---- ОПРЕДЕЛЕНИЕ НОМЕРА ПОЛЯ ----
oprf:
        punk = 0
        IF cy < 1 * 14 OR cy > 2 * 14 THEN RETURN
        FOR c = 1 TO numd
         IF INT(cx / 8) >= pu(c, 1) - 1 AND INT(cx / 8) < (pu(c, 2) + pu(c, 1)) - 1 THEN punk = c
        NEXT c
        RETURN
'------ НИЖНЯЯ СТРОКА СТАТУСА ----------
statusline:
        COLOR 4
        LOCATE 23, 3
        PRINT "Текущий режим: "; mode$(mode);
         LOCATE 24, 3
         PRINT "                                    ";
         LOCATE 24, 3
         PRINT "Смещение по : X = "; smex; ",        ";
         LOCATE 24, 27
         PRINT "Y = "; smey; ".";
        LOCATE 23, 41
        PRINT "Текущий цвет: ";
        LINE (439, 308)-(489, 319), col, BF
        LINE (438, 307)-(490, 320), 15, B
        LOCATE 24, 41
        PRINT " Шаг курсора: ";
        LINE (439, 308 + 15)-(489, 321 + 15), 15, BF
        LOCATE 24, 56
        PRINT mstep;
        LINE (438, 307 + 15)-(490, 322 + 15), 3, B
        LOCATE 23, 63
        COLOR 2
        PRINT "Увеличение:"; magnify; "x ";
        LINE (495, 307)-(624, 322), 4, B
        LINE (494, 306)-(625, 321), 12, B
         LINE (15, 322)-(304, 308), 1, B
         LINE (15, 322)-(304, 336), 1, B
         LINE (320, 322)-(431, 336), 1, B
         LINE (320, 322)-(431, 308), 1, B
        RETURN
'-------- Вывод окна номер 1 --------------
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
'-------- Вывод окна номер 2 --------------
putwindow1:
        wind1 = 1
        winx = winx - 1
        winy = winy - 1
        dwinx1 = winx
        dwiny1 = winy
        GET (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), savbar1
        LINE (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), wincol, BF
        LINE (winx * 8 - 3, winy * 14 - 3)-(winx * 8 + 8 * winl + 2, winy * 14 + 14 * winh + 2), winborcol, B
        winx = winx + 1
        winy = winy + 1
        RETURN
'--------Очистка окна номер 2--------
killwindow1:
        wind1 = 0
        PUT (dwinx1 * 8 - 3, dwiny1 * 14 - 3), savbar1, PSET
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
'------------------------------------------------------------
'-----------------------MENU---------------------------------
'------------------------------------------------------------
'-=1=--------------------------------------------------------
'------------------------------------------------------------
help:
        pun$(1) = "Клавиши управления "
        pun$(2) = "Верхние меню       "
        pun$(3) = "Замечания          "
        pun$(4) = "О программе        "
        uplx = 4
        uply = 4
        ml = 19
        num = 4
        GOSUB menu
IF stroka = 4 THEN
        winx = 10
        winy = 5
        winl = 59
        winh = 5
        wincol = 15
        winborcol = 1
        GOSUB putwindow
        COLOR 1
        LOCATE winy, winx + 1
        PRINT "             Графический редактор 'СОВОК'       "
        LOCATE winy + 1, winx + 1
        PRINT "Программный продукт создан Мамичевым Антоном Евгеньевичем"
        LOCATE winy + 2, winx + 1
        PRINT "            специально для МУПК города Азова"
        GOSUB subba
        coo = 1
ciu:    LOCATE winy + 4, winx + 14
        COLOR coo
        PRINT "(C) 1998 by DIGGER from Lotos Software"
        coo = coo + 1: IF coo = 9 THEN coo = 1
        tyu = 1
rett:   tyu = tyu + 1
        IF INKEY$ <> "" THEN GOTO are
        IF tyu < 1000 THEN GOTO rett
        GOTO ciu
are:
        GOSUB killwindow
        END IF

IF stroka = 1 THEN
        LINE (16, 12)-(600, 240), 15, BF
        LINE (16, 12)-(600, 240), 1, B
        winy = 2
        winx = 4
        COLOR 4
        LOCATE winy, winx
        PRINT "F1, П, G -- меню помощи"
        COLOR 4
        LOCATE winy + 1, winx
        PRINT "F2, Ф, A -- файловое меню"
        COLOR 4
        LOCATE winy + 2, winx
        PRINT "F3, Г, U -- меню графических инструментов"
        COLOR 4
        LOCATE winy + 3, winx
        PRINT "F4, Ц, W -- меню выбора цветов"
        COLOR 4
        LOCATE winy + 4, winx
        PRINT "F5, У, E -- меню увеличения"
        COLOR 4
        LOCATE winy + 5, winx
        PRINT "F6, Т, N -- текстовое меню"
        COLOR 4
        LOCATE winy + 6, winx
        PRINT "F7, О, J -- очистка и восстановление экрана"
        COLOR 4
        LOCATE winy + 7, winx
        PRINT "F8, Е, T -- меню дополнительных возможностей"
        COLOR 4
        LOCATE winy + 8, winx
        PRINT "8, 2, 4, 6 -- изменение положения картинки относительно окна просмотра"
        COLOR 4
        LOCATE winy + 9, winx
        PRINT "КУРСОРЫ -- движение курсора на экране и в меню"
        COLOR 4
        LOCATE winy + 10, winx
        PRINT "SPACE -- использование инструмента и выбор пункта меню"
        COLOR 4
        LOCATE winy + 11, winx
        PRINT "ENTER -- выбор пункта меню"
        COLOR 4
        LOCATE winy + 12, winx
        PRINT "ESC -- выход"
        COLOR 4
        LOCATE winy + 13, winx
        PRINT "К, R -- установка нового начала НЕПРЕРЫВНОЙ ЛИНИИ"
        COLOR 4
        LOCATE winy + 14, winx
        PRINT "ЛЕВАЯ КНОПКА МЫШИ -- аналог SPACE"
        COLOR 4
        LOCATE winy + 15, winx
        PRINT "ПРАВАЯ КНОПКА МЫШИ -- аналог ESC"
        GOSUB kkey
retw:   IF INKEY$ = "" THEN GOTO retw
        CLS
        GOSUB outall
        pereris = 1
        GOSUB readyscreen
               
                END IF
        RETURN
'
subba:
RESTORE aboutscr.bas1
x0 = -13: y0 = -1  'Координата в знакоместах
x0 = x0 * 8: y0 = y0 * 14
aboutscr.bas: READ value
   IF value <> 65535 THEN
      IF value = 1000 THEN READ x, y, c: PSET (x + x0, y + y0), c
      IF value = 2000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c
      IF value = 3000 THEN READ x, y, r, c: CIRCLE (x + x0, y + y0), r, c
      IF value = 4000 THEN READ x, y, c, f: PAINT (x + x0, y + y0), c, f
      IF value = 5000 THEN READ x, y, r, c: CIRCLE (x + x0, y + y0), r: PAINT (x + x0, y + y0), c, c
      IF value = 6000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c, B
      IF value = 7000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c, BF
      IF value = 8000 THEN
         READ arx0, ary0, r1, a1, a2, c
         FOR nast = a1 + 1 TO a2 + 1 STEP SGN(a2 - a1)
         LINE (arx0 + x0 + r1 * COS(nast * 3.14 / 180), ary0 + y0 + r1 * SIN(nast * 3.14 / 180))-(arx0 + x0 + r1 * COS((nast - 1) * 3.14 / 180), ary0 + y0 + r1 * SIN((nast - 1) * 3.14 / 180)), c
         NEXT nast
      END IF
      IF value = 8500 THEN READ elx, ely, elr1, elr2, c: CIRCLE (elx + x0, ely + y0), elr1, c, , , elr2 / elr1
      IF value = 9000 THEN
           READ x, y, c, n
           COLOR c: LOCATE y + 1, x + 1
           FOR i = 1 TO n
           READ a
           PRINT CHR$(a);
           NEXT i
           END IF
GOTO aboutscr.bas
   END IF
RETURN
aboutscr.bas1:
DATA  6000, 183, 101, 270, 134, 1, 2000, 202.25, 103, 187.25, 109.75, 1, 2000, 187.25
DATA  109.75, 202.75, 115.25, 1, 2000, 202.75, 115.25, 190.25, 120.75, 1, 2000, 190.25, 120.75, 199
DATA  114, 1, 2000, 199, 114, 191.75, 107.75, 1, 2000, 191.75, 107.75, 194, 109.5, 1
DATA  2000, 194, 109.5, 202.25, 103, 1, 2000, 195.25, 112.75, 197.5, 115, 1, 4000, 191.75
DATA  110.25, 1, 1, 4000, 195.75, 107, 1, 1, 4000, 197.75, 114.25, 1, 1, 4000
DATA  198.5, 115.5, 1, 1, 1000, 197, 106, 1, 1000, 197, 114, 1, 1000, 195
DATA  112, 1, 8500, 206, 109.75, 6.25, 5.255949, 12, 8500, 206, 110, 5.25, 4.5, 12
DATA  4000, 207, 105, 12, 12, 4000, 206.5, 114.75, 12, 12, 2000, 204.75, 104.25, 209.5
DATA  119.5, 10, 2000, 209.5, 119.75, 219.5, 104, 10, 2000, 219.5, 104, 216.75, 104.25, 10
DATA  2000, 216.75, 104.25, 210.5, 115, 10, 2000, 210.5, 115, 207, 103.75, 10, 2000, 207
DATA  103.75, 204.75, 104.25, 10, 2000, 204.75, 104.25, 205, 104.5, 10, 4000, 206.75, 107.25, 10
DATA  10, 3000, 218, 110, 10.02343, 9, 3000, 218, 110.5, 7.258086, 9, 4000, 211.75, 105.25
DATA  9, 9, 2000, 221.25, 103, 219, 120.5, 14, 2000, 219, 120.5, 219, 120.5, 14
DATA  2000, 219, 120.5, 222, 120.5, 14, 2000, 222, 120.5, 223.25, 112.5, 14, 2000, 223.25
DATA  112.5, 228.75, 120, 14, 2000, 228.75, 120, 232, 121.25, 14, 2000, 232, 121.25, 223.5
DATA  110.25, 14, 2000, 223.5, 110.25, 235, 103, 14, 2000, 235, 103, 230.75, 103, 14
DATA  2000, 230.75, 103, 224, 107.25, 14, 2000, 224, 107.25, 224.75, 103.25, 14, 2000, 224.75
DATA  103.25, 221.5, 103, 14, 4000, 222.5, 109.75, 14, 14, 2000, 211, 116, 218, 106
DATA  10, 2000, 218, 106, 212, 117, 10, 2000, 212, 117, 219, 106, 10, 2000
DATA  219, 106, 218, 106, 10, 2000, 218, 106, 210, 117, 10, 2000, 210, 117
DATA  210, 118, 10, 2000, 210, 118, 216, 107, 10, 2000, 216, 107, 213, 115
DATA  10, 2000, 213, 115, 211, 117, 10, 2000, 211, 117, 213, 116, 10, 2000
DATA  213, 116, 211, 115, 10, 2000, 211, 115, 211, 118, 10, 2000, 211, 118
DATA  217, 107, 10, 2000, 217, 107, 217, 106, 10, 2000, 224, 112, 231, 120
DATA  14, 2000, 231, 120, 230, 121, 14, 2000, 230, 121, 223, 113, 14, 2000
DATA  223, 113, 232, 121, 14, 2000, 232, 121, 225, 111, 14, 2000, 225, 111
DATA  231, 120, 14, 2000, 223.5, 110, 228.25, 116, 14, 2000, 228.5, 116.25, 223.5, 109.25
DATA  14, 2000, 223.5, 109.25, 226.25, 112.75, 14, 2000, 226, 112.5, 222.25, 109.5, 14, 2000
DATA  223.25, 112.75, 225.5, 115.75, 14, 2000, 211.5, 115.75, 213.25, 113.5, 10, 2000, 213.5, 111.5
DATA  209.75, 115.5, 10, 2000, 209.75, 115.5, 209.75, 116.25, 10, 2000, 209.75, 116.25, 213.5, 113.25
DATA  10, 2000, 213.5, 113.25, 213, 112.25, 10, 2000, 213, 112.25, 210, 115.75, 10, 7000
DATA  211.75, 113, 212.25, 116.25, 10, 7000, 212.5, 114, 213.25, 115.25, 10, 7000, 211, 114.25
DATA  211.75, 115.25, 10, 7000, 210.25, 115, 212, 116.25, 10, 7000, 210.75, 114.75, 212, 116.25
DATA  10, 8000, 233.6141, 120.438, 5.67248, 253.5058, 101.2865, 5, 8000, 233.375, 119.375, 4.759858, 256.3674, 330.0533
DATA  5, 8000, 233.7704, 122.8721, 3.29013, 108.0302, 11.00786, 5, 8000, 236.1042, 115.9583, 1.548554, 42.2952, 123.4474
DATA  5, 8000, 234.9688, 120.4375, 4.542013, 229.2099, 122.8958, 5, 8000, 233.75, 122.9375, 1.8125, 133.5793, 46.42071
DATA  5, 8000, 236.1154, 124.1827, 1.117416, 176.545, 322.322, 5, 8000, 233.625, 118.375, 2.128673, 220.2568, 319.7432
DATA  5, 4000, 230, 118.75, 5, 5, 8500, 239, 122.25, 5.25, 5, 4, 8500, 239
DATA  122.25, 3.5, 3.5, 4, 8500, 253, 122.25, 5.75, 5.024938, 4, 8500, 253, 122.25, 4
DATA  3.5, 4, 4000, 251.75, 118.25, 6, 4, 4000, 235.75, 119, 4, 4, 1000, 232
DATA  116, 5, 1000, 234, 117, 5, 1000, 231, 125, 5, 1000, 232, 125, 5
DATA  1000, 232, 125, 5, 1000, 231, 124, 5, 1000, 237, 118, 4, 1000, 236
DATA  120, 4, 1000, 235, 120, 4, 1000, 244, 120, 4, 1000, 243, 120, 4
DATA  1000, 241, 118, 4, 1000, 243, 124, 4, 1000, 241, 126, 4, 1000, 237
DATA  127, 4, 1000, 237, 126, 4, 1000, 249, 122, 6, 1000, 248, 122, 6
DATA  1000, 249, 121, 6, 1000, 248, 121, 6, 1000, 248, 124, 6, 1000, 248
DATA  124, 6, 1000, 249, 124, 6, 1000, 249, 125, 6, 1000, 251, 126, 6
DATA  1000, 255, 126, 6, 1000, 257, 125, 6, 1000, 257, 124, 6, 1000, 258
DATA  124, 6, 1000, 258, 123, 6, 1000, 258, 122, 6, 1000, 258, 121, 6
DATA  1000, 258, 120, 6, 1000, 257, 120, 6, 1000, 258, 119, 6, 1000, 257
DATA  119, 6, 1000, 255, 118, 6, 1000, 251, 118, 6, 1000, 250, 119, 6
DATA  1000, 249, 119, 6, 1000, 249, 120, 6, 1000, 248, 120, 6, 1000, 248
DATA  123, 6, 2000, 253.5, 114.5, 256, 131.75, 13, 2000, 256, 131.75, 258, 131.75, 13
DATA  2000, 258, 131.75, 258, 123.75, 13, 2000, 258, 123.75, 264.75, 132.25, 13, 2000, 264.75
DATA  132.25, 268.5, 133.25, 13, 2000, 268.5, 133.25, 257.75, 121, 13, 2000, 257.75, 121, 268.75
DATA  105.5, 13, 2000, 268.75, 105.5, 265.25, 107, 13, 2000, 265.25, 107, 257.5, 117.75, 13
DATA  2000, 257.5, 117.75, 256.75, 113.75, 13, 2000, 256.75, 113.75, 253.5, 114.25, 13, 4000, 256.5
DATA  121.5, 13, 13, 4000, 264, 110, 13, 13, 4000, 265, 130, 13, 13, 1000
DATA  260, 117, 13, 1000, 262, 127, 13, 1000, 264, 129, 13, 1000, 264, 129
DATA  13, 1000, 263, 128, 13, 2000, 239.75, 113.5, 241, 129.5, 7, 2000, 241, 129.5
DATA  250.75, 132.75, 7, 2000, 250.75, 132.75, 246, 122.5, 7, 2000, 246, 122.5, 254.25, 110.75
DATA  7, 2000, 254.25, 110.75, 239.75, 113.5, 7, 2000, 243, 114.75, 243.5, 121.5, 7, 2000
DATA  243.5, 121.5, 249, 113.75, 7, 2000, 249, 113.75, 243, 114.5, 7, 2000, 243.25, 123.75
DATA  243.75, 128.25, 7, 2000, 243.75, 128.25, 247, 129.75, 7, 2000, 247, 129.75, 243.25, 123.5
DATA  7, 4000, 241.5, 122.25, 7, 7, 4000, 251, 113, 7, 7, 1000, 247, 120
DATA  7, 1000, 247, 119, 7, 1000, 246, 121, 7, 8000, 199.1508, 135.019, 16.32487, 235.9349
DATA  307.0815, 12, 8000, 217.7152, 107.201, 17.04902, 119.7395, 57.03184, 12, 8000, 199.875, 150.725, 20.72839, 241.5807
DATA  298.4193, 12, 8000, 216.7431, 114.7982, 19.03304, 111.5219, 55.61413, 12, 2000, 190, 121.75, 190, 132.75
DATA  12, 2000, 227.5, 130.5, 226.75, 121.5, 12, 4000, 205.75, 123, 12, 12, 6000, 182
DATA  100, 271, 135, 1, 6000, 180, 98, 273, 137, 11, 2000, 194.5, 121.5, 193.5
DATA  123.75, 14, 2000, 193.5, 123.75, 196.25, 122.25, 14, 2000, 196.25, 122.25, 192.75, 122.25, 14
DATA  2000, 192.75, 122.25, 196, 123.5, 14, 2000, 196, 123.5, 194.5, 121.5, 14, 2000, 196.75
DATA  124.25, 198.25, 122.5, 14, 2000, 198.25, 122.5, 198.75, 123, 14, 2000, 198.75, 123, 197.5
DATA  124.5, 14, 2000, 197.5, 124.5, 196.5, 124.5, 14, 2000, 196.5, 124.5, 198.25, 123.75, 14
DATA  2000, 198.25, 123.75, 199.5, 125, 14, 2000, 199.5, 125, 199, 125.25, 14, 2000, 199
DATA  125.25, 198, 124, 14, 2000, 198, 124, 198, 124, 14, 2000, 193.5, 126.5, 194
DATA  125.75, 14, 2000, 194, 125.75, 193.5, 125.5, 14, 2000, 193.5, 125.5, 195, 125.75, 14
DATA  2000, 195, 125.75, 197.25, 125.75, 14, 2000, 197.25, 125.75, 199, 124.25, 14, 2000, 199
DATA  124.25, 200, 122.25, 14, 2000, 200, 122.25, 199, 121.25, 14, 2000, 199, 121.25, 197
DATA  121, 14, 2000, 193.25, 126.75, 194.25, 126.75, 14, 2000, 194.25, 126.75, 194.5, 125.75, 14
DATA 65535
kkey:
RESTORE key.bas1
x0 = 28: y0 = -6  'Координата в знакоместах
x0 = x0 * 8: y0 = y0 * 14 + 4
key.bas: READ value
   IF value <> 65535 THEN
      IF value = 1000 THEN READ x, y, c: PSET (x + x0, y + y0), c
      IF value = 2000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c
      IF value = 3000 THEN READ x, y, r, c: CIRCLE (x + x0, y + y0), r, c
      IF value = 4000 THEN READ x, y, c, f: PAINT (x + x0, y + y0), c, f
      IF value = 5000 THEN READ x, y, r, c: CIRCLE (x + x0, y + y0), r: PAINT (x + x0, y + y0), c, c
      IF value = 6000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c, B
      IF value = 7000 THEN READ x, y, x1, y1, c: LINE (x + x0, y + y0)-(x1 + x0, y1 + y0), c, BF
      IF value = 8000 THEN
         READ arx0, ary0, r1, a1, a2, c
         FOR nast = a1 + 1 TO a2 + 1 STEP SGN(a2 - a1)
         LINE (arx0 + x0 + r1 * COS(nast * 3.14 / 180), ary0 + y0 + r1 * SIN(nast * 3.14 / 180))-(arx0 + x0 + r1 * COS((nast - 1) * 3.14 / 180), ary0 + y0 + r1 * SIN((nast - 1) * 3.14 / 180)), c
         NEXT nast
      END IF
      IF value = 8500 THEN READ elx, ely, elr1, elr2, c: CIRCLE (elx + x0, ely + y0), elr1, c, , , elr2 / elr1
      IF value = 9000 THEN
           READ x, y, c, n
           COLOR c: LOCATE y + 1, x + 1
           FOR i = 1 TO n
           READ a
           PRINT CHR$(a);
           NEXT i
           END IF
GOTO key.bas
   END IF
   RETURN
key.bas1:
DATA  2000, 207, 123, 252, 96, 1, 2000, 252, 96, 331, 101, 1, 2000, 331
DATA  101, 298, 132, 1, 2000, 298, 132, 207, 123, 1, 2000, 298, 132, 305
DATA  172, 1, 2000, 305, 172, 195, 160, 1, 2000, 195, 160, 208, 123, 1
DATA  2000, 331, 101, 342, 136, 1, 2000, 342, 136, 305, 172, 1, 2000, 235
DATA  121, 261, 101, 1, 2000, 261, 101, 271, 102, 1, 2000, 271, 102, 262
DATA  110, 1, 2000, 262, 110, 288, 103, 1, 2000, 288, 103, 299, 103, 1
DATA  2000, 299, 103, 259, 115, 1, 2000, 259, 115, 275, 126, 1, 2000, 275
DATA  126, 259, 124, 1, 2000, 259, 124, 254, 116, 1, 2000, 254, 116, 247
DATA  122, 1, 2000, 247, 122, 234, 122, 1, 4000, 252, 112, 1, 1, 4000
DATA  326, 134, 8, 1, 4000, 267, 141, 7, 1, 4000, 295, 118, 7, 1
DATA  4000, 256, 111, 12, 7, 2000, 234, 123, 248, 123, 4, 2000, 248, 123
DATA  248, 122, 4, 2000, 248, 122, 254, 117, 4, 2000, 261, 126, 275, 126
DATA  4, 2000, 260, 126, 258, 122, 4, 2000, 260, 115, 299, 104, 4, 2000
DATA  271, 102, 264, 108, 4, 2000, 234, 124, 248, 124, 4, 2000, 248, 124
DATA  249, 123, 4, 2000, 249, 123, 249, 122, 4, 2000, 249, 122, 254, 118
DATA  4, 2000, 261, 127, 275, 127, 4, 2000, 261, 115, 300, 105, 4, 2000
DATA  300, 105, 293, 106, 4, 2000, 265, 108, 271, 103, 4, 2000, 156, 171
DATA  202, 138, 7, 2000, 156, 171, 313, 191, 7, 2000, 313, 191, 356, 133
DATA  7, 2000, 356, 133, 342, 131, 7, 2000, 342, 131, 344, 136, 7, 2000
DATA  344, 136, 306, 172, 7, 2000, 306, 172, 305, 173, 7, 2000, 305, 173
DATA  195, 161, 7, 2000, 195, 161, 194, 159, 7, 2000, 194, 159, 202, 137
DATA  7, 4000, 243, 177, 7, 7, 2000, 201, 139, 194, 161, 7, 2000, 194
DATA  161, 200, 142, 7, 2000, 200, 142, 201, 140, 7, 2000, 201, 140, 201
DATA  142, 7, 2000, 201, 142, 193, 148, 7, 2000, 193, 148, 200, 145, 7
DATA  2000, 321, 157, 332, 146, 7, 2000, 341, 139, 344, 136, 7, 2000, 344
DATA  136, 341, 139, 7, 2000, 342, 132, 344, 134, 7, 2000, 344, 134, 343
DATA  133, 7, 2000, 343, 133, 342, 133, 7, 2000, 313, 191, 312, 208, 8
DATA  2000, 312, 208, 356, 147, 8, 2000, 356, 147, 356, 133, 8, 2000, 356
DATA  133, 313, 190, 8, 2000, 311, 208, 155, 189, 7, 2000, 155, 189, 156
DATA  171, 7, 2000, 311, 192, 312, 192, 7, 4000, 335, 170, 8, 8, 2000
DATA  312, 192, 312, 208, 7, 4000, 262, 195, 7, 7, 2000, 312, 190, 156
DATA  171, 8, 2000, 156, 171, 203, 136, 8, 2000, 155, 189, 156, 171, 8
DATA  2000, 155, 189, 313, 208, 8, 2000, 355, 133, 341, 130, 8,65535


'--------------------------------------------------------------------
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
        IF stroka = 6 THEN GOSUB quit: RETURN
        IF stroka = 5 THEN
                SCREEN 0
                CLS
                PRINT "Наберите EXIT для возврата..."
                SHELL
                pereris = 0
                GOSUB readyscreen
                GOSUB outall
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
               pereris = 0
                GOSUB readyscreen
                GOSUB outall
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
                pereris = 0
                GOSUB readyscreen
                GOSUB outall
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
'------------------------------------------------------
'-=3=----------- Выбор графического режима ------------
'------------------------------------------------------
graph:
        pun$(1) = "Точка                     "
        pun$(2) = "Линия                     "
        pun$(3) = "Круг                      "
        pun$(4) = "Окраска                   "
        pun$(5) = "Дуга                      "
        pun$(6) = "Прямоугольник             "
        pun$(7) = "Закрашенный прямоугольник "
        pun$(8) = "Непрерывная линия         "
        pun$(9) = "Эллипс                    "

        uplx = 15
        uply = 4
        ml = 26
        num = 9
        GOSUB menu
        IF stroka > 9 THEN RETURN
        IF stroka <= 9 THEN mode = stroka
        clfl = 0
        GOSUB statusline
        RETURN
'------------------------------------------
'-=4=-----Выбор текущего цвета-------------
'------------------------------------------
colors:
        winx = 15
        winy = 5
        winl = 49
        winh = 2
        wincol = 8
        winborcol = 1
cols1:
        IF mmouse = 1 THEN col = INT((rt / 8) * 14 / 80 + .5) + 1
        GOSUB putwindow
        FOR i = 0 TO 14
        LINE (winx * 8 + i * 25, (winy - 1) * 14 + 3)-(winx * 8 + (i + 1) * 25, (winy + 1) * 14 - 3), i + 1, BF
        LINE (winx * 8 + i * 25, (winy - 1) * 14 + 3)-(winx * 8 + (i + 1) * 25, (winy + 1) * 14 - 3), 1, B
        NEXT i
        LINE (winx * 8, (winy - 1) * 14 + 3)-(winx * 8 + 15 * 25, (winy + 1) * 14 - 3), 1, B
        GOSUB curcol
sopros:
        flag = 0
        io = mousex
art:    GOSUB inke1
        rt = mousex
        IF mmouse = 1 THEN IF io <> rt THEN GOTO obba
        IF a$ = "" THEN GOTO art
obba:    GOSUB coll
        IF mmouse = 0 THEN
        IF k = 77 THEN IF col < 15 THEN col = col + 1
        IF k = 75 THEN IF col > 1 THEN col = col - 1
        ELSE
        col = INT((rt / 8) * 14 / 80 + .5) + 1
        END IF
        GOSUB statusline
        GOSUB curcol
        IF a = 13 OR a = 32 THEN GOTO ecol
        IF a = 27 THEN flag = 15: GOTO ecol
        GOTO sopros
ecol:
        GOSUB killwindow
RETURN
'-----------------------------------------------
'-=5=----Меню увеличения------------------------
'-----------------------------------------------
magnify:
        pun$(1) = "Увеличение в 2 раза "
        pun$(2) = "Увеличение в 3 раза "
        pun$(3) = "Увеличение в 4 раза "
        pun$(4) = "Нормальный размер   "
        pun$(5) = "Включить сетку      "
        pun$(6) = "Выключить сетку     "
        pun$(7) = "Цвет сетки          "
        pun$(8) = "Выход               "
        uplx = 35
        uply = 4
        ml = 20
        num = 7
        GOSUB menu
        fff = magnify
        IF stroka > 7 THEN RETURN
        IF stroka = 7 THEN
                winx = 15
                winy = 15
                winl = 49
                winh = 2
                wincol = 8
                winborcol = 1
                ttt = col
                col = gridcol
                GOSUB cols1
                gridcol = col
                col = ttt
                GOSUB statusline
        END IF
        IF stroka = 1 THEN magnify = 2
        IF stroka = 2 THEN magnify = 3
        IF stroka = 3 THEN magnify = 4
        IF stroka = 4 THEN magnify = 1: smex = 0: smey = 0: smexa = 0: smeya = 0
        IF stroka = 5 THEN grid = 1
        IF stroka = 6 THEN grid = 0
        IF fff = 1 AND magnify <> 1 THEN
                        ux = cx
                        uy = cy
                        mumu = 1
                        winx = 2
                        winy = 2
                        winl = 24
                        winh = 1
                        GOSUB putwindow1
                        LOCATE winy, winx
                        COLOR 10
                       
                        PRINT "Введите центр увеличения"
snova:
                        GOSUB setcursor
                        GOSUB cursor
                        IF escf = 1 THEN mumu = 0: GOSUB killwindow1: RETURN
                        IF cy <= 33 THEN GOTO snova
                        GOSUB killwindow1
                        mumu = 0
                        smexa = 320 + cx
                        smeya = 120 + cy
                        smex = (320 - cx * magnify)
                        smey = (120 - cy * magnify)
                        cx = ux
                        cy = uy
                        END IF
        CLS
        GOSUB outall
        GOSUB setcursor
        GOSUB killcursor
        pereris = 1
        GOSUB readyscreen
        pereris = 0
        RETURN
'------------------------------------------------
'-=6=------Текст---------------------------------
'------------------------------------------------
TEXT:
        pun$(1) = "Сверху - вниз   "
        pun$(2) = "Слева - направо "
        uplx = 47
        uply = 4
        ml = 16
        num = 2
        GOSUB menu
        IF stroka > 100 THEN RETURN
        IF stroka = 1 THEN ax = 0: ay = 1 ELSE ax = 1: ay = 0
        tex = 4: tey = 5
        ess = 0
        COLOR col
        PUT (tex * 8, tey * 14), tcurs, XOR
entertext:
        GOSUB inke
        PUT (tex * 8, tey * 14), tcurs, XOR
        IF a = 27 THEN RETURN
        IF a = 0 THEN
        IF k = 77 THEN IF tex < 77 THEN tex = tex + 1 ELSE tex = 2
        IF k = 75 THEN IF tex > 2 THEN tex = tex - 1 ELSE tex = 77
        IF k = 80 THEN IF tey < 21 THEN tey = tey + 1 ELSE tey = 3
        IF k = 72 THEN IF tey > 3 THEN tey = tey - 1 ELSE tey = 21
        IF k = 77 OR k = 75 OR k = 80 OR k = 82 OR k = 13 THEN ess = 0
        END IF

        IF a = 13 THEN ess = 0
        IF a <> 0 THEN
        IF a > 31 THEN
        LOCATE tey + 1, tex + 1: PRINT CHR$(a)
        IF ess = 0 THEN
          pc(current) = 9000
          pc(current + 1) = tex
          pc(current + 2) = tey
          pc(current + 3) = col
          pc(current + 4) = 1
          dud = current + 4
          pc(current + 5) = a
          current = current + 6
        ess = 5
    END IF
   END IF
        IF ess = 1 THEN pc(dud) = pc(dud) + 1: pc(current) = a: current = current + 1
        IF ess = 5 THEN ess = 1
        tex = tex + ax
        IF tex > 76 THEN tex = 3: IF tey < 20 THEN tey = tey + 1
        IF tey < 20 THEN tey = tey + ay
        IF a = 13 THEN tex = 2: IF tey < 20 THEN tey = tey + 1
        END IF
        PUT (tex * 8, tey * 14), tcurs, XOR
        GOTO entertext
wyhoda:
        PUT (tex * 8, tey * 8), tcurs, XOR
        RETURN
'---------------------------------------------------------
'-=7=------------- ПРОЦЕДУРА ОЧИСТКИ ЭКРАНА --------------
'---------------------------------------------------------
clearscreen:
        winx = 46
        winy = 5
        winh = 1
        winl = 25
        wincol = 4
        winborcol = 10
        GOSUB putwindow1
        LOCATE winy, winx
        PRINT "Вы хотите очистить экран?"
        pun$(1) = "Да, пожалуйста! "
        pun$(2) = "Нет, спасибо!   "
        pun$(3) = "Вернуть картинку"

        uplx = 50
        uply = 6
        ml = 16
        num = 3
        GOSUB menu
        IF stroka = 90 OR stroka = 1 THEN
        lf = 0
        cf = 0
        bfl = 0
        fbfl = 0
        tf = 0
        clfl = 0
        arfl = 0
        GOSUB readyscreen: scur = current: current = 1
        END IF
        GOSUB setcursor
        IF stroka <> 1 THEN GOSUB killwindow1
        GOSUB killcursor
        IF stroka = 3 THEN current = scur: CLS : GOSUB outall: pereris = 1: GOSUB readyscreen
        RETURN
'--------------------------------------------------------------
'-=8=---------Расширенные функции------------------------------
'--------------------------------------------------------------
addi:
        pun$(1) = "Просмотреть программу        "
        pun$(2) = "Удалить последний элемент    "
        pun$(3) = "Удалить произвольный элемент "
        pun$(4) = "Очистка экрана               "
IF cmouse = 1 THEN pun$(5) = "Управление курсором " + "мышкой   "
IF mmouse = 1 THEN pun$(6) = "Управление меню     " + "мышкой   "
IF cmouse = 0 THEN pun$(5) = "Управление курсором " + "клавишами"
IF mmouse = 0 THEN pun$(6) = "Управление меню     " + "клавишами"
                
        pun$(7) = "Выбор шага курсора           "
IF stepout = 0 THEN pun$(8) = "Пошаговый вывод выключен     "
IF stepout = 1 THEN pun$(8) = "Пошаговый вывод включен      "
         pun$(9) = "Заново вывести картинку      "
        pun$(10) = "Выход                        "
        uplx = 50
        uply = 4
        ml = 29
        num = 10
        GOSUB menu
        IF stroka = 5 THEN cmouse = 1 - cmouse: GOTO addi
        IF stroka = 6 THEN mmouse = 1 - mmouse: GOTO addi
        IF stroka = 4 THEN GOSUB clearscreen: RETURN
        IF stroka = 10 THEN GOSUB quit: RETURN
        IF stroka = 8 THEN stepout = 1 - stepout
        IF stroka = 3 THEN GOSUB delete
        IF stroka = 9 THEN CLS : GOSUB outall: pereris = 1: GOSUB readyscreen
        IF stroka = 1 THEN
                CLS
                k = 0
                d = 1
                deln = 0
sttr1:
                IF k = 20 THEN
                LOCATE 24, 1
                PRINT "Для продолжения нажмите любую клавишу...";
                GOSUB inke
                IF a = 27 THEN CLS : GOSUB outall: pereris = 1: GOSUB readyscreen: RETURN
                CLS
                k = 0
                END IF
                k = k + 1
                IF d >= current THEN GOTO rtyu1
                te = pc(d)
        IF te = 1000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        c = pc(d + 3)
                        tr$ = "PSET (" + STR$(x) + ", " + STR$(y) + ")," + STR$(c)
                        d = d + 4
                END IF
        IF te = 2000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        x1 = pc(d + 3)
                        y1 = pc(d + 4)
                        c = pc(d + 5)
                        tr$ = "LINE (" + STR$(x) + ", " + STR$(y) + ")-(" + STR$(x1) + ", " + STR$(y1) + ")," + STR$(c)
                        d = d + 6
                END IF
        IF te = 3000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        r = pc(d + 3)
                        c = pc(d + 4)
                        tr$ = "CIRCLE (" + STR$(x) + ", " + STR$(y) + "), " + STR$(r) + ", " + STR$(c)
                        d = d + 5
                END IF
        IF te = 4000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        c = pc(d + 3)
                        c1 = pc(d + 4)
                        tr$ = "PAINT (" + STR$(x) + ", " + STR$(y) + "), " + STR$(c) + ", " + STR$(c1)
                        d = d + 5
                END IF
        IF te = 5000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        c = pc(d + 3)
                        c1 = pc(d + 4)
                        tr$ = "CIRCLE (" + STR$(x) + ", " + STR$(y) + "), " + STR$(c) + ", " + STR$(c1)
                        d = d + 5
                END IF
        IF te = 6000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        x1 = pc(d + 3)
                        y1 = pc(d + 4)
                        c = pc(d + 5)
                        tr$ = "LINE (" + STR$(x) + ", " + STR$(y) + ")-(" + STR$(x1) + ", " + STR$(y1) + "), " + STR$(c) + ", B"
                        d = d + 6
                END IF
        IF te = 7000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        x1 = pc(d + 3)
                        y1 = pc(d + 4)
                        c = pc(d + 5)
                        tr$ = "LINE (" + STR$(x) + ", " + STR$(y) + ")-(" + STR$(x1) + ", " + STR$(y1) + "), " + STR$(c) + ", BF"
                        d = d + 6
                END IF
        IF te = 8000 THEN
                        x = pc(d + 1)
                        y = pc(d + 2)
                        r = pc(d + 3)
                        a1 = pc(d + 4)
                        a2 = pc(d + 5)
                        c = pc(d + 6)
                        d = d + 7
                        tr$ = "ARC (" + STR$(INT(x)) + "," + STR$(INT(y)) + "," + STR$(INT(r)) + "," + STR$(a1) + "," + STR$(a2) + "," + STR$(c) + ")"


                END IF
        IF te = 8500 THEN
        elx = pc(d + 1)
        ely = pc(d + 2)
        elr1 = pc(d + 3)
        elr2 = pc(d + 4)
        c = pc(d + 5)
        d = d + 6
        tr$ = "CIRCLE (" + STR$(elx) + "," + STR$(ely) + ")," + STR$(INT(elr1)) + "," + STR$(col) + ", , ," + STR$(elr2 / elr1)

        END IF
        IF te = 9000 THEN
                        tr$ = ""
                        ttx = pc(d + 1)
                        tty = pc(d + 2)
                        ttc = pc(d + 3)
                        ttn = pc(d + 4)
                        d = d + 4
                        tr$ = "COLOR " + STR$(ttc)
                        tr$ = tr$ + ": LOCATE " + STR$(tty + 1) + ", " + STR$(ttx + 1)
                        tr$ = tr$ + ": PRINT " + CHR$(34)
caca:
                        ttn = ttn - 1
                        d = d + 1
                        tr$ = tr$ + CHR$(pc(d))
                        IF ttn <> 0 THEN GOTO caca
                        tr$ = tr$ + CHR$(34)
                        d = d + 1
                END IF
kukue:
                deln = deln + 1
                PRINT deln; ": "; tr$

        GOTO sttr1

rtyu1:
        IF INKEY$ = "" THEN GOTO rtyu1
        CLS
        yui = stepout
        stepout = 0
        GOSUB outall
        pereris = 1
        GOSUB readyscreen
        stepout = yui
END IF

        IF stroka = 2 THEN
        IF current < 2 THEN RETURN
cyc:
        current = current - 1
        IF pc(current) >= 1000 THEN
                 IF mode = 8 AND pc(current) = 2000 THEN clx = (pc(current + 1) - smex) / magnify: cly = (pc(current + 2) - smey) / magnify: clfl = 1
                        pc(current) = 0
                        GOSUB readyscreen
                        GOSUB outall
                        pereris = 1
                        GOSUB readyscreen
                        pereris = 0
                        RETURN
                END IF
        GOTO cyc
        END IF
IF stroka = 7 THEN
        winx = 10
        winy = 4
        winh = 4
        winl = 7
        wincol = 9
        winborcol = 14
        GOSUB putwindow1
        LINE ((winx + winl - 1) * 8 - 1, winy * 14 - 14)-((winx + winl) * 8 - 54, winy * 14 - 14), 5
        LINE ((winx + winl - 1) * 8 - 1, winy * 14 - 14)-((winx + winl - 1) * 8 - 1, winy * 14 + 33), 5
        LINE ((winx + winl) * 8 - 56, winy * 14 - 14)-((winx + winl - 1) * 8 - 1, winy * 14 + 33), 5
        PAINT ((winx + winl - 1) * 8 - 3, winy * 14 - 13), 5, 5
cursize:
        ddx = winx * 8 + 48 - mstep
        ddy = (winy + winh - 1) * 14 - mstep - 8
        LINE (winx * 8 - 2, ddy)-(winx * 8 + 47, ddy), 15
        GOSUB inke
        LINE (ddx - 1, ddy)-(winx * 8 - 2, ddy), 9
        LINE (ddx, ddy)-(winx * 8 + 47, ddy), 5
        IF k = 80 THEN IF mstep > 1 THEN mstep = mstep - 1
        IF k = 72 THEN IF mstep < 48 THEN mstep = mstep + 1
        GOSUB statusline
        IF (a <> 13) AND (a <> 27) THEN GOTO cursize
        GOSUB killwindow1
END IF
RETURN
'-------------DELETE ELEMENT------------------
delete:
        oi$ = nam$
        nam$ = ""
        ix = 10
        iy = 10
        il = 38

        nam$ = "Введите номер удаляемого элемента: "
        GOSUB inname
        k = 0
yu:     k = k + 1
        IF k > LEN(nam$) THEN RETURN
        IF MID$(nam$, k, 1) < "0" OR MID$(nam$, k, 1) > "9" THEN GOTO yu
        delnum = VAL(MID$(nam$, k, LEN(nam$) - k + 1))
        GOSUB deletet
        nam$ = oi$
        CLS
        yui = stepout
        stepout = 0
        GOSUB outall
        pereris = 1
        GOSUB readyscreen
        stepout = yui
RETURN

'--------------DELETE ELEMENT WITH NUMBER delnum ----------
deletet:
        uio = 0
        nut = 0
cycle:
        uio = uio + 1
        IF pc(uio) > 900 AND pc(uio) = INT(pc(uio)) THEN nut = nut + 1
        IF uio >= current THEN RETURN
        IF nut <> delnum THEN GOTO cycle
        uiot = uio
cycle1:
        uiot = uiot + 1
        IF pc(uiot) < 900 OR pc(uiot) <> INT(pc(uiot)) THEN GOTO cycle1
        FOR itt = 1 TO current - uiot
        pc(uio) = pc(uiot)
        uio = uio + 1
        uiot = uiot + 1
        NEXT itt
        PRINT current
        current = current - (uiot - uio)
        PRINT current
        RETURN

FUNCTION max (a, b)
IF a >= b THEN max = a ELSE max = b
END FUNCTION

FUNCTION min (a, b)
IF a >= b THEN min = b ELSE min = a
END FUNCTION

FUNCTION mouseb
regs.ax = &H3
CALL INTERRUPT(&H33, regs, regs)
mouseb = regs.bx
END FUNCTION

FUNCTION mousex
regs.ax = &H3
CALL INTERRUPT(&H33, regs, regs)
mousex = regs.cx
END FUNCTION

FUNCTION mousey
regs.ax = &H3
CALL INTERRUPT(&H33, regs, regs)
mousey = regs.dx
END FUNCTION

