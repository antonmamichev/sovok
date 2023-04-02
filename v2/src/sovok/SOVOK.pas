{$A+,B-,D+,E+,F-,G-,I+,L+,N+,O-,P-,Q-,R-,S+,T-,V+,X+}
{$M 11000,0,655360}
{(C) 1998 by Digger From Delta Hackers Group}
{Hardly programming till late night and early morning!}
{Nothing wIlL be uNpaID}
{$I-,F+}
{setWRITEmode}
PROGRAM sovok;
USES dos,mouses,graph,crt;
CONST maxx=640;
      maxy=350;
      maxp=500;
      maxl=800;
      maxa=300;
      maxc=300;
      maxe=300;
      maxr=300;
      maxb=300;
      maxf=200;
      maxt=20;
      maxel=maxp+maxl+maxa+maxc+maxe+maxr+maxb+maxf+maxt;
      ars=1;
      mscp=5;
      versionname='4.3.0 BETA';
type
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
source=FUNCTION (n:INTEGER):STRING;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
type
button=object
caption:STRING;
x,y,lx,ly,col:INTEGER;
active,visual:boolean;
CONSTRUCTOR init(x1,y1,lx1,ly1:INTEGER;name1:STRING);
PROCEDURE show(status:boolean);
PROCEDURE hide;
FUNCTION onclick(ax,ay:INTEGER):boolean;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
panel=object
x,y,lx,ly,col,cols,colrs,size:INTEGER;
   visual,  saving:boolean;
                 p:pointer;
CONSTRUCTOR init(x1,y1,lx1,ly1,col1,col2,col3:INTEGER;savin1:boolean);
FUNCTION onclick(mx,my:word):boolean;
PROCEDURE show;
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
menu=object
fields:array[1..15] of STRING[20];
p:pointer;
prev,nfields,tx,ty,lx,ly:INTEGER;
size:word;
visual:boolean;
FUNCTION init(n:INTEGER;x:STRING;y:INTEGER):INTEGER;
PROCEDURE coord(x1,y1:INTEGER);
PROCEDURE show;
PROCEDURE hide;
FUNCTION onclick(mx,my:word):boolean;
FUNCTION getfield(mx,my:word):INTEGER;
END;
gonza=set of char;
ganza=array[1..150]of STRING[12];
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
querybar=object
caption,query:STRING;
x,y,nm,ly,lx:INTEGER;
visual,moving:boolean;
symbols:gonza;
pan:panel;
CONSTRUCTOR init(x1,y1,nm1:INTEGER;quer1:STRING;symbol1:gonza;movin:boolean);
PROCEDURE show;
PROCEDURE use;
FUNCTION onclick(mx,my:word):boolean;
PROCEDURE changecoord(xa,ya:INTEGER);
PROCEDURE move;
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
statusbar=object
    x0,y0,lx0,ly0:INTEGER;
    st1,st2,st3,st4:button;
    stpan,inkc,papc:panel;
    visual:boolean;
CONSTRUCTOR init(x,y:INTEGER;filename,mode,magnIFy:STRING;step:boolean;papcol,inkcol:INTEGER);
PROCEDURE show;
PROCEDURE showwr;
FUNCTION onclick(mx,my:word):boolean;
PROCEDURE refresh(filename,mode,magnIFy:STRING;step:boolean;papcol,inkcol:INTEGER);
PROCEDURE changecoord(nx,ny:INTEGER);
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
vscrollbar=object
x,y,lx,ly,number,len,lbar,h:longint;
visual:boolean;
smes:longint;
mpan,movp:panel;
up,down:button;
CONSTRUCTOR init(x1,y1,lx1,ly1,number1,len1,smes1:INTEGER);
PROCEDURE show;
PROCEDURE setsmes(n:INTEGER);
FUNCTION smesh:INTEGER;
FUNCTION move(mx,my:word):INTEGER;
FUNCTION onclick(mx,my:word):boolean;
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
hscrollbar=object
x,y,lx,ly,number,len,lbar,h:longint;
visual:boolean;
smes:longint;
mpan,movp:panel;
left,right:button;
CONSTRUCTOR init(x1,y1,lx1,ly1,number1,len1:INTEGER;smes1:INTEGER);
PROCEDURE show;
PROCEDURE setsmes(n:INTEGER);
FUNCTION smesh:INTEGER;
FUNCTION move(mx,my:word):INTEGER;
FUNCTION onclick(mx,my:word):boolean;
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
scrollbar=object
x,y,lx,ny,all,smg,sml:INTEGER;
      sou:source;
active,saving,more:boolean;
moverec:vscrollbar;
master,slave:panel;
CONSTRUCTOR init(x1,y1,lx1,ny1,all1:INTEGER;sou1:source;savin1:boolean);
PROCEDURE show;
PROCEDURE outallrecs;
PROCEDURE inccursor;
PROCEDURE deccursor;
FUNCTION use:boolean;
FUNCTION onclick(mx,my:INTEGER):boolean;
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
selectcursorstep=object
x1,y1,lx,ly,curxstep,curystep: INTEGER;
itspanelslave,itspanelmaster,lightbulb: panel;
itsstepx: hscrollbar;
itsstepy: vscrollbar;
btok,btswitch: button;
CONSTRUCTOR init(x,y,xs,ys: INTEGER);
PROCEDURE show;
PROCEDURE use;
PROCEDURE getsteps(VAR viax,viay:INTEGER);
PROCEDURE hide;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
type
ginza=array[1..maxt]of STRING[94];
graphboard=object
x,y,x1,y1:INTEGER;
board:panel;
hscro:hscrollbar;
vscro:vscrollbar;
scrov,scroh:boolean;
fit:button;
magnify:REAL;
elements:array[1..maxel,1..2] of INTEGER;
points:array[1..maxp*3] of INTEGER;
lines:array[1..maxl*5] of INTEGER;
arcs:array[1..maxa*6] of INTEGER;
circles:array[1..maxc*4] of INTEGER;
ellipses:array[1..maxe*5] of INTEGER;
rectangles:array[1..maxr*5] of INTEGER;
bars:array[1..maxb*5] of INTEGER;
fills:array[1..maxf*4] of INTEGER;
{texts:ginza;}
npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,ntexts:INTEGER;
nelements:INTEGER;
CONSTRUCTOR init(magnIFy1:real);
PROCEDURE onclick(mx,my:INTEGER);
PROCEDURE show;
PROCEDURE refreshm(sx,sy:INTEGER);
PROCEDURE magnyx(mx,my:INTEGER);
PROCEDURE demagnyx(mx,my:INTEGER);
PROCEDURE outall(step:boolean;startvalue:integer);
PROCEDURE save(name:STRING);
PROCEDURE load(name:STRING);
PROCEDURE merge(name:STRING);
PROCEDURE makeBASICblock(name:STRING);
PROCEDURE outelement(i:INTEGER);
PROCEDURE delelement(n:INTEGER);
PROCEDURE delmarked;
FUNCTION fitonscreen:real;
FUNCTION magnx(numb:INTEGER):INTEGER;
FUNCTION magny(numb:INTEGER):INTEGER;
FUNCTION divix(numb:INTEGER):INTEGER;
FUNCTION diviy(numb:INTEGER):INTEGER;
PROCEDURE hide;
END;
VAR gd,gm:INTEGER;
    files:ganza;
    bhelp,bfiles,bgraph,bcol,bmag,{btext,}be:button;
    mhelp,mfiles,mgraph,mcol,mmag,{mtext,}me,mm:menu;
    curstep: selectcursorstep;
    inputnumber,startingout:querybar;
    filenames:scrollbar;
    stat:statusbar;
    hscro,ttt:hscrollbar;
    vscro,vscro1:vscrollbar;
    colors:array [0..16] of button;
    mark:array[1..maxel] of boolean;
    upmenu,pcolors,information,noticegraph:panel;
    board:graphboard;
    mx,my,mb:word;
{    font:file of char;}
    fontop:boolean;
    {Программные переменные}
    fbold,fotp,event,coll,step:boolean;
    moldx,moldy:word;
    cfont,fsx,fsy,fosx,fosy,fodi,fossx,fossy,inkcol,papcol,fcou,mode,activeboard,
    nboards,undo,prevx,prevy,xstep,ystep:INTEGER;
    gfname,path,prevtime:STRING;
    modes:array[1..20] of STRING[20];
    tempstep:boolean;
{Процедура выхода}
PROCEDURE exitsovok(n:INTEGER);
BEGIN
board.save('undo.und');
{closefont;}
closegraph;
clrscr;
chdir(path);
WRITELN('SOVOK GRAPHICS EDITOR Version '+versionname);
WRITELN('(C) 2003 Мамичев Антон. director@azov.info; http://www.azov.info');
WRITELN('Текущая картинка сохранена в файле undo.und.');
WRITELN('Для восстановления используйте пункт Вернуть картинку в меню Дополнения.');
halt;
END;


{++++++++++++++++++++++++++++++++++++++++++++++++++++}
{Подпрограммы для работы с Мышкой}
{++++++++++++++++++++++++++++++++++++++++++++++++++++}
PROCEDURE mouseread1(VAR x,y,xb:word);
VAR x1,y1:word;
        c:char;
        xstep,ystep:INTEGER;
BEGIN
curstep.getsteps(xstep,ystep);
mouseread(x,y,xb);
IF step THEN BEGIN
             IF keypressed THEN
             BEGIN
                c := readkey;
                IF c in ['2','4','6','8',#27] THEN
                   BEGIN
                     case c of
                         '6':x := x+xstep;
                         '4':x := x-xstep;
                         '8':y := y-xstep;
                         '2':y := y+xstep;
                         #27:exitsovok(0);
                     END;
                    mousesetpos(x,y);
                   END;
             END;
END;
END;

FUNCTION mbutton:boolean;
VAR dfmx,dfmy,dfmb:word;
BEGIN
mbutton := FALSE;
mouseread(dfmx,dfmy,dfmb);
IF dfmb<>0 THEN mbutton := TRUE;
END;

FUNCTION mlbutton:boolean;
VAR mx,my,mb:word;
BEGIN
mlbutton := FALSE;
mouseread1(mx,my,mb);
IF mb=1 THEN mlbutton := TRUE;
END;

FUNCTION mrbutton:boolean;
VAR mx,my,mb:word;
BEGIN
mrbutton := FALSE;
mouseread1(mx,my,mb);
IF mb=2 THEN mrbutton := TRUE;
END;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++}


CONSTRUCTOR selectcursorstep.init;
VAR sme: INTEGER;
BEGIN
sme:=2;
x1:=x+sme;
y1:=y+sme;
lx:=120-sme*2;
ly:=120-sme*2;
curxstep:=xs;
curystep:=ys;
itspanelmaster.init(x1-sme,y1-sme,lx+sme*2,ly+20,7,8,0,true);
itspanelslave.init(x1,y1,lx,ly,7,8,0,false);

itsstepx.init(x1,y1+ly-13,lx-17,13,lx,30,curxstep);
itsstepy.init(x1+lx-15,y1,15,ly,ly,30,curystep);

btok.init(x1+lx-45,y1+ly+3,45,12,'OK');
btswitch.init(x1+19,y1+ly+3,49,12,'ШАГ');
lightbulb.init(x1+2,y1+ly+3,14,12,4,12,0,false);

END;

PROCEDURE selectcursorstep.show;
BEGIN
itspanelmaster.show;
itspanelslave.show;
itsstepx.show;
itsstepy.show;
lightbulb.show;
btok.show(false);
btswitch.show(false);
setfillstyle(1,1);
bar(x1+1,y1+1,x1+curxstep,y1+curystep);

END;

PROCEDURE selectcursorstep.getsteps(VAR viax,viay:INTEGER);
BEGIN
viax:=curxstep;
viay:=curystep;
END;

PROCEDURE selectcursorstep.use;
VAR mx,my,mb:word;
    exxiit:BOOLEAN;
BEGIN
mouseshow;
mousehide;
show;
mouseshow;
exxiit:=false;
REPEAT
mouseread1(mx,my,mb);
if mb<>0 then begin
IF itsstepx.onclick(mx,my) THEN BEGIN
                          itsstepx.move(mx,my);
                          curxstep := itsstepx.smesh+1;

                          setfillstyle(1,7);
                          bar(x1+1,y1+1,x1+lx-18,y1+ly-18);

                          setfillstyle(1,1);
                          bar(x1+1,y1+1,x1+curxstep,y1+curystep);
                          END;
IF itsstepy.onclick(mx,my) THEN BEGIN
                          itsstepy.move(mx,my);
                          curystep := itsstepy.smesh+1;

                          setfillstyle(1,7);
                          bar(x1+1,y1+1,x1+lx-18,y1+ly-18);

                          setfillstyle(1,1);
                          bar(x1+1,y1+1,x1+curxstep,y1+curystep);

                          END;
IF btok.onclick(mx,my) THEN exxiit:=true;
IF btswitch.onclick(mx,my) THEN BEGIN
                                step:=not step;
                                if step THEN
                                lightbulb.init(x1+2,y1+ly+3,14,12,2,10,0,false)
                                ELSE
                                lightbulb.init(x1+2,y1+ly+3,14,12,4,12,0,false);
                                repeat until not mbutton;
                                lightbulb.show;
                                btswitch.show(false);
                                END;
end;
UNTIL exxiit or keypressed;
mousehide;
hide;
mouseshow;
repeat until not mbutton;
END;

PROCEDURE selectcursorstep.hide;
BEGIN
itspanelmaster.hide;
END;


{Нажатие клавиши (без задержки)}
FUNCTION nkeypressed(n:char):boolean;
BEGIN
nkeypressed := FALSE;
IF keypressed THEN IF readkey=n THEN nkeypressed := TRUE;
END;



FUNCTION koof:real;
  VAR Xasp, Yasp: Word;
BEGIN
  GetAspectRatio(Xasp, Yasp);
  koof  :=  (Xasp / Yasp);
END;

PROCEDURE circletr(x,y,r,r1:INTEGER);
VAR i,s:INTEGER;
    x1,y1,x0,y0:INTEGER;
 BEGIN
      setWRITEmode(xorput);
      s := 15;{round(140/(2*3.1415926*r/100))+1;}
      i := 0;
      x1 := x+round(r*cos((i)*3.14/180));
      y1 := y+round(r1*sin((i)*3.14/180){*koof});
      i := s;
       WHILE(i<360+s) do
         BEGIN
         x0 := x+round(r*cos((i)*3.14/180));
         y0 := y+round(r1*sin((i)*3.14/180){*koof});
         {IF (x0<>x1)and(y0<>y1) THEN}
         BEGIN line(x0,y0,x0,y0);line(x0,y0,x1,y1);END;
         x1 := x0;
         y1 := y0;
         I := I+s;
       END;
 setWRITEmode(0);
 END;

PROCEDURE swap(VAR a,b:INTEGER);
VAR c:INTEGER;
BEGIN
     c := a;
     a := b;
     b := c;
END;

PROCEDURE swapr(VAR a,b:real);
VAR c:real;
BEGIN
     c := a;
     a := b;
     b := c;
END;

PROCEDURE QuickSort(VAR A: ganza; Lo, Hi: INTEGER);
PROCEDURE Sort(l, r: INTEGER);
VAR
  i, j:INTEGER;
  x, y: STRING;
BEGIN
  i  :=  l; j  :=  r; x  :=  a[(l+r) DIV 2];
  REPEAT
    WHILE a[i] < x do i  :=  i + 1;
    WHILE x < a[j] do j  :=  j - 1;
    IF i <= j THEN
    BEGIN
      y  :=  a[i]; a[i]  :=  a[j]; a[j]  :=  y;
      i  :=  i + 1; j  :=  j - 1;
    END;
  UNTIL i > j;
  IF l < j THEN Sort(l, j);
  IF i < r THEN Sort(i, r);
END;
BEGIN {QuickSort};
  Sort(Lo,Hi);
END;

FUNCTION makecatalogue(VAR a:ganza;mask:STRING):INTEGER;
VAR
  Dirinfo: SearchRec;
  n,i:INTEGER;
  k:STRING;
BEGIN
  n := -1;
{  closegraph;}

  FindFirst(mask, 255, Dirinfo);
  WHILE (DosError = 0)and(n<100) do
  BEGIN
  n := n+1;
  a[n] := dirinfo.name;
  IF not (dirinfo.attr and 16 =16) THEN
  BEGIN
  k := a[n];
  for i := 1 to length(a[n]) do
  IF ((k[i]>='A')and(k[i]<='Z')) THEN k[i] := chr(ord(k[i])+ord('a')-ord('A'));
  a[n] := k;
  END else a[n] := a[n]+#255;
{  WRITELN(a[n],'  ',dirinfo.attr);}
    FindNext(Dirinfo);
  END;

  makecatalogue := n;
  quicksort(a,1,n);
END;

FUNCTION stri(n:INTEGER):STRING;
VAR f:STRING;
BEGIN
str(n,f);
IF (n<10)and(n>=0)THEN f := '0'+f;
IF (n<0)and(n>-10)THEN BEGIN str(abs(n),f);f := '-0'+f;END;
stri := f;
END;

FUNCTION makenumber(n:STRING):word;
VAR n1,n2:INTEGER;
BEGIN
n1 := ord(n[1]);
n2 := ord(n[2]);
makenumber := n1+256*n2;
END;

FUNCTION makeword(n:INTEGER):STRING;
VAR n1,n2:INTEGER;
BEGIN
makeword := '  ';
n1 := n div 256;
n2 := n-n*256;
makeword[2] := chr(n1);
makeword[1] := chr(n2);
END;

PROCEDURE getarcparam(arx2,ary2,arx,ary,arx1,ary1:real;VAR arax0,aray0,an1,an3,ra1:real);
VAR quarter:INTEGER;
         a,b,c,a1,b1,c1:real;
         arx0,ary0,an,an2:real;
FUNCTION arccos(cosr,sinr:real):INTEGER;
FUNCTION qua(s,c:real):INTEGER;
BEGIN
IF (s>0) and(c>0) THEN qua := 1 else IF (s>0) and(c<0) THEN qua := 2 else
IF (s<0) and(c<0) THEN qua := 3 else qua := 4;
END;
BEGIN
IF cosr<>0 THEN
BEGIN
an := arctan(sinr/cosr)*180/pi;quarter := qua(sinr,cosr);
IF (quarter=2)or(quarter=3) THEN an := an+180;
an := an+720;an := an-trunc(an/360)*360;arccos := round(an);
END else arccos := 90; END;
BEGIN
 a := 2*(ary-ary2); b := sqr(arx)-sqr(arx2)+sqr(ary)-sqr(ary2); c := 2*(arx-arx2);
a1 := 2*(arx-arx1);b1 := sqr(arx1)-sqr(arx)+sqr(ary1)-sqr(ary);c1 := 2*(ary-ary1);
arx0 := (b*c1+a*b1)/((c*c1-a*a1)+0.01);
ary0 := -(a1*arx0+b1)/(c1+0.01);
        ra1 := round(sqrt(sqr(arx2 - arx0)+sqr(ary2 - ary0)));
        an1 := arccos(arx2-arx0,ary2-ary0);
        an2 := arccos(arx1-arx0,ary1-ary0);
        an3 := arccos(arx-arx0,ary-ary0);
        IF an1>an3 THEN swapr(an1,an3);
        IF an2<an1 THEN BEGIN an3 := an3-360;swapr(an1,an3);END;
        IF an2>an3 THEN BEGIN an3 := an3-360;swapr(an1,an3);END;
arax0 := round(arx0);
aray0 := round(ary0);
END;

PROCEDURE arcnew(arx0,ary0,an1,an3,ra1,s:real);
VAR x1,y1,x0,y0,i:real;
BEGIN
i := an1;
x1 := round(arx0+ra1*cos(i*pi/180));
y1 := round(ary0+ra1*sin(i*pi/180));
i := an1+s;
WHILE(i<=an3) do
BEGIN
x0 := arx0+ra1*cos(i*pi/180);
y0 := ary0+ra1*sin(i*pi/180);
BEGIN line(round(x0),round(y0),round(x0),round(y0));line(round(x0),round(y0),round(x1),round(y1));END;
x1 := x0;
y1 := y0;
I := I+s;
END;
x0 := arx0+ra1*cos(an3*pi/180);
y0 := ary0+ra1*sin(an3*pi/180);
line(round(x0),round(y0),round(x0),round(y0));line(round(x0),round(y0),round(x1),round(y1));
END;

PROCEDURE panel3d(x,y,lx,ly:INTEGER;status:boolean;col:INTEGER);
VAR tx,ty:INTEGER;
BEGIN
     setfillstyle(1,col);
     bar(x+1,y+1,x+lx-1,y+ly-1);
case status of
FALSE:
     BEGIN
     setcolor(15);
     line(x,y,x+lx,y);
     line(x,y,x,y+ly);
     setcolor(0);
     line(x,y+ly,x+lx,y+ly);
     line(x+lx,y,x+lx,y+ly);
     END;
TRUE:
     BEGIN
     setcolor(0);
     line(x,y,x+lx,y);
     line(x,y,x,y+ly);
     setcolor(15);
     line(x,y+ly,x+lx,y+ly);
     line(x+lx,y,x+lx,y+ly);
     END;
END;
END;

PROCEDURE showtext(x,y,lx,ly:INTEGER;name:STRING;status:boolean;col:INTEGER);
VAR tx,ty:INTEGER;
BEGIN
     mousehide;
     tx := (lx div 2)-(8*length(name)) div 2+1;
     ty := (ly div 2)-8 div 2;
     panel3d(x,y,lx,ly,status,col);
     setcolor(0);
case status
of
FALSE: outtextxy(x+tx,y+ty+1,name);
TRUE: outtextxy(x+tx+1,y+ty+2,name);
END;
mouseshow;
END;

PROCEDURE refreshstatus;
BEGIN
stat.refresh(gfname,modes[mode],stri(round(100*board.magnIFy)),step,papcol,inkcol);
END;

PROCEDURE showtime;
VAR hh,mm,ss,ss100:word;
time:STRING;
BEGIN
gettime(hh,mm,ss,ss100);
time :=  stri(hh)+':'+stri(mm)+':'+stri(ss);
IF prevtime<>time THEN showtext(560,6,72,14,time,TRUE,7);
prevtime := time;
END;


{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
PROCEDURE pause(lon:word);
   label tt;
   BEGIN
   asm
   mov cx,lon
tt:hlt
   loop tt
   END;
 END;

FUNCTION inbar(mx,my,x1,y1,x2,y2:INTEGER):boolean;
BEGIN
IF (mx>x1)and(mx<x2)and(my>y1)and(my<y2)THEN inbar := TRUE else inbar := FALSE;
END;

{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
CONSTRUCTOR scrollbar.init;
BEGIN
x := x1;
y := y1;
lx := lx1;
ny := ny1;
all := all1;
IF all<ny THEN ny := all;
IF all=ny THEN more := FALSE else more := TRUE;
sou := sou1;
saving := savin1;
master.init(x,y,lx+12,ny*12+3,8,7,0,saving);
IF more THEN slave.init(x+2,y+1,lx-2,ny*12+1,15,8,7,FALSE) else
slave.init(x+2,y+1,lx+9,ny*12+1,15,8,7,FALSE);
IF more THEN moverec.init(x+lx+1,y,11,ny*12+2,all,ny,0);
active := FALSE;
END;
PROCEDURE scrollbar.inccursor;
BEGIN                   IF sml+1<board.nelements THEN BEGIN
                        sml := sml+1;
                        IF sml+1<=board.nelements THEN
                        IF sml>smg+ny-2 THEN
                        BEGIN
                        inc(smg);
                        moverec.setsmes(smg-1);
                        outallrecs;
                        END;
                        END;
END;
PROCEDURE scrollbar.deccursor;
BEGIN
                        IF sml>0 THEN BEGIN
                        sml := sml-1;
                        IF sml+1>1 THEN IF sml<smg-1 THEN
                        BEGIN
                        dec(smg);
                        moverec.setsmes(smg-1);
                        outallrecs;
                        END;
                        END;
END;
PROCEDURE scrollbar.outallrecs;
VAR i,lstep,size:INTEGER;
    p:pointer;
BEGIN
IF ny<>0 THEN BEGIN
mousehide;
i := 0;
setactivepage(1);
slave.show;
REPEAT
setcolor(0);
IF i+smg-1=sml THEN BEGIN setfillstyle(1,7);bar(slave.x+1,y+2+i*12,slave.x+slave.lx-1,y+1+(i+1)*12);END;
IF mark[i+smg] THEN BEGIN setfillstyle(1,7);bar(slave.x+1,y+2+i*12,slave.x+slave.lx-1,y+1+(i+1)*12);setcolor(4);END;
IF mark[i+smg] and(i+smg-1=sml) THEN BEGIN
   setfillstyle(1,8);
   bar(slave.x+1,y+2+i*12,slave.x+slave.lx-1,y+1+(i+1)*12);
   setcolor(4);
   END;
outtextxy(slave.x+2,slave.y+3+i*12,sou(i+smg));
i := i+1;
UNTIL (i=ny)or(i+smg>all);

lstep := (slave.ly) div ny;
size := imagesize(slave.x,slave.y,slave.x+slave.lx-1,slave.y+lstep+1);
getmem(p,size);
for i := 1 to ny do BEGIN
setactivepage(1);
getimage(slave.x,slave.y+(i-1)*lstep,slave.lx+slave.x-1,slave.y+i*lstep+1,p^);
setactivepage(0);
putimage(slave.x,slave.y+(i-1)*lstep,p^,0);
END;
freemem(p,size);
setactivepage(0);

mouseshow;
END;
END;

FUNCTION scrollbar.onclick;
BEGIN
IF inbar(mx,my,master.x,master.y,master.x+master.lx,master.y+master.ly) THEN onclick := TRUE else onclick := FALSE;
END;

PROCEDURE scrollbar.show;
VAR i:INTEGER;
BEGIN
for i := 1 to ny do mark[i] := FALSE;
master.show;
IF more THEN moverec.show;
smg := 1;sml := 0;
outallrecs;
active := TRUE;
END;
FUNCTION scrollbar.use;
VAR
    exi:boolean;
    ch:char;
    lp:INTEGER;
BEGIN
use := FALSE;
exi := FALSE;
mouseread1(mx,my,mb);
ch := '#';
IF keypressed THEN ch := readkey;
IF (ch=#13)or(ch=#27) THEN exi := TRUE;
IF ch=#0 THEN ch := readkey;

IF mb<>0 THEN
BEGIN
IF not inbar(mx,my,master.x,master.y,master.lx+master.x,master.ly+master.y) THEN exi := TRUE;
IF not exi THEN
    BEGIN
    IF more and moverec.onclick(mx,my) THEN BEGIN
                          moverec.move(mx,my);
                          smg := moverec.smesh+1;
                          outallrecs;
                          END else
       BEGIN
       use := TRUE;
       sml := (my-y-2)div(12);
       sml := sml+smg-1;
       IF mb=2 THEN mark[sml+1] := not mark[sml+1];
       outallrecs;
         REPEAT UNTIL not mbutton;
       END;
    END;
END;
END;

PROCEDURE scrollbar.hide;
BEGIN
IF more THEN moverec.hide;
slave.hide;
master.hide;
active := FALSE;
END;

FUNCTION filespisok(n:INTEGER):STRING;
BEGIN
filespisok := files[n];
END;

FUNCTION inputfilename(VAR gfn:STRING;mask:STRING):boolean;
VAR n,k,i:INTEGER;
    mainpan,secpan:panel;
    ntrflnm:querybar;
    dd,kk:STRING;
    bok,cancel:button;
    poi,exi:boolean;
    x,y,prev:INTEGER;
    ch:char;
BEGIN
inputfilename := FALSE;
n := makecatalogue(files,'*.*');
k := 10;
IF n<10 THEN k := n;
x := 100;
y := 100;
mainpan.init(x,y,199,183,8,7,0,TRUE);
bok.init(x+142,y+17,54,14,'OK');
cancel.init(x+142,y+36,54,14,'Отмена');
filenames.init(x+5,y+18,119,k,n,filespisok,FALSE);
secpan.init(x+2,y+16,136,127,8,7,0,FALSE);
ntrflnm.init(x+3,y+152,21,'Имя файла',['0'..'9','A'..'Z','.','a'..'z','_'],FALSE);
mainpan.show;
secpan.show;
filenames.show;
ntrflnm.caption := gfname{files[filenames.sml+1]};
ntrflnm.show;
bok.show(FALSE);
cancel.show(FALSE);
showtext(x+1,y+1,197,13,'Выберите файл',FALSE,1);
prev := -23;
REPEAT UNTIL not mbutton;
REPEAT
exi := FALSE;
mouseread1(mx,my,mb);
ch := ';';
IF keypressed THEN ch := readkey;
IF mbutton or(ch=#13)or(ch=#27)THEN
BEGIN
IF filenames.onclick(mx,my) THEN IF filenames.use
                                       THEN
                                       BEGIN
                                       dd := files[filenames.sml+1];
                                       IF dd[length(dd)]<>#255 THEN
                                       ntrflnm.caption := dd;
                                       ntrflnm.show;
                                       IF prev=filenames.sml THEN
                                         IF dd[length(dd)]=#255 THEN
                                          BEGIN
                                          chdir(copy(dd,1,length(dd)-1));
                                          n := makecatalogue(files,'*.*');
                                          k := 10;
                                          IF n<10 THEN k := n;
                                          filenames.hide;
                                          filenames.init(x+3,y+17,120,k,n,filespisok,FALSE);
                                          secpan.show;
                                          filenames.show;
                                          END else BEGIN exi := TRUE;inputfilename := TRUE;gfn := ntrflnm.caption;END;
                                       prev := filenames.sml;
                                       END;
IF (bok.onclick(mx,my))or(ch=#13) THEN BEGIN exi := TRUE;inputfilename := TRUE;gfn := ntrflnm.caption;END;
IF cancel.onclick(mx,my) or (ch=#27) THEN exi := TRUE;
IF ntrflnm.onclick(mx,my) THEN BEGIN
                               ntrflnm.use;
                               kk := ntrflnm.caption;
                               poi := FALSE;
                               for i := 1 to length(kk) do IF kk[i]='.' THEN poi := TRUE;
                               IF not poi THEN kk := kk+'.'+mask;
                               ntrflnm.caption := kk;
                               ntrflnm.show;
                               END;
END;
UNTIL exi;
pause(4);
ntrflnm.hide;
filenames.hide;
mainpan.hide;
END;


FUNCTION elem(n:INTEGER):STRING;
VAR strf,ggg:STRING;
    k:INTEGER;
BEGIN
strf := stri(n)+':'+modes[board.elements[n,1]];
k := board.elements[n,2];
case board.elements[n,1] of
1:strf := strf+' ('+stri(board.points[k*3+1])+','+stri(board.points[k*3+2])+'),'+stri(board.points[k*3+3])+';';
2:strf := strf+' ('+stri(board.lines[k*5+1])+','+stri(board.lines[k*5+2])+')-('+stri(board.lines[k*5+3])+
                     ','+stri(board.lines[k*5+4])+'),'+stri(board.lines[k*5+5])+';';
4:strf := strf+' ('+stri(board.arcs[k*6+1])+','+stri(board.arcs[k*6+2])+','+stri(board.arcs[k*6+3])+
                     ','+stri(round(board.arcs[k*6+4]/91))+','+stri(round(board.arcs[k*6+5]/91))+','+
                     stri(board.arcs[k*6+6])+');';
5:strf := strf+' ('+stri(board.circles[k*4+1])+','+stri(board.circles[k*4+2])+'),'+stri(board.circles[k*4+3])+
                     ','+stri(board.circles[k*4+4])+';';

6:strf := strf+' ('+stri(board.ellipses[k*5+1])+','+stri(board.ellipses[k*5+2])+','+stri(board.ellipses[k*5+3])+
                     ','+stri(board.ellipses[k*5+4])+'),'+stri(board.ellipses[k*5+5])+';';
7:strf := strf+' ('+stri(board.rectangles[k*5+1])+','+stri(board.rectangles[k*5+2])+')-('+stri(board.rectangles[k*5+3])+
                     ','+stri(board.rectangles[k*5+4])+'),'+stri(board.rectangles[k*5+5])+';';
8:strf := strf+' ('+stri(board.bars[k*5+1])+','+stri(board.bars[k*5+2])+')-('+stri(board.bars[k*5+3])+
                     ','+stri(board.bars[k*5+4])+'),'+stri(board.bars[k*5+5])+';';
9:strf := strf+' ('+stri(board.fills[k*4+1])+','+stri(board.fills[k*4+2])+'),'+stri(board.fills[k*4+3])+
                     ','+stri(board.fills[k*4+4])+';';
END;
elem := strf;
END;
FUNCTION marked:boolean;
VAR nas:INTEGER;
BEGIN
marked := FALSE;
for nas := 1 to board.nelements do IF mark[nas]=TRUE THEN marked := TRUE;
END;
PROCEDURE showprogram;
VAR mainpan,lef:panel;
    text:scrollbar;
    bok,cbut,del,sback,sforw,all,des,inv:button;
    x,y,lp,i:INTEGER;
    exi,ku:boolean;
    mx,my:word;
    ch:char;
    lyy:INTEGER;
BEGIN
IF board.nelements<>0 THEN BEGIN
x := 100;
y := 50;
mainpan.init(x,y,425,251,8,7,0,TRUE);
lef.init(x+2,y+2,356,247,8,7,0,FALSE);
text.init(x+4,y+4,340,20,board.nelements,elem,FALSE);
lyy := 15;
  bok.init(x+361,y+2+(lyy+3)*0,61,lyy,'OK');
 cbut.init(x+361,y+2+(lyy+3)*1,61,lyy,'Отмена');
  del.init(x+361,y+2+(lyy+3)*2,61,lyy,'Удалить');
sforw.init(x+361,y+2+(lyy+3)*3,61,lyy,'Вперед');
sback.init(x+361,y+2+(lyy+3)*4,61,lyy,'Назад');
  all.init(x+361,y+2+(lyy+3)*5,61,lyy,'+ все');
  des.init(x+361,y+2+(lyy+3)*6,61,lyy,'- все');
  inv.init(x+361,y+2+(lyy+3)*7,61,lyy,'Инверс');
board.save('undo.und');
mainpan.show;
lef.show;
bok.show(FALSE);
cbut.show(FALSE);
del.show(FALSE);
sback.show(FALSE);
sforw.show(FALSE);
all.show(FALSE);
des.show(FALSE);
inv.show(FALSE);
text.show;
mouseshow;
REPEAT UNTIL not mbutton;
REPEAT
mouseread1(mx,my,mb);
ch := 'f';
IF keypressed THEN ch := readkey;
IF ch=#0 THEN ch := readkey;
IF ch=#72 THEN text.deccursor;
IF ch=#80 THEN text.inccursor;
exi := FALSE;
IF (mb<>0)or(ch in[#13,#27]) THEN
BEGIN
IF mb<>0 THEN IF bok.onclick(mx,my) or (ch=#13) THEN exi := TRUE;
IF mb<>0 THEN IF cbut.onclick(mx,my) or (ch=#27) THEN BEGIN exi := TRUE;board.load('undo.und');END;
IF mb=0 THEN exi := TRUE;
IF not exi THEN BEGIN
IF text.onclick (mx,my) THEN text.use;
lp := text.sml+1;
IF del.onclick(mx,my) THEN BEGIN pause(1);IF not marked THEN mark[lp] := TRUE;
                                 board.delmarked;lef.show;
                                 text.hide;text.init(x+4,y+4,340,20,board.nelements,elem,FALSE);
                                 text.show;del.show(FALSE);END;
IF sback.onclick(mx,my) THEN
                        BEGIN
                        IF not marked THEN text.deccursor;
                        IF not marked THEN
                        IF lp>1 THEN
                        BEGIN
                        swap(board.elements[lp,1],board.elements[lp-1,1]);
                        swap(board.elements[lp,2],board.elements[lp-1,2]);
                        END;
                        IF marked THEN
                        for i := 2 to board.nelements do
                        BEGIN
                        IF mark[i] THEN BEGIN
                        ku := mark[i];
                        mark[i] := mark[i-1];
                        mark[i-1] := ku;
                        swap(board.elements[i,1],board.elements[i-1,1]);
                        swap(board.elements[i,2],board.elements[i-1,2]);
                        END;
                        END;
                        pause(1);text.outallrecs;sback.show(FALSE);END;
IF sforw.onclick(mx,my) THEN
                        BEGIN
                        IF not marked THEN text.inccursor;
                        IF not marked THEN
                        IF lp<board.nelements THEN
                        BEGIN
                        swap(board.elements[lp,1],board.elements[lp+1,1]);
                        swap(board.elements[lp,2],board.elements[lp+1,2]);
                        END;
                        IF marked THEN
                        for i := board.nelements-1 downto 1 do
                        BEGIN
                        IF mark[i] THEN BEGIN
                        ku := mark[i];
                        mark[i] := mark[i+1];
                        mark[i+1] := ku;
                        swap(board.elements[i,1],board.elements[i+1,1]);
                        swap(board.elements[i,2],board.elements[i+1,2]);
                        END;
                        END;

                        pause(1);text.outallrecs;sforw.show(FALSE);END;
IF all.onclick(mx,my) THEN BEGIN for i := 1 to board.nelements do mark[i] := TRUE;text.outallrecs;all.show(FALSE);END;
IF des.onclick(mx,my) THEN BEGIN for i := 1 to board.nelements do mark[i] := FALSE;text.outallrecs;des.show(FALSE);END;
IF inv.onclick(mx,my) THEN BEGIN for i := 1 to board.nelements do mark[i] := not mark[i];text.outallrecs;inv.show(FALSE);END;
REPEAT UNTIL not mbutton;
END;
END;
showtime;
UNTIL exi;
text.hide;
mainpan.hide;
END;
mb := 1;
END;

{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
CONSTRUCTOR graphboard.init;
VAR u:INTEGER;
BEGIN
npoints := 0;
nlines := 0;
narcs := 0;
ncircles := 0;
nellipses := 0;
nrectangles := 0;
nbars := 0;
nfills := 0;
nelements := 0;
x := 2;
y := 27;
x1 := x+620;
y1 := y+311;
magnIFy := magnIFy1;
scrov := TRUE;
scroh := TRUE;
IF (y1-y)/magnIFy>maxy THEN scrov := FALSE;
IF (x1-x)/magnIFy>maxx THEN scroh := FALSE;
IF scrov THEN vscro.init(x1,y,15,y1-y,round(maxy*magnIFy),round((y1-y)/magnIFy),0);
IF scroh THEN hscro.init(x,y1,x1-x,12,round(maxx*magnIFy),round((x1-x)/magnIFy),0);
fit.init(x1+1,y1+1,14,11,'F');
board.init(x,y,x1-x,y1-y,15,8,0,FALSE);
END;
PROCEDURE graphboard.hide;
BEGIN
IF scrov THEN vscro.hide;
IF scroh THEN hscro.hide;
board.hide;
END;

{$I-}
PROCEDURE graphboard.save;
VAR g:file of INTEGER;
    i,j,e:INTEGER;
    a,mag,smex,smey:INTEGER;

BEGIN
assign(g,name);
reWRITE(g);
i := 1;
WRITE(g,i);
mag := round(magnIFy*100);
smex := hscro.smesh;
smey := vscro.smesh;
WRITE(g,mag,smex,smey,inkcol,papcol);
a := 0;
WRITE(g,nelements,npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,a);
for i := 1 to nelements do WRITE(g,elements[i,1],elements[i,2]);
for i := 1 to npoints*3 do WRITE(g,points[i]);
for i := 1 to nlines*5 do WRITE(g,lines[i]);
for i := 1 to narcs*6 do WRITE(g,arcs[i]);
for i := 1 to ncircles*4 do WRITE(g,circles[i]);
for i := 1 to nellipses*5 do WRITE(g,ellipses[i]);
for i := 1 to nrectangles*5 do WRITE(g,rectangles[i]);
for i := 1 to nbars*5 do WRITE(g,bars[i]);
for i := 1 to nfills*4 do WRITE(g,fills[i]);
close(g);
END;


PROCEDURE graphboard.makeBASICblock;
VAR g:text;
    i,j,e:INTEGER;
    mag,smex,smey:INTEGER;
    cou:INTEGER;
 ststr,strig:STRING;
  PROCEDURE WRITEnext(k:longint;n:boolean);
    BEGIN
     IF cou=0 THEN WRITE(g,'     DATA ');
     WRITE(g,stri(k));
     cou := cou+1;
     IF (cou=18) or n THEN BEGIN cou := 0;WRITELN(g,'');END else WRITE(g,',');
    END;
BEGIN
assign(g,name);
reWRITE(g);
WRITELN(g,name+':');
cou := 0;
WRITEnext(nelements,TRUE);
for j := 1 to nelements do
BEGIN
WRITEnext(elements[j,1]+1230,FALSE);
e := elements[j,2];
case elements[j,1] of
1:for i := 1 to 3 do WRITEnext(points[i+e*3],FALSE);
2:for i := 1 to 5 do WRITEnext(lines[i+e*5],FALSE);
4:for i := 1 to 6 do WRITEnext(arcs[i+e*6],FALSE);
5:for i := 1 to 4 do WRITEnext(circles[i+e*4],FALSE);
6:for i := 1 to 5 do WRITEnext(ellipses[i+e*5],FALSE);
7:for i := 1 to 5 do WRITEnext(rectangles[i+e*5],FALSE);
8:for i := 1 to 5 do WRITEnext(bars[i+e*5],FALSE);
9:for i := 1 to 4 do WRITEnext(fills[i+e*4],FALSE);
END;
END;
WRITEnext(32767,TRUE);
close(g);
END;

PROCEDURE graphboard.load;
VAR g:file of INTEGER;
    i,j,e:INTEGER;
    mag,smex,smey:INTEGER;
BEGIN
assign(g,name);
reset(g);
read(g,i);
IF (i<1)or(i>30) THEN exit;
read(g,mag,smex,smey,inkcol,papcol);
magnIFy := mag/100;
refreshm(smex,smey);

read(g,nelements,npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,ntexts);
for i := 1 to nelements do read(g,elements[i,1],elements[i,2]);
for i := 1 to npoints*3 do read(g,points[i]);
for i := 1 to nlines*5 do read(g,lines[i]);
for i := 1 to narcs*6 do read(g,arcs[i]);
for i := 1 to ncircles*4 do read(g,circles[i]);
for i := 1 to nellipses*5 do read(g,ellipses[i]);
for i := 1 to nrectangles*5 do read(g,rectangles[i]);
for i := 1 to nbars*5 do read(g,bars[i]);
for i := 1 to nfills*4 do read(g,fills[i]);
for i := 1 to ntexts do
BEGIN
read(g,e);
for j := 1 to e do
BEGIN
read(g,e);
END;
END;
close(g);
END;
{-*MERGE*--*MERGE*--*MERGE*--*MERGE*--*MERGE*--*MERGE*--*MERGE*--*MERGE*-}
PROCEDURE graphboard.merge;
VAR g:file of INTEGER;
    j,e:INTEGER;
    i,k:INTEGER;
    mag,smex,smey:INTEGER;
    nelements1,npoints1,
    nlines1,narcs1,
    ncircles1,nellipses1,
    nrectangles1,
    nbars1,nfills1,ntexts1:INTEGER;
BEGIN
assign(g,name);
reset(g);
read(g,i);
IF (i<1)or(i>30) THEN exit;
read(g,mag,smex,smey,inkcol,papcol);
magnIFy := mag/100;
refreshm(smex,smey);

read(g,nelements1,npoints1,nlines1,narcs1,
ncircles1,nellipses1,nrectangles1,nbars1,nfills1,ntexts1);

for i := nelements+1 to nelements1+nelements do
    BEGIN
    read(g,elements[i,1],elements[i,2]);
    case elements[i,1] of
1:elements[i,2] := elements[i,2]+npoints;
2:elements[i,2] := elements[i,2]+nlines;
4:elements[i,2] := elements[i,2]+narcs;
5:elements[i,2] := elements[i,2]+ncircles;
6:elements[i,2] := elements[i,2]+nellipses;
7:elements[i,2] := elements[i,2]+nrectangles;
8:elements[i,2] := elements[i,2]+nbars;
9:elements[i,2] := elements[i,2]+nfills;
    END;
    END;
for i := npoints*3+1 to npoints*3+npoints1*3 do read(g,points[i]);
for i := nlines*5+1 to nlines*5+nlines1*5 do read(g,lines[i]);
for i := narcs*6+1 to narcs*6+narcs1*6 do read(g,arcs[i]);
for i := ncircles*4+1 to ncircles*4+ncircles1*4 do read(g,circles[i]);
for i := nellipses*5+1 to nellipses*5+nellipses1*5 do read(g,ellipses[i]);
for i := nrectangles*5+1 to nrectangles*5+nrectangles1*5 do read(g,rectangles[i]);
for i := nbars*5+1 to nbars*5+nbars1*5 do read(g,bars[i]);
for i := nfills*4+1 to nfills*4+nfills1*4 do read(g,fills[i]);
    nelements := nelements+nelements1;
    npoints := npoints+npoints1;
    nlines := nlines+nlines1;
    narcs := narcs+narcs1;
    ncircles := ncircles+ncircles1;
    nellipses := nellipses+nellipses1;
    nrectangles := nrectangles+nrectangles1;
    nbars := nbars+nbars1;
    nfills := nfills+nfills1;
close(g);
END;
FUNCTION min(a,b:real):real;
BEGIN
IF a>=b THEN min := b else min := a;
END;
FUNCTION max(a,b:real):real;
BEGIN
IF a<=b THEN max := b else max := a;
END;



PROCEDURE graphboard.magnyx;
VAR kx,ky:real;
    smex,smey:INTEGER;
BEGIN
IF magnIFy<8 THEN
  BEGIN
  kx := divix(mx);
  ky := diviy(my);
  magnIFy := magnIFy*2;
  smex := round((kx+x)*magnIFy-(x+((x1-x) div 2)));
  smey := round((ky+y)*magnIFy-(y+((y1-y) div 2)));
  IF smex<0 THEN smex := 0;
  IF smey<0 THEN smey := 0;
  refreshm(smex,smey);
  outall(FALSE,0);
  stat.hide;
  stat.show;
  refreshstatus;
  END;
END;
PROCEDURE graphboard.demagnyx;
VAR kx,ky:real;
    smex,smey:INTEGER;
BEGIN
IF magnIFy>1 THEN
  BEGIN
  kx := divix(mx);
  ky := diviy(my);
  magnIFy := magnIFy/2;
  smex := round((kx+x)*magnIFy-(x+((x1-x) div 2)));
  smey := round((ky+y)*magnIFy-(y+((y1-y) div 2)));
  IF smex<0 THEN smex := 0;
  IF smey<0 THEN smey := 0;
  refreshm(smex,smey);
  outall(FALSE,0);
  stat.hide;
  stat.show;
  refreshstatus;
  END;
END;



PROCEDURE graphboard.onclick;
VAR ax,ay,ab,ax1,ay1,ab1,r,r1,r2,gx,gy,cx,cy,cb,dx,dy,db:word;
    x0,y0,an1,an2,ra1:real;
smex,smey:INTEGER;
strig,ststr:STRING[89];
BEGIN
stat.hide;
IF vscro.onclick(mx,my) and scrov THEN BEGIN vscro.move(mx,my);outall(FALSE,0);END;
IF hscro.onclick(mx,my) and scroh THEN BEGIN hscro.move(mx,my);outall(FALSE,0);END;
mouseread1(ax,ay,ab);
IF inbar(mx,my,x,y,x1,y1) THEN
IF not stat.onclick(mx,my) THEN
IF (divix(mx)>0)and(diviy(my)>0)and(divix(mx)<maxx)and(diviy(my)<maxy) THEN
IF ab=2 THEN BEGIN
case mode of
11:magnyx(ax,ay);
10:demagnyx(ax,ay);
END;
END else
IF ab<>0 THEN
{---------- Обработчик ГРАФИЧЕСКИХ КОМАНД --------------}
BEGIN
mousehide;
case mode of
1:IF npoints<maxp THEN
  BEGIN
  nelements := nelements+1;
  elements[nelements,1] := 1;
  elements[nelements,2] := npoints;
  putpixel(mx,my,inkcol);
  points[npoints*3+1] := divix(mx);
  points[npoints*3+2] := diviy(my);
  points[npoints*3+3] := inkcol;
  npoints := npoints+1;
  END;
2:IF nlines<maxl THEN
  BEGIN
  REPEAT
  nelements := nelements+1;
  elements[nelements,1] := 2;
  elements[nelements,2] := nlines;
  setWRITEmode(xorput);
  ax1 := mx;ay1 := my;ab1 := 0;setcolor(14);
  line (mx,my,ax1,ay1);
  REPEAT
  REPEAT UNTIL mlbutton;
  REPEAT
  mouseread1(ax,ay,ab);
  IF ax<=x THEN ax := x+1;
  IF ax>=x1 THEN ax := x1-1;
  IF ay<=y THEN ay := y+1;
  IF ay>=y1 THEN ay := y1-1;
  UNTIL (((ax<>ax1)or(ay<>ay1)))or(ab=0)or(ab=3);
  line (mx,my,ax1,ay1);
  ax1 := ax;ay1 := ay;
  line (mx,my,ax1,ay1);
  UNTIL (ab=0)or(ab=3);
  setWRITEmode(0);
  setcolor(inkcol);
  line (mx,my,ax,ay);
  lines[nlines*5+1] := divix(mx);
  lines[nlines*5+2] := diviy(my);
  lines[nlines*5+3] := divix(ax);
  lines[nlines*5+4] := diviy(ay);
  lines[nlines*5+5] := inkcol;
  nlines := nlines+1;
  IF not(keypressed and (readkey='r')) THEN
  BEGIN
  mouseread1(ax,ay,ax1);
  mx := ax;my := ay;
  IF ax<=x THEN ax := x+1;
  IF ax>=x1 THEN ax := x1-1;
  IF ay<=y THEN ay := y+1;
  IF ay>=y1 THEN ay := y1-1;
  END;
  UNTIL(ab=0);
  END;
4:IF narcs<maxa THEN
  BEGIN
  r1 := 0;
  ax := mx;ay := my;
  setWRITEmode(xorput);
  setcolor(14);
  line(mx,my,ax,ay);
   REPEAT
  IF r1=0 THEN BEGIN
  line(mx,my,ax,ay);
  mouseread1(ax,ay,ab);
  line(mx,my,ax,ay);
  END;
  IF (ab=3)and(r1=0) THEN BEGIN
                     line(mx,my,ax,ay);
                     ax1 := (mx+ax) div 2;
                     ay1 := (my+ay) div 2+4;
                     mousesetpos(ax1,ay1);
                     r1 := 4;
                     getarcparam(ax,ay,mx,my,ax1,ay1,x0,y0,an1,an2,ra1);
                     arcnew(x0,y0,an1,an2,ra1,ars);
                     cx := ax1;
                     cy := ay1;
                     END;
  IF r1<>0 THEN
  BEGIN
  REPEAT
  mouseread1(ax1,ay1,ab);
  UNTIL (ax1<>cx)or(ay1<>cy)or(ab=0);
  getarcparam(ax,ay,mx,my,cx,cy,x0,y0,an1,an2,ra1);
  arcnew(x0,y0,an1,an2,ra1,ars);
  cx := ax1;
  cy := ay1;
  getarcparam(ax,ay,mx,my,ax1,ay1,x0,y0,an1,an2,ra1);
  arcnew(x0,y0,an1,an2,ra1,ars);
  END;
  UNTIL ab=0;
  
IF r1=0 THEN line(mx,my,ax,ay) else
BEGIN
  arcnew(x0,y0,an1,an2,ra1,ars);
  setWRITEmode(0);
setcolor(inkcol);
  arcnew(x0,y0,an1,an2,ra1,1);
  nelements := nelements+1;
  elements[nelements,1] := 4;
  elements[nelements,2] := narcs;

  arcs[narcs*6+1] := divix(round(x0));
  arcs[narcs*6+2] := diviy(round(y0));
  arcs[narcs*6+3] := round(ra1/magnIFy);
  arcs[narcs*6+4] := round(an1*91);
  arcs[narcs*6+5] := round(an2*91);
  arcs[narcs*6+6] := inkcol;
  narcs := narcs+1;

END;
  setWRITEmode(0);
  setcolor(inkcol);
  END;
5:IF ncircles<maxc THEN
  BEGIN
  nelements := nelements+1;
  elements[nelements,1] := 5;
  elements[nelements,2] := ncircles;
  setWRITEmode(xorput);
  ax1 := mx;ay1 := my;ab1 := 0;setcolor(14);
  r := 0;
  REPEAT
  REPEAT
  mouseread1(ax,ay,ab);
  gx := abs(ax-mx);
  gy := round(gx*koof);
  IF inbar(mx+gx,my,x,y,x1,y1) and
  inbar(mx-gx,my,x,y,x1,y1) and
  inbar(mx,my+gy,x,y,x1,y1) and
  inbar(mx,my-gy,x,y,x1,y1) THEN cy := gx;
  UNTIL ((ax<>ax1)or(ay<>ay1) or(ab<>1));
  ax1 := ax;ay1 := ay;
{CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC}
circletr(mx,my,r1,round(r1*koof));
{CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC}

 IF ab=3 THEN
  BEGIN
   mousesetpos(mx,my);
{   rectangle (mx-r1,my-r2,mx+r1,my+r2);}
circletr(mx,my,r1,round(r1*koof));
   dx := ax-mx;
   dy := ay-my;
  REPEAT
   {rectangle (mx-r1,my-r2,mx+r1,my+r2);}
   circletr(mx,my,r1,round(r1*koof));

   mouseread1(ax,ay,ab);
   IF inbar(ax+r1,ay,x,y,x1,y1)and inbar(ax-r1,ay,x,y,x1,y1) THEN mx := ax;
   IF inbar(ax,ay+r2,x,y,x1,y1)and inbar(ax,ay-r2,x,y,x1,y1) THEN my := ay;
   mousesetpos(mx,my);
   ax := mx;
   ay := my;
   {rectangle (mx-r1,my-r2,mx+r1,my+r2);}
   circletr(mx,my,r1,round(r1*koof));

  UNTIL (ab<>3);
   ax := ax+dx;
   ay := ay+dy;
   mousesetpos(ax,ay);
{   rectangle (mx-r1,my-r2,mx+r1,my+r2);}
circletr(mx,my,r1,round(r1*koof));

 END;
  r1 := cy;
  r2 := round(r1*koof);
{  rectangle (mx-r1,my-r2,mx+r1,my+r2);}
circletr(mx,my,r1,round(r1*koof));



  UNTIL (ab=0);
{  rectangle (mx-r1,my-r2,mx+r1,my+r2);}
circletr(mx,my,r1,round(r1*koof));

    setWRITEmode(0);
  setcolor(inkcol);
  circle(mx,my,r1);
  circles[ncircles*4+1] := divix(mx);
  circles[ncircles*4+2] := diviy(my);
  circles[ncircles*4+3] := round(r1/magnIFy);
  circles[ncircles*4+4] := inkcol;
  ncircles := ncircles+1;
  END;

6:IF nellipses<maxe THEN
  BEGIN
  nelements := nelements+1;
  elements[nelements,1] := 6;
  elements[nelements,2] := nellipses;
  setWRITEmode(xorput);
  ax1 := mx;ay1 := my;ab1 := 0;setcolor(14);
  r := 0;
  REPEAT
  REPEAT
  mouseread1(ax,ay,ab);
  cx := ax;
  cy := ay;
  IF inbar(mx+abs(ax-mx),my,x,y,x1,y1) and
  inbar(mx-abs(ax-mx),my,x,y,x1,y1) THEN cx := abs(ax-mx) else mousesetpos(ax1,ay1);
  IF inbar(mx,my+abs(ay-my),x,y,x1,y1) and
  inbar(mx,my-abs(ay-my),x,y,x1,y1) THEN cy := abs(ay-my) else mousesetpos(ax1,ay1);
  UNTIL ((ax<>ax1)or(ay<>ay1) or(ab<>1));
  ax1 := ax;ay1 := ay;
   circletr(mx,my,r1,r2);


 IF ab=3 THEN
  BEGIN
   mousesetpos(mx,my);
   circletr(mx,my,r1,r2);

   dx := ax-mx;
   dy := ay-my;
  REPEAT
   circletr(mx,my,r1,r2);

   mouseread1(ax,ay,ab);
   IF inbar(ax+r1,ay,x,y,x1,y1)and inbar(ax-r1,ay,x,y,x1,y1) THEN mx := ax;
   IF inbar(ax,ay+r2,x,y,x1,y1)and inbar(ax,ay-r2,x,y,x1,y1) THEN my := ay;
   mousesetpos(mx,my);
   ax := mx;
   ay := my;
   circletr(mx,my,r1,r2);

  UNTIL (ab<>3);
   ax := ax+dx;
   ay := ay+dy;
   mousesetpos(ax,ay);
   circletr(mx,my,r1,r2);

 END;
  r1 := cx;
  r2 := cy;
   circletr(mx,my,r1,r2);



  UNTIL (ab=0);
   circletr(mx,my,r1,r2);

  setWRITEmode(0);
  setcolor(inkcol);
  ellipse(mx,my,0,360,r1,r2);
  ellipses[nellipses*5+1] := divix(mx);
  ellipses[nellipses*5+2] := diviy(my);
  ellipses[nellipses*5+3] := round(r1/magnIFy);
  ellipses[nellipses*5+4] := round(r2/magnIFy);
  ellipses[nellipses*5+5] := inkcol;
  nellipses := nellipses+1;
  END;

7:IF nrectangles<maxr THEN
  BEGIN
  nelements := nelements+1;
  elements[nelements,1] := 7;
  elements[nelements,2] := nrectangles;
  setWRITEmode(xorput);
  ax1 := mx;ay1 := my;ab1 := 0;setcolor(14);
  REPEAT
  REPEAT
  mouseread1(ax,ay,ab);
  IF ax<=x THEN ax := x+1;
  IF ax>=x1 THEN ax := x1-1;
  IF ay<=y THEN ay := y+1;
  IF ay>=y1 THEN ay := y1-1;
  UNTIL (ax<>ax1)or(ay<>ay1)or(ab=0);
  rectangle (mx,my,ax1,ay1);
   IF ab=3 THEN
      BEGIN
      mousesetpos(mx,my);
      dx := ax-mx;
      dy := ay-my;
      ax := mx;ay := my;
      r1 := ax1-mx;r2 := ay1-my;
      rectangle (mx,my,mx+r1,my+r2);
  REPEAT
   rectangle (ax,ay,ax+r1,ay+r2);
   mouseread1(ax,ay,ab);
   IF ax+r1>x1 THEN ax := x1-r1-1;
   IF ay+r2>y1 THEN ay := y1-r2-1;
   IF ax<x THEN ax := x+1;
   IF ay<y THEN ay := y+1;
   rectangle (ax,ay,ax+r1,ay+r2);
  UNTIL (ab<>3);
   rectangle (ax,ay,ax+r1,ay+r2);
   ax := ax+dx;
   ay := ay+dy;
   mousesetpos(ax,ay);
   mx := ax-dx;
   my := ay-dy;
 END;
  ax1 := ax;ay1 := ay;
  rectangle(mx,my,ax1,ay1);
  UNTIL (ab=0);
  setWRITEmode(0);
  setcolor(inkcol);
  rectangle (mx,my,ax,ay);
  rectangles[nrectangles*5+1] := divix(mx);
  rectangles[nrectangles*5+2] := diviy(my);
  rectangles[nrectangles*5+3] := divix(ax);
  rectangles[nrectangles*5+4] := diviy(ay);
  rectangles[nrectangles*5+5] := inkcol;
  nrectangles := nrectangles+1;
  END;

8:IF nbars<maxb THEN
  BEGIN
  nelements := nelements+1;
  elements[nelements,1] := 8;
  elements[nelements,2] := nbars;
  setWRITEmode(xorput);
  ax1 := mx;ay1 := my;ab1 := 0;setcolor(14);
  REPEAT
  REPEAT
  mouseread1(ax,ay,ab);
    IF ax<=x THEN ax := x+1;
  IF ax>=x1 THEN ax := x1-1;
  IF ay<=y THEN ay := y+1;
  IF ay>=y1 THEN ay := y1-1;

  UNTIL (ax<>ax1)or(ay<>ay1)or(ab=0);
  rectangle (mx,my,ax1,ay1);
     IF ab=3 THEN
      BEGIN
      mousesetpos(mx,my);
      dx := ax-mx;
      dy := ay-my;
      ax := mx;ay := my;
      r1 := ax1-mx;r2 := ay1-my;
      rectangle (mx,my,mx+r1,my+r2);
  REPEAT
   rectangle (ax,ay,ax+r1,ay+r2);
   mouseread1(ax,ay,ab);
   IF ax+r1>x1 THEN ax := x1-r1-1;
   IF ay+r2>y1 THEN ay := y1-r2-1;
   IF ax<x THEN ax := x+1;
   IF ay<y THEN ay := y+1;
   rectangle (ax,ay,ax+r1,ay+r2);
  UNTIL (ab<>3);
   rectangle (ax,ay,ax+r1,ay+r2);
   ax := ax+dx;
   ay := ay+dy;
   mousesetpos(ax,ay);
   mx := ax-dx;
   my := ay-dy;
 END;

  ax1 := ax;ay1 := ay;
  rectangle(mx,my,ax1,ay1);
  UNTIL (ab=0);
  setWRITEmode(0);
  setfillstyle(1,inkcol);
  bar(mx,my,ax,ay);
  bars[nbars*5+1] := divix(mx);
  bars[nbars*5+2] := diviy(my);
  bars[nbars*5+3] := divix(ax);
  bars[nbars*5+4] := diviy(ay);
  bars[nbars*5+5] := inkcol;
  nbars := nbars+1;
  END;
9:IF nfills<maxf THEN
  BEGIN
  stat.hide;
  nelements := nelements+1;
  elements[nelements,1] := 9;
  elements[nelements,2] := nfills;
  setcolor(papcol);
  rectangle(x,y,x1,y1);
  setfillstyle(1,inkcol);
  floodfill(mx,my,papcol);
  fills[nfills*4+1] := divix(mx);
  fills[nfills*4+2] := diviy(my);
  fills[nfills*4+4] := inkcol;
  fills[nfills*4+3] := papcol;
  nfills := nfills+1;
  stat.show;
  END;
10:magnyx(mx,my);
11:demagnyx(mx,my);
13:
  BEGIN
  END;
14:
  BEGIN
  END;
15:
  BEGIN
  mousehide;
  inkcol := getpixel(mx,my);
  refreshstatus;
  mouseshow;
  END;
16:
  BEGIN
  mousehide;
  papcol := getpixel(mx,my);
  refreshstatus;
  mouseshow;
  END;
 END;
 mouseshow;
END;
stat.show;
END;


PROCEDURE deletesarray(VAR a:ginza;VAR nina:INTEGER;ndel:INTEGER);
VAR i,m:INTEGER;
BEGIN
for i := ndel+1 to nina do
BEGIN
a[i-1] := a[i];
END;
nina := nina-1;
IF nina<0 THEN nina := 0;
END;

PROCEDURE deletearray(VAR a:array of INTEGER;VAR nina:INTEGER;ndel,koof:INTEGER);
VAR i,m:INTEGER;
BEGIN
m := ndel*koof;
for i := (ndel+1)*koof to (nina+1)*koof do
BEGIN
a[m] := a[i];
m := m+1;
END;
nina := nina-1;
IF nina<0 THEN nina := 0;
END;

PROCEDURE graphboard.delelement;
VAR k:INTEGER;
    i:INTEGER;
BEGIN
k := elements[n,2];
case elements[n,1] of
1:deletearray(points,npoints,k,3);
2:deletearray(lines,nlines,k,5);
4:deletearray(arcs,narcs,k,6);
5:deletearray(circles,ncircles,k,4);
6:deletearray(ellipses,nellipses,k,5);
7:deletearray(rectangles,nrectangles,k,5);
8:deletearray(bars,nbars,k,5);
9:deletearray(fills,nfills,k,4);
{12:deletesarray(texts,ntexts,k);}
END;
for i := 1 to nelements do
    IF elements[i,1]=elements[n,1] THEN
    IF elements[i,2]>elements[n,2] THEN elements[i,2] := elements[i,2]-1;
elements[n,1] := 0;
END;
PROCEDURE graphboard.delmarked;
VAR i:INTEGER;
    n,k:INTEGER;
BEGIN
n := 0;
k := -15;
for i := 1 to nelements do IF mark[i] THEN BEGIN mark[i] := FALSE;
                                               n := n+1;
                                               delelement(i);
                                               IF k<0 THEN k := i;
                                               END;
IF n<>0 THEN BEGIN
    for i := k+1 to nelements do
    IF elements[i,1]<>0 THEN BEGIN
    elements[k,1] := elements[i,1];
    elements[k,2] := elements[i,2];
    k := k+1;
END;

END;
nelements := nelements-n;
END;
PROCEDURE graphboard.refreshm;
VAR g1,g2:boolean;
BEGIN
IF scroh THEN hscro.movp.hide;
IF scrov THEN vscro.movp.hide;
g1 := scroh;
g2 := scrov;
scroh := TRUE;
scrov := TRUE;
IF magnIFy<1 THEN scroh := FALSE;
IF magnIFy<1 THEN scrov := FALSE;
IF sx>round(maxx*magnIFy)-round((x1-x)) THEN
                             sx := round(maxx*magnIFy)-round((x1-x));
IF sy>round(maxy*magnIFy)-round((y1-y)) THEN
                             sy := round(maxy*magnIFy)-round((y1-y));
IF scroh THEN hscro.init(x,y1,x1-x,12,round(maxx*magnIFy),round((x1-x)),sx);
IF scrov THEN vscro.init(x1,y,15,y1-y,round(maxy*magnIFy),round((y1-y)),sy);
fit.show(FALSE);
hscro.left.show(FALSE);
hscro.right.show(FALSE);
vscro.up.show(FALSE);
vscro.down.show(FALSE);
IF scroh THEN hscro.movp.show;
IF scrov THEN vscro.movp.show;
END;

PROCEDURE graphboard.show;
BEGIN
board.show;
fit.show(FALSE);
hscro.show;
vscro.show;
END;
{qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq}
FUNCTION graphboard.magnx;
BEGIN
magnx := round((numb+x)*magnIFy-hscro.smes);
END;
FUNCTION graphboard.magny;
BEGIN
magny := round((numb+y)*magnIFy-vscro.smes);
END;

FUNCTION graphboard.divix;
BEGIN
divix := round(((numb-x*magnIFy)+hscro.smes)/magnIFy);
END;
FUNCTION graphboard.diviy;
BEGIN
diviy := round(((numb-y*magnIFy)+vscro.smes)/magnIFy);
END;
{qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq}

PROCEDURE graphboard.outelement;
VAR strig,ststr:STRING;
    n1,n2:INTEGER;
    x0,y0,an1,an2,ra1:real;
    r,b:boolean;
BEGIN
case elements[i,1] of
1:
putpixel(magnx(points[elements[i,2]*3+1]),
         magny(points[elements[i,2]*3+2]),
         points[elements[i,2]*3+3]);
2:
BEGIN
setcolor(lines[elements[i,2]*5+5]);
line(    magnx(lines[elements[i,2]*5+1]),
         magny(lines[elements[i,2]*5+2]),
         magnx(lines[elements[i,2]*5+3]),
         magny(lines[elements[i,2]*5+4]));

END;
3:;
4:
BEGIN
  x0 := magnx(arcs[elements[i,2]*6+1]);
  y0 := magny(arcs[elements[i,2]*6+2]);
  setcolor(arcs[elements[i,2]*6+6]);
  arcnew(x0,y0,arcs[elements[i,2]*6+4]/91,arcs[elements[i,2]*6+5]/91,
  arcs[elements[i,2]*6+3]*magnIFy,1);
END;
5:
BEGIN
setcolor(circles[elements[i,2]*4+4]);
circle(  magnx(circles[elements[i,2]*4+1]),
         magny(circles[elements[i,2]*4+2]),
         round(circles[elements[i,2]*4+3]*magnIFy));

END;

6:BEGIN
setcolor(ellipses[elements[i,2]*5+5]);
ellipse(magnx(ellipses[elements[i,2]*5+1]),
         magny(ellipses[elements[i,2]*5+2]),0,360,
         round(ellipses[elements[i,2]*5+3]*magnIFy),
         round(ellipses[elements[i,2]*5+4]*magnIFy));

END;
7:BEGIN
setcolor(rectangles[elements[i,2]*5+5]);
rectangle( magnx(rectangles[elements[i,2]*5+1]),
           magny(rectangles[elements[i,2]*5+2]),
           magnx(rectangles[elements[i,2]*5+3]),
           magny(rectangles[elements[i,2]*5+4]));
END;

8:BEGIN
setfillstyle(1,bars[elements[i,2]*5+5]);
bar( magnx(bars[elements[i,2]*5+1]),
         magny(bars[elements[i,2]*5+2]),
         magnx(bars[elements[i,2]*5+3]),
         magny(bars[elements[i,2]*5+4]));
END;
9:BEGIN
setfillstyle(1,fills[elements[i,2]*4+4]);
floodfill( magnx(fills[elements[i,2]*4+1]),
         magny(fills[elements[i,2]*4+2]),
         fills[elements[i,2]*4+3]);
END;
10:;
11:;
13:;
14:;
END;
END;



PROCEDURE graphboard.outall;
VAR i:INTEGER;
check:boolean;
 size,lstep:INTEGER;
    p:pointer;
BEGIN
IF magnIFy=fitonscreen THEN BEGIN
                           vscro.smes := -10;
                           hscro.smes := -38;
                           END;

stat.hide;
mousehide;
setactivepage(1);
IF step THEN setvisualpage(1);
cleardevice;
board.show;
check := step;
IF step THEN REPEAT UNTIL not mbutton;
for i := 1 to nelements do
BEGIN
outelement(i);
IF step THEN showtext(1,1,630,15,elem(i),false,7);
IF check THEN
IF step and (i>=startvalue) THEN BEGIN
                   REPEAT UNTIL mbutton or keypressed;
                   if keypressed then readkey;
                   IF mrbutton THEN check := FALSE;
                   REPEAT UNTIL not mbutton;END;
             END;

x := x+1;
x1 := x1-1;
lstep := (y1-y) div 10;
size := imagesize(x,y,x1,y+lstep);
getmem(p,size);
for i := 1 to 10 do BEGIN
setactivepage(1);
getimage(x,y+(i-1)*lstep,x1,y+i*lstep,p^);
setactivepage(0);
putimage(x,y+(i-1)*lstep,p^,0);
END;
freemem(p,size);
lstep := y1-lstep*10;
size := imagesize(x,y1-lstep,x1,y1);
getmem(p,size);
setactivepage(1);
getimage(x,y1-lstep,x1,y1,p^);
setactivepage(0);
putimage(x,y1-lstep,p^,0);
freemem(p,size);
setcolor(10);
x := x-1;
x1 := x1+1;
IF magnIFy=fitonscreen THEN rectangle(magnx(0),magny(0),magnx(maxx+1),magny(maxy+1));
setvisualpage(0);
stat.show;
mouseshow;
END;

FUNCTION graphboard.fitonscreen;
BEGIN
fitonscreen := 0.85;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}



{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
CONSTRUCTOR statusbar.init;
BEGIN
visual := TRUE;
x0 := x;
y0 := y;
IF not stpan.visual THEN stpan.init(x,y,250,100,8,7,0,TRUE);
lx0 := stpan.lx;
ly0 := stpan.ly;
st1.init(x+2,y+2,246,15,'Файл:'+filename);
st2.init(x+2,y+19,246,15,'Режим:'+mode);
st3.init(x+2,y+36,246,15,'Размер:'+magnIFy+'%');
IF step THEN st4.init(x+2,y+53,246,15,'Шаг включен')
else st4.init(x+2,y+53,246,15,'Шаг выключен');
papc.init(x+2,y+70,246,28,papcol,8,7,FALSE);
inkc.init(x+8,y+76,234,16,inkcol,8,7,FALSE);
END;

PROCEDURE statusbar.showwr;
BEGIN
IF visual THEN BEGIN
st1.show(FALSE);
st2.show(FALSE);
st3.show(FALSE);
st4.show(FALSE);
papc.show;
inkc.show;
END;
END;

PROCEDURE statusbar.refresh;
BEGIN
IF visual THEN BEGIN
init(x0,y0,gfname,mode,magnIFy,step,papcol,inkcol);
showwr;
END;
END;

FUNCTION statusbar.onclick;
BEGIN
onclick := FALSE;
IF visual THEN
IF (mx>x0)and(mx<x0+lx0)and(my>y0)and(my<y0+ly0)THEN onclick := TRUE;
END;

PROCEDURE statusbar.changecoord;
BEGIN
IF visual THEN BEGIN
x0 := nx;y0 := ny;
stpan.hide;
stpan.x := nx;stpan.y := ny;
st1.x := nx+2;st1.y := ny+2;
st2.x := nx+2;st2.y := ny+19;
st3.x := nx+2;st3.y := ny+36;
st4.x := nx+2;st4.y := ny+53;
papc.x := nx+2;papc.y := ny+70;
inkc.x := nx+8;inkc.y := ny+76;
stpan.show;
showwr;
END;
END;

PROCEDURE statusbar.show;
BEGIN
IF visual THEN
BEGIN
stpan.show;
st1.show(FALSE);
st2.show(FALSE);
st3.show(FALSE);
st4.show(FALSE);
papc.show;
inkc.show;
END;
END;

PROCEDURE statusbar.hide;
BEGIN
IF visual THEN
stpan.hide;
END;

{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
CONSTRUCTOR vscrollbar.init;
VAR ay:INTEGER;
BEGIN
x := x1;y := y1;lx := lx1;ly := ly1;number := number1;len := len1;
mpan.init(x,y,lx,ly,7,8,0,FALSE);
smes := smes1;
h := lx-1;
up.init(x+1,y+1,lx-2,h-2,'^');
up.col := 8;
down.init(x+1,y+ly-h+1,lx-2,h-1,'v');
down.col := 8;

lbar := round((len*(ly-2*h))/number);
ay := round(smes*(ly-2*h)/ number + y + h);
movp.init(x+1,ay,lx-2,lbar,8,7,0,TRUE);
END;


PROCEDURE vscrollbar.setsmes;
VAR ay:INTEGER;
BEGIN
smes := n;
movp.hide;
ay := round((smes)*(ly-2*h)/number + y + h);
movp.y := ay;
show;
END;

FUNCTION vscrollbar.smesh;
BEGIN
smesh := smes;
END;


PROCEDURE vscrollbar.show;
BEGIN
visual := TRUE;
mpan.show;
up.show(FALSE);
down.show(FALSE);
IF movp.visual THEN movp.hide;
movp.show;
END;

FUNCTION vscrollbar.onclick;
BEGIN
IF inbar(mx,my,x,y,x+lx,y+ly) THEN onclick := TRUE else onclick := FALSE;
END;

FUNCTION vscrollbar.move;
VAR ax,ay,bx,by:INTEGER;
rx,ry,rb,as:word;
BEGIN
IF up.onclick(mx,my) THEN BEGIN
                          REPEAT
                          IF smes>0 THEN smes := smes-1;
                          IF smes<0 THEN smes := 0;
                          ay := round(smes*(ly-2*h)/ number + y + h);
                          movp.hide;
                          movp.y := ay;
                          movp.show;
                          pause(mscp);
                          UNTIL not mbutton;
                          up.show(FALSE);
                          END;
IF down.onclick(mx,my) THEN BEGIN
                          REPEAT
                          IF smes<number-len THEN smes := smes+1;
                          IF smes>number-len THEN smes := number-len;
                          ay := round(smes*(ly-2*h)/ number + y + h);
                          movp.hide;
                          movp.y := ay;
                          movp.show;
                          pause(mscp);
                          UNTIL not mbutton;
                            down.show(FALSE);
                            END;
IF movp.onclick(mx,my) THEN BEGIN
                       by := my-movp.y;
                       REPEAT
                       mouseread1(rx,ry,rb);
                       ax := rx;ay := ry;
                       as := ay;
                       ay := ay-by;
                       IF ay<y+h THEN ay := y+h;
                       IF ay>y+ly-h-lbar THEN ay := y+ly-h-lbar;
                       IF bx<>as THEN BEGIN
                       movp.hide;
                       movp.y := ay;
                       movp.show;
                       END;
                       smes := round(((ay-(y+h))*number)/(ly-2*h));
                       bx := as;
                       UNTIL not mbutton;
                       END;
END;

PROCEDURE vscrollbar.hide;
BEGIN
visual := FALSE;
movp.hide;
mpan.hide;
END;


{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}





{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
CONSTRUCTOR hscrollbar.init;
VAR ax:INTEGER;
BEGIN
x := x1;y := y1;lx := lx1;ly := ly1;number := number1;len := len1;
mpan.init(x,y,lx,ly,7,8,0,FALSE);
h := ly-1;
left.init(x+1,y+1,h-2,ly-1,'<');
left.col := 8;
right.init(x+lx-h+1,y+1,h-1,ly-1,'>');
right.col := 8;
smes := smes1;
lbar := round((len*(lx-2*h))/number);
ax := round((smes)*(lx-2*h)/ number + x + h);
movp.init(ax,y+1,lbar,h,8,7,0,TRUE);
END;

PROCEDURE hscrollbar.show;
BEGIN
visual := TRUE;
mpan.show;
left.show(FALSE);
right.show(FALSE);
movp.show;
END;

FUNCTION hscrollbar.onclick;
BEGIN
IF inbar(mx,my,x,y,x+lx,y+ly) THEN onclick := TRUE else onclick := FALSE;
END;

FUNCTION hscrollbar.move;
VAR ax,ay,ab,bx,by:INTEGER;
rx,ry,rb,as:word;
BEGIN
IF left.onclick(mx,my) THEN BEGIN
                          REPEAT
                          IF smes>0 THEN smes := smes-1;
                          IF smes<0 THEN smes := 0;
                          ax := round(smes*(lx-2*h)/ number + x + h);
                          movp.hide;
                          movp.x := ax;
                          movp.show;
                          pause(mscp);
                          UNTIL not mbutton;
                          left.show(FALSE);
                          END;
IF right.onclick(mx,my) THEN BEGIN
                          REPEAT
                          IF smes<number-len THEN smes := smes+1;
                          IF smes>number-len THEN smes := number-len;
                          ax := round(smes*(lx-2*h)/ number + x + h);
                          movp.hide;
                          movp.x := ax;
                          movp.show;
                          pause(mscp);
                          UNTIL not mbutton;
                            right.show(FALSE);
                            END;
IF movp.onclick(mx,my) THEN BEGIN
                       bx := mx-movp.x;
                       REPEAT
                       mouseread1(rx,ry,rb);
                       ax := rx;ay := ry;
                       as := ax;
                       ax := ax-bx;
                       IF ax<x+h THEN BEGIN ax := x+h;bx := mx-movp.x;END;
                       IF ax>x+lx-h-lbar THEN ax := x+lx-h-lbar;
                       IF by<>as THEN BEGIN
                                       movp.hide;
                                       movp.x := ax;
                                       movp.show;
                                      END;
                       smes := round(((ax-(x+h))*number)/(lx-2*h));
                       by := as;
                       UNTIL not mbutton;
                       END;
END;

PROCEDURE hscrollbar.setsmes;
VAR ax:INTEGER;
BEGIN
smes := n;
movp.hide;
ax := round((smes)*(lx-2*h)/ number + x + h);
movp.x := ax;
show;
END;
FUNCTION hscrollbar.smesh;
BEGIN
smesh := smes;
END;

PROCEDURE hscrollbar.hide;
BEGIN
visual := FALSE;
movp.hide;
mpan.hide;
END;


{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}

CONSTRUCTOR querybar.init;
BEGIN
x := x1;y := y1;nm := nm1;query := quer1;
symbols := symbol1;
moving := movin;
IF query<>'' THEN ly := 30;
lx := nm*9+4;
pan.init(x,y,lx,ly,8,7,0,TRUE);
caption := '';
END;
PROCEDURE querybar.show;
BEGIN
mousehide;
pan.show;
visual := TRUE;
setcolor(7);
IF query<>'' THEN outtextxy(x+5,y+5,query);
showtext(x+2,y+ly-12,lx-4,10,caption,TRUE,7);
mouseshow;
END;
PROCEDURE querybar.changecoord;
BEGIN
hide;
x := xa;
y := ya;
pan.x := xa;
pan.y := ya;
show;
END;
PROCEDURE querybar.move;
VAR mx,my,mb:word;
    tx,ty,alx,aly:word;
BEGIN
IF moving THEN BEGIN
mouseread1(mx,my,mb);
tx := mx-x;
ty := my-y;
REPEAT
mouseread1(mx,my,mb);
IF (alx<>mx)or(aly<>my) THEN IF ((mx-tx)<maxx-lx)and((my-ty)<maxy-ly)and ((my-ty)>30)
   THEN changecoord(mx-tx,my-ty);
alx := mx;aly := my;
UNTIL mb=0;
END;
END;
PROCEDURE querybar.use;
VAR j:char;
    mx,my,mb:word;
    exitt:boolean;
BEGIN
mousehide;
setcolor(10);
IF query<>'' THEN outtextxy(x+5,y+5,query);
 showtext(x+2,y+ly-12,lx-4,10,caption+'█',TRUE,7);

mouseshow;
REPEAT mouseread1(mx,my,mb); UNTIL mb=0;
REPEAT
exitt := FALSE;
REPEAT
showtime;
mouseread1(mx,my,mb);
IF (mb=1)and(not onclick(mx,my))THEN exitt := TRUE;
IF (mb=1)and(onclick(mx,my)) THEN move;
UNTIL keypressed or exitt;

IF not exitt THEN j := readkey;
IF not exitt THEN
BEGIN
     IF length(caption)>0 THEN IF j=#08 THEN delete(caption,length(caption),1);
     IF (length(caption)<nm) and (j in symbols) THEN caption := caption+j;
 showtext(x+2,y+ly-12,lx-4,10,caption+'█',TRUE,7);
 END;

UNTIL (j=#13)or (j=#27) or exitt;
mousehide;
setcolor(7);
IF query<>'' THEN outtextxy(x+5,y+5,query);
mouseshow;

END;
FUNCTION querybar.onclick;
BEGIN
IF inbar(mx,my,x,y,x+lx,y+ly) THEN BEGIN event := TRUE;onclick := TRUE; END else onclick := FALSE;
END;
PROCEDURE querybar.hide;
BEGIN
mousehide;
visual := FALSE;
pan.hide;
mouseshow;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
FUNCTION menu.init;
BEGIN
fields[n] := x;
IF nfields>15 THEN nfields := 0;
IF nfields<n THEN nfields := n;
visual := FALSE;
END;

PROCEDURE menu.coord;
BEGIN
tx := x1;
ty := y1;
lx := 20*8+4;
ly := nfields*14;
END;

PROCEDURE menu.show;
VAR i:INTEGER;
BEGIN
mousehide;
IF not visual THEN BEGIN
size := imagesize(tx-1,ty-1,tx+lx+1,ty+ly+2);
getmem(p,size);
getimage(tx-1,ty-1,tx+lx+1,ty+ly+2,p^);
END;
visual := TRUE;
setfillstyle(1,15);
bar(tx-1,ty-1,tx+lx+1,ty+ly+2);
setfillstyle(1,7);
bar(tx,ty,tx+lx,ty+ly+1);
for i := 0 to nfields-1 do
showtext(tx+2,ty+i*14+2,20*8,12,fields[i+1],FALSE,7);
mouseshow;
prev := -1;
END;
FUNCTION menu.getfield;
VAR t:INTEGER;
BEGIN
mx := mx-tx;
my := my-ty;
t := trunc(my/14);
IF prev<>-1 THEN showtext(tx+2,ty+prev*14+2,20*8,12,fields[prev+1],FALSE,7);
prev := trunc(my/14);
showtext(tx+2,ty+prev*14+2,20*8,12,fields[prev+1],TRUE,7);
showtext(tx+2,ty+t*14+2,20*8,12,fields[t+1],TRUE,7);
getfield := prev;
END;

PROCEDURE menu.hide;
BEGIN
mousehide;
IF visual THEN BEGIN
putimage(tx-1,ty-1,p^,normalput);
freemem(p,size);
END;
visual := FALSE;
mouseshow;
END;

FUNCTION menu.onclick;
BEGIN
IF not((mx>tx)and (mx<tx+lx) and (my>ty)and(my<ty+ly))and(visual)
THEN
BEGIN
hide;
visual := FALSE;
END;
onclick := FALSE;
IF (visual) and ((mx>tx)and (mx<tx+lx) and (my>ty)and(my<ty+ly)) THEN
BEGIN
event := TRUE;
onclick := TRUE;
END;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
FUNCTION stria(k:longint):STRING;
VAR ak:STRING;
BEGIN
str(k,ak);
stria := ak;
END;
CONSTRUCTOR panel.init;
BEGIN
x := x1;y := y1;lx := lx1;ly := ly1;col := col1;cols := col2;colrs := col3;saving := savin1;
visual := FALSE;
END;
PROCEDURE panel.show;
BEGIN
mousehide;
IF not visual THEN
IF saving THEN BEGIN
size := imagesize(x,y,x+lx,y+ly);

getmem(p,size);
getimage(x,y,x+lx,y+ly,p^);
END;
setcolor(cols);
line(x,y,x+lx,y);
line(x,y,x,y+ly);

setfillstyle(1,col);
bar(x+1,y+1,x+lx-1,y+ly-1);
setcolor(colrs);
line(x,y+ly,x+lx,y+ly);
line(x+lx,y,x+lx,y+ly);
visual := TRUE;
mouseshow;
END;
PROCEDURE panel.hide;
BEGIN
mousehide;
IF saving and visual THEN BEGIN
putimage(x,y,p^,normalput);
freemem(p,size);
END;
visual := FALSE;
mouseshow;
END;

FUNCTION panel.onclick;
BEGIN
IF inbar(mx,my,x,y,x+lx,y+ly) THEN BEGIN event := TRUE;onclick := TRUE;END else onclick := FALSE;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}

CONSTRUCTOR button.init;
BEGIN
x := x1;
y := y1;
lx := lx1;
ly := ly1;
caption := name1;
visual := FALSE;
col := 7;
END;
PROCEDURE button.show;
BEGIN
active := status;
visual := TRUE;
showtext(x,y,lx,ly,caption,status,col);
END;
FUNCTION button.onclick;
BEGIN
IF not(inbar(ax,ay,x,y,x+lx,y+ly))and(active)
THEN
BEGIN
show(FALSE);
active := FALSE;
END;
onclick := FALSE;
IF visual THEN
IF inbar(ax,ay,x,y,x+lx,y+ly) THEN
IF not active THEN
BEGIN
event := TRUE;
onclick := TRUE;
show(TRUE);
END
else
BEGIN
onclick := FALSE;
show(FALSE);
END;
END;
PROCEDURE button.hide;
BEGIN
visual := FALSE;
active := FALSE;
END;
{WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW}
PROCEDURE newfile;
BEGIN
        stat.hide;
          board.save('undo.und');
          board.hide;
          inkcol := 1;
          papcol := 15;
          board.init(1);
          board.show;
          board.outall(FALSE,0);
          fcou := fcou+1;
          gfname := 'noname'+stri(fcou)+'.svk';
          stat.show;
END;

PROCEDURE movestatus;
VAR mx1,my1,mb:word;
    mx,my,tx,ty,lx,ly:INTEGER;

BEGIN
mouseread1(mx1,my1,mb);
tx := mx1-stat.x0;
ty := my1-stat.y0;
setWRITEmode(xorput);
setcolor(14);
lx := mx1;ly := my1;
REPEAT
mouseread1(mx1,my1,mb);
mx := mx1;my := my1;
IF my-ty<=board.y THEN my := ty+board.y;
IF mx-tx<=board.x THEN mx := board.x+tx;
IF my-ty>=board.y1-stat.ly0 THEN my := ty+board.y1-stat.ly0;
IF mx-tx>=board.x1-stat.lx0 THEN mx := board.x1+tx-stat.lx0;
IF (lx<>mx)or(ly<>my) THEN
   BEGIN
    mousehide;
    rectangle(lx-tx,ly-ty,lx-tx+stat.lx0,ly-ty+stat.ly0);
    rectangle(mx-tx,my-ty,mx-tx+stat.lx0,my-ty+stat.ly0);
    mouseshow;
    lx := mx;ly := my;
   END;
UNTIL not mbutton;
mousehide;
rectangle(lx-tx,ly-ty,lx-tx+stat.lx0,ly-ty+stat.ly0);
setWRITEmode(0);
stat.changecoord(lx-tx,ly-ty);
mouseshow;
END;

{------------------------------------------------------------------}
{------------------------------------------------------------------}
PROCEDURE clicks;
          VAR number,i,j:INTEGER;
{-------- Помощь -----------}
   PROCEDURE ohelp(n:INTEGER);
VAR ppan:panel;
   BEGIN
   case n of
1:BEGIN


END;

END;
   pause(5);
   mhelp.hide;
   END;
{-------- Файлы -----------}
PROCEDURE ofiles(n:INTEGER);
VAR t:boolean;
    kk,kk1:STRING;
BEGIN
tempstep:=step;
step:=false;
case n of
0:BEGIN mfiles.hide;newfile;END;
1:BEGIN IF inputfilename(gfname,'svk') THEN board.save(gfname);
      mfiles.hide;refreshstatus;END;
2:BEGIN
        IF copy(gfname,1,6)='noname' THEN
        BEGIN IF inputfilename(gfname,'svk') THEN board.save(gfname);END
        else board.save(gfname);
        mfiles.hide;refreshstatus;END;
3:BEGIN t := inputfilename(gfname,'svk');
board.save('undo.und');
      IF t THEN board.load(gfname);
      mfiles.hide;
      IF t THEN BEGIN
      stat.hide;board.outall(FALSE,0);stat.show;
      END;
      END;
4:BEGIN
board.save('undo.und');
        t := TRUE;
        IF copy(gfname,1,6)='noname' THEN
        BEGIN t := inputfilename(gfname,'svk');IF t THEN board.load(gfname);END
        else board.load(gfname);
        mfiles.hide;
        IF t THEN BEGIN
        stat.hide;board.outall(FALSE,0);stat.show;
        END;
        refreshstatus;
        END;
5:BEGIN
board.save('undo.und');
      IF inputfilename(gfname,'svk') THEN board.merge(gfname);
      mfiles.hide;stat.hide;board.outall(FALSE,0);stat.show;END;
6:BEGIN
board.save('undo.und');
       kk := gfname;
       j := pos('.',gfname);
      IF j<>0 THEN delete(gfname,j,length(gfname)-j+1);
      gfname := gfname+'.bas';
      {t := inputfilename(gfname,'bas');}
      {IF t THEN }board.makebasicblock(gfname);
      mfiles.hide;
      noticegraph.show;
      outtextxy(200,120,'Создан DATA-блок с именем:');
      outtextxy(250,140,gfname);
      outtextxy(154,180,'Для продолжения нажмите любую клавишу');
      readkey;
      noticegraph.hide;
      gfname := kk;
      END;
7:BEGIN pause(5);exitsovok(0);END;
END;
step:=tempstep;
refreshstatus;
bfiles.show(FALSE);
mouseread1(mx,my,mb);
END;

{-------- Графика -----------}
   PROCEDURE ograph(n:INTEGER);
   BEGIN
   mode := n+1;
   IF mode>2 THEN mode := mode+1;
   case mode of
   0:;
   1:;
   2:;
   3:;
   4:;
   5:;
   6:;
   END;
   pause(5);
   mgraph.hide;
   refreshstatus;
   END;
{-------- Цвета -----------}
PROCEDURE ocol(n:INTEGER);
VAR i:INTEGER;
BEGIN
IF n<2 THEN BEGIN
 IF n=0 THEN coll := TRUE else coll := FALSE;
 pcolors.show;
 for i := 0 to 14 do colors[i].show(((coll and (i=inkcol-1))or((not coll) and (i=papcol-1))));
 END else
case n of
4:papcol := inkcol;
5:inkcol := papcol;
6:BEGIN inkcol := papcol+inkcol;papcol := inkcol-papcol;inkcol := inkcol-papcol;END;

2:mode := 15;
3:mode := 16;
END;
IF n>1 THEN BEGIN refreshstatus;mcol.hide;END;
END;
{-------- Увеличение -----------}
   PROCEDURE omag(n:INTEGER);
   BEGIN
   case n of
   0:mode := 10;
   1:mode := 11;
   2:board.magnIFy := 1;
   3:board.magnIFy := board.fitonscreen;
   END;
   mmag.hide;
   refreshstatus;
   IF n>1 THEN BEGIN
   IF n=2 THEN board.refreshm(0,0);
   IF n=3 THEN board.refreshm(-35,-13);
   board.outall(FALSE,0);
   END;
   END;
{-------- Дополнения -----------}
FUNCTION vali(st:STRING):INTEGER;
VAR
n,e:INTEGER;
BEGIN
val(st,n,e);
vali := n;
END;
FUNCTION getnextnumber(st:STRING;VAR n:INTEGER):STRING;
VAR o:INTEGER;
  h:STRING;
BEGIN
WHILE (not(st[n] in ['0'..'9'])) do n := n+1;
h := '';
REPEAT
h := h+st[n];
n := n+1;
UNTIL (n>length(st))or(not(st[n] in ['0'..'9']));
getnextnumber := h;
END;

PROCEDURE oe(n:INTEGER);
VAR f,g,n1,n2:INTEGER;
    st,st1,st2:STRING;
PROCEDURE Wwrite(nas:INTEGER;text:STRING;nasa:INTEGER);
BEGIN
outtextxy(information.x+3,information.y+5+16*nas,text+'('+stri(nasa)+')');
END;
BEGIN
IF me.visual THEN me.hide;
case n of
0:
  BEGIN
  tempstep:=step;
  step:=false;
       for f := 1 to board.nelements do mark[f] := FALSE;
       inputnumber.show;
       inputnumber.use;
       st := inputnumber.caption;
       st1 := '';
       f := 1;
       st2 := getnextnumber(st,f);
       f := 1;
    REPEAT
          IF st[f]='-' THEN BEGIN
                             n1 := vali(st2);n2 := vali(getnextnumber(st,f));
                             for g := n1 to n2 do mark[g] := TRUE;WRITELN(n1,';;;',n2);
                            END;
          IF f<=length(st) THEN BEGIN
                                 st1 := st2;
                                 st2 := getnextnumber(st,f);
                                 WRITELN(st2);
                                 mark[vali(st2)] := TRUE;
                                END;
    UNTIL f>=length(st);
    board.save('undo.und');
    board.delmarked;
    inputnumber.hide;
    board.outall(FALSE,0);
    step:=tempstep;
    END;
1:IF board.nelements>0 THEN
     BEGIN
      for f := 0 to board.nelements do mark[f] := FALSE;
      mark[board.nelements] := TRUE;
      board.delmarked;
      board.outall(FALSE,0);
     END;
2:BEGIN
          board.load('undo.und');
          board.outall(FALSE,0);
     END;
3:begin
       tempstep:=step;
       step:=false;
       startingout.query:='Введите начальный номер элемента (max='+stri(board.nelements)+')';
      repeat
       startingout.caption:='0';
       startingout.show;
       startingout.use;
       until (vali(startingout.caption)>=0)and(vali(startingout.caption)<=board.nelements);
       board.outall(TRUE,vali(startingout.caption));
       step:=tempstep;
       end;
4:BEGIN stat.hide;showprogram;board.outall(FALSE,0);stat.show;END;
5:BEGIN stat.hide;stat.visual := not stat.visual;stat.show;END;

6:BEGIN curstep.use;refreshstatus;END;

7:BEGIN
mousehide;
information.show;
showtext(information.x+3,information.y+3,information.lx-6,14,'File:'+gfname,FALSE,7);
wWRITE(1,'Количество элементов.......'+stri(board.nelements),maxel);
wWRITE(2,'Количество точек...........'+stri(board.npoints),maxp);
wWRITE(3,'Количество линий...........'+stri(board.nlines),maxl);
wWRITE(4,'Количество дуг.............'+stri(board.narcs),maxa);
wWRITE(5,'Количество кругов..........'+stri(board.ncircles),maxc);
wWRITE(6,'Количество эллипсов........'+stri(board.nellipses),maxe);
wWRITE(7,'Количество прямоугольников.'+stri(board.nrectangles),maxr);
wWRITE(8,'Количество панелей.........'+stri(board.nbars),maxb);
wWRITE(9,'Количество окрасок.........'+stri(board.nfills),maxf);
showtext(information.x+3,information.y+3+16*10,information.lx-6,16,'The Scoop Editor. Version '+versionname,TRUE,7);
showtext(information.x+3,information.y+3+16*11,information.lx-6,16,'(C) 2002 by Digger from DHG',FALSE,7);
mouseshow;
REPEAT UNTIL not mbutton;
REPEAT UNTIL mbutton or keypressed;
IF keypressed THEN readkey;
information.hide;
END;
END;
refreshstatus;
END;


{ПРОЦЕДУРА ВЫБОРА ЦВЕТА}
PROCEDURE selectcolors;
VAR i,pcol,ppap:INTEGER;
BEGIN
pcol := inkcol;
ppap := papcol;
     for i := 0 to 14 do
     IF colors[i].onclick(mx,my) THEN IF coll THEN inkcol := i+1 else papcol := i+1;
                     for i := 0 to 14 do BEGIN
                         colors[i].visual := FALSE;
                         colors[i].active := FALSE;
                         END;
                     IF (pcol<>inkcol)or(ppap<>papcol) THEN
                         pause(5);
                     pcolors.hide;
                     IF (pcol<>inkcol)or(ppap<>papcol) THEN
                     refreshstatus;
END;

VAR k:INTEGER;
    c:char;
PROCEDURE hidemenus;
BEGIN
IF mfiles.visual THEN BEGIN mfiles.hide;bfiles.show(FALSE);END;
IF mgraph.visual THEN BEGIN mgraph.hide;bgraph.show(FALSE);END;
IF mcol.visual THEN BEGIN mcol.hide;bcol.show(FALSE);END;
IF mmag.visual THEN BEGIN mmag.hide;bmag.show(FALSE);END;
{IF mtext.visual THEN BEGIN mtext.hide;btext.show(FALSE);END;}
IF me.visual THEN BEGIN me.hide;be.show(FALSE);END;
IF mhelp.visual THEN BEGIN mhelp.hide;bhelp.show(FALSE);END;
END;
BEGIN
c := '■';
IF keypressed THEN
BEGIN
c := readkey;
{IF c=#27 THEN exitsovok(0);}
IF c=#0 THEN c := readkey;
k := ord(c)-59+1;
END;
IF (k<=11)and(k>=1)THEN hidemenus;
case k of
1:mfiles.show;
2:mgraph.show;
3:mcol.show;
4:mmag.show;
5:me.show;
6:me.show;
7:mhelp.show;
10:begin closegraph;halt;end;
END;
IF mb<>0 THEN BEGIN
event := FALSE;
{Если включен выбор цветов}
IF mb<>0 THEN IF pcolors.visual THEN BEGIN selectcolors;REPEAT UNTIL not mbutton;mcol.hide;bcol.show(FALSE);END;
IF mb<>0 THEN IF mhelp.onclick(mx,my) THEN ohelp(mhelp.getfield(mx,my));
IF mb<>0 THEN IF mfiles.onclick(mx,my) THEN ofiles(mfiles.getfield(mx,my));
IF mb<>0 THEN IF mgraph.onclick(mx,my) THEN ograph(mgraph.getfield(mx,my));
IF mb<>0 THEN IF mcol.onclick(mx,my) THEN ocol(mcol.getfield(mx,my));
IF mb<>0 THEN IF mmag.onclick(mx,my) THEN omag(mmag.getfield(mx,my));
IF mb<>0 THEN IF me.onclick(mx,my) THEN oe(me.getfield(mx,my));

IF mb<>0 THEN IF bhelp.onclick(mx,my) THEN mhelp.show;
IF mb<>0 THEN IF bfiles.onclick(mx,my) THEN mfiles.show;
IF mb<>0 THEN IF bgraph.onclick(mx,my) THEN mgraph.show;
IF mb<>0 THEN IF bcol.onclick(mx,my) THEN mcol.show;
IF mb<>0 THEN IF bmag.onclick(mx,my) THEN mmag.show;
IF mb<>0 THEN IF be.onclick(mx,my) THEN me.show;
IF mb<>0 THEN IF stat.onclick(mx,my) THEN movestatus;
IF mb<>0 THEN IF not event THEN board.onclick(mx,my);

END;
REPEAT
mouseread1(mx,my,mb);
UNTIL mb=0;
END;

{УСТАНОВКИ НАЧАЛЬНЫХ ЗНАЧЕНИЙ ПЕРЕМЕННЫХ}
PROCEDURE preparevars;
BEGIN
      inkcol := 1;
      papcol := 15;
      mode := 1;
      activeboard := 1;
      nboards := 1;
      step := FALSE;
      fsx := 8;
      fsy := 8;
      fosx := 1;
      fosy := 1;
      fodi := 1;
      fotp := FALSE;
      fossx := 8;
      fossy := 8;
      cfont := 1;
      fbold := FALSE;
      fcou := 0;
      gfname := 'noname'+stri(fcou)+'.svk';
       modes[1] := 'Точка';
       modes[2] := 'Линия';
       modes[4] := 'Дуга';
       modes[5] := 'Круг';
       modes[6] := 'Эллипс';
       modes[7] := 'Прямоугольник';
       modes[8] := 'Панель';
       modes[9] := 'Окраска';
       modes[10] := 'Увеличение';
       modes[11] := 'Уменьшение';
       modes[12] := 'Добавление текста';
       modes[13] := 'Текст сверху-вниз';
       modes[14] := 'Выбор элемента';
       modes[15] := 'Выбор цвета чернил';
       modes[16] := 'Выбор цвета фона';

END;

{УСТАНОВКИ НАЧАЛЬНЫХ ЗНАЧЕНИЙ ОБЪЕКТОВ}
PROCEDURE preparing;
VAR i:INTEGER;
    BEGIN
      curstep.init(220,60,10,10);
      board.init(1);
      upmenu.init(1,2,638,23,7,15,0,FALSE);
{---------------------------ФАЙЛОВОЕ МЕНЮ-----------------------------------}
      mfiles.init(1,'Новый',
      mfiles.init(2,'Записать файл',
      mfiles.init(3,'Перезаписать',
      mfiles.init(4,'Загрузить файл',
      mfiles.init(5,'Перезагрузить',
      mfiles.init(6,'Добавить файл',
      mfiles.init(7,'Сделать DATA блок',
      mfiles.init(8,'Выход',0))))))));
      mfiles.coord(5,40);
      bfiles.init(58-53,5,45,17,'Файлы');

{---------------------------МЕНЮ ГРАФИКИ-------------------------------------}
      mgraph.init(1,'Точка',
      mgraph.init(2,'Линия',
      mgraph.init(3,'Дуга',
      mgraph.init(4,'Круг',
      mgraph.init(5,'Эллипс',
      mgraph.init(6,'Прямоугольник',
      mgraph.init(7,'Панель',
      mgraph.init(8,'Окраска',0))))))));
      mgraph.coord(15,40);
      bgraph.init(52,5,61,17,'Графика');

{---------------------------МЕНЮ ЦВЕТОВ--------------------------------------}
      mcol.init(1,'Другой цвет чернил',
      mcol.init(2,'Другой цвет фона',
      mcol.init(6,'Чернила как фон',
      mcol.init(5,'Фон как чернила',
      mcol.init(3,'Выбор цвета чернил',
      mcol.init(4,'Выбор цвета фона',
      mcol.init(7,'Обменять цвета',0)))))));
      mcol.coord(60,40);
      bcol.init(115,5,44,17,'Цвета');
{---------------------------МЕНЮ УВЕЛИЧЕНИЯ----------------------------------}
      mmag.init(1,'Увеличить в 2 раза',
      mmag.init(2,'Уменьшить в 2 раза',
      mmag.init(3,'Нормальный размер',
      mmag.init(4,'Разместить на экране',0))));
      mmag.coord(120,40);
      bmag.init(214-53,5,83,17,'Увеличение');
{---------------------------МЕНЮ ДОПОЛНЕНИЙ----------------------------------}
      me.init(1,'Удалить элемент',
      me.init(2,'Удалить последний',
      me.init(3,'Вернуть картинку',
      me.init(4,'Вывод картинки',
      me.init(5,'Просмотр программы',
      me.init(6,'Вкл/выкл статус',
      me.init(8,'Информация',
      me.init(7,'Настройка шага',0))))))));
      me.coord(255,40);
      bE.init(299-53,5,84,17,'Дополнения');
{-----------------------------МЕНЮ ПОМОЩИ------------------------------------}
    { mhelp.init(1,'Клавиши управления',
      mhelp.init(2,'О программе',0));
      mhelp.coord(445,40);
      bhelp.init(505,5,51,17,'Помощь');}

      stat.init(10,200,gfname,modes[mode],stri(round(100*board.magnIFy)),step,papcol,inkcol);
      pcolors.init(147,97,148+111,21,8,7,0,TRUE);

      noticegraph.init(147,97,148+160,101,7,8,0,TRUE);

      information.init(180,40,315,200,7,0,0,TRUE);

      for i := 0 to 14 do
          BEGIN
           colors[i].init(150+i*17,100,15,15,'');
           colors[i].col := i+1;
          END;
      inputnumber.init(60,120,40,'Введите номер(а) удаляемых элементов',['0'..'9','-','\',','],TRUE);
      startingout.init(60,120,40,'Введите начальный номер ',['0'..'9','-','\',','],TRUE);
END;

{ПРОЦЕДУРА ЗАПУСКА ПРИЛОЖЕНИЯ}
PROCEDURE run;
BEGIN
     upmenu.show;
     bhelp.show(FALSE);
     bfiles.show(FALSE);
     bgraph.show(FALSE);
     bcol.show(FALSE);
     bmag.show(FALSE);
     bE.show(FALSE);
     board.show;
     showtime;
     IF paramcount<>0 THEN
       BEGIN
        board.load(paramstr(1));
        board.outall(FALSE,0);
        refreshstatus;
       END;
      inkcol := 1;
      papcol := 15;

     stat.show;
     prevtime := '';
     mouseshow;
     REPEAT
       showtime;
       mouseread1(mx,my,mb);
       IF (mb<>0)or keypressed THEN clicks;
     UNTIL nkeypressed(#2);
END;
PROCEDURE showintro;
var iny,inx:integer;
BEGIN
        board.load('intro.svk');
        setvisualpage(1);
        if (board.nelements=219)and(board.npoints=13)and(board.nlines=103)and(board.ncircles=1)and(board.nfills=44)

        then board.outall(false,1) else
        begin
        setactivepage(1);
        settextstyle(0,0,11);
        setcolor(14);
        setbkcolor(1);
        outtextxy(5,40,'"СОВОК"');
        settextstyle(0,0,1);
        setcolor(7);
        outtextxy(370,2,'Заставка отсутствует или искажена');
        settextstyle(0,0,2);
        outtextxy(10,120,'Версия '+versionname);
        settextstyle(0,0,1);
        setcolor(15);
        end;
        setvisualpage(1);
        setactivepage(1);
        iny:=193;
        inx:=500;

        setcolor(10);
        outtextxy(4,iny+120,'(C) 2002 Мамичев Антон. E-mail: antonmamichev@live.ru  https://mamichev.ru');
        setcolor(8);
        outtextxy(4,iny+130,'Данная  программа  является  объектом  авторского  права  и  защищена  законом');
        outtextxy(4,iny+140,'Российской Федерации "Об авторских и смежных правах".');
        setcolor(15);
        outtextxy(1,1,'https://mamichev.ru/content/sovok');
        setcolor(15);
        outtextxy(inx,iny+10,'Векторный');
        outtextxy(inx,iny+20,'графический');
        outtextxy(inx,iny+30,'редактор');
        settextstyle(0,0,2);
        setcolor(14);
        outtextxy(inx+14,iny+40,'"СОВОК"');
        settextstyle(0,0,1);
        setcolor(15);
        outtextxy(inx,15,'Версия '+versionname);
        outtextxy(inx,iny+55,'с');
        outtextxy(inx,iny+65,'возможностью');
        outtextxy(inx,iny+75,'экспорта');
        outtextxy(inx,iny+85,'изображений');
        outtextxy(inx,iny+95,'в формат');
        outtextxy(inx,iny+105,'DATA-BASIC');
     repeat until keypressed;readkey;
        setbkcolor(0);

     setactivepage(0);
     setvisualpage(0);
     cleardevice;

END;
{ОСНОВНАЯ ПРОГРАММА}
BEGIN
     mouseread(moldx,moldy,mb);
     getdir(3,path);
     fontop := FALSE;
     gd := detect;
     initgraph(gd,gm,path);
     setgraphmode(1);
     setvisualpage(0);
     setactivepage(0);
     cleardevice;
     preparevars;
     showintro;
     preparing;
     run;
     exitsovok(0);
END.