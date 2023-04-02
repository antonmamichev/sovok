UNIT svkread;
INTERFACE
USES graph,crt;
CONST maxx=640;maxy=350;maxp=500;maxl=800;maxa=300;maxc=300;
      maxe=300;maxr=300;maxb=300;maxf=200;maxt=20;
      maxel=maxp+maxl+maxa+maxc+maxe+maxr+maxb+maxf+maxt;
      ars=1;mscp=5;
      versionname='3.5.2 BETA';
var   name:string;
      elements:array[1..maxel,1..2] of INTEGER;
      points:array[1..maxp*3] of INTEGER;
      lines:array[1..maxl*5] of INTEGER;
      arcs:array[1..maxa*6] of INTEGER;
      circles:array[1..maxc*4] of INTEGER;
      ellipses:array[1..maxe*5] of INTEGER;
      rectangles:array[1..maxr*5] of INTEGER;
      bars:array[1..maxb*5] of INTEGER;
      fills:array[1..maxf*4] of INTEGER;
      npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,ntexts:INTEGER;
      nelements,inkcol,papcol,gd,gm:INTEGER;
      magnify:real;
      x,y,sssx,sssy,x1,y1:integer;
PROCEDURE loadscreen(name:string);
PROCEDURE outall(ssx,ssy:integer);

IMPLEMENTATION
FUNCTION magnx(numb:INTEGER):INTEGER;
BEGIN
magnx := round((numb+x)*magnIFy-sssx);
END;

FUNCTION magny(numb:INTEGER):INTEGER;
BEGIN
magny := round((numb+y)*magnIFy-sssy);
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

PROCEDURE outelement(i:integer);
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

PROCEDURE outall(ssx,ssy:integer);
VAR i:INTEGER;
check:boolean;
 size,lstep:INTEGER;
    p:pointer;
BEGIN
sssx:=-ssx;
sssy:=-ssy;
cleardevice;
for i := 1 to nelements do outelement(i);
END;

PROCEDURE load(name:string);
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

PROCEDURE loadscreen(name:string);
begin
x:=0;
y:=0;
load(name);
end;
end.