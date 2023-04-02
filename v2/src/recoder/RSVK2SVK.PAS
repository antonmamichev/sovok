{$A+,B-,D+,E+,F-,G-,I+,L+,N+,O+,P-,Q-,R-,S+,T-,V+,X+}
{$M 16384,0,655360}
uses crt;
const maxx=640;
      maxy=350;
      maxp=400;
      maxl=400;
      maxa=400;
      maxc=400;
      maxe=200;
      maxr=400;
      maxb=400;
      maxf=400;
      maxt=20;
      maxel=maxp+maxl+maxa+maxc+maxe+maxr+maxb+maxf+maxt;
      ars=1;
      mscp=5;

type
ginza=array[1..maxt]of string[94];
var oldname,newname:string;
    oldfile:file of single;
    tempv,tmp1:single;
    j,i,ne:longint;
elements:array[1..maxel,1..2] of integer;
points:array[1..maxp*3] of integer;
lines:array[1..maxl*5] of integer;
arcs:array[1..maxa*6] of integer;
circles:array[1..maxc*4] of integer;
ellipses:array[1..maxe*5] of integer;
rectangles:array[1..maxr*5] of integer;
bars:array[1..maxb*5] of integer;
fills:array[1..maxf*4] of integer;
texts:ginza;
npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,ntexts:integer;
nelements:integer;
    inkcol,papcol:integer;

procedure save;
var g:file of integer;
    i,j,e:integer;
    mag,smex,smey:integer;
begin
assign(g,newname);
rewrite(g);
i:=1;
write(g,i);
mag:=100;
smex:=0;
smey:=0;
write(g,mag,smex,smey,inkcol,papcol);
write(g,nelements,npoints,nlines,narcs,ncircles,nellipses,nrectangles,nbars,nfills,ntexts);
for i:=1 to nelements do write(g,elements[i,1],elements[i,2]);
for i:=1 to npoints*3 do write(g,points[i]);
for i:=1 to nlines*5 do write(g,lines[i]);
for i:=1 to narcs*6 do write(g,arcs[i]);
for i:=1 to ncircles*4 do write(g,circles[i]);
for i:=1 to nellipses*5 do write(g,ellipses[i]);
for i:=1 to nrectangles*5 do write(g,rectangles[i]);
for i:=1 to nbars*5 do write(g,bars[i]);
for i:=1 to nfills*4 do write(g,fills[i]);
for i:=1 to ntexts do
begin
e:=length(texts[i]);
write(g,e);
for j:=1 to e do
begin
e:=ord(texts[i,j]);
write(g,e);
end;
end;
close(g);
end;

begin
clrscr;
npoints:=0;
nlines:=0;
narcs:=0;
ncircles:=0;
nellipses:=0;
nrectangles:=0;
nbars:=0;
nfills:=0;
ntexts:=0;
nelements:=0;

{oldname:='morda.svk';}
if paramcount=0 then begin
writeln;
writeln('(C) 1999 Old to new Sovok Files Recoder by Digger //DHG');
writeln;
writeln('Format: rsvk2svk.exe <filename>');
writeln;
writeln(' Where: filename - Name of the Old Sovok data file');
writeln;
halt;
end;
oldname:=paramstr(1);
newname:=oldname;

j:=pos('.',oldname);
if j<>0 then delete(oldname,j,length(oldname)-j+1);
oldname:=oldname+'.old';

assign(oldfile,newname);
rename(oldfile,oldname);
reset(oldfile);
read(oldfile,tempv);
ne:=round(tempv)-1;
i:=1;
repeat
read(oldfile,tempv);
case round(tempv) of

1000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=1;
          elements[nelements,2]:=npoints;
          read(oldfile,tmp1);
          points[npoints*3+1]:=round(tmp1);
          read(oldfile,tmp1);
          points[npoints*3+2]:=round(tmp1);
          read(oldfile,tmp1);
          points[npoints*3+3]:=round(tmp1);
          npoints:=npoints+1;
          i:=i+3;
          end;
2000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=2;
          elements[nelements,2]:=nlines;
          read(oldfile,tmp1);
          lines[nlines*5+1]:=round(tmp1);
          read(oldfile,tmp1);
          lines[nlines*5+2]:=round(tmp1);
          read(oldfile,tmp1);
          lines[nlines*5+3]:=round(tmp1);
          read(oldfile,tmp1);
          lines[nlines*5+4]:=round(tmp1);
          read(oldfile,tmp1);
          lines[nlines*5+5]:=round(tmp1);
          nlines:=nlines+1;
          i:=i+5;
          end;
3000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=5;
          elements[nelements,2]:=ncircles;
          read(oldfile,tmp1);
          circles[ncircles*4+1]:=round(tmp1);
          read(oldfile,tmp1);
          circles[ncircles*4+2]:=round(tmp1);
          read(oldfile,tmp1);
          circles[ncircles*4+3]:=round(tmp1);
          read(oldfile,tmp1);
          circles[ncircles*4+4]:=round(tmp1);
          ncircles:=ncircles+1;
          i:=i+4;
          end;
4000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=9;
          elements[nelements,2]:=nfills;
          read(oldfile,tmp1);
          fills[nfills*4+1]:=round(tmp1);
          read(oldfile,tmp1);
          fills[nfills*4+2]:=round(tmp1);
          read(oldfile,tmp1);
          fills[nfills*4+4]:=round(tmp1);
          read(oldfile,tmp1);
          fills[nfills*4+3]:=round(tmp1);
          nfills:=nfills+1;
          i:=i+4;
          end;

5000:{begin
          nelements:=nelements+1;
          elements[nelements,1]:=;
          elements[nelements,2]:=n;
          read(oldfile,tmp1);
          [n*3+1]:=round(tmp1);
          read(oldfile,tmp1);
          [n*3+2]:=round(tmp1);
          read(oldfile,tmp1);
          [n*3+3]:=round(tmp1);
          read(oldfile,tmp1);
          [n*3+4]:=round(tmp1);
          read(oldfile,tmp1);
          [n*3+5]:=round(tmp1);
          n:=n+1;
          i:=i+;
          end};

6000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=7;
          elements[nelements,2]:=nrectangles;
          read(oldfile,tmp1);
          rectangles[nrectangles*5+1]:=round(tmp1);
          read(oldfile,tmp1);
          rectangles[nrectangles*5+2]:=round(tmp1);
          read(oldfile,tmp1);
          rectangles[nrectangles*5+3]:=round(tmp1);
          read(oldfile,tmp1);
          rectangles[nrectangles*5+4]:=round(tmp1);
          read(oldfile,tmp1);
          rectangles[nrectangles*5+5]:=round(tmp1);
          nrectangles:=nrectangles+1;
          i:=i+5;
          end;

7000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=8;
          elements[nelements,2]:=nbars;
          read(oldfile,tmp1);
          bars[nbars*5+1]:=round(tmp1);
          read(oldfile,tmp1);
          bars[nbars*5+2]:=round(tmp1);
          read(oldfile,tmp1);
          bars[nbars*5+3]:=round(tmp1);
          read(oldfile,tmp1);
          bars[nbars*5+4]:=round(tmp1);
          read(oldfile,tmp1);
          bars[nbars*5+5]:=round(tmp1);
          nbars:=nbars+1;
          i:=i+5;
          end;

8500:begin
          nelements:=nelements+1;
          elements[nelements,1]:=6;
          elements[nelements,2]:=nellipses;
          read(oldfile,tmp1);
          ellipses[nellipses*5+1]:=round(tmp1);
          read(oldfile,tmp1);
          ellipses[nellipses*5+2]:=round(tmp1);
          read(oldfile,tmp1);
          ellipses[nellipses*5+3]:=round(tmp1);
          read(oldfile,tmp1);
          ellipses[nellipses*5+4]:=round(tmp1);
          read(oldfile,tmp1);
          ellipses[nellipses*5+5]:=round(tmp1);
          nellipses:=nellipses+1;
          i:=i+5;
          end;

8000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=4;
          elements[nelements,2]:=narcs;
          read(oldfile,tmp1);
          arcs[narcs*6+1]:=round(tmp1);
          read(oldfile,tmp1);
          arcs[narcs*6+2]:=round(tmp1);
          read(oldfile,tmp1);
          arcs[narcs*6+3]:=round(tmp1);
          read(oldfile,tmp1);
          arcs[narcs*6+4]:=round(tmp1)*91;
          read(oldfile,tmp1);
          arcs[narcs*6+5]:=round(tmp1)*91;
          read(oldfile,tmp1);
          arcs[narcs*6+6]:=round(tmp1);
          narcs:=narcs+1;
          i:=i+6;
          end;

{3000:begin
          nelements:=nelements+1;
          elements[nelements,1]:=;
          elements[nelements,2]:=n;
          read(oldfile,tmp1);
          [n*+1]:=round(tmp1);
          read(oldfile,tmp1);
          [n*+2]:=round(tmp1);
          read(oldfile,tmp1);
          [n*+3]:=round(tmp1);
          read(oldfile,tmp1);
          [n*+4]:=round(tmp1);
          read(oldfile,tmp1);
          [n*+5]:=round(tmp1);
          n:=n+1;
          i:=i+;
          end;}




end;




i:=i+1;
until i>=ne;
close(oldfile);

inkcol:=1;
papcol:=15;
save;
writeln;
writeln('(C) 1999 Old to new Sovok Files Recoder by Digger //DHG');
end.