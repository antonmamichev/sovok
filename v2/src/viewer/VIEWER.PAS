uses graph,crt,svkread,dos;
       Var gd, gm: Integer;
begin
     IF paramcount<>0 THEN
       BEGIN
    gd:= detect;
    Initgraph (gd, gm, '');
    Setgraphmode (1);
    Loadscreen (paramstr(1));
    cleardevice;
    setbkcolor(15);

    Outall (0, 0);
    Readkey;
    if paramstr(2)<>'1' then closegraph;
    end
       else
       begin
       clrscr;
       writeln('    �ᯮ���� ᫥���騥 ��ࠬ����:');writeln;
       writeln('    viewer.exe [filename] [gr],');
       writeln('    ���');
       writeln('    filename - ��� 䠩�� � ���⨭��� �� �����;');
       writeln('    gr - ��⠢���� ����᪨� ०�� �� ��室� (1 - �� �⪫����).');writeln;
       writeln('    (C) 2002 ��⮭ ����祢 (https://mamichev.ru)');
       ;
       end;
end.
