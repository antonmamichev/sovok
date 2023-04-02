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
       writeln('    Используйте следующие параметры:');writeln;
       writeln('    viewer.exe [filename] [gr],');
       writeln('    где');
       writeln('    filename - имя файла с картинкой из СОВКА;');
       writeln('    gr - оставлять графический режим при выходе (1 - не отключать).');writeln;
       writeln('    (C) 2002 Антон Мамичев (https://mamichev.ru)');
       ;
       end;
end.
