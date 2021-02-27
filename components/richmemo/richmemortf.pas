unit RichMemoRTF;

interface

{$mode objfpc}{$h+}

uses
  Classes, SysUtils, LCLProc, LCLIntf, LConvEncoding, Graphics,
  RichMemo, RTFParsPre211, RTFCustomParser;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream; mode:TLoadingMode=lmDefault): Boolean;
procedure RegisterRTFLoader;

type
  TSaveParams = record // reserved
    start  : Integer; // the first character for the extract
    len    : Integer; // the number of characters to extract
  end;

// the function depends on GetStyleRange and to be implemented properly
// if GetStyleRange, GetParaMetric, GetParaAlignment is not working properly
// the resulting RTF would not contain any styles or the text styles would be wrong
procedure IntSaveStream(ARich: TcustomRichMemo; SaveParams: TSaveParams; Dst: TStream);
function SaveStream(ARich: TCustomRichMemo; Dst: TStream): Boolean;
procedure RegisterRTFSaver;

implementation
uses LazUTF8;

type
  { TRTFMemoParser }

  TRTFMemoParser = class(TRTFCustomParser)
  private
    fLoadingMode: TLoadingMode;
    procedure LoadFromChunks;
  protected
    procedure PushText; override;
  public
    Memo  : TCustomRichMemo;
    constructor Create(AMemo: TCustomRichMemo; AStream: TStream);
    procedure StartReading; override;
    property LoadingMode: TLoadingMode read fLoadingMode write fLoadingMode;
  end;

{ TRTFMemoParserr }

procedure TRTFMemoParser.LoadFromChunks;
var
  offset, i, len: Integer;
begin
  Memo.Lines.BeginUpdate;
  try
    offset := 0;
    for i:=0 to Length(Chunks)-1 do begin

      len := UTF8Length(Chunks[i].Text);
      Memo.InDelText(Chunks[i].Text, offset, 0);
      Memo.SetParaMetric(offset, len, Chunks[i].prm.pm);
      if Chunks[i].prm.tabs.Count>0 then
        Memo.SetParaTabs(offset, len, Chunks[i].prm.tabs);
      Memo.SetTextAttributes(offset, len, Chunks[i].prm.fnt);
      if Chunks[i].Link<>'' then
        Memo.SetLink(offset, len, true, Chunks[i].Link);

      inc(offset, len);
    end;
  finally
    Memo.Lines.EndUpdate;
  end;
end;

procedure TRTFMemoParser.PushText;
var
  len   : Integer;
  pf    : PRTFFONT;
  selst : Integer;
  b     : string;
begin

  if fLoadingMode<>lmDefault then begin
    inherited PushText;
    exit;
  end;

  if not Assigned(prm) then exit;
  if txtlen=0 then Exit;

  b:=Copy(txtbuf, 1, txtlen);
  len:=UTF8Length(b);

  txtlen:=0;
  txtbuf:='';
  if len=0 then Exit;

  Memo.SelStart:=MaxInt;
  selst:=Memo.SelStart;
  // in order to get the start selection, we need to switch to the last character
  // and then get the value. SelStart doesn't match GetTextLen, since
  // "StartSel" is based on number of visible characters (i.e. line break is 1 character)
  // while GetTextLen is based on number of actual string characters
  // selst:=Memo.GetTextLen;

  Memo.SelStart:=selst;
  Memo.SelLength:=0;
  Memo.SelText:=b;

  if Assigned(prm) then begin
    prm.pm.FirstLine:=prm.pm.HeadIndent+prm.pm.FirstLine;
    Memo.SetParaMetric(selst, 1, prm.pm );
    prm.pm.FirstLine:=prm.pm.FirstLine-prm.pm.HeadIndent;

    Memo.SetParaAlignment(selst, 1, prm.pa );

    if prm.tabs.Count>0 then
      Memo.SetParaTabs(selst, 1, prm.tabs);
  end;

//  Memo.GetTextAttributes(selst, font);
  pf:=Fonts[prm.fnum];
  if Assigned(pf) then prm.fnt.Name:=pf^.rtfFName;
  //prm.fnt.Size:=round(fsz);
  //prm.fnt.Style:=fst;
  //prm.fnt.Color:=ColorToRGB(fColor);
  //prm.fnt.HasBkClr:=hasbk;
  //prm.fnt.BkColor:=bcolor;
  Memo.SetTextAttributes(selst, len, prm.fnt);

  if Field.valid then begin
    if ResolveHyperlink(b) then
      Memo.SetLink(selst, len, true, b);
  end;
end;

constructor TRTFMemoParser.Create(AMemo:TCustomRichMemo;AStream:TStream);
begin
  inherited Create(AStream);
  Memo:=AMemo;
end;

procedure TRTFMemoParser.StartReading;
begin
  if fLoadingMode=lmFast then begin
    inherited StartReading;
    Consolidate;
    LoadFromChunks;
  end else
  if fLoadingMode=lmWidgetset then begin
    inherited StartReading;
    Memo.Lines.BeginUpdate;
    //DumpChunks;
    //Consolidate;
    Memo.LoadFromChunkArray(Chunks);
    Memo.Lines.EndUpdate;
  end else begin
    Memo.Lines.BeginUpdate;
    try
      inherited StartReading;
    finally
      Memo.Lines.EndUpdate;
    end;
  end;
  Memo.SelStart:=0;
  Memo.SelLength:=0;
end;

function MVCParserLoadStream(ARich: TCustomRichMemo; Source: TStream;
  mode: TLoadingMode): Boolean;
var
  p   : TRTFMemoParser;
begin
  Result:=Assigned(ARich) and Assigned(Source);
  if not Result then Exit;

  p:=TRTFMemoParser.Create(ARich, Source);
  p.LoadingMode := mode;
  try
    p.StartReading;
  finally
    p.Free;
  end;
  Result:=True;
end;

procedure RegisterRTFLoader;
begin
  RTFLoadStream:=@MVCParserLoadStream;
  LangConvInit;
end;

function SaveStream(ARich: TCustomRichMemo; Dst: TStream): Boolean;
var
  p : TSaveParams;
begin
  FillChar(p, sizeof(p), 0);
  p.start:=-1;
  p.len:=-1;
  IntSaveStream(ARich, p, Dst);
  Result:=True;
end;

procedure RegisterRTFSaver;
begin
  RTFSaveStream:=@SaveStream;
end;

type
  TStyleRange = class(TObject)
    font       : TFontParams;
    fontId     : Integer; // assigned font ID
    colorId    : Integer;
    textStart  : Integer;
    textLength : Integer;
    linkRef    : string;
    next       : TStyleRange;
  end;

procedure FreeStyleList(var root: TStyleRange);
var
  t: TStyleRange;
begin
  while Assigned(root) do begin
    t:=root.next;
    root.Free;
    root:=t;
  end;
end;

procedure PrepareFontTable(styleslist: TStyleRange; afontTable: TStringList);
var
  rng : TStyleRange;
  i   : integer;
begin
  rng:=styleslist;
  while Assigned(rng) do begin
    i:=afontTable.IndexOf(rng.font.Name);
    if i<0 then
      i:=afontTable.Add(rng.font.Name);
    rng.fontId:=i;
    rng:=rng.next;
  end;
  // {\f0\fswiss\fcharset0 Arial;}
end;

function ColorToRtfText(const cl: TColor): string;
var
  r: integer;
begin
  r:=ColorToRGB(cl);
  Result:=
    '\red'+IntToStR( byte( (r and clRed) shr 0) )
    +'\green'+IntToStR( byte( (r and clLime) shr 8) )
    +'\blue'+IntToStR( byte( (r and clBlue) shr 16) );
end;

procedure PrepareColorTable(styleslist: TStyleRange; acolorTable: TStringList);
var
  rng : TStyleRange;
  i   : integer;
  t   : string;
begin
  rng:=styleslist;
  acolorTable.Add('');
  while Assigned(rng) do begin
    if rng.font.Color=clBlack then
      rng.colorId:=0
    else begin
      t:=ColorToRtfText(rng.font.Color);
      i:=acolorTable.IndexOf(t);
      if i<0 then i:=acolorTable.Add(t);
      rng.colorId:=i;
    end;
    rng:=rng.next;
  end;
  // {\f0\fswiss\fcharset0 Arial;}
end;

function GetRTFWriteText(const u: UnicodeString; var idx : integer; var isNewPara: Boolean): string;
var
  i : integer;
begin
  Result:='';
  i:=idx;
  isNewPara:=false;
  while i<=length(u) do begin
    if u[i]='\' then Result:=Result+'\\'
    else if u[i]='{' then Result:=Result+'\{'
    else if u[i]='}' then Result:=Result+'\}'
    else if u[i]=#10 then begin
      Result:=Result+'\par ';
      isNewPara:=true;
      inc(i);
      Break;
    end else if u[i]=#13 then begin
      Result:=Result+'\par ';
      isNewPara:=true;
      inc(i);
      Break;
    end else if u[i]<#127 then Result:=Result+char(byte(u[i]))
    else Result:=Result+'\u'+IntToStr(word(u[i]))+'  '; // adding a blank "space" character replacement
    inc(i);
  end;
  idx:=i;
end;

procedure IntSaveStream(ARich: TcustomRichMemo; SaveParams: TSaveParams;
  Dst: TStream);
var
  ofs     : Integer;
  needlen : Integer;
  endless : Boolean;
  root    : TStyleRange; // first in the list
  last    : TStyleRange; // last in the list
  rng     : TStyleRange; // temproray
  st, len : Integer;
  u       : UnicodeString;
  fontTable  : TStringList;
  colorTable : TStringList;
  i         : Integer;
  isnewpara : Boolean;
  s         : string;
  aLinkRef  : string;

  isbold    : Boolean;
  isitalic  : Boolean;
  isuline   : Boolean;
  issupersub: Boolean;
  isColor   : integer;

  pm : TParaMetric;

  procedure RtfOut(const s: string);
  begin
    Dst.Write(s[1], length(s));
  end;

begin
  if SaveParams.start<0 then ofs:=0
  else ofs:=SaveParams.start;
  root:=nil;
  last:=nil;
  needlen:=SaveParams.len;
  endless:=needlen<0;

  while ARich.GetStyleRange(ofs, st, len) do begin
    rng:=TStyleRange.Create;
    rng.textStart:=st;
    if not endless then begin
      if needlen<len then rng.textLength:=needlen
      else rng.textLength:=len;
      dec(needLen, len);
    end else
      rng.textLength:=len;
    ARich.GetTextAttributes(ofs, rng.font);

    if ARich.GetLink(ofs, aLinkRef) then
      rng.linkRef := aLinkRef;

    if not Assigned(root) then root:=rng;
    if Assigned(last) then last.next:=rng;
    last:=rng;

    inc(ofs, len);
    if not endless and (needLen<=0) then break;
  end;

  if root=nil then begin
    // GetStyleRange failed - fallback to simple style export!
    root:=TStyleRange.Create;
    root.textStart:=0;
    root.textLength:=MaxInt;
    root.font.Name:=ARich.Font.Name;
    root.font.Size:=ARich.Font.Size;
  end;

  fontTable:=TStringList.Create;
  colorTable:=TStringList.Create;
  try
    PrepareFontTable(root, fontTable);
    PrepareColorTable(root, colorTable);

    RtfOut('{\rtf1\ansi\ansicp1252\deff0\deflan1033');

    // start of RTF
    if fontTable.Count>0 then begin
      // at least on font should be present anyway.
      RtfOut('{\fonttbl');
      for i:=0 to fontTable.Count-1 do begin
        // setting font id, charset to 0 and name
        RtfOut('{\f'+IntToStR(i)+'\fcharset0 '+fontTable[i]+';}');
      end;
      RtfOut('}');
    end;
    if colorTable.Count>1 then begin
      RtfOut('{\colortbl');
      for i:=0 to colorTable.Count-1 do begin
        RtfOut( colortable[i] );
        RtfOut( ';');
      end;
      RtfOut('}');
    end;

    isnewpara := true;
    rng:=root;
    isbold:=false;
    isitalic:=false;
    issupersub:=false;
    iscolor:=0;
    while Assigned(rng) do begin
      u:=ARich.GetUText(rng.textStart, rng.textLength);
      RtfOut('\f'+IntToStr(rng.fontId));
      RtfOut('\fs'+IntToStr(rng.font.Size*2));
      if (fsBold in rng.font.Style) then begin
        RtfOut('\b');
        isbold:=true;
      end else begin
        if isbold then RtfOut('\b0');
        isbold:=false;
      end;
      if (fsUnderline in rng.font.Style) then begin
        RtfOut('\ul');
        isuline:=true
      end else begin
        if isuline then RtfOut('\ulnone');
        isuline:=false;
      end;
      if isColor<>rng.colorId then begin
        RtfOut('\cf'+IntToStR(rng.colorId));
        isColor:=rng.ColorId;
      end;
      if (fsItalic in rng.font.Style) then begin
        RtfOut('\i');
        isitalic:=true;
      end else begin
        if isitalic then RtfOut('\i0');
        isitalic:=false;
      end;

      if rng.font.VScriptPos=vpSuperScript then begin
        RtfOut('\super');
        issupersub:=true;
      end;
      if rng.font.VScriptPos=vpSubScript then begin
        RtfOut('\sub');
        issupersub:=true;
      end;
      if rng.font.VScriptPos=vpNormal then begin
        if issupersub then RtfOut('\nosupersub');
        issupersub:=false;
      end;

      RtfOut(' ');

      i:=1;
      if rng.linkRef<>'' then begin
        RtfOut('{\field ');
        RtfOut(format('{\*\fldinst HYPERLINK "%s"}',[rng.linkRef]));
        RtfOut(format('{\fldrslt{%s}}}',[GetRTFWriteText(u, i, isnewpara)]));
      end else begin
        while i<=length(u) do begin
          if isNewPara then begin
            ARich.GetParaMetric(i+rng.textStart, pm);
            RtfOut('\pard');
            case ARich.GetParaAlignment(i+rng.TextStart) of
              paRight:   RtfOut('\qr');
              paCenter:  RtfOut('\qc');
              paJustify: RtfOut('\qj');
            else
            end;
            RtfOut('\li'+IntToStr(round(pm.HeadIndent*20)));
            if pm.FirstLine-pm.HeadIndent<>0 then
              RtfOut('\fi'+IntToStr(round((pm.FirstLine-pm.HeadIndent)*20)));
            if pm.TailIndent<>0 then RtfOut('\ri'+IntToStr(round(pm.TailIndent*20)));
            if pm.SpaceAfter<>0 then RtfOut('\sa'+IntToStr(round(pm.SpaceAfter*20)));
            if pm.SpaceBefore<>0 then RtfOut('\sb'+IntToStr(round(pm.SpaceBefore*20)));
            if pm.LineSpacing<>0 then RtfOut('\sl'+IntToStr(round(pm.LineSpacing*200))+'\slmult1');
            RtfOut(' ');
          end;
          s:=GetRTFWriteText(u, i, isnewpara);
          RtfOut(s);
        end;
      end;
      rng:=rng.next;
    end;

    // end of RTF
    RtfOut('}');
  finally
    fontTable.Free;
    colorTable.Free;
  end;
  FreeStyleList(root);
end;

initialization

end.
