unit RTFCustomParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LConvEncoding, Graphics, RichMemo, RTFParsPre211;

type
  TEncConvProc = function (const s: string): string;

//todo: rewrite! it's not language based but fontchar-set based
procedure LangConvAdd(lang: Integer; convproc: TEncConvProc);
function LangConvGet(lang: Integer; var convproc: TEncConvProc): Boolean;

type

  { TRTFCustomParser }

  TRTFCustomParser = class(TRTFParser)
  protected
    defColor : TColor;
    txtbuf   : String; // keep it UTF8 encoded!
    txtlen    : Integer;

    HLFromCTable : Boolean;

    prm       : TRTFParams;
    lang      : Integer;
    langproc  : TEncConvProc;
    deflang   : integer;

    skipNextCh: Boolean; // For a Unicode escape the control word \u is used,
                         // followed by a 16-bit signed decimal integer giving
                         // the Unicode UTF-16 code unit number. For the benefit
                         // of programs without Unicode support, this must be followed
                         // by the nearest representation of this character in the specified code page.
                         // For example, \u1576? would give the Arabic letter bāʼ ب, specifying that
                         // older programs which do not have Unicode support should render it as a
                         // question mark instead.

    procedure AddText(const atext: string);
    function  ResolveHyperlink(out alinkref: string): boolean;
  protected
    procedure classUnk;
    procedure classText;
    procedure classControl;
    procedure classGroup;
    procedure classEof;
    procedure doChangePara(aminor, aparam: Integer);

    procedure doDestination(aminor, aparam: Integer);
    procedure doSpecialChar;
    procedure doChangeCharAttr(aminor, aparam: Integer);

    procedure SetLanguage(AlangCode: integer);

    function DefaultTextColor: TColor;
    procedure PushText; virtual;
    procedure Consolidate;
  public
    chunks: TRTFChunkArray;
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure StartReading; override;
  end;

  procedure LangConvInit;

implementation

var
  LangConvTable : array of record lang: integer; proc: TEncConvProc end;
  LangCount     : Integer = 0;

procedure LangConvAdd(lang: Integer; convproc: TEncConvProc);
var
  i  : integer;
begin
  for i:=0 to LangCount-1 do
    if LangConvTable[i].lang=lang then begin
      LangConvTable[i].proc:=convproc;
      Exit;
    end;
  if LangCount=length(LangConvTable) then begin
    if LangCount=0 then SetLength(LangConvTable, 64)
    else SetLength(LangConvTable, LangCount*2);
  end;
  LangConvTable[LangCount].lang:=lang;
  LangConvTable[LangCount].proc:=convproc;
  inc(LangCount);
end;

function LangConvGet(lang: Integer; var convproc: TEncConvProc): Boolean;
var
  i  : integer;
begin
  for i:=0 to LangCount-1 do
    if LangConvTable[i].lang=lang then begin
      convproc:=LangConvTable[i].proc;
      Result:=true;
      Exit;
    end;
  Result:=false;
end;

procedure LangConvInit;
begin

  if LangCount>0 then
    exit;

  LangConvAdd(1052, @CP1250ToUTF8); // Albanian
  LangConvAdd(1050, @CP1250ToUTF8); // Croatian
  LangConvAdd(1029, @CP1250ToUTF8); // Czech
  LangConvAdd(1038, @CP1250ToUTF8); // Hungarian
  LangConvAdd(1045, @CP1250ToUTF8); // Polish
  LangConvAdd(1048, @CP1250ToUTF8); // Romanian
  LangConvAdd(2074, @CP1250ToUTF8); // Serbian - Latin
  LangConvAdd(1051, @CP1250ToUTF8); // Slovak
  LangConvAdd(1060, @CP1250ToUTF8); // Slovenian

  LangConvAdd(2092, @CP1251ToUTF8); // Azeri - Cyrillic
  LangConvAdd(1059, @CP1251ToUTF8); // Belarusian
  LangConvAdd(1026, @CP1251ToUTF8); // Bulgarian
  LangConvAdd(1071, @CP1251ToUTF8); // FYRO Macedonia
  LangConvAdd(1087, @CP1251ToUTF8); // Kazakh
  LangConvAdd(1088, @CP1251ToUTF8); // Kyrgyz - Cyrillic
  LangConvAdd(1104, @CP1251ToUTF8); // Mongolian
  LangConvAdd(1049, @CP1251ToUTF8); // Russian
  LangConvAdd(3098, @CP1251ToUTF8); // Serbian - Cyrillic
  LangConvAdd(1092, @CP1251ToUTF8); // Tatar
  LangConvAdd(1058, @CP1251ToUTF8); // Ukrainian
  LangConvAdd(2115, @CP1251ToUTF8); // Uzbek - Cyrillic

  LangConvAdd(1078, @CP1252ToUTF8); // Afrikaans
  LangConvAdd(1069, @CP1252ToUTF8); // Basque
  LangConvAdd(1027, @CP1252ToUTF8); // Catalan
  LangConvAdd(1030, @CP1252ToUTF8); // Danish
  LangConvAdd(2067, @CP1252ToUTF8); // Dutch - Belgium
  LangConvAdd(1043, @CP1252ToUTF8); // Dutch - Netherlands
  LangConvAdd(3081, @CP1252ToUTF8); // English - Australia
  LangConvAdd(10249,@CP1252ToUTF8); // English - Belize
  LangConvAdd(4105, @CP1252ToUTF8); // English - Canada
  LangConvAdd(9225, @CP1252ToUTF8); // English - Caribbean
  LangConvAdd(2057, @CP1252ToUTF8); // English - Great Britain
  LangConvAdd(6153, @CP1252ToUTF8); // English - Ireland
  LangConvAdd(8201, @CP1252ToUTF8); // English - Jamaica
  LangConvAdd(5129, @CP1252ToUTF8); // English - New Zealand
  LangConvAdd(13321,@CP1252ToUTF8); // English - Phillippines
  LangConvAdd(7177, @CP1252ToUTF8); // English - Southern Africa
  LangConvAdd(11273,@CP1252ToUTF8); // English - Trinidad
  LangConvAdd(1033, @CP1252ToUTF8); // English - United States
  LangConvAdd(12297,@CP1252ToUTF8); // English - Zimbabwe
  LangConvAdd(1080, @CP1252ToUTF8); // Faroese
  LangConvAdd(1035, @CP1252ToUTF8); // Finnish
  LangConvAdd(2060, @CP1252ToUTF8); // French - Belgium
  LangConvAdd(3084, @CP1252ToUTF8); // French - Canada
  LangConvAdd(1036, @CP1252ToUTF8); // French - France
  LangConvAdd(5132, @CP1252ToUTF8); // French - Luxembourg
  LangConvAdd(6156, @CP1252ToUTF8); // French - Monaco
  LangConvAdd(4108, @CP1252ToUTF8); // French - Switzerland
  LangConvAdd(1110, @CP1252ToUTF8); // Galician
  LangConvAdd(3079, @CP1252ToUTF8); // German - Austria
  LangConvAdd(1031, @CP1252ToUTF8); // German - Germany
  LangConvAdd(5127, @CP1252ToUTF8); // German - Liechtenstein
  LangConvAdd(4103, @CP1252ToUTF8); // German - Luxembourg
  LangConvAdd(2055, @CP1252ToUTF8); // German - Switzerland
  LangConvAdd(1039, @CP1252ToUTF8); // Icelandic
  LangConvAdd(1057, @CP1252ToUTF8); // Indonesian
  LangConvAdd(1040, @CP1252ToUTF8); // Italian - Italy
  LangConvAdd(2064, @CP1252ToUTF8); // Italian - Switzerland
  LangConvAdd(2110, @CP1252ToUTF8); // Malay - Brunei
  LangConvAdd(1086, @CP1252ToUTF8); // Malay - Malaysia
  LangConvAdd(1044, @CP1252ToUTF8); // Norwegian - Bokml
  LangConvAdd(2068, @CP1252ToUTF8); // Norwegian - Nynorsk
  LangConvAdd(1046, @CP1252ToUTF8); // Portuguese - Brazil
  LangConvAdd(2070, @CP1252ToUTF8); // Portuguese - Portugal
  LangConvAdd(1274, @CP1252ToUTF8); // Spanish - Argentina
  LangConvAdd(16394,@CP1252ToUTF8); // Spanish - Bolivia
  LangConvAdd(13322,@CP1252ToUTF8); // Spanish - Chile
  LangConvAdd(9226, @CP1252ToUTF8); // Spanish - Colombia
  LangConvAdd(5130, @CP1252ToUTF8); // Spanish - Costa Rica
  LangConvAdd(7178, @CP1252ToUTF8); // Spanish - Dominican Republic
  LangConvAdd(12298,@CP1252ToUTF8); // Spanish - Ecuador
  LangConvAdd(17418,@CP1252ToUTF8); // Spanish - El Salvador
  LangConvAdd(4106, @CP1252ToUTF8); // Spanish - Guatemala
  LangConvAdd(18442,@CP1252ToUTF8); // Spanish - Honduras
  LangConvAdd(2058, @CP1252ToUTF8); // Spanish - Mexico
  LangConvAdd(19466,@CP1252ToUTF8); // Spanish - Nicaragua
  LangConvAdd(6154, @CP1252ToUTF8); // Spanish - Panama
  LangConvAdd(15370,@CP1252ToUTF8); // Spanish - Paraguay
  LangConvAdd(10250,@CP1252ToUTF8); // Spanish - Peru
  LangConvAdd(20490,@CP1252ToUTF8); // Spanish - Puerto Rico
  LangConvAdd(1034, @CP1252ToUTF8); // Spanish - Spain (Traditional)
  LangConvAdd(14346,@CP1252ToUTF8); // Spanish - Uruguay
  LangConvAdd(8202, @CP1252ToUTF8); // Spanish - Venezuela
  LangConvAdd(1089, @CP1252ToUTF8); // Swahili
  LangConvAdd(2077, @CP1252ToUTF8); // Swedish - Finland
  LangConvAdd(1053, @CP1252ToUTF8); // Swedish - Sweden

  LangConvAdd(1032, @CP1253ToUTF8); // greek

  LangConvAdd(1068, @CP1254ToUTF8); // Azeri - Latin
  LangConvAdd(1055, @CP1254ToUTF8); // turkish
  LangConvAdd(1091, @CP1254ToUTF8); // Uzbek - Latin

  LangConvAdd(1037, @CP1255ToUTF8); // hebrew

  LangConvAdd(5121, @CP1256ToUTF8); // Arabic - Algeria
  LangConvAdd(15361,@CP1256ToUTF8); // Arabic - Bahrain
  LangConvAdd(3073, @CP1256ToUTF8); // Arabic - Egypt
  LangConvAdd(2049, @CP1256ToUTF8); // Arabic - Iraq
  LangConvAdd(11265,@CP1256ToUTF8); // Arabic - Jordan
  LangConvAdd(13313,@CP1256ToUTF8); // Arabic - Kuwait
  LangConvAdd(12289,@CP1256ToUTF8); // Arabic - Lebanon
  LangConvAdd(4097, @CP1256ToUTF8); // Arabic - Libya
  LangConvAdd(6145, @CP1256ToUTF8); // Arabic - Morocco
  LangConvAdd(8193, @CP1256ToUTF8); // Arabic - Oman
  LangConvAdd(16385,@CP1256ToUTF8); // Arabic - Qatar
  LangConvAdd(1025, @CP1256ToUTF8); // Arabic - Saudi Arabia
  LangConvAdd(10241,@CP1256ToUTF8); // Arabic - Syria
  LangConvAdd(7169, @CP1256ToUTF8); // Arabic - Tunisia
  LangConvAdd(14337,@CP1256ToUTF8); // Arabic - United Arab Emirates
  LangConvAdd(9217, @CP1256ToUTF8); // Arabic - Yemen
  LangConvAdd(1065, @CP1256ToUTF8); // Farsi - Persian
  LangConvAdd(1056, @CP1256ToUTF8); // Urdu

  LangConvAdd(1061, @CP1257ToUTF8); // Estonian
  LangConvAdd(1062, @CP1257ToUTF8); // Latvian
  LangConvAdd(1063, @CP1257ToUTF8); // Lithuanian

  LangConvAdd(1066, @CP1258ToUTF8); // vietnam
end;

function CharToByte(const ch: AnsiChar): Byte;
begin
  Result:=0;
  if ch in ['0'..'9'] then Result:=byte(ch)-byte('0')
  else if ch in ['a'..'f'] then Result:=byte(ch)-byte('a')+10
  else if ch in ['A'..'F'] then Result:=byte(ch)-byte('A')+10
end;

function RTFCharToByte(const s: string): byte; inline;
begin
  // \'hh 	A hexadecimal value, based on the specified character set (may be used to identify 8-bit values).
  Result:=(CharToByte(s[3]) shl 4) or (CharToByte(s[4]));
end;

{ TRTFCustomParser }

procedure TRTFCustomParser.AddText(const atext: string);
var
  nl : Integer;
  l  : Integer;
begin
  nl:=txtlen+length(atext);
  if nl>length(txtbuf) then begin
    l:=length(txtbuf);
    while l<nl do
      if l=0 then l:=256
      else l:=l*2;
    SetLength(txtbuf, l);
  end;
  Move(atext[1], txtbuf[txtlen+1], length(atext));
  inc(txtlen, length(atext));
end;

function TRTFCustomParser.ResolveHyperlink(out alinkref: string): boolean;
var
  p: SizeInt;
begin
  result := (Field.valid);
  if result then begin
    p := pos('HYPERLINK', Field.rtfFieldInst);
    result := p>0;
    if result then begin
      aLinkref := Trim(copy(Field.rtfFieldInst, p + 9, Length(Field.rtfFieldInst)));
      p := 1;
      if (aLinkRef<>'') and (aLinkref[p] in ['"','''']) then Delete(aLinkRef, p, 1);
      p := Length(aLinkref);
      if (aLinkRef<>'') and (aLinkref[p] in ['"','''']) then Delete(aLinkRef, p, 1);
    end;
  end;
end;

procedure TRTFCustomParser.classUnk;
var
  txt : string;
  ws : UnicodeString;
begin
  if not Assigned(prm) then exit;

  txt:=GetRtfText;
  if (txt='\object') then begin
    SkipGroup;
    Exit;
  end;
  if (length(txt)>2) and (txt[1]='\') and (txt[2]='u') and (txt[3] in ['0'..'9']) then begin
    SetLength(Ws,1);
    ws[1]:=UnicodeChar(rtfParam);
    AddText( UTF8Encode(ws) );
    skipNextCh:=true;
  end;
end;

procedure TRTFCustomParser.classText;
var
  txt : string;
  bt  : Char;
begin
  if not Assigned(prm) then exit;
  if skipNextCh then begin
    skipNextCh:=false;
    Exit;
  end;

  txt:=Self.GetRtfText;

  if (length(txt)=4) and (txt[1]='\') and (txt[2]=#39) then begin
    if Assigned(langproc) then begin
      bt:=char(RTFCharToByte(txt));

      AddText( langproc(bt) );
    end;
  end else if (length(txt)=2) and (txt[1]='\') and (txt[2] in ['\','{','}']) then begin
    AddText(txt[2]);
  end else begin
    AddText(txt);
  end;
end;

procedure TRTFCustomParser.classControl;
begin
  if not Assigned(prm) then exit;

  if txtlen>0 then begin
    PushText;
  end;
  //writeln('ctrl: ', rtfClass,' ', rtfMajor, ' ', Self.GetRtfText, ' ',rtfMinor,' ', rtfParam);
  case rtfMajor of
    rtfDestination: doDestination(rtfMinor, rtfParam);
    rtfSpecialChar: doSpecialChar;
    rtfCharAttr: doChangeCharAttr(rtfMinor, rtfParam);
    rtfParAttr: doChangePara(rtfMinor, rtfParam);
  end;
end;

procedure TRTFCustomParser.classGroup;
var
  t : TRTFParams;
begin
  if not Assigned(prm) then exit;

  case rtfMajor of
    rtfBeginGroup: begin
      t:=TRTFParams.Create(prm);
      prm:=t;
    end;
    rtfEndGroup: begin
      if Assigned(prm) then begin
        t:=prm.prev;
        prm.Free;
        prm:=t;
      end;
    end;
  end;
end;

procedure TRTFCustomParser.classEof;
begin
  PushText;
end;

procedure TRTFCustomParser.doChangePara(aminor, aparam: Integer);
const
  TabAl : array [rtfTabPos..rtfTabDecimal] of TTabAlignment = (
    tabLeft, tabRight, tabCenter, tabDecimal);
begin
  case aminor of
    rtfParDef:      prm.ResetDefault; // reset clear formatting
    rtfQuadLeft:    prm.pa:=paLeft;
    rtfQuadRight:   prm.pa:=paRight;
    rtfQuadJust:    prm.pa:=paJustify;
    rtfQuadCenter:  prm.pa:=paCenter;
    rtfFirstIndent: begin
      prm.pm.FirstLine:=aparam / 20;
    end;
    rtfLeftIndent: begin
      prm.pm.HeadIndent:=aparam / 20;
    end;
    rtfRightIndent:  prm.pm.TailIndent  := aparam / 20;
    rtfSpaceBefore:  prm.pm.SpaceBefore := aparam / 20;
    rtfSpaceAfter:   prm.pm.SpaceAfter  := aparam / 20;
    rtfSpaceBetween: prm.pm.LineSpacing := aparam / 200;
      // \slN - surprise! the "line spacing" is actually a multiplier based on the FONT size, not linesize
      // where linesize = fontsize * 1.2
    rtfLanguage: begin
      SetLanguage(rtfParam);
    end;
    rtfTabPos,//; rtfKstr : 'tx'; rtfkHash : 0),
    rtfTabRight, // rtfKstr : 'tqr'; rtfkHash : 0),
    rtfTabCenter, //; rtfKstr : 'tqc'; rtfkHash : 0),
    rtfTabDecimal: //; rtfKstr : 'tqdec'; rtfkHash : 0),
      prm.AddTab(aparam / 20, TabAl[aminor]);
  end;
end;

procedure TRTFCustomParser.doDestination(aminor, aparam: Integer);
begin
  case aminor of
    rtfDefaultLanguage:
      deflang:=aparam;
  end;
end;

procedure TRTFCustomParser.doSpecialChar;
const
  {$ifdef MSWINDOWS}
  CharPara = #13#10;
  {$else}
  CharPara = #10;
  {$endif}
  CharTab  = #9;
  CharLine = #13;
begin
  case rtfMinor of
    rtfOptDest: SkipGroup;
    rtfLine: AddText(CharLine);
    rtfPar:  begin
      AddText(CharPara);
      if deflang<>0 then
        SetLanguage(deflang);
    end;
    rtfTab:  AddText(CharTab);
  end;
end;

procedure TRTFCustomParser.doChangeCharAttr(aminor, aparam: Integer);
var
  p : PRTFColor;
const
  HColor : array [1..16] of TColor = (
    clBlack
    ,clBlue
    ,clAqua // Cyan
    ,clLime // Green
    ,clFuchsia  //Magenta
    ,clRed
    ,clYellow
    ,clGray // unused!
    ,clNavy // DarkBlue
    ,clTeal // DarkCyan
    ,clGreen  // DarkGreen
    ,clPurple // clDarkMagenta
    ,clMaroon // clDarkRed
    ,clOlive // clDarkYellow
    ,clGray  //clDarkGray
    ,clSilver //clLightGray
  );
begin
  case aminor of
    rtfPlain: prm.fnt.Style:=[];
    rtfBold: if aparam=0 then Exclude(prm.fnt.Style,fsBold)  else Include(prm.fnt.Style, fsBold);
    rtfItalic: if aparam=0 then Exclude(prm.fnt.Style,fsItalic)  else Include(prm.fnt.Style, fsItalic);
    rtfStrikeThru: if aparam=0 then Exclude(prm.fnt.Style,fsStrikeOut)  else Include(prm.fnt.Style, fsStrikeOut);
    rtfFontNum: prm.fnum:=aparam;
    rtfFontSize: prm.fnt.Size:=round(aparam/2);
    rtfUnderline: if aparam=0 then Exclude(prm.fnt.Style,fsUnderline)  else Include(prm.fnt.Style, fsUnderline);
    rtfNoUnderline: Exclude(prm.fnt.Style, fsUnderline);

    rtfSuperScript: prm.fnt.VScriptPos:=vpSuperscript;
    rtfSubScript  : prm.fnt.VScriptPos:=vpSubScript;
    rtfNoSuperSub : prm.fnt.VScriptPos:=vpNormal;

    rtfHighlight: begin
      prm.fnt.HasBkClr := (aparam>0) and (aparam<=high(HColor));
      if prm.fnt.HasBkClr then begin
        if HLFromCTable then prm.fnt.BkColor:=HColor[aparam]
        else begin
          p:=Colors[aparam];
          if Assigned(p) then prm.fnt.BkColor:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue)
          // fallback?
          else prm.fnt.BkColor:=HColor[aparam];
        end;
      end;
    end;
    rtfForeColor: begin
      if rtfParam<>0 then p:=Colors[rtfParam]
      else p:=nil;
      if not Assigned(p) then
        prm.fnt.Color:=DefaultTextColor
      else
        prm.fnt.Color:=RGBToColor(p^.rtfCRed, p^.rtfCGreen, p^.rtfCBlue);
    end;
  end;
end;

procedure TRTFCustomParser.SetLanguage(AlangCode: integer);
begin
  lang:=AlangCode;
  langproc:=nil;
  LangConvGet(lang, langproc);
end;

function TRTFCustomParser.DefaultTextColor: TColor;
begin
  Result:=ColorToRGB(defColor);
end;

procedure TRTFCustomParser.PushText;
var
  i     : Integer;
  pf    : PRTFFONT;
  b     : string;
begin
  if not Assigned(prm) then exit;
  if txtlen=0 then Exit;

  b:=Copy(txtbuf, 1, txtlen);

  txtLen := 0;
  txtBuf := '';
  if Length(b)=0 then Exit;

  i := Length(chunks);
  SetLength(Chunks, i + 1);
  Chunks[i].Text := b;
  Chunks[i].prm := TRTFParams.Create(nil);

  if Assigned(prm) then begin
    Chunks[i].prm.fnt := prm.fnt;
    Chunks[i].prm.pm := prm.pm;
    Chunks[i].prm.pa := prm.pa;
    Chunks[i].prm.tabs := prm.tabs;
  end;

  pf:=Fonts[prm.fnum];
  if Assigned(pf) then
    Chunks[i].prm.fnt.Name:=pf^.rtfFName;

  if Field.valid then begin
    if ResolveHyperlink(b) then
      Chunks[i].Link := b;
  end;
end;

procedure TRTFCustomParser.Consolidate;
begin

end;

constructor TRTFCustomParser.Create(AStream: TStream);
begin
  inherited create(AStream);
  defColor := clBlack;

  ClassCallBacks[rtfText]:=@classText;
  ClassCallBacks[rtfControl]:=@classControl;
  ClassCallBacks[rtfGroup]:=@classGroup;
  ClassCallBacks[rtfUnknown]:=@classUnk;
  ClassCallBacks[rtfEof]:=@classEof;
end;

destructor TRTFCustomParser.Destroy;
var
  t: TRTFParams;
  c: TChunk;
  i: Integer;
begin
  // cleanup
  while Assigned(prm) do begin
    t:=prm;
    prm:=prm.prev;
    t.Free;
  end;
  for i:=0 to Length(chunks)-1 do begin
    chunks[i].prm.Free;
    chunks[i].Text:='';
    chunks[i].Link:='';
  end;
  inherited Destroy;
end;

procedure TRTFCustomParser.StartReading;
var
  t : TRTFParams;
begin
  try

    prm:=TRTFParams.Create(nil);
    prm.fnt.Size:=12; //\fsN Font size in half-points (the default is 24).
    prm.fnum:=0;
    prm.ResetDefault;

    inherited StartReading;
    PushText;

    // clear the stack, if overflow
    while Assigned(prm) do begin
      t:=prm.prev;
      prm.Free;
      prm:=t;
    end;

  finally
  end;
end;

end.

