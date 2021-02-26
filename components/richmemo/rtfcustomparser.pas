unit RTFCustomParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RTFParsPre211;

type
  TVScriptPos = (vpNormal, vpSubScript, vpSuperScript);

  TParaAlignment  = (paLeft, paRight, paCenter, paJustify);
  TParaMetric = record
    FirstLine   : Double; // in points
    TailIndent  : Double; // in points
    HeadIndent  : Double; // in points
    SpaceBefore : Double; // in points
    SpaceAfter  : Double; // in points
    LineSpacing : Double; // multiplier - matching CSS line-height by percentage/em
                          // note, that normal LineSpacing is 1.2, not 1.0
  end;

const
  DefLineSpacing     = 1.2;
  SingleLineSpacing  = DefLineSpacing;
  OneHalfLineSpacing = DefLineSpacing * 1.5;
  DoubleLineSpacing  = DefLineSpacing * 2.0;


type
  TParaNumStyle   = (pnNone, pnBullet, pnNumber, pnLowLetter
    , pnLowRoman, pnUpLetter, pnUpRoman, pnCustomChar);

  TParaNumbering  = record
    Style       : TParaNumStyle;
    Indent      : Double;
    CustomChar  : WideChar;
    NumberStart : Integer;  // used for pnNumber only
    SepChar     : WideChar;
    ForceNewNum : Boolean;  // if true and Style is pnNumber, NumberStart is used for the new numbering
  end;

const
  SepNone = #0;
  SepPar  = ')';
  SepDot  = '.';


type
  TTextModifyMask  = set of (tmm_Color, tmm_Name, tmm_Size, tmm_Styles, tmm_BackColor);
  TParaModifyMask = set of (pmm_FirstLine, pmm_HeadIndent, pmm_TailIndent, pmm_SpaceBefore, pmm_SpaceAfter, pmm_LineSpacing);

  TSearchOption = (soMatchCase, soWholeWord, soBackward);
  TSearchOptions = set of TSearchOption;

  TParaRange = record
    start      : Integer; // the first character in the paragraph
    lengthNoBr : Integer; // the length of the paragraph, excluding the line break character
    length     : Integer; // the length of the paragrpah, including the line break, if present
    // the last line in the control doesn't contain a line break character,
    // thus length = lengthNoBr
  end;

type
  TTabAlignment = (tabLeft, tabCenter, tabRight, tabDecimal, tabWordBar);

  TTabStop = record
    Offset : Double;
    Align  : TTabAlignment; // not used
  end;

  TTabStopList = record
    Count : Integer;
    Tabs  : array of TTabStop;
  end;

type
  TRectOffsets = record
    Left   : Double;
    Top    : Double;
    Right  : Double;
    Bottom : Double;
  end;

  TPrintParams = record
    JobTitle  : String;       // print job title to be shown in system printing manager
    Margins   : TRectOffsets; // margins in points
    SelStart  : Integer;
    SelLength : Integer;
  end;

  TPrintMeasure = record
    Pages     : Integer;
  end;

  TPrintAction = (paDocStart, paPageStart, paPageEnd, paDocEnd);

  TPrintActionEvent = procedure (Sender: TObject;
    APrintAction: TPrintAction;
    PrintCanvas: TCanvas;
    CurrentPage: Integer; var AbortPrint: Boolean) of object;

type
  TLinkAction = (laClick);

  TLinkMouseInfo = record
    Button  : Integer;
    LinkRef : String;
  end;

  TLinkActionEvent = procedure (Sender: TObject;
    ALinkAction: TLinkAction;
    const info: TLinkMouseInfo;
    LinkStart, LinkLen: Integer) of object;

  TTextUIFeature = (uiLink);
  TTextUIFeatures = set of TTextUIFeature;

  TTextUIParam = record
    features : TTextUIFeatures;
    linkref  : String;
  end;

  TFontParams  = record
    Name      : String;
    Size      : Integer;
    Color     : TColor;
    Style     : TFontStyles;
    HasBkClr  : Boolean;
    BkColor   : TColor;
    VScriptPos  : TVScriptPos;
  end;


  { TRTFParams }

  TRTFParams = class(TObject)
  public
    fnt  : TFontParams;
    pm   : TParaMetric;
    pa   : TParaAlignment;
    fnum : Integer; // font index in the font table

    prev : TRTFParams;
    tabs : TTabStopList;
    constructor Create(aprev: TRTFParams);
    procedure ResetDefault;
    procedure AddTab(AOffset: double; ta: TTabAlignment);
  end;

  TChunk = record
    prm: TRTFParams;
    Text: string;
    Link: string;
  end;

  { TRTFCustomParser }

  TRTFCustomParser = class(TRTFParser)
  private
    defColor : TColor;
    txtbuf   : String; // keep it UTF8 encoded!
    txtlen    : Integer;

    HLFromCTable : Boolean;

    prm       : TRTFParams;
    lang      : Integer;
    //langproc  : TEncConvProc;
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
    procedure PushText;
  public
    chunks: array of TChunk;
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure StartReading;
  end;


implementation

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

{ TRTFParams }

constructor TRTFParams.Create(aprev: TRTFParams);
begin
  prev:=aprev;
  if Assigned(prev) then begin
    fnt:=prev.fnt;
    pm:=prev.pm;
    pa:=prev.pa;
    fnum:=prev.fnum;
  end else begin
    FillChar(fnt, SizeOf(fnt), 0);
    FillChar(pm, sizeof(pm), 0);
    pm.LineSpacing:=DefLineSpacing;
  end;
end;

procedure TRTFParams.ResetDefault;
begin
  // default values are taken from RTF specs
  // see section "Paragraph Formatting Properties"
  pa:=paLeft;
  pm.FirstLine:=0;
  pm.HeadIndent:=0;
  pm.TailIndent:=0;
  pm.SpaceBefore:=0;
  pm.SpaceAfter:=0;
  pm.LineSpacing:=0;
  tabs.Count:=0;
end;

procedure TRTFParams.AddTab(AOffset: double; ta: TTabAlignment);
begin
  if tabs.Count=length(tabs.Tabs) then begin
    if tabs.Count=0 then SetLength(tabs.Tabs, 4)
    else SetLength(tabs.Tabs, tabs.Count*2);
  end;
  tabs.Tabs[tabs.Count].Offset:=AOffset;
  tabs.Tabs[tabs.Count].Align:=ta;
  inc(tabs.Count);
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
    //if Assigned(langproc) then begin
    //  bt:=char(RTFCharToByte(txt));
    //
    //  AddText( langproc(bt) );
    //end;
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
  //langproc:=nil;
  //LangConvGet(lang, langproc);
end;

function TRTFCustomParser.DefaultTextColor: TColor;
begin
  Result:=ColorToRGB(defColor);
end;

procedure TRTFCustomParser.PushText;
var
  i     : Integer;
  len   : Integer;
  pf    : PRTFFONT;
  selst : Integer;
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

