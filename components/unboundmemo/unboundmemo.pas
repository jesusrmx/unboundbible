unit UnboundMemo;

{$ifdef windows}
  {$define winmode}
{$endif}

interface

uses
  {$ifdef windows} Windows, rmWinEx, {$endif}
  Forms, SysUtils, LResources, Classes, Graphics, Controls, ExtCtrls,
  LCLProc, LCLType, LazUTF8, RichMemo, RichMemoEx, UnitLib,
  {$ifdef winmode} UmParseWin; {$else} UmParse; {$endif}

type
  TUnboundMemo = class(TRichMemoEx)
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  private
    FLinkable : boolean;
    FParagraphic : boolean;
    SelStartTemp : integer;
    SelLengthTemp : integer;
    procedure SetSel(x1,x2: integer);
    procedure GetSel(var x1,x2: integer);
    {$ifdef unix} function  Colored: boolean; {$endif}
    function  GetColorLink: string;
    function  GetLink: string;
    function  GetParagraphNumber(Pos: integer; select: boolean): integer;
    procedure GetParagraphRange;
    function  GetStartSelection: integer;
    function  GetEndSelection: integer;
  public
    Hyperlink : string;
    ParagraphStart : integer;
    ParagraphCount : integer;
    constructor Create(AOwner: TComponent); override;
    function Foreground: integer;
    procedure SelectParagraph(n : integer);
    procedure SelectWord;
    procedure SelectAll;
    procedure SaveSelection;
    procedure RestoreSelection;
    procedure LoadText(Source: string; jtag: boolean = false);
    procedure LoadHtml(Source: string);
  published
    property Linkable    : boolean read FLinkable    write FLinkable    default False;
    property Paragraphic : boolean read FParagraphic write FParagraphic default False;
  end;

const
  fgText     = 0;
  fgLink     = 1;
  fgStrong   = 2;
  fgFootnote = 3;

procedure Register;

implementation

constructor TUnboundMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Hyperlink := '';
  ParagraphStart := 0;
  ParagraphCount := 0;
  Cursor := crArrow;

  {$ifdef windows} if Font.Name = 'default' then Font.Name := 'Tahoma'; {$endif}
end;

procedure TUnboundMemo.SetSel(x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_SETSEL, x1, x2);
  // использование обычных процедур производит дерганье текста
  {$else}
  SelStart  := x1;
  SelLength := x2-x1;
  {$endif}
end;

procedure TUnboundMemo.GetSel(var x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_GETSEL, integer(@x1), integer(@x2));
  {$else}
  x1 := SelStart;
  x2 := SelStart + SelLength;
  {$endif}
end;

function TUnboundMemo.Foreground: integer;
begin
  Result := fgText;
  if SelAttributes.Color = clSysLink  then Result := fgLink else
  if SelAttributes.Color = clSysBrown then Result := fgStrong else
  if SelAttributes.Color = clSysTeal  then Result := fgFootnote;
end;

{$ifdef unix}
function TUnboundMemo.Colored: boolean;
begin
  Result := Foreground = fgLink;
end;
{$endif}

function TUnboundMemo.GetColorLink: string;
var
  fore : integer;
  n1,n2,x0,x1,x2 : integer;
  limit : integer;
begin
  Result := '';
  if SelLength > 0 then Exit;

  fore := Foreground;
  if fore = fgText then Exit;
  GetSel(n1,n2);

  x0 := SelStart;
  x1 := x0;
  repeat
    dec(x1);
    SetSel( {$ifdef unix} x1,x1 {$else} x1+1,x1+1 {$endif} );
  until (Foreground <> fore) or (x1 < 0);

  inc(x1);
  if x1 < 0 then inc(x1);
  limit := Utf8Length(Text);

  x2 := x0;
  repeat
    inc(x2);
    SetSel( {$ifdef unix} x2,x2 {$else} x2+1,x2+1 {$endif} );
  until (Foreground <> fore) or (x2 > limit);

  SetSel(x1, x2); Result := RemoveCRLF(SelText);
  SetSel(n1, n2); Result := Trim(Result);
end;

function TUnboundMemo.GetLink: string;
begin
  if Foreground <> fgText then Result := GetColorLink;
end;

procedure TUnboundMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1 : integer = 0;
  x2 : integer = 0;
begin
  if Linkable then Hyperlink := GetLink else Hyperlink := '';

  if Paragraphic and (Button = mbLeft) then
    begin
      if Hyperlink <> '' then GetSel(x1,x2);
      GetParagraphRange;
      if Hyperlink <> '' then SetSel(x1,x2);
    end;

  inherited;
end;

procedure TUnboundMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$ifdef windows}
    if Linkable and not ReadOnly and (Key = VK_CONTROL) then ShowCaret(Handle);
  {$endif}
end;

{$ifdef windows}
function TUnboundMemo.GetParagraphNumber(Pos: integer; select: boolean): integer;
var
  p : TParaRange;
  x : integer;
begin
  GetParaRange(Pos, p);
  x := FindRightWordBreak(p.start +1);
  Result := ToInt(GetTextRange(Handle, p.start, x-p.start));
  if select then SetSel(p.start,p.start+1);
end;
{$endif}

{$ifdef unix}
function TUnboundMemo.GetParagraphNumber(Pos: integer; select: boolean): integer;
var
  p : TParaRange;
  x : integer;
begin
  GetParaRange(Pos, p);

  x := p.start;
  repeat
    inc(x);
    SetSel(x, x);
  until not Colored;

  SetSel(p.start, x); Result := ToInt(SelText);
  if select then SetSel(p.start,p.start+1);
end;
{$endif}

procedure TUnboundMemo.GetParagraphRange;
var
  ParagraphEnd : integer;
  x1, x2 : integer;
begin
  GetSel(x1,x2);

  ParagraphStart := GetParagraphNumber(x1, x1=x2);
  ParagraphCount := 1;

  if x1 <> x2 then
    begin
      ParagraphEnd := GetParagraphNumber(x2, false);
      ParagraphCount := ParagraphEnd - ParagraphStart + 1;
      {$ifdef unix} SetSel(x1,x2); {$endif}
    end;
end;

{$ifdef windows}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  w, line : string;
  i, len, x : integer;
begin
  HideSelection := False; // important

  w := ' ' + IntToStr(n) + ' ';
  len := length(w);

  for i:=0 to LineCount - 1 do
    begin
      line := Lines[i];

      if copy(line,1,len) = w then
         begin
           x := LineIndex(i);
           SetSel(x,x+1);
           HideCursor;
         end;
    end;

  ParagraphStart := n;
  ParagraphCount := 1;
end;
{$endif}

{$ifdef unix}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  i, x : integer;
  L : boolean;
begin
  L := False;
  x := 0;

  i := 0;
  while True do
    begin
      SetSel(i,i);
      if SelStart <> i then break;

      if Colored then
        begin
          if not L then
            begin
              inc(x);
              if x = n then
                begin
                  SetSel(i,i+1);
                  break;
                end;
            end;
          L := True;
        end;

      if not Colored then L := False;
      inc(i);
    end;

  SetFocus;
end;
{$endif}

function TUnboundMemo.GetStartSelection: integer;
var
  i, temp : integer;
begin
  temp := SelStart;
     i := SelStart;

  SetSel(i-1,i);
  while (SelText <> ' ') and (i > 0)  do
    begin
      dec(i);
      SetSel(i-1,i);
    end;

  Result := i;
  SetSel(temp, temp);
end;

function TUnboundMemo.GetEndSelection: integer;
var
  i, len, temp : integer;
begin
  temp := SelStart;
     i := SelStart;
   len := i + 50;

  SetSel(i,i+1);
  while (Utf8LowerCase(SelText) <> Utf8UpperCase(SelText)) and (i < len) do
    begin
      inc(i);
      SetSel(i,i+1);
    end;

  Result := i;
  SetSel(temp, temp);
end;

procedure TUnboundMemo.SelectWord;
begin
  SetSel(GetStartSelection, GetEndSelection);
end;

procedure TUnboundMemo.SelectAll;
begin
  Hide_Selection;
  inherited;
  GetParagraphRange;
  Show_Selection;
end;

procedure TUnboundMemo.SaveSelection;
begin
  SelStartTemp  := SelStart;
  SelLengthTemp := SelLength;
end;

procedure TUnboundMemo.RestoreSelection;
begin
  SelStart  := SelStartTemp;
  SelLength := SelLengthTemp;
end;

procedure TUnboundMemo.LoadText(Source: string; jtag: boolean = false);
begin
  if not jtag then
    begin
      Replace(Source, '<J>','');
      Replace(Source,'</J>','');
    end;
  {$ifdef winmode}
    LoadRichText(ParseWin(Source, Font));
  {$else}
    Parse(Self, Source);
    if ReadOnly then HideCursor;
  {$endif}
end;

procedure TUnboundMemo.LoadHtml(Source: string);
begin
  {$ifdef winmode}
    LoadRichText(ParseWin(Source, Font, true));
  {$else}
    Parse(Self, Source, true);
    if ReadOnly then HideCursor;
  {$endif}
end;

procedure Register;
begin
  {$I unboundmemoicon.lrs}
  RegisterComponents('Common Controls',[TUnboundMemo]);
end;

end.

