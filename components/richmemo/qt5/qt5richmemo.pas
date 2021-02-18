unit qt5richmemo;

interface

{$mode delphi}

{$define RMQT5_TEXTFORMATS} // the feature is available in Qt5 Trunk
                            // it allows to query Qt5TextEdit for character formats array
{$ifdef RMQT5_NOTEXTFORMATS}{$undef RMQT5_TEXTFORMATS}{$endif}

//
// Following class methods are need for the implementation
//  QTextCharFormatH
//  QTextBlockFormatH
uses
  LCLType, Controls, StdCtrls, Graphics,
  qt5, qtobjects, qtwidgets, qtprivate,
  WSProc,
  RichMemo, WSRichMemo;

type
  TCustomRichMemoInt   = class(TCustomRichMemo);

  { TQtRichTextEdit }

  TQtRichTextEdit = class(TQtTextEdit)
  private
    anchor: Widestring;
  public
    function SlotMouse(Sender: QObjectH; Event: QEventH): Boolean; override; cdecl;
  end;

  { TQtWSCustomRichMemo }

  TQtWSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TIntParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;
    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;

    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer; var rng: TParaRange): Boolean; override;
    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer; const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer; var ui: TTextUIParam): Boolean; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer; override;

    class function isInternalChange(const AWinControl: TWinControl; Params: TTextModifyMask
      ): Boolean; override;
    class procedure SetTextAttributesInternal(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AModifyMask: TTextModifyMask; const Params: TIntFontParams); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;

  end;

type
  TEditorState = record
    selst  : integer; // selection start
    sellen : integer; // selection length
  end;

// no sanity check is done in these functions
procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);

implementation

const
  WordWrapMap: array[Boolean] of QTextEditLineWrapMode =
  (
    QTextEditNoWrap,
    QTextEditWidgetWidth
  );

  AlignmentMap: array[TIntParaAlignment] of QtAlignment =
  (
    QtAlignLeft,
    QtAlignRight,
    QtAlignHCenter,
    QtAlignJustify
  );


function QBrushToColor(brush: QBrushH): TQColor;
var
  aColor: PQColor;
begin
  aColor := QBrush_color(brush);
  if aColor<>nil then result := aColor^
  else                fillChar(result, sizeOf(TQColor), 0);
end;

function SameColor(A, B: TQColor): boolean;
begin
  result := (a.r=b.r) and (a.g=b.g) and (a.b=b.b) and (a.Alpha=b.Alpha) and
            (a.ColorSpec=b.ColorSpec);
end;

function PrivateGetFormatRange(cursor: QTextCursorH; const start: Integer;
  out rangeStart, rangeEnd: Integer): boolean;
var
  fmtRef, fmtCur: QTextCharFormatH;
  refIsAnchor: Boolean;
  refHRef, refFont: WideString;
  font: QFontH;
  refForeColor, refBackColor: TQColor;
  brush: QBrushH;

  function SameFormats: boolean;
  var
    tmpStr: wideString;
    aColor: TQColor;
  begin
    // deal with hyperlinks
    result := QTextCharFormat_isAnchor(fmtCur)=refIsAnchor;
    if not result then exit;
    if refIsAnchor then begin
      QTextCharFormat_anchorHref(fmtCur, @tmpStr);
      result := tmpStr=refHRef;
      exit;
    end;
    // colors
    QTextFormat_foreground(QTextFormatH(fmtCur), brush);
    result := SameColor(QBrushToColor(brush), refForeColor);
    if not result then exit;
    // deal with formats
    QTextCharFormat_font(fmtCur, font);
    QFont_toString(font, @tmpStr);
    result := tmpStr=refFont;
  end;

begin
  result := false;
  QTextCursor_setPosition(cursor, start );

  fmtRef := QTextCharFormat_Create();
  fmtCur := QTextCharFormat_Create();

  rangeStart := QTextCursor_Position(cursor);
  rangeEnd   := rangeStart;

  QTextCursor_charFormat(cursor, fmtRef);

  QTextCharFormat_anchorHref(fmtRef, @refHRef);
  refIsAnchor := QTextCharFormat_isAnchor(fmtRef);
  if not refIsAnchor then begin
    font := QFont_Create();
    brush := QBrush_Create();
    QTextCharFormat_font(fmtRef, font);
    QFont_toString(font, @refFont);

    QTextFormat_foreground(QTextFormatH(fmtRef), brush);
    refForeColor := QBrushToColor(brush);
  end;

  // find left limit
  while QTextCursor_movePosition(cursor, QTextCursorPreviousCharacter) do begin
    dec(rangeStart);
    QTextCursor_charFormat(cursor, fmtCur);
    if not SameFormats then
      break;
  end;

  QTextCursor_setPosition(cursor, rangeEnd);
  while QTextCursor_movePosition(cursor, QTextCursorNextCharacter) do begin
    QTextCursor_charFormat(cursor, fmtCur);
    if not SameFormats then
      break;
    inc(rangeEnd);
  end;

  if not refIsAnchor then begin
    QFont_Destroy(font);
    QBrush_Destroy(brush);
  end;
  QTextCharFormat_Destroy(fmtRef);
  QTextCharFormat_Destroy(fmtCur);

  result := true;
end;

{ TQtRichTextEdit }

function TQtRichTextEdit.SlotMouse(Sender: QObjectH; Event: QEventH): Boolean;
  cdecl;
var
  Pos: TQtPoint;
  tc: QTextCursorH;
  rangeStart, rangeEnd: Integer;
  mi: TLinkMouseInfo;
begin
  if (LCLObject is TCustomRichMemo) then begin
    QMouseEvent_pos(QMouseEventH(Event), @Pos);
    case QEvent_type(Event) of
      QEventMouseButtonPress:
        begin
          Anchor := '';
          QTextEdit_anchorAt(QTextEditH(widget), @Anchor, @Pos);
        end;
      QEventMouseButtonRelease:
        if Anchor<>'' then begin
          tc := QTextCursor_Create();
          QTextEdit_cursorForPosition(QTextEditH(widget), tc, @pos);
          rangeStart := QTextCursor_position(tc);
          mi.LinkRef:=UTF8Encode(Anchor);
          mi.Button:=mbLeft;
          if PrivateGetFormatRange(tc, rangeStart, rangeStart, rangeEnd) then
            TCustomRichMemoInt(LCLObject).doLinkACtion(laClick, mi, rangeStart, rangeEnd-rangeStart);
          QTextCursor_Destroy(tc);
          Anchor := '';
        end;
    end;
  end;
  Result:=inherited SlotMouse(Sender, Event);
end;

{ TQtWSCustomRichMemo }

class function TQtWSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtTextEdit: TQtRichTextEdit;
begin
  QtTextEdit := TQtRichTextEdit.Create(AWinControl, AParams);
  QtTextEdit.AcceptRichText := True;
  QtTextEdit.ClearText;
  QtTextEdit.setBorder(TCustomMemo(AWinControl).BorderStyle = bsSingle);
  QtTextEdit.setReadOnly(TCustomMemo(AWinControl).ReadOnly);
  QtTextEdit.setLineWrapMode(WordWrapMap[TCustomMemo(AWinControl).WordWrap]);
  // create our FList helper
  QtTextEdit.FList := TQtMemoStrings.Create(TCustomMemo(AWinControl));
  QtTextEdit.setScrollStyle(TCustomMemo(AWinControl).ScrollBars);
  QtTextEdit.setTabChangesFocus(not TCustomMemo(AWinControl).WantTabs);
  QtTextEdit.AttachEvents;
  QTextEdit_setTabStopWidth(QTextEditH(QtTextEdit.Widget), 55);

  Result := TLCLIntfHandle(QtTextEdit);
end;

class procedure TQtWSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AAlign: TIntParaAlignment);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  // alignment
  QTextEdit_setAlignment(w, AlignmentMap[AAlign]);

  te.setSelection(ss, sl);
end;

const
  QNormal = 50;
  QBold   = 75;

class function TQtWSCustomRichMemo.GetTextAttributes(
  const AWinControl: TWinControl; TextStart: Integer; var Params: TIntFontParams
  ): Boolean;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : WideString;
  clr: TQColor;
  bck : TEditorState;
begin
  InitFontParams(Params);
  if not WSCheckHandleAllocated(AWinControl, 'GetTextAttributes') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  MakeBackup(te, bck);

  // If selstart is at the end of the line (before the EOL char) and we set Len=1
  // it would move the cursor at the begining of the next block which would make
  // that the retrieved attibutes do not match the previous block attributes
  // (the expected behaviour) but the attributes of the next block which is wrong
  //te.setSelection(TextStart, 1);
  te.setSelection(TextStart, 0);

  //todo!
  ws:='';
  QTextEdit_fontFamily(w, @ws);
  if ws<>'' then Params.Name:=UTF8Encode(ws);

  Params.Size:=round(QTextEdit_fontPointSize(w));
  if QTextEdit_fontWeight(w)>=QBold then Include(Params.Style, fsBold);
  if QTextEdit_fontItalic(w) then Include(Params.Style, fsItalic);
  if QTextEdit_fontUnderline(w) then Include(Params.Style, fsUnderline);

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.Color));

  FillChar(clr, sizeof(clr), 0);
  QTextEdit_textBackgroundColor(w, @clr);
  TQColorToColorRef(clr, TColorRef(params.BkColor));
  //todo!
  params.HasBkClr:=false;

  ApplyBackup(te, bck);

  Result:=true;
end;

class procedure TQtWSCustomRichMemo.SetTextAttributes(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const Params: TIntFontParams);
var
  w : QTextEditH;
  te : TQtTextEdit;
  ss, sl :  Integer;
  ws : WideString;
  clr: TQColor;
const
  QNormal = 50;
  QBold   = 75;
const
  QIsBold: array [Boolean] of integer = (QNormal, QBold);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextAttributes') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  ss:=te.getSelectionStart;
  sl:=te.getSelectionLength;

  te.setSelection(TextStart, TextLen);

  ws:=UTF8Decode(Params.Name);
  if ws<>'' then QTextEdit_setFontFamily(w, @ws);
  if Params.Size>0 then QTextEdit_setFontPointSize(w, Params.Size);
  QTextEdit_setFontUnderline(w, fsUnderline in Params.Style);
  QTextEdit_setFontWeight(w, QisBold[fsBold in Params.Style]);
  QTextEdit_setFontItalic(w, fsItalic in Params.Style);

  ColorRefToTQColor(Params.Color, clr);
  QTextEdit_setTextColor(w, @clr);

  //todo:
  {
  if not Params.HasBkClr then begin
    ColorRefToTQColor(Params.BkColor, clr);
    clr.Alpha:=0;
  end else
    ColorRefToTQColor(Params.BkColor, clr);
  QTextEdit_setTextBackgroundColor(w, @clr);
  }

  te.setSelection(ss, sl);
end;

class function TQtWSCustomRichMemo.GetParaRange(const AWinControl: TWinControl;
  TextStart: Integer; var rng: TParaRange): Boolean;
var
  te : TQtTextEdit;
  tc : QTextCursorH;
begin
  result := false;
  rng.start := TextStart;
  rng.length := 0;
  rng.lengthNoBr := 0;

  if not WSCheckHandleAllocated(AWinControl, 'GetParaRange') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);

  tc := QTextCursor_create();
  QTextEdit_textCursor(QTextEditH(te.Widget), tc);

  QTextCursor_setPosition(tc, TextStart);
  QTextCursor_movePosition(tc, QTextCursorStartOfBlock);
  rng.start:= QTextCursor_Position(tc);
  if QTextCursor_movePosition(tc, QTextCursorEndOfBlock) then
  begin
    rng.lengthNoBr := QTextCursor_position(tc) -  rng.start;
    if QTextCursor_movePosition(tc, QTextCursorNextCharacter) then
      rng.length := QTextCursor_position(tc) -  rng.start
    else
      rng.length := rng.lengthNoBr;
  end;

  QTextCursor_destroy(tc);
  result := true;
end;

class procedure TQtWSCustomRichMemo.InDelText(const AWinControl: TWinControl;
  const TextUTF8: String; DstStart, DstLen: Integer);
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  AText: UnicodeString;
begin
  if not WSCheckHandleAllocated(AWinControl, 'InDelText') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_create();
  QTextEdit_textCursor(w, tc);
  QTextCursor_setPosition(tc, DstStart);
  QTextCursor_setPosition(tc, DstStart + DstLen, QTextCursorKeepAnchor);
  QTextCursor_removeSelectedText(tc);
  AText := UTF8Decode(TextUTF8);
  QTextCursor_insertText(tc, @AText);
  QTextCursor_Destroy(tc);
end;

class procedure TQtWSCustomRichMemo.SetTextUIParams(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ui: TTextUIParam);
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  address: UnicodeString;
  fmt: QTextCharFormatH;
  Color: TQColor;
  Brush: QBrushH;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetTextUIParams') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_create();
  QTextEdit_textCursor(w, tc);

  fmt := QTextCharFormat_Create();
  QTextCursor_charFormat(tc, fmt);

  ColorRefToTQColor(ColorToRGB(clGreen), Color);
  Brush := QBrush_create(@Color, QtSolidPattern);
  QTextFormat_setForeground(QTextFormatH(fmt), brush);
  QBrush_Destroy(Brush);
  QTextCharFormat_setAnchor(fmt, true);
  address := UTF8Decode(ui.linkref);
  QTextCharFormat_setAnchorHref(fmt, @address);

  QTextCursor_setPosition(tc, TextStart);
  QTextCursor_setPosition(tc, TextStart + TextLen, QTextCursorKeepAnchor);
  QTextCursor_setCharFormat(tc, fmt);

  QTextCharFormat_Destroy(fmt);
  QTextCursor_Destroy(tc);
end;

class function TQtWSCustomRichMemo.GetTextUIParams(
  const AWinControl: TWinControl; TextStart: Integer; var ui: TTextUIParam
  ): Boolean;
var
  te: TQtTextEdit;
  w: QTextEditH;
  tc: QTextCursorH;
  address: UnicodeString;
  fmt: QTextCharFormatH;
  Color: TQColor;
  Brush: QBrushH;
begin
  result := false;
  if not WSCheckHandleAllocated(AWinControl, 'SetTextUIParams') then
    Exit;

  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);
  tc := QTextCursor_create();
  QTextEdit_textCursor(w, tc);
  QTextCursor_setPosition(tc, TextStart);

  fmt := QTextCharFormat_Create();
  QTextCursor_charFormat(tc, fmt);
  if QTextCharFormat_isAnchor(fmt) then
  begin
    QTextCharFormat_anchorHref(fmt, @address);
    ui.linkref := UTF8Encode(address);
    result := true;
  end;
  QTextCharFormat_Destroy(fmt);

  QTextCursor_Destroy(tc);
end;

class function TQtWSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
var
  w : QTextEditH;
  te : TQtTextEdit;
  ws : Widestring;
  fl : QTextDocumentFindFlags;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetParaAlignment') then
    Exit;
  te:=TQtTextEdit(AWinControl.Handle);
  w:=QTextEditH(te.Widget);

  fl:=0;
  if soMatchCase in SearchOpts.Options then fl:=fl or QTextDocumentFindCaseSensitively;
  if soWholeWord in SearchOpts.Options then fl:=fl or QTextDocumentFindWholeWords;
  if soBackward in SearchOpts.Options then fl:=fl or QTextDocumentFindBackward;

  //todo: range filtering in Serach Opts
  ws:=UTF8Decode(ANiddle);
  if QTextEdit_find(w, @ws, fl) then  Result:=te.getSelectionStart
  else Result:=-1;
end;

class function TQtWSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result := false;

  //Result:=inherited isInternalChange(AWinControl, Params);
end;

class procedure TQtWSCustomRichMemo.SetTextAttributesInternal(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AModifyMask: TTextModifyMask; const Params: TIntFontParams);
begin
  SetTextAttributes(AWinControl, TextStart, TextLen, Params);
end;

class function TQtWSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart,
  RangeLen: Integer): Boolean;
var
  qcur : QTextCursorH;
  te : TQtTextEdit;
  {$ifdef RMQT5_TEXTFORMATS}
  bck : TEditorState;
  al : QtAlignment;
  qblck : QTextBlockH;
  qbfmt : QTextBlockFormatH;
  i   : integer;
  cnt : integer;
  rng : array of TTextRange;
  blckofs: integer;
  {$endif}
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetStyleRange') then begin
    Result:=false;
    Exit;
  end;

  te:=TQtTextEdit(AWinControl.Handle);
  qcur := QTextCursor_Create();

  {$ifndef RMQT5_TEXTFORMATS}
  QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
  result := PrivateGetFormatRange(qCur, TextStart, RangeStart, RangeLen);
  if result then
    RangeLen := RangeLen - RangeStart
  else begin
    RangeStart:=TextStart;
    RangeLen:=1;
    Result:=true;
  end;
  QTextCursor_Destroy(qcur);
  {$else}
  MakeBackup(te, bck);
  qblck := QTextBlock_Create();
  try
    te.setSelection(TextStart, 0);
    QTextEdit_textCursor(QTextEditH(te.Widget), qcur);
    QTextCursor_block(qcur, qblck);

    cnt := QTextBlock_textFormatsCount(qblck);
    SetLength(rng, cnt);
    if cnt>0 then begin
      blckofs := QTextBlock_position(qblck);
      textStart := textStart - blckofs;
      for i:=0 to cnt-1 do begin
        if (textStart >= rng[i].start) and (textStart<rng[i].start+rng[i].length) then
        begin
          RangeStart := rng[i].start + blckofs;
          RangeLen := rng[i].length;
          break;
        end;
      end;
    end;
  finally
    QTextBlock_Destroy(qblck);
    QTextCursor_Destroy(qcur);
    ApplyBackup(te, bck);
  end;
  {$endif}
end;

class function TQtWSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer;
  var AAlign: TIntParaAlignment): Boolean;
var
  te : TQtTextEdit;
  al : QtAlignment;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetParaAlignment') then begin
    Result:=false;
    Exit;
  end;
  te:=TQtTextEdit(AWinControl.Handle);
  al:=QTextEdit_alignment(QTextEditH(te.Widget));
  if QtAlignLeading and al > 0 then AAlign:=paLeft
  else if QtAlignTrailing and al > 0 then AAlign:=paRight
  else if QtAlignCenter and al > 0 then AAlign:=paCenter
  else if QtAlignJustify and al > 0 then AAlign:=paJustify
  else AAlign:=paLeft;
  Result:=true;
end;

procedure MakeBackup(te: TQtTextEdit; out backup: TEditorState);
begin
  backup.selst:=te.getSelectionStart;
  backup.sellen:=te.getSelectionLength;
end;

procedure ApplyBackup(te: TQtTextEdit; const backup: TEditorState);
begin
  te.setSelection(backup.selst, backup.sellen);
end;

end.
