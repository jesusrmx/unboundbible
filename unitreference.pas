unit UnitReference;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitData, UnitLib;

type
  TReferenceAlias = record
    xreferences, book, chapter, verse, xbook, xchapter, xfromverse, xtoverse, votes : string;
  end;

  TReference = class(TModule)
  private
    z : TReferenceAlias;
    function GetData(Verse: TVerse): TVerseArray;
  public
    constructor Create(filePath: string);
  end;

  TReferences = class(TFPGList<TReference>)
  private
    procedure Load;
  public
    constructor Create;
    function GetData(Verse: TVerse; language: string; out info: string): TVerseArray;
    destructor Destroy; override;
  end;

var
  References : TReferences;

implementation

const
  unboundAlias : TReferenceAlias = (
      xreferences : 'xreferences';
      book        : 'book';
      chapter     : 'chapter';
      verse       : 'verse';
      xbook       : 'xbook';
      xchapter    : 'xchapter';
      xfromverse  : 'xfromverse';
      xtoverse    : 'xtoverse';
      votes       : 'votes';
     );

    mybibleAlias : TReferenceAlias = (
      xreferences : 'cross_references';
      book        : 'book';
      chapter     : 'chapter';
      verse       : 'verse';
  //  toverse     : 'verse_end';
      xbook       : 'book_to';
      xchapter    : 'chapter_to';
      xfromverse  : 'verse_to_start';
      xtoverse    : 'verse_to_end';
      votes       : 'votes';
     );

//========================================================================================
//                                   TReference
//========================================================================================

constructor TReference.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundAlias;
  if format = mybible then z := mybibleAlias;
  if connected and not TableExists(z.xreferences ) then connected := false;
end;

function TReference.GetData(Verse: TVerse): TVerseArray;
var
  V : TVerse;
  v_from, v_to : string;
  i, id, toverse, votes, count : integer;
begin
  SetLength(Result,0);

  id := EncodeID(Verse.book);
  v_from := ToStr(Verse.number);
  v_to   := ToStr(Verse.number + Verse.count - 1);

  try
    try
        Query.SQL.Text := 'SELECT * FROM ' + z.xreferences +
          ' WHERE '  + z.book    + ' = '  + ToStr(id) +
            ' AND '  + z.chapter + ' = '  + ToStr(Verse.chapter) +
            ' AND (' + z.verse + ' BETWEEN ' + v_from + ' AND ' + v_to + ') ';

        Query.Open;
        Query.Last;
        SetLength(Result, Query.RecordCount);
        Query.First;

        count := 0;
        for i:=0 to Query.RecordCount-1 do
          try
            v := minVerse;
            try v.book    := Query.FieldByName(z.xbook     ).AsInteger; except end;
            try v.chapter := Query.FieldByName(z.xchapter  ).AsInteger; except end;
            try v.number  := Query.FieldByName(z.xfromverse).AsInteger; except end;
            try toverse   := Query.FieldByName(z.xtoverse  ).AsInteger; except end;
            try votes     := Query.FieldByName(z.votes     ).AsInteger; except end;

            if votes <= 1 then continue;

            v.book := DecodeID(v.book);
            if toverse = 0 then v.count := 1
               else v.count := toverse - v.number + 1;

            Result[count] := v;
            count += 1;
          finally
            Query.Next;
          end;
        SetLength(Result, count);
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

//=================================================================================================
//                                       TReferences
//=================================================================================================

constructor TReferences.Create;
begin
  inherited;
  Load;
end;

procedure TReferences.Load;
var
  Item : TReference;
  List : TStringArray;
  f : string;
begin
  List := GetDatabaseList;

  for f in List do
    begin
      if Pos('.xrefs.',f) = 0 then continue; // .crossreferences.
      Item := TReference.Create(f);
      if Item.connected then Add(Item) else Item.Free;
    end;
end;

function TReferences.GetData(Verse: TVerse; language: string; out info: string): TVerseArray;
var
  filename : string;
  i : integer;
begin
  SetLength(Result,0);
  info := '';

  if self.Count = 0 then Exit;
  filename := BoolToStr(Prefix('ru', language),'obru.xrefs.unbound','ob.xrefs.unbound');

  for i:=0 to Count-1 do
    if Items[i].filename = filename then
        begin
          Result := Items[i].GetData(Verse);
          info := Items[i].info;
        end;
end;

destructor TReferences.Destroy;
var i : integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Destroy;
end;

initialization
  References := TReferences.Create;

finalization
  References.Free;

end.
