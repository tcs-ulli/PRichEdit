unit richdocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  rvsBreak      = -1;
  rvsCheckPoint = -2;
  rvsPicture    = -3;
  rvsHotSpot    = -4;
  rvsComponent  = -5;
  rvsBullet     = -6;

type

  { TParagraphInfo }

  TParagraphInfo = class
     SameAsPrev: Boolean;
     Center: Boolean;
     imgNo: Integer; { for rvsJump# used as jump id }
     gr: TPersistent;
     destructor Destroy; override;
  end;
  TParagraphClass = class of TParagraphInfo;
  TTextParagraph = class(TParagraphInfo)
    StyleNo : Integer;
  end;
  TNonTextParagraph = class(TParagraphInfo)
  end;
  TLineBreakParagraph = class(TParagraphInfo)
  end;
  TCheckpointParagraph = class(TParagraphInfo)
  end;
  TPictureParagraph = class(TNonTextParagraph)
  end;
  TCustomBulledParagraph = class(TNonTextParagraph)
  end;
  THotSpotParagraph = class(TCustomBulledParagraph)
  end;
  TBulletParagraph = class(TCustomBulledParagraph)
  end;
  TCommentParagraph = class(TParagraphInfo)
  end;
  TControlParagraph = class(TNonTextParagraph)
  end;

  { TCustomRichDocument }

  TCustomRichDocument = class(TStringList)
  private
    FFilename: string;
    procedure SetFilename(AValue: string);
  public
    procedure Open;virtual;abstract;
    procedure Delete(Index: Integer); override;
    function AsString : string;virtual;
    property FileName : string read FFilename write SetFilename;
  end;

implementation

{ TParagraphInfo }

destructor TParagraphInfo.Destroy;
begin
  if Assigned(gr) then FreeAndNil(gr);
  inherited Destroy;
end;

{ TCustomRichDocument }

procedure TCustomRichDocument.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

procedure TCustomRichDocument.Delete(Index: Integer);
begin
  if Assigned(Objects[Index]) then
    begin
      Objects[Index].Free;
      Objects[Index] := nil;
    end;
  inherited Delete(Index);
end;

function TCustomRichDocument.AsString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
    Result := Result+Get(i);
end;

end.

