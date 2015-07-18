unit richdocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TParagraphInfo = class
     StyleNo: Integer;
     SameAsPrev: Boolean;
     Center: Boolean;
     imgNo: Integer; { for rvsJump# used as jump id }
     gr: TPersistent;
  end;

  TRichDocument = class(TStringList)
  public
  end;

implementation

end.

