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
  TParagraphInfo = class
     SameAsPrev: Boolean;
     Center: Boolean;
     imgNo: Integer; { for rvsJump# used as jump id }
     gr: TPersistent;
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

  TCustomRichDocument = class(TStringList)
  public
  end;

implementation

end.

