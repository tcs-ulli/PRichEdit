unit oodocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils,richdocument, zipper,
  XMLRead, DOM, XMLWrite;

type

  { TODFDocument }

  TODFDocument = class(TCustomRichDocument)
  private
    Content:txmldocument;
    FFilename : string;
    procedure ReadStyles(AStylesNode: TDOMNode);
    procedure ReadAutomaticStyles(AStylesNode: TDOMNode);
    procedure ReadMasterStyles(AStylesNode: TDOMNode);
  public
    procedure Open; override;
  end;

implementation

{ TODFDocument }

procedure TODFDocument.ReadStyles(AStylesNode: TDOMNode);
begin

end;

procedure TODFDocument.ReadAutomaticStyles(AStylesNode: TDOMNode);
begin

end;

procedure TODFDocument.ReadMasterStyles(AStylesNode: TDOMNode);
begin

end;

function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if ANode = nil then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

procedure TODFDocument.Open;
var
  FilePath: String;
  UnZip: TUnZipper;
  FileList: TStringList;
  Doc: TXMLDocument;
  StylesNode: TDOMNode;
  BodyNode: TDOMNode;
  TextNode: TDOMNode;
  tmp: DOMString;
  ChildNode: TDOMNode;
  nodeName: DOMString;
  cellText: String;
  hyperlink: String;
  subnode: TDOMNode;
  procedure AddToCellText(AText: String);
  begin
    if cellText = ''
       then cellText := AText
       else cellText := cellText + AText;
  end;
begin
  FilePath := GetTempDir(false);
  UnZip := TUnZipper.Create;
  FileList := TStringList.Create;
  try
    FileList.Add('styles.xml');
    FileList.Add('content.xml');
    FileList.Add('settings.xml');
    UnZip.OutputPath := FilePath;
    Unzip.UnZipFiles(FileName,FileList);
  finally
    FreeAndNil(FileList);
    FreeAndNil(UnZip);
  end; //try
  Doc := nil;
  try
    // process the styles.xml file
    ReadXMLFile(Doc, FilePath+'styles.xml');
    DeleteFile(FilePath+'styles.xml');

    StylesNode := Doc.DocumentElement.FindNode('office:styles');
    ReadStyles(StylesNode);

    StylesNode := Doc.DocumentElement.FindNode('office:automatic-styles');
    ReadAutomaticStyles(StylesNode);

    StylesNode := Doc.DocumentElement.FindNode('office:master-styles');
    ReadMasterStyles(StylesNode);

    Doc.Free;

    //process the content.xml file
    ReadXMLFile(Doc, FilePath+'content.xml');
    DeleteFile(FilePath+'content.xml');

    StylesNode := Doc.DocumentElement.FindNode('office:automatic-styles');
    ReadStyles(StylesNode);

    BodyNode := Doc.DocumentElement.FindNode('office:body');
    if not Assigned(BodyNode) then Exit;

    TextNode := BodyNode.FindNode('office:text');
    if not Assigned(TextNode) then Exit;
    cellText := '';
    hyperlink := '';
    ChildNode := TextNode.FirstChild;
    while Assigned(childnode) do
    begin
      nodeName := childNode.NodeName;
      if nodeName = 'text:p' then begin
        // Each 'text:p' node is a paragraph --> we insert a line break after the first paragraph
        if cellText <> '' then
          cellText := cellText + LineEnding;
        subnode := childnode.FirstChild;
        while Assigned(subnode) do
        begin
          nodename := subnode.NodeName;
          case nodename of
            '#text' :
              AddToCellText(subnode.TextContent);
            'text:a':     // "hyperlink anchor"
              begin
                hyperlink := GetAttrValue(subnode, 'xlink:href');
                AddToCellText(subnode.TextContent);
              end;
            'text:span':
              AddToCellText(subnode.TextContent);
          end;
          subnode := subnode.NextSibling;
        end;
      end;
      childnode := childnode.NextSibling;
    end;
    AddObject(CellText,nil);
  finally
    if Assigned(Doc) then Doc.Free;
  end;
end;

end.

