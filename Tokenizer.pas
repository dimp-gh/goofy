unit Tokenizer;
{$mode objfpc}{$H+}
interface

uses Classes,
     Contnrs;
type
   TokenType = (ttIntegerLiteral, ttIdentifier, ttUnknown);
      
   TToken = class(TObject)
   public
      Kind: TokenType;
      Value: String;
      LineNo, CharNo: Integer;
      constructor Create(s: String; k: TokenType; line: Integer; ch: Integer);
   end;
   
   TTokenList = TObjectList;
   
function TokenizeFile(path: String): TTokenList;
function TokenizeStringList(sl: TStringList): TTokenList;
procedure PrintTokenList(tokens: TTokenList);

implementation

constructor TToken.Create(s: String; k: TokenType; line: Integer; ch: Integer);
begin
   Self.Kind := k;
   Self.Value := s;
   Self.LineNo := line;
   Self.CharNo := ch;
end;

function IsIntegerLiteral(s: String): Boolean;
const
   digits: Set of Char = ['0'..'9'];
var
   i: Integer;
begin
   // Hello, mr. Nazarov. Here we meet again.
   for i := 1 to Length(s) do
      if not(s[i] in digits) then
      begin
         Result := False;
         Exit;
      end;
   Result := True;
end;

function IsIdentifier(s: String): Boolean;
const
   allowed: Set of Char = ['a'..'z', 'A'..'Z', '_', '0'..'9'];
var
   i: Integer;
begin
   for i := 1 to Length(s) do
      if not(s[i] in allowed) then
      begin
         Result := False;
         Exit;
      end;
   Result := True;
end;

function ClassifyToken(s: String): TokenType;
begin
   if IsIntegerLiteral(s) then
      Result := ttIntegerLiteral
   else if IsIdentifier(s) then
      Result := ttIdentifier
   else
      Result := ttUnknown;
end;

function TokenizeFile(path: String): TTokenList;
var
   lines: TStringList;
begin
   // TODO: check is path exists
   lines := TStringList.Create;
   lines.LoadFromFile(path);
   Result := TokenizeStringList(lines);
end;

function TokenizeStringList(sl: TStringList): TTokenList;
const
   delimiters: Set of Char = [' ', #9];
var
   l, c: Integer;
   s, acc: String;
   tokens: TTokenList;
   t: TToken;
begin
   tokens := TTokenList.Create;
   for l := 0 to sl.Count - 1 do
   begin
      s := sl[l];
      acc := '';
      for c := 1 to Length(s) do
      begin
         if s[c] in delimiters then
         begin
            writeln('DEBUG: Inserting token: ', acc);
            t := TToken.Create(acc, ClassifyToken(acc), l + 1, c - Length(acc));
            tokens.Add(t);
            acc := '';
         end
         else
            acc := acc + s[c];
      end;
      // handling token at the end of the line
      if acc <> '' then
      begin
         writeln('DEBUG: Inserting token at the end of line: ', acc);
         t := TToken.Create(acc, ClassifyToken(acc), l, c);
         tokens.Add(t);
         acc := '';
      end;
   end;
   Result := tokens;
end;

procedure PrintTokenList(tokens: TTokenList);
var
   i: Integer;
   t: TToken;
begin
   for i := 0 to tokens.Count - 1 do
   begin
      t := tokens[i] as TToken;
      writeln('Token #', i + 1, ' is ', t.Value, ', type of token is ', t.Kind);
   end;
end;

initialization

end.
