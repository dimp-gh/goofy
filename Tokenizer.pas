unit Tokenizer;
{$mode objfpc}{$H+}
interface

uses Classes,
     Contnrs;
type
   TokenType = (
      // non-greedy tokens
      ttIntegerLiteral,
      ttIdentifier,
      ttEquals,
      ttLambdaArrow,
      // greedy tokens
      ttOpenParen,
      ttCloseParen,
      // 'unknown token' is good for error handling
      ttUnknown);
      
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

function IsOpenParen(s: String): Boolean;
begin
   Result := s = '('; 
end;

function IsCloseParen(s: String): Boolean;
begin
   Result := s = ')'; 
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

function ClassifyGreedyToken(s: String): TokenType;
begin
   if IsOpenParen(s) then
      Result := ttOpenParen
   else if IsCloseParen(s) then
      Result := ttCloseParen
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
   line, acc: String;
   tokens: TTokenList;
   t: TToken;
   tt, gtt: TokenType;
begin
   tokens := TTokenList.Create;
   for l := 0 to sl.Count - 1 do
   begin
      line := sl[l];
      acc := '';
      for c := 1 to Length(line) do
      begin
         // TODO: check for greedy tokens right here (like parentheses)
         // if any of greedy tokens matches `line[c]` - create a new token and clear acc.
         gtt := ClassifyGreedyToken(line[c]);
         if gtt <> ttUnknown then
         begin
            if acc <> '' then
            begin
               tt := ClassifyToken(acc);
               t := TToken.Create(acc, tt, l + 1, c - Length(acc));
               tokens.Add(t);
               acc := '';
            end;
            t := TToken.Create(line[c], gtt, l + 1, c);
            tokens.Add(t);            
         end
         else
         begin
            if line[c] in delimiters then
            begin
               if acc <> '' then
               begin
                  tt := ClassifyToken(acc);
                  t := TToken.Create(acc, tt, l + 1, c - Length(acc));
                  tokens.Add(t);
                  acc := '';
               end;
            end
            else
               acc := acc + line[c];
         end;
      end;
      // handling token at the end of the line
      if acc <> '' then
      begin
         tt := ClassifyToken(acc);
         t := TToken.Create(acc, tt, l + 1, c - Length(acc) + 1);
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
      writeln('Token #', i + 1, ' is ', t.Value, ', type of token is ', t.Kind, '. Position - ', t.LineNo ,':', t.CharNo);
   end;
end;

initialization

end.
