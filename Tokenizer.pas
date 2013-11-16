unit Tokenizer;
{$mode objfpc}{$H+}
interface

uses
   Classes,
   Contnrs,
   SysUtils;

type
   TokenType = (
      // 'unknown token' is good for error handling
      ttUnknown,
      // non-greedy tokens
      ttIntegerLiteral,
      ttIdentifier,
      ttLet,
      ttLetRec,
      ttIn,
      ttEquals,
      ttLambda,
      ttLambdaArrow,
      // greedy tokens
      ttOpenParen,
      ttCloseParen);
      
   TToken = class(TObject)
   public
      Kind: TokenType;
      Value: String;
      LineNo, CharNo: Integer;
      constructor Create(s: String;
                         k: TokenType = ttUnknown;
                         line: Integer = 0;
                         ch: Integer = 0);
   end;
   
   TTokenList = TObjectList;
   
   ETokenizeError = class(Exception);
      
function TokenizeFile(path: String): TTokenList;
function TokenizeStringList(sl: TStringList): TTokenList;
procedure PrintTokenList(tokens: TTokenList);
procedure ReportTokenizeErrors(sourcePath: String; tokens: TTokenList);
procedure Push(t: TToken; tokens: TTokenList);
function Pop(tokens: TTokenList): TToken;
function Copy(ts: TTokenList): TTokenList;

implementation

constructor TToken.Create(s: String;
                          k: TokenType = ttUnknown;
                          line: Integer = 0;
                          ch: Integer = 0);
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
   allowed: Set of Char = ['a'..'z', 'A'..'Z', '_', '-', '0'..'9'];
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

function IsLet(s: String): Boolean;
begin
   Result := s = 'let'; 
end;

function IsLetRec(s: String): Boolean;
begin
   Result := s = 'letrec'; 
end;

function IsEquals(s: String): Boolean;
begin
   Result := s = '='; 
end;

function IsLambda(s: String): Boolean;
begin
   Result := s = 'fn';
end;

function IsLambdaArrow(s: String): Boolean;
begin
   Result := s = '=>';
end;

function IsIn(s: String): Boolean;
begin
   Result := s = 'in'; 
end;

function ClassifyToken(s: String): TokenType;
begin
   if IsIntegerLiteral(s) then
      Result := ttIntegerLiteral
   else if IsLambdaArrow(s) then
      Result := ttLambdaArrow
   else if IsEquals(s) then
      Result := ttEquals
   else if IsLambda(s) then
      Result := ttLambda
   else if IsLet(s) then
      Result := ttLet
   else if IsLetRec(s) then
      Result := ttLetRec
   else if IsIn(s) then
      Result := ttIn
   else if IsIdentifier(s) then
      Result := ttIdentifier
   else
      Result := ttUnknown;
end;

function ClassifyGreedyToken(s: String): TokenType;
begin
   // for simplification, only parens are greedy tokens
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
         // if any of greedy tokens matches `line[c]` - create a new token and clear acc.
         // In another words, greedy tokens are the ones that do not require whitespace
         // to separate them and other tokens.
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

procedure ReportTokenizeErrors(sourcePath: String; tokens: TTokenList);
var
   i: Integer;
   t: TToken;
   ShouldThrow: Boolean;
begin
   ShouldThrow := False;
   for i := 0 to tokens.Count - 1 do
   begin
      t := tokens[i] as TToken;
      if t.Kind = ttUnknown then
      begin
         ShouldThrow := True;
         // TODO: draw pretty report for every bad token
         // example:
         // (let f = fn x => 5 in '(f f))
         //                       ^
         // Error: Bad token ' at position x:23
         writeln('Error: Bad token "', t.Value, '" at position ', t.LineNo, ':', t.CharNo);
      end
   end;
   if ShouldThrow then
      raise ETokenizeError.Create('Errors were found on tokenization. Process aborted');
end;

function Pop(tokens: TTokenList): TToken;
var
   t: TToken;
begin
   t := tokens.First as TToken;
   if t <> nil then
   begin
      tokens.Extract(t);
      Result := t;
   end
   else
      Result := TToken.Create('', ttUnknown, 14, 88);
end;

procedure Push(t: TToken; tokens: TTokenList);
begin
   tokens.Insert(0, t);
end;

function Copy(ts: TTokenList): TTokenList;
begin
   Result := TTokenList.Create;
   Result.Assign(ts);
end;

initialization

end.
