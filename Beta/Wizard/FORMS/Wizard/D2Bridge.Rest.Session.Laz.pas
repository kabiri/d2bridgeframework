unit D2Bridge.Rest.Session;

{ Copyright <COPYRIGHTYEAR> D2Bridge Framework by Talis Jonatas Gomes }

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}  

interface

uses
  Classes, SysUtils,
  Prism.Types,
  D2Bridge.Rest.Session.Interfaces,
  D2Bridge.Rest.Session.BaseClass; //, Adds Your Uses units


type
 TD2BridgeRestSession = class(TD2BridgeRestSessionBaseClass, ID2BridgeRestSession)
  strict private
   procedure Exec_CreateDM;
   procedure Exec_DestroyDM;   
  private

  public
   //DM: TDM;
   //Variables
   //Classes

   constructor Create;
   destructor Destroy; override;
 end;


implementation


{ TD2BridgeRestSession }

procedure TD2BridgeRestSession.Exec_CreateDM;
begin
{ 
 DM:= nil;

 if Path <> '/whois' then
 begin
  DM:= TDM.Create(nil);
 end;
} 
end;

procedure TD2BridgeRestSession.Exec_DestroyDM;
begin
{
 try
 //Destroy all instanced Object
 if Assigned(DM) then
  DM.Free;
 except
 end;
}
end;

constructor TD2BridgeRestSession.Create;
begin
 inherited;

{
  Data:= TMyCLass.Create;
}

{
  TThread.Synchronize(nil, Exec_CreateDM);   
} 

{
 if WebMetho = wmtGET then //Get
 begin
  if Path = '/api/ping' then
  begin
   //Instance Class...
  end;

  if RequireJWT then
  begin

  end;
 end;
}
end;


destructor TD2BridgeRestSession.Destroy;
begin
{
 TThread.Synchronize(nil, Exec_DestroyDM);
}

 //Destroy all instanced Object


 inherited;
end;

end.

