unit DSincronizadorModuloWeb;

interface

uses
  ActiveX, SysUtils, Classes, ExtCtrls, DIntegradorModuloWeb, Dialogs, Windows, IDataPrincipalUnit,
    ISincronizacaoNotifierUnit;

type
  TStepGettersEvent = procedure(name: string; step, total: integer) of object;
  TServerToClientBlock = array of TDataIntegradorModuloWebClass;
  TGetterBlocks = array of TServerToClientBlock;
  TDataSincronizadorModuloWeb = class(TDataModule)
    sincronizaRetaguardaTimer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure sincronizaRetaguardaTimerTimer(Sender: TObject);
  private
    atualizando: boolean;
    FonStepGetters: TStepGettersEvent;
    procedure SetonStepGetters(const Value: TStepGettersEvent);
  protected
    posterDataModules: array of TDataIntegradorModuloWebClass;
    Fnotifier: ISincronizacaoNotifier;
    function getNewDataPrincipal: IDataPrincipal; virtual; abstract;
  public
    getterBlocks: TGetterBlocks;
    procedure saveAllToRemote;
    procedure addPosterDataModule(dm: TDataIntegradorModuloWebClass);
    procedure addGetterBlock(getterBlock: TServerToClientBlock);
    procedure ativar;
    procedure desativar;
    procedure getUpdatedData;
    procedure threadedGetUpdatedData;
    procedure threadedSaveAllToRemote;
    property notifier: ISincronizacaoNotifier read FNotifier write FNotifier;
  published
    property onStepGetters: TStepGettersEvent read FonStepGetters write SetonStepGetters;
  end;

  TRunnerThreadGetters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    Fsincronizador: TDataSincronizadorModuloWeb;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
    procedure Setsincronizador(const Value: TDataSincronizadorModuloWeb);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
    property sincronizador: TDataSincronizadorModuloWeb read Fsincronizador write Setsincronizador;
  protected
    procedure setMainFormGettingTrue;
    procedure finishGettingProcess;
    procedure Execute; override;
  end;

  TRunnerThreadPuters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    Fsincronizador: TDataSincronizadorModuloWeb;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
    procedure Setsincronizador(const Value: TDataSincronizadorModuloWeb);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
    property sincronizador: TDataSincronizadorModuloWeb read Fsincronizador write Setsincronizador;
  protected
    procedure setMainFormPuttingTrue;
    procedure finishPuttingProcess;
    procedure Execute; override;
  end;
  


var
  DataSincronizadorModuloWeb: TDataSincronizadorModuloWeb;
  salvandoRetaguarda, gravandoVenda: boolean;

implementation

uses ComObj, DLog;

{$R *.dfm}

procedure TDataSincronizadorModuloWeb.addPosterDataModule(
  dm: TDataIntegradorModuloWebClass);
var
  size: integer;
begin
  size := length(posterDataModules);
  SetLength(posterDataModules, size + 1);
  posterDataModules[size] := dm;
end;

procedure TDataSincronizadorModuloWeb.saveAllToRemote;
var
  i: integer;
  dm: IDataPrincipal;
  dmIntegrador: TDataIntegradorModuloWeb;
begin
  if gravandoVenda then exit;
  dm := getNewDataPrincipal;
  if dm.sincronizar then
  begin
    try try
      for i := 0 to length(posterDataModules)-1 do
      begin
        dmIntegrador := posterDataModules[i].Create(nil);
        try
          dmIntegrador.notifier := FNotifier;
          dmIntegrador.dmPrincipal := dm;
          dmIntegrador.postRecordsToRemote;
        finally
          FreeAndNil(dmIntegrador);
        end;
      end;
    except
      on e: Exception do
        DataLog.log('Erros ao dar saveAllToRemote. Erro: ' + e.Message, 'Sync');
    end;
    finally
      dm := nil;
    end;
  end;
end;

procedure TDataSincronizadorModuloWeb.threadedGetUpdatedData;
var
  t: TRunnerThreadGetters;
begin
  t := TRunnerThreadGetters.Create(true);
  t.sincronizador := self;
  t.notifier := notifier;
  t.Resume;
end;

procedure TDataSincronizadorModuloWeb.getUpdatedData;
var
  i, j: integer;
  block: TServerToClientBlock;
  dm: IDataPrincipal;
begin
  dm := getNewDataPrincipal;
  try
    for i := 0 to length(getterBlocks) - 1 do
    begin
      block := getterBlocks[i];
      dm.startTransaction;
      try
        for j := 0 to length(block) - 1 do
        begin
          with block[j].Create(nil) do
          begin
            notifier := self.notifier;
            dmPrincipal := dm;
            getDadosAtualizados;
            if Assigned(onStepGetters) then onStepGetters(block[j].className, i+1, length(getterBlocks));
            free;
          end;
        end;
        dm.commit;
      except
        dm.rollback;
      end;
    end;
  finally
    dm := nil;
  end;
end;

procedure TDataSincronizadorModuloWeb.DataModuleCreate(Sender: TObject);
begin
  SetLength(posterDataModules, 0);
  SetLength(getterBlocks, 0);
  sincronizaRetaguardaTimer.Enabled := false;
  atualizando := false;
  salvandoRetaguarda := false;
  gravandoVenda := false;
end;

procedure TDataSincronizadorModuloWeb.ativar;
begin
  sincronizaRetaguardaTimer.Enabled := true;
end;

procedure TDataSincronizadorModuloWeb.desativar;
begin
  sincronizaRetaguardaTimer.Enabled := false;
end;

procedure TDataSincronizadorModuloWeb.addGetterBlock(
  getterBlock: TServerToClientBlock);
var
  size: integer;
begin
  size := length(getterBlocks);
  SetLength(getterBlocks, size + 1);
  getterBlocks[size] := getterBlock;
end;

{ TRunnerThread }

procedure TRunnerThreadGetters.Execute;
begin
  inherited;
  FreeOnTerminate := True;
  Synchronize(setMainFormGettingTrue);
  CoInitializeEx(nil, 0);
  try
    sincronizador.getUpdatedData;
  finally
    CoUninitialize;
    Synchronize(finishGettingProcess);
  end;
end;

{ TRunnerThreadPuters }

procedure TRunnerThreadPuters.Execute;
begin
  inherited;
  FreeOnTerminate := True;
  if salvandoRetaguarda or gravandoVenda then exit;
  Synchronize(setMainFormPuttingTrue);
  salvandoRetaguarda := true;
  try
    CoInitializeEx(nil, 0);
    try
      sincronizador.saveAllToRemote;
    finally
      CoUninitialize;
    end;
  finally
    salvandoRetaguarda := false;
    if notifier <> nil then
      notifier.unflagSalvandoDadosServidor;
    Synchronize(finishPuttingProcess);
  end;
end;

procedure TDataSincronizadorModuloWeb.sincronizaRetaguardaTimerTimer(
  Sender: TObject);
begin
  threadedSaveAllToRemote;
end;

procedure TDataSincronizadorModuloWeb.threadedSaveAllToRemote;
var
  t: TRunnerThreadPuters;
begin
  t := TRunnerThreadPuters.Create(true);
  t.sincronizador := self;
  t.notifier := notifier;
  t.Resume;
end;

procedure TDataSincronizadorModuloWeb.SetonStepGetters(
  const Value: TStepGettersEvent);
begin
  FonStepGetters := Value;
end;

procedure TRunnerThreadGetters.finishGettingProcess;
var
  i, j: integer;
  block: TServerToClientBlock;
begin
  //DataPrincipal.refreshData;
  for i := 0 to length(sincronizador.getterBlocks) - 1 do
  begin
    block := sincronizador.getterBlocks[i];
    for j := 0 to length(block) - 1 do
    begin
      block[j].updateDataSets;
    end;
  end;
  notifier.unflagBuscandoDadosServidor;
end;

procedure TRunnerThreadGetters.setMainFormGettingTrue;
begin
  if notifier <> nil then
    notifier.flagBuscandoDadosServidor;
end;

procedure TRunnerThreadPuters.finishPuttingProcess;
begin
  notifier.unflagSalvandoDadosServidor;
end;

procedure TRunnerThreadPuters.setMainFormPuttingTrue;
begin
  notifier.flagSalvandoDadosServidor;
end;

procedure TRunnerThreadPuters.Setnotifier(
  const Value: ISincronizacaoNotifier);
begin
  Fnotifier := Value;
end;

procedure TRunnerThreadGetters.Setnotifier(
  const Value: ISincronizacaoNotifier);
begin
  Fnotifier := Value;
end;

procedure TRunnerThreadPuters.Setsincronizador(
  const Value: TDataSincronizadorModuloWeb);
begin
  Fsincronizador := Value;
end;

procedure TRunnerThreadGetters.Setsincronizador(
  const Value: TDataSincronizadorModuloWeb);
begin
  Fsincronizador := Value;
end;

end.





