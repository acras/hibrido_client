unit DSincronizadorModuloWeb;

interface

uses
  ActiveX, SysUtils, Classes, ExtCtrls, DIntegradorModuloWeb, Dialogs, Windows, IDataPrincipalUnit,
    ISincronizacaoNotifierUnit;

type
  TRunnerThreadGetters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
  protected
    procedure setMainFormGettingTrue;
    procedure finishGettingProcess;
    procedure Execute; override;
  end;

  TRunnerThreadPuters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
  protected
    procedure setMainFormPuttingTrue;
    procedure finishPuttingProcess;
    procedure Execute; override;
  end;

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
    notifier: ISincronizacaoNotifier;
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
  published
    property onStepGetters: TStepGettersEvent read FonStepGetters write SetonStepGetters;
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
      FreeAndNil(dm);
    end;
  end;
end;

procedure TDataSincronizadorModuloWeb.threadedGetUpdatedData;
var
  t: TRunnerThreadGetters;
begin
  t := TRunnerThreadGetters.Create(true);
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
    FreeAndNil(dm);
  end;

{  //atualizar os dados no sistema
  DataPrincipal.refreshData;
  for i := 0 to length(getterBlocks) - 1 do
  begin
    block := getterBlocks[i];
    dm.startTransaction;
    try
      for j := 0 to length(block) - 1 do

  dsAtualizar.Refresh;
  dmAtualizar.selecionar;
}
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
    DataSincronizadorModuloWeb.getUpdatedData;
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
      DataSincronizadorModuloWeb.saveAllToRemote;
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
  for i := 0 to length(DataSincronizadorModuloWeb.getterBlocks) - 1 do
  begin
    block := DataSincronizadorModuloWeb.getterBlocks[i];
    for j := 0 to length(block) - 1 do
    begin
      block[j].updateDataSets;
    end;
  end;
  //FormPrincipal.buscandoDadosServidor := false;
end;

procedure TRunnerThreadGetters.setMainFormGettingTrue;
begin
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

end.





