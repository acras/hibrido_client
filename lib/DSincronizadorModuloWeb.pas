unit DSincronizadorModuloWeb;

interface

uses
  ActiveX, SysUtils, Classes, ExtCtrls, DIntegradorModuloWeb, Dialogs, Windows, IDataPrincipalUnit,
    ISincronizacaoNotifierUnit, IdHTTP;

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
    procedure SetthreadControl(const Value: IThreadControl);
  protected
    posterDataModules: array of TDataIntegradorModuloWebClass;
    Fnotifier: ISincronizacaoNotifier;
    FThreadControl: IThreadControl;
    function getNewDataPrincipal: IDataPrincipal; virtual; abstract;
  public
    getterBlocks: TGetterBlocks;
    procedure addPosterDataModule(dm: TDataIntegradorModuloWebClass);
    procedure addGetterBlock(getterBlock: TServerToClientBlock);
    procedure ativar;
    procedure desativar;
    procedure getUpdatedData;
    procedure threadedGetUpdatedData;
    procedure saveAllToRemote(wait: boolean = false); virtual;
    property notifier: ISincronizacaoNotifier read FNotifier write FNotifier;
    property threadControl: IThreadControl read FthreadControl write SetthreadControl;
  published
    property onStepGetters: TStepGettersEvent read FonStepGetters write SetonStepGetters;
  end;

  TRunnerThreadGetters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    FThreadControl: IThreadControl;
    Fsincronizador: TDataSincronizadorModuloWeb;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
    procedure Setsincronizador(const Value: TDataSincronizadorModuloWeb);
    procedure SetThreadControl(const Value: IThreadControl);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
    property ThreadControl: IThreadControl read FThreadControl write SetThreadControl;
    property sincronizador: TDataSincronizadorModuloWeb read Fsincronizador write Setsincronizador;
  protected
    procedure setMainFormGettingTrue;
    procedure setMainFormGettingFalse;
    procedure finishGettingProcess;
    procedure Execute; override;
  end;

  TRunnerThreadPuters = class(TThread)
  private
    Fnotifier: ISincronizacaoNotifier;
    FthreadControl: IThreadControl;
    Fsincronizador: TDataSincronizadorModuloWeb;
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
    procedure Setsincronizador(const Value: TDataSincronizadorModuloWeb);
    procedure SetthreadControl(const Value: IThreadControl);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
    property sincronizador: TDataSincronizadorModuloWeb read Fsincronizador write Setsincronizador;
    property threadControl: IThreadControl read FthreadControl write SetthreadControl;
  protected
    procedure setMainFormPuttingTrue;
    procedure finishPuttingProcess;
    procedure Execute; override;
  end;
  


var
  DataSincronizadorModuloWeb: TDataSincronizadorModuloWeb;
  salvandoRetaguarda, gravandoVenda: boolean;

implementation

uses ComObj, DLog, acNetUtils;

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
  http: TidHTTP;
begin
  dm := getNewDataPrincipal;
  http := getHTTPInstance;
  try
    for i := 0 to length(getterBlocks) - 1 do
    begin
      if (Self.FThreadControl <> nil) and (not Self.FThreadControl.getShouldContinue) then
        Break;

      block := getterBlocks[i];
      dm.startTransaction;
      try
        for j := 0 to length(block) - 1 do
        begin
          if (Self.FThreadControl <> nil) and (not Self.FThreadControl.getShouldContinue) then
            Break;

          with block[j].Create(nil) do
          begin
            notifier := self.notifier;
            dmPrincipal := dm;
            getDadosAtualizados(http);
            if Assigned(onStepGetters) then onStepGetters(getHumanReadableName, i+1, length(getterBlocks));
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
    http := nil;
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
  if Self.FthreadControl = nil then
    Synchronize(setMainFormGettingTrue);
  CoInitializeEx(nil, 0);
  try
    sincronizador.getUpdatedData;
  finally
    CoUninitialize;
    if Self.FthreadControl = nil then
      Synchronize(finishGettingProcess);
  end;
end;

{ TRunnerThreadPuters }

procedure TRunnerThreadPuters.Execute;
var
  i: integer;
  dm: IDataPrincipal;
  dmIntegrador: TDataIntegradorModuloWeb;
  http: TIdHTTP;
begin
  inherited;
  if salvandoRetaguarda or gravandoVenda then exit;
  if Self.FthreadControl = nil then
    Synchronize(setMainFormPuttingTrue);
  salvandoRetaguarda := true;
  try
    CoInitializeEx(nil, 0);
    try
      http := nil;
      if gravandoVenda then exit;
      dm := sincronizador.getNewDataPrincipal;
      try
        try
          http := getHTTPInstance;
          for i := 0 to length(sincronizador.posterDataModules)-1 do
          begin
            if (Self.FThreadControl <> nil) and (not Self.FThreadControl.getShouldContinue) then
              Break;
            dmIntegrador := sincronizador.posterDataModules[i].Create(nil);
            try
              dmIntegrador.notifier := FNotifier;
              dmIntegrador.threadControl := Self.FthreadControl;
              dmIntegrador.dmPrincipal := dm;
              dmIntegrador.postRecordsToRemote(http);
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
        if http <> nil then
          FreeAndNil(http);
      end;
    finally
      CoUninitialize;
    end;
  finally
    salvandoRetaguarda := false;
    if Self.FthreadControl = nil then
      Synchronize(finishPuttingProcess);
  end;
end;

procedure TDataSincronizadorModuloWeb.sincronizaRetaguardaTimerTimer(
  Sender: TObject);
begin
  SaveAllToRemote;
end;

procedure TDataSincronizadorModuloWeb.SaveAllToRemote(wait: boolean = false);
var
  t: TRunnerThreadPuters;
begin
  t := TRunnerThreadPuters.Create(true);
  t.sincronizador := self;
  t.notifier := notifier;
  t.threadControl := Self.FThreadControl;
  t.FreeOnTerminate := not wait;
  t.Start;
  if wait then
  begin
    t.WaitFor;
    FreeAndNil(t);
  end;
end;

procedure TDataSincronizadorModuloWeb.SetonStepGetters(
  const Value: TStepGettersEvent);
begin
  FonStepGetters := Value;
end;

procedure TDataSincronizadorModuloWeb.SetthreadControl(const Value: IThreadControl);
begin
  FthreadControl := Value;
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
  setMainFormGettingFalse;
end;

procedure TRunnerThreadGetters.setMainFormGettingFalse;
begin
  if notifier <> nil then
    notifier.unflagBuscandoDadosServidor;
end;

procedure TRunnerThreadGetters.setMainFormGettingTrue;
begin
  if notifier <> nil then
    notifier.flagBuscandoDadosServidor;
end;

procedure TRunnerThreadPuters.finishPuttingProcess;
begin
  if notifier <> nil then
    notifier.unflagSalvandoDadosServidor;
end;

procedure TRunnerThreadPuters.setMainFormPuttingTrue;
begin
  if FNotifier <> nil then
    FNotifier.flagSalvandoDadosServidor;
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

procedure TRunnerThreadPuters.SetthreadControl(const Value: IThreadControl);
begin
  FthreadControl := Value;
end;

procedure TRunnerThreadGetters.Setsincronizador(
  const Value: TDataSincronizadorModuloWeb);
begin
  Fsincronizador := Value;
end;

procedure TRunnerThreadGetters.SetThreadControl(const Value: IThreadControl);
begin
  FThreadControl := Value;
end;

end.





