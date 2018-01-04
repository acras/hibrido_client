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
    function ShouldContinue: boolean;
  protected
    Fnotifier: ISincronizacaoNotifier;
    FThreadControl: IThreadControl;
    FCustomParams: ICustomParams;
    FDataLog: ILog;
  public
    posterDataModules: array of TDataIntegradorModuloWebClass;
    getterBlocks: TGetterBlocks;
    function getNewDataPrincipal: IDataPrincipal; virtual; abstract;
    procedure addPosterDataModule(dm: TDataIntegradorModuloWebClass);
    procedure addGetterBlock(getterBlock: TServerToClientBlock);
    procedure ativar;
    procedure desativar;
    procedure getUpdatedData;
    procedure threadedGetUpdatedData;
    procedure saveAllToRemote(wait: boolean = false); virtual;
    property notifier: ISincronizacaoNotifier read FNotifier write FNotifier;
    property threadControl: IThreadControl read FthreadControl write FthreadControl;
    property CustomParams: ICustomParams read FCustomParams write FCustomParams;
    property Datalog: ILog read FDataLog write FDataLog;
  published
    property onStepGetters: TStepGettersEvent read FonStepGetters write SetonStepGetters;
  end;

  TCustomRunnerThread = class(TThread)
  private
    procedure Setnotifier(const Value: ISincronizacaoNotifier);
    procedure Setsincronizador(const Value: TDataSincronizadorModuloWeb);
  protected
    Fnotifier: ISincronizacaoNotifier;
    FthreadControl: IThreadControl;
    Fsincronizador: TDataSincronizadorModuloWeb;
    FCustomParams: ICustomParams;
    FDataLog: ILog;
    function ShouldContinue: boolean;
    procedure Log(const aLog, aClasse: string);
  public
    property notifier: ISincronizacaoNotifier read Fnotifier write Setnotifier;
    property sincronizador: TDataSincronizadorModuloWeb read Fsincronizador write Setsincronizador;
    property threadControl: IThreadControl read FthreadControl write FthreadControl;
    property CustomParams: ICustomParams read FCustomParams write FCustomParams;
    property DataLog: ILog read FDataLog write FDataLog;
  end;

  TRunnerThreadGetters = class(TCustomRunnerThread)
  private
    procedure setMainFormGettingFalse;
    procedure finishGettingProcess;
  protected
    procedure setMainFormGettingTrue;
  public
    procedure Execute; override;
  end;

  TRunnerThreadPuters = class(TCustomRunnerThread)
  protected
    procedure setMainFormPuttingTrue;
    procedure finishPuttingProcess;
  public
    procedure Execute; override;
  end;


var
  DataSincronizadorModuloWeb: TDataSincronizadorModuloWeb;
  salvandoRetaguarda, gravandoVenda: boolean;

implementation

uses ComObj, acNetUtils;

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
  t.threadControl := self.threadControl;
  t.CustomParams := self.CustomParams;
  t.DataLog := Self.Datalog;
  t.notifier := notifier;
  t.Start;
end;

function TDataSincronizadorModuloWeb.ShouldContinue: boolean;
begin
  Result := true;
  if Self.FThreadControl <> nil then
    result := Self.FThreadControl.getShouldContinue;
end;

procedure TDataSincronizadorModuloWeb.getUpdatedData;
var
  i, j: integer;
  block: TServerToClientBlock;
  dm: IDataPrincipal;
  http: TidHTTP;
  dimw: TDataIntegradorModuloWeb;
  dimwName: string;
begin
  CoInitializeEx(nil, 0);
  try
    dm := getNewDataPrincipal;
    http := getHTTPInstance;
    try
      for i := 0 to length(getterBlocks) - 1 do
      begin
        if not Self.ShouldContinue then
          Break;

        block := getterBlocks[i];
        dm.startTransaction;
        try
          for j := 0 to length(block) - 1 do
          begin
            if not Self.ShouldContinue then
              Break;

            dimw := block[j].Create(nil);
            try
              dimwName := dimw.getHumanReadableName;
              dimw.notifier := Self.Fnotifier;
              dimw.dmPrincipal := dm;
              dimw.threadcontrol := Self.FThreadControl;
              dimw.CustomParams := Self.FCustomParams;
              dimw.DataLog := Self.FDataLog;
              dimw.getDadosAtualizados(http);
              if Assigned(onStepGetters) then onStepGetters(dimw.getHumanReadableName, i+1, length(getterBlocks));
            finally
              dimw.free;
            end;
          end;
          dm.commit;
        except
          on E: Exception do
          begin
            dm.rollback;
            if assigned (self.FDataLog) then
            begin
              SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
              Self.FDataLog.log(Format('Erro em GetUpdateData para a classe "%s":'+#13#10+'%s', [dimwName,e.Message]));
              SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
            end;
          end;
        end;
      end;
    finally
      dm := nil;
      http := nil;
    end;
  finally
    CoUninitialize;
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
  t.CustomParams := Self.FCustomParams;
  t.FreeOnTerminate := not wait;
  t.DataLog := Self.FDataLog;
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

{TRunnerThreadGetters}

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

procedure TRunnerThreadGetters.Execute;
begin
  inherited;
  FreeOnTerminate := True;
  if Self.Fnotifier <> nil then
    Synchronize(Self.setMainFormGettingTrue);
  CoInitializeEx(nil, 0);
  try
    sincronizador.notifier := Self.notifier;
    sincronizador.threadControl := Self.threadControl;
    sincronizador.Datalog := Self.DataLog;
    sincronizador.CustomParams := Self.CustomParams;
    sincronizador.getUpdatedData;
  finally
    CoUninitialize;
    if Self.Fnotifier <> nil then
      Synchronize(finishGettingProcess);
  end;
end;

{TRunnerThreadPuters}

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

procedure TRunnerThreadPuters.Execute;
var
  i: integer;
  dm: IDataPrincipal;
  dmIntegrador: TDataIntegradorModuloWeb;
  http: TIdHTTP;
  lTranslateTableNames: TTranslateTableNames;
begin
  inherited;
  if salvandoRetaguarda or gravandoVenda then exit;
  if Self.Fnotifier <> nil then
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
          lTranslateTableNames := TTranslateTableNames.Create;
          for i := 0 to length(sincronizador.posterDataModules)-1 do
          begin
            if not Self.ShouldContinue then
              Break;
            dmIntegrador := sincronizador.posterDataModules[i].Create(nil);
            try
              if (dmIntegrador.getNomeTabela <> EmptyStr) and (dmIntegrador.NomeSingular <> EmptyStr) then
              begin
                if not lTranslateTableNames.ContainsKey(dmIntegrador.getNomeTabela) then
                  lTranslateTableNames.Add(LowerCase(Trim(dmIntegrador.getNomeTabela)), LowerCase(Trim(dmIntegrador.NomeSingular)));
              end;

              dmIntegrador.SetTranslateTableNames(lTranslateTableNames);
              dmIntegrador.notifier := FNotifier;
              dmIntegrador.threadControl := Self.FthreadControl;
              dmIntegrador.CustomParams := Self.FCustomParams;
              dmIntegrador.dmPrincipal := dm;
              dmIntegrador.DataLog := Self.FDataLog;
              dmIntegrador.postRecordsToRemote(http);
            finally
              FreeAndNil(dmIntegrador);
            end;
          end;
        except
          on e: Exception do
          begin
            Self.log('Erros ao dar saveAllToRemote. Erro: ' + e.Message, 'Sync');
          end;
        end;
      finally
        dm := nil;
        if http <> nil then
          FreeAndNil(http);
        FreeAndNil(lTranslateTableNames);
      end;
    finally
      CoUninitialize;
    end;
  finally
    salvandoRetaguarda := false;
    if Self.Fnotifier <> nil then
      Synchronize(finishPuttingProcess);
  end;
end;

{ TCustomRunnerThread }

procedure TCustomRunnerThread.Log(const aLog, aClasse: string);
begin
  if Self.FDataLog <> nil then
   Self.FDataLog.log(aLog, aClasse);
end;

procedure TCustomRunnerThread.Setnotifier(const Value: ISincronizacaoNotifier);
begin
  Fnotifier := Value;
end;

procedure TCustomRunnerThread.Setsincronizador(const Value: TDataSincronizadorModuloWeb);
begin
  Fsincronizador := Value;
end;

function TCustomRunnerThread.ShouldContinue: boolean;
begin
  result := true;
  if Self.FThreadControl <> nil then
    result := Self.FThreadControl.getShouldContinue;
end;

end.





