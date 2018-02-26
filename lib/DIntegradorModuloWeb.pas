unit DIntegradorModuloWeb;

interface

uses
  SysUtils, ExtCtrls, DBClient, idHTTP, MSXML2_TLB, dialogs, acStrUtils, acNetUtils,
  DB, IdMultipartFormData, IdBaseComponent, IdComponent, IdTCPConnection, forms,
  IdTCPClient, IdCoder, IdCoder3to4, IdCoderUUE, IdCoderXXE, Controls,
  IDataPrincipalUnit, idURI, System.Classes, Windows,
  ISincronizacaoNotifierUnit, Data.SqlExpr,
  Xml.XMLIntf, Winapi.ActiveX, XML.XMLDoc, System.Generics.Collections, HTTPApp, StrUtils,
  Soap.EncdDecd, Variants {$IFDEF VER250}, Data.DBXJSON, Data.DBXPlatform {$ENDIF} {$IFDEF VER300}, System.JSON {$ENDIF};

type
  TStringDictionary = class(TDictionary<String, String>)
  end;

  TXMLNodeDictionary = class(TDictionary<string, IXMLDomNode>)
  end;

  TTabelaDetalhe = class;

  TTabelaDetalheList = class(TObjectList<TTabelaDetalhe>)
  end;

  TFieldDictionary = class
  private
   FFieldName: string;
   FfieldType: TFieldType;
    procedure SetFieldName(const Value: string);
  public
    property FieldName: string read FFieldName write SetFieldName;
    property DataType: TFieldType read FFieldType write FFieldType;
  end;

  TFieldDictionaryList = class(TDictionary <string, TFieldDictionary>)
  private
    FDm : IDataPrincipal;
    FTableName: string;
    procedure getTableFields;
  public
    constructor Create(const aTableName: string; aDm : IDataPrincipal);
    destructor Destroy; override;
  end;

  EIntegradorException = class(Exception)
  end;

  TDMLOperation = (dmInsert, dmUpdate);
  THttpAction = (haGet, haPost);
  TParamsType = (ptParam, ptJSON);

  TAnonymousMethod = reference to procedure(aDataSet: TDataSet);

  TNameTranslation = class
  public
    server: string;
    pdv: string;
    lookupRemoteTable: string;
    fkName: string;
  end;

  TNameTranslationsList = class (TObjectList<TNameTranslation>)
  end;

  TTranslationSet = class
    public
      Translations: TNameTranslationsList;
      constructor create(owner: TComponent);
      destructor Destroy; override;
      procedure add(serverName, pdvName: string;
        lookupRemoteTable: string = ''; fkName: string = '');
      function translateServerToPDV(serverName: string; duasVias: boolean): string;
      function translatePDVToServer(pdvName: string): string;
      function size: integer;
      function get(index: integer): TNameTranslation;
  end;

  TTabelaDependente = class
  public
    nomeTabela: string;
    nomeFK: string;
  end;

  TTabelaDependenteList = class (TObjectList<TTabelaDependente>)
  end;

  TJSONArrayContainer = class
  private
    FJSonArray: TJsonArray;
  public
    nomePluralDetalhe: string;
    nomeSingularDetalhe: string;
    nomeTabela: string;
    nomePkLocal: string;
    function getJsonArray: TJsonArray;
    destructor Destroy; override;
  end;

  TDetailList = class(TDictionary<String, TJSONArrayContainer>)
  public
    destructor Destroy; override;
  end;


  TDataIntegradorModuloWeb = class(TDataModule)
  private
    FdmPrincipal: IDataPrincipal;
    Fnotifier: ISincronizacaoNotifier;
    FthreadControl: IThreadControl;
    FCustomParams: ICustomParams;
    FstopOnPostRecordError: boolean;
    FStopOnGetRecordError : boolean;
    procedure addTabelaDetalheParams(valorPK: integer;
      params: TStringList;
      tabelaDetalhe: TTabelaDetalhe);
    function GetErrorMessage(const aXML: string): string;
    procedure SetDataLog(const Value: ILog);
    procedure UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : TTabelaDetalheList);
    procedure SetthreadControl(const Value: IThreadControl);
    procedure OnWorkHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function getFieldInsertList(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
    function BinaryFromBase64(const base64: string): TBytesStream;
    procedure SelectDetailsIterate(aDetailList: TDetailList; aValorPK: integer);
    procedure addTabelaDetalheParamsIterate(valorPK: integer; params: TStringList);
    procedure ExecQuery(aQry: TSQLDataSet);
    function CheckQryCommandTextForDuasVias(const aId: integer;  Integrador: TDataIntegradorModuloWeb): string;
    procedure UpdateVersionId(const aId, aLastVersionId: integer);
    procedure resyncRecord(const aId: integer);
  protected
    FFieldList : TFieldDictionaryList;
    FDataLog: ILog;
    FTranslateTableNames: TStringDictionary;
    FnomeTabela: string;
    FnomeSingular: string;
    FNomePlural: string;
    FnomePKLocal: string;
    nomePKRemoto: string;
    nomeGenerator: string;
    usePKLocalMethod: Boolean;
    duasVias: boolean;
    useMultipartParams: boolean;
    clientToServer: boolean;
    encodeJsonValues : boolean;
    tabelasDependentes: TTabelaDependenteList;
    tabelasDetalhe: TTabelaDetalheList;
    offset: integer;
    paramsType: TParamsType;
    function getVersionFieldName: string; virtual;
    procedure Log(const aLog: string; aClasse: string = ''); virtual;
    function extraGetUrlParams: String; virtual;
    procedure beforeRedirectRecord(idAntigo, idNovo: integer); virtual;
    function ultimaVersao: integer; virtual;
    function getRequestUrlForAction(toSave: boolean; versao: integer = -1): string; virtual;
    function importRecord(node: IXMLDomNode): boolean; virtual;
    procedure updateInsertRecord(node: IXMLDomNode; const id: integer);
    function jaExiste(const id: integer; Integrador: TDataIntegradorModuloWeb): boolean;
    function getFieldList(node: IXMLDomNode): string;
    function getFieldUpdateList(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
    function getFieldValues(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
    function translateFieldValue(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string; virtual;
    function translateFieldNamePdvToServer(node: IXMLDomNode): string;
    function translateFieldNameServerToPdv(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string; virtual;
    function translateTypeValue(fieldType, fieldValue: string): string;
    function translateValueToServer(translation: TNameTranslation;
      fieldName: string; field: TField;
      nestedAttribute: string = ''; fkName: string = ''; fieldValue: String = ''): string; virtual;
    function translateValueFromServer(fieldName, value: string; Integrador: TDataIntegradorModuloWeb): string; virtual;
    procedure redirectRecord(idAntigo, idNovo: integer);
    function getFieldAdditionalList(node: IXMLDomNode): string; virtual;
    function getFieldAdditionalValues(node: IXMLDomNode): string; virtual;
    function getFieldAdditionalUpdateList(node: IXMLDomNode): string; virtual;
    function nomeActionSave: string; virtual;
    function nomeActionGet: string; virtual;
    function nomeSingularSave: string; virtual;
    function nomeSingularGet: string; virtual;
    procedure updateSingletonRecord(node: IXMLDOMNode);
    function getOrderBy: string; virtual;
    procedure addMoreParams(ds: TDataSet; params: TStringList); virtual;
    procedure prepareMultipartParams(ds: TDataSet;
      multipartParams: TIdMultiPartFormDataStream); virtual; abstract;
    function singleton: boolean;
    function getUpdateBaseSQL(node: IXMLDOMNode; Integrador: TDataIntegradorModuloWeb): string;
    procedure addDetails(ds: TDataSet; params: TStringList);
    function addTranslatedParams(ds: TDataSet;
      params: TStringList;
      translations: TTranslationSet; nestedAttribute: string = ''): IXMLDomDocument2;
    function getAdditionalSaveConditions: string; virtual;
    function gerenciaRedirecionamentos(idLocal, idRemoto: integer): boolean; virtual;
    function getNewDataPrincipal: IDataPrincipal; virtual; abstract;
    function maxRecords: integer; virtual;
    function getTimeoutValue: integer; virtual;
    function getDateFormat: String; virtual;
    function getAdditionalDetailFilter:String; virtual;
    function shouldContinue: boolean;
    procedure onDetailNamesMalformed(configName, tableName: string); virtual;
    function getIncludeFieldNameOnList(const aDMLOperation: TDMLOperation; const aFieldName: string; Integrador: TDataIntegradorModuloWeb): boolean; virtual;
    function getObjectsList: string; virtual;
    function getUpdateStatement(node: IXMLDomNode; const id: integer): String; virtual;
    function getInsertStatement(node: IXMLDomNode): String; virtual;
    function getNewId(node: IXMLDomNode): Integer; virtual;
    function post(ds: TDataSet; http: TidHTTP; url: string): string; virtual;
    procedure addDetailsToJsonList(aDetailList: TDetailList; aDs: TDataSet); virtual;
    procedure SelectDetails(aDetailList: TDetailList; aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe); virtual;
    function getJsonObject(aDs: TDataSet; aTranslations: TTranslationSet; aDict: TStringDictionary; aNestedAttribute: string = '') : TJsonObject; virtual;
    procedure addMasterTableToJson(aDetailList: TDetailList; aDs: TDataSet; apStream: TStringStream); virtual;
    procedure RunDataSet(const aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe; aProc: TAnonymousMethod); virtual;
    function GetIdRemoto(aDoc: IXMLDomDocument2): integer;
    function getXMLContentAsXMLDom(const aXMLContent: string): IXMLDomDocument2;
    procedure SetdmPrincipal(const Value: IDataPrincipal); virtual;
    function getdmPrincipal: IDataPrincipal; virtual;
    function JsonObjectHasPair(const aName: string; aJson: TJSONObject): boolean;
    function DataSetToArray(aDs: TDataSet): TStringDictionary; virtual;
    procedure BeforeUpdateInsertRecord(const aTableName: String; node: IXMLDomNode; const id: integer; var handled: boolean); virtual;
    procedure ExecInsertRecord(node: IXMLDomNode; const id: integer; Integrador: TDataIntegradorModuloWeb); virtual;
    function getTranslatedTable(const aServerName: string): TDataIntegradorModuloWeb; virtual;
    function getNomeSingular: string; virtual;
    procedure SetNomeSingular(const Value: string); virtual;
    function GetNomePlural: string; virtual;
    procedure setNomePlural(const Value: string); virtual;
    function GetNomePKLocal: string; virtual;
    procedure setNomePKLocal(const Value: string); virtual;
    procedure SetQueryParameters(qry: TSQLDataSet; DMLOperation: TDMLOperation; node: IXMLDomNode; ChildrenNodes: TXMLNodeDictionary;
      Integrador: TDataIntegradorModuloWeb); virtual;
    function GetDefaultValueForSalvouRetaguarda: Char; virtual;
  public
    translations: TTranslationSet;
    verbose: boolean;
    property notifier: ISincronizacaoNotifier read Fnotifier write Fnotifier;
    property threadControl: IThreadControl read FthreadControl write SetthreadControl;
    property CustomParams: ICustomParams read FCustomParams write FCustomParams;
    property dmPrincipal: IDataPrincipal read getdmPrincipal write SetdmPrincipal;
    property stopOnPostRecordError: boolean read FstopOnPostRecordError write FstopOnPostRecordError;
    property stopOnGetRecordError: boolean read FStopOnGetRecordError write FstopOnGetRecordError;
    function buildRequestURL(nomeRecurso: string; params: string = ''; httpAction: THttpAction = haGet): string; virtual; abstract;
    procedure getDadosAtualizados(http: TIdHTTP = nil); virtual;
    function saveRecordToRemote(ds: TDataSet; var salvou: boolean; http: TidHTTP = nil): IXMLDomDocument2;
    procedure migrateSingletonTableToRemote;
    procedure postRecordsToRemote(http: TidHTTP = nil); virtual;
    class procedure updateDataSets; virtual;
    procedure afterDadosAtualizados; virtual;
    function getHumanReadableName: string; virtual;
    property DataLog: ILog read FDataLog write SetDataLog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getNomeTabela: string; virtual;
    procedure setNomeTabela(const Value: string); virtual;
    procedure SetTranslateTableNames(aTranslateTableNames: TStringDictionary);
    property nomeTabela: string read GetNomeTabela write setNomeTabela;
    property NomeSingular: string read getNomeSingular write SetNomeSingular;
    property nomePlural: string read GetNomePlural write setNomePlural;
    property nomePKLocal: string read GetNomePKLocal write setNomePKLocal;
    function getFieldDictionaryList: TFieldDictionaryList;
    function getTabelasDetalhe: TTabelaDetalheList;
  end;

  TDataIntegradorModuloWebClass = class of TDataIntegradorModuloWeb;

  TTabelaDetalhe = class(TDataIntegradorModuloWeb)
  private
    FnomeParametro: string;
    FnomeFK: string;
  protected
    function GetNomeParametro: string; virtual;
    procedure setNomeParametro(const Value: string); virtual;
    function GetNomeFK: string; virtual;
    procedure setNomeFK(const Value: string); virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property nomeParametro: string read GetNomeParametro write setNomeParametro;
    property nomeFK: string read GetNomeFK write setNomeFK;
  end;

var
  DataIntegradorModuloWeb: TDataIntegradorModuloWeb;
implementation

uses AguardeFormUn, ComObj, idCoderMIME, IdGlobal;

{$R *.dfm}

function TDataIntegradorModuloWeb.extraGetUrlParams: String;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.getObjectsList: string;
begin
  Result := '/' + dasherize(nomePlural) + '//' + dasherize(FnomeSingular);
end;

procedure TDataIntegradorModuloWeb.getDadosAtualizados(http: TIdHTTP = nil);
var
  url, xmlContent, erro: string;
  doc: IXMLDomDocument2;
  list : IXMLDomNodeList;
  i, numRegistros: integer;
  node : IXMLDomNode;
  keepImporting: boolean;
  vLog: string;
  LastId, LastVersionId: integer;
begin
  keepImporting := true;
  while keepImporting do
  begin
    if (not self.shouldContinue) then
      Break;

    url := getRequestUrlForAction(false, ultimaVersao) + extraGetUrlParams;
    if notifier <> nil then
      notifier.setCustomMessage('Buscando ' + getHumanReadableName + '...');
    numRegistros := 0;
    {$IFDEF HibridoClientDLL}
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE OR 4 OR FOREGROUND_INTENSITY ); //);
    writeln('URL: ' + url);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
    {$ENDIF}

    Self.Log(Format('Buscando %s',[getHumanReadableName]));
    Self.Log(url);

    xmlContent := getRemoteXmlContent(url, http, erro);

    if (erro <> EmptyStr) then
    begin
      vLog := Format('Erro importando "%s": "%s". '+ #13#10, [getHumanReadableName, GetErrorMessage(erro)]);
      Self.Log(vLog);
      raise EIntegradorException.Create(vLog);
    end;

    if trim(xmlContent) <> '' then
    begin
      doc := CoDOMDocument60.Create;
      try
        doc.loadXML(xmlContent);
        list := doc.selectNodes(Self.getObjectsList);
        numRegistros := list.length;
        if notifier <> nil then
          notifier.setCustomMessage(IntToStr(numRegistros) + ' novos');
        for i := 0 to numRegistros-1 do
        begin
          if (not self.shouldContinue) then
            Break;

          if notifier <> nil then
            notifier.setCustomMessage('Importando ' + getHumanReadableName + ': ' + IntToStr(i+1) +
            '/' + IntToStr(numRegistros));
          node := list.item[i];
          if node<>nil then
          begin
            if not importRecord(node) and Self.FStopOnGetRecordError then
              Break;
            if i = numRegistros - 1 then
            begin
              LastId := strToIntDef(node.selectSingleNode(dasherize(nomePKRemoto)).text, -1);
              LastVersionId :=  strToIntDef(node.selectSingleNode(dasherize(Self.getVersionFieldName)).text, -1);
              if (LastId > 0) and (LastVersionId > 0) then
                Self.UpdateVersionId(LastId, LastVersionId);
            end;
          end;
        end;
      finally
        doc := nil;
      end;
    end;
    keepImporting := (maxRecords > 0) and (numRegistros >= maxRecords);
  end;
  afterDadosAtualizados;
end;

function TDataIntegradorModuloWeb.GetDefaultValueForSalvouRetaguarda: Char;
begin
  Result := 'S';
end;

procedure TDataIntegradorModuloWeb.UpdateVersionId(const aId, aLastVersionId: integer);
var
  qryVersionId: TSQLDataSet;
begin
  dmPrincipal.startTransaction;
  try
    //se for o último registro do xml, atualizar o version_id
    qryVersionId := dmPrincipal.getQuery;
    try
      qryVersionId.CommandText := 'UPDATE ' + Self.nomeTabela +' SET ' + Self.getVersionFieldName + ' = :NewVersion, '+
                                  'SalvouRetaguarda = ' + QuotedStr(Self.GetDefaultValueForSalvouRetaguarda) +
                                  ' WHERE ' + Self.getVersionFieldName + ' = (SELECT MAX(' + Self.getVersionFieldName + ') FROM ' + Self.nomeTabela +')';
      qryVersionId.ParamByName('NewVersion').AsInteger := aLastVersionId;

      Self.ExecQuery(qryVersionId);
    finally
      qryVersionId.Free;
    end;
     dmPrincipal.commit;
  except
    dmPrincipal.rollBack;
    raise;
  end;
end;


function TDataIntegradorModuloWeb.getHumanReadableName: string;
begin
  result := ClassName;
end;

function TDataIntegradorModuloWeb.maxRecords: integer;
begin
  result := 0;
end;

function TDataIntegradorModuloWeb.importRecord(node : IXMLDomNode): boolean;
var
  id: integer;
begin
  Result := False;
  if not singleton then
  begin
    id := strToIntDef(node.selectSingleNode(dasherize(nomePKRemoto)).text, -1);
    if id >= 0 then
    begin
      dmPrincipal.startTransaction;
      try
        Self.updateInsertRecord(node, id);
        dmPrincipal.commit;
        Result := True;
      except
        on E:Exception do
        begin
          dmPrincipal.rollBack;
          Self.log(Format('Erro ao importar a tabela "%s":', [self.nomeTabela]));
          {$IFDEF HibridoClientDLL}
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
          {$ENDIF}
          Self.log(e.Message);
          {$IFDEF HibridoClientDLL}
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
          {$ENDIF}
          Self.resyncRecord(id);
        end;
      end;
    end;
  end
  else
    updateSingletonRecord(node);
end;

procedure TDataIntegradorModuloWeb.resyncRecord(const aId: integer);
var
  qry: TSQLDataSet;
begin
  qry := DMPrincipal.getQuery;
  try
    if aId > 0 then
    begin
      qry.CommandText := 'UPDATE ' + Self.nomeTabela + ' Set SalvouRetaguarda = ''N'' ' + Self.CheckQryCommandTextForDuasVias(aId, Self);
      ExecQuery(qry);
    end;
  finally
    qry.Free;
  end;
end;

function TDataIntegradorModuloWeb.shouldContinue: boolean;
begin
  Result := true;
  if Self.FThreadControl <> nil then
    result := Self.FThreadControl.getShouldContinue;
end;

function TDataIntegradorModuloWeb.singleton: boolean;
begin
  result := (nomePKLocal = '') and (nomePKRemoto = '');
end;

function TDataIntegradorModuloWeb.jaExiste(const id: integer; Integrador: TDataIntegradorModuloWeb): boolean;
var
  qry: string;
begin
  qry := 'SELECT count(1) FROM ' + Integrador.nomeTabela + Self.CheckQryCommandTextForDuasVias(Id, Integrador);
  result := dmPrincipal.getSQLIntegerResult(qry) > 0;
end;

function TDataIntegradorModuloWeb.getUpdateStatement(node: IXMLDomNode; const id: integer): String;
begin
  Result := getUpdateBaseSQL(node, Self) + Self.CheckQryCommandTextForDuasVias(Id, Self);
end;

function TDataIntegradorModuloWeb.getInsertStatement(node: IXMLDomNode): String;
begin
  Result := 'INSERT INTO ' + nomeTabela + getFieldList(node) + ' values ' + getFieldValues(node, Self);
end;

procedure TDataIntegradorModuloWeb.BeforeUpdateInsertRecord(const aTableName: String; node: IXMLDomNode; const id: integer; var handled: boolean);
begin
  handled := False;
end;

function TDataIntegradorModuloWeb.getTranslatedTable(const aServerName: string): TDataIntegradorModuloWeb;
var
  Detalhe : TDataIntegradorModuloWeb;
begin
  Result := nil;

  for Detalhe in tabelasDetalhe do
    if AnsiSameText(underscorize(aServerName), Detalhe.nomePlural) then
    begin
      Result := Detalhe;
      break;
    end;
end;

procedure TDataIntegradorModuloWeb.SetQueryParameters(qry: TSQLDataSet; DMLOperation: TDMLOperation; node: IXMLDomNode;  ChildrenNodes: TXMLNodeDictionary;  Integrador: TDataIntegradorModuloWeb);
var
  i: integer;
  ValorCampo: UTF8String;
  Field: TFieldDictionary;
  lFormatSettings: TFormatSettings;
begin
  //Preenche os Parametros
  for i := 0 to node.childNodes.length - 1 do
  begin
    if (node.childNodes[i].attributes.getNamedItem('type') <> nil) and (node.childNodes[i].attributes.getNamedItem('type').text = 'array') then
    begin
      if (ChildrenNodes <> nil) and (not ChildrenNodes.ContainsKey(node.childNodes[i].nodeName)) then
        ChildrenNodes.Add(node.childNodes[i].nodeName, node.childNodes[i]);
    end;

    name := LowerCase(translateFieldNameServerToPdv(node.childNodes.item[i], Integrador));
    ValorCampo := translateFieldValue(node.childNodes.item[i], Integrador);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(DMLOperation, name, Integrador) then
      begin
        if ValorCampo = 'NULL' then
        begin
          qry.ParamByName(name).Value := unassigned;
          qry.ParamByName(name).DataType := ftString;
        end
        else
        begin
          Field := nil;
          if Integrador.FFieldList <> nil then
            Field := Integrador.FFieldList.Items[Lowercase(name)];
          if Field <> nil then
          begin
            case Field.DataType of
              ftString, ftMemo: qry.ParamByName(name).AsString := ValorCampo;
              ftInteger: qry.ParamByName(name).AsInteger := StrToInt(ValorCampo);
              ftLargeint: qry.ParamByName(name).AsLargeInt := StrToInt(ValorCampo);
              ftDateTime, ftTimeStamp:
                begin
                  ValorCampo := StringReplace(ValorCampo, '''','', [rfReplaceAll]);
                  ValorCampo := Trim(StringReplace(ValorCampo, '.','/', [rfReplaceAll]));
                  lFormatSettings.DateSeparator := '/';
                  lFormatSettings.TimeSeparator := ':';
                  lFormatSettings.ShortDateFormat := 'dd/MM/yyyy hh:mm:ss';
                  qry.ParamByName(name).AsDateTime := StrToDateTime(ValorCampo, lFormatSettings);
                end;
              ftCurrency, ftTime:
                begin
                  ValorCampo := StringReplace(ValorCampo, '''','', [rfReplaceAll]);
                  qry.ParamByName(name).AsCurrency := StrToCurr(ValorCampo);
                end;
              ftSingle, ftFloat, ftFMTBcd:
              begin
                ValorCampo := StringReplace(ValorCampo, '.', ',',[rfReplaceAll]);
                ValorCampo := StringReplace(ValorCampo, '''','', [rfReplaceAll]);
                qry.ParamByName(name).AsFloat := StrToFloat(ValorCampo);
              end;
              ftBlob:
              begin
                qry.ParamByName(name).LoadFromStream(self.BinaryFromBase64(ValorCampo), ftBlob);
              end
            else
              qry.ParamByName(name).AsString := ValorCampo;
            end;
          end;
        end;
      end;
  end;
end;

procedure TDataIntegradorModuloWeb.ExecQuery(aQry: TSQLDataSet);
var
  i: integer;
  Paramlog, vLog: string;
begin
  try
    aQry.ExecSQL;
  except
    on E:Exception do
    begin
      ParamLog := EmptyStr;
      for i := 0 to aQry.Params.Count - 1 do
        ParamLog := ParamLog + aQry.Params[i].Name + ' = "' + aQry.Params[i].AsString + '"' + #13#10;
      vLog := 'Erro ExecSQL: ' + #13#10 + aQry.CommandText + #13#10 + ParamLog + #13#10 + e.Message;
      Raise EIntegradorException.Create(vLog);
    end;
  end;
end;

function TDataIntegradorModuloWeb.CheckQryCommandTextForDuasVias(const aId: integer; Integrador: TDataIntegradorModuloWeb): string;
begin
  Result := EmptyStr;
  if DuasVias then
    Result := ' WHERE idRemoto = ' + IntToStr(aId)
  else
    Result := ' WHERE ' + Integrador.nomePKLocal + ' = ' + IntToStr(aId);
end;

procedure TDataIntegradorModuloWeb.ExecInsertRecord(node: IXMLDomNode; const id: integer; Integrador: TDataIntegradorModuloWeb);
var
  i: integer;
  name: string;
  qry: TSQLDataSet;
  FieldsListUpdate, FieldsListInsert : string;
  Existe: Boolean;
  NewId: integer;
  ChildrenNodes: TXMLNodeDictionary;
  ChildNode: TPair<string, IXMLDomNode>;
  ChildId: integer;
  handled : boolean;
  list : IXMLDomNodeList;
  numRegistros: integer;
  nodeItem : IXMLDomNode;
  Detail: TDataIntegradorModuloWeb;
  DMLOperation: TDMLOperation;
  StrCheckInsert: TStringList;

begin
  Existe := jaExiste(id, Integrador);
  qry := dmPrincipal.getQuery;
  ChildrenNodes := TXMLNodeDictionary.Create;
  try
    if Existe then
    begin
      DMLOperation := dmUpdate;
      FieldsListUpdate := Self.getFieldUpdateList(node, Integrador);
      if not StrUtils.ContainsText(FieldsListUpdate, 'SALVOURETAGUARDA') then
        FieldsListUpdate := 'SALVOURETAGUARDA = ' + QuotedStr(Self.GetDefaultValueForSalvouRetaguarda)+ ','+ FieldsListUpdate;

      qry.CommandText := 'UPDATE ' + Integrador.nomeTabela + ' SET ' + FieldsListUpdate;
      qry.CommandText := qry.CommandText + CheckQryCommandTextForDuasVias(Id, Integrador) + ' and SALVOURETAGUARDA = ''S''';
    end
    else
    begin
      DMLOperation := dmInsert;
      FieldsListInsert := self.getFieldInsertList(node, Integrador);
      if not StrUtils.ContainsText(FieldsListInsert, 'SALVOURETAGUARDA') then
        FieldsListInsert := ':SALVOURETAGUARDA,' + FieldsListInsert;

      NewId := Integrador.getNewId(Node);
      if NewId > 0 then
      begin
        StrCheckInsert := TStringList.Create;
        try
          StrCheckInsert.Delimiter := ':';
          StrCheckInsert.DelimitedText := FieldsListInsert;
          if StrCheckInsert.IndexOf(Integrador.nomePKLocal+',') < 0 then
            FieldsListInsert := ':'+ Integrador.nomePKLocal + ',' + FieldsListInsert;
        finally
          StrCheckInsert.Free;
        end;
      end;
      qry.CommandText := 'INSERT INTO ' + Integrador.nomeTabela + '(' + StringReplace(FieldsListInsert, ':', '', [rfReplaceAll]) + ') values (' + FieldsListInsert + ')';
      if qry.Params.ParamByName(Integrador.nomePkLocal) <> nil then
        qry.ParamByName(Integrador.nomePkLocal).AsInteger := NewId;
      if qry.Params.ParamByName('SALVOURETAGUARDA') <> nil then
        qry.ParamByName('SALVOURETAGUARDA').asString := Self.GetDefaultValueForSalvouRetaguarda;

    end;

    Self.SetQueryParameters(qry, DMLOperation, node, ChildrenNodes, Integrador);

    Self.ExecQuery(qry);

    for ChildNode in ChildrenNodes do
    begin
      Detail := Self.getTranslatedTable(ChildNode.Key);
      if Detail <> nil then
      begin
        list := ChildNode.Value.selectNodes(dasherize(Detail.fnomeSingular));
        if list <> nil then
        begin
          numRegistros := list.length;
          for i := 0 to numRegistros-1 do
          begin
            nodeItem := list.item[i];
            ChildId := 0;
            if nodeItem.selectSingleNode('./id') <> nil then
              ChildId := StrToIntDef(nodeItem.selectSingleNode('./id').text, 0);
            handled := False;
            Self.BeforeUpdateInsertRecord(Detail.FnomeTabela, nodeItem, ChildId, handled);
            if not handled then
              Self.ExecInsertRecord(nodeItem, ChildId, Detail);
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(qry);
    FreeAndNil(ChildrenNodes);
  end;
end;

procedure TDataIntegradorModuloWeb.updateInsertRecord(node: IXMLDomNode; const id: integer);
var
  handled : boolean;
begin
  handled := False;
  Self.BeforeUpdateInsertRecord(Self.FNomeTabela, node, id, handled);
  if not handled then
    Self.ExecInsertRecord(node, id, Self);
end;

procedure TDataIntegradorModuloWeb.UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : TTabelaDetalheList);
var
  j : integer;
  vNode: IXMLDomNode;
  vNodeList: IXMLDOMNodeList;
  vIdRemoto, vPkLocal : String;
  vNomePlural, vNomeSingular: string;
  Detalhe: TTabelaDetalhe;
begin
  try
    for Detalhe in pTabelasDetalhe do
    begin
      vNomePlural := Detalhe.nomePlural;
      vNomeSingular := Detalhe.nomeSingular;

      if VNomePlural = EmptyStr then
      begin
        onDetailNamesMalformed(Detalhe.nomeTabela, 'NomePlural');
        exit;
      end;

      if vNomeSingular = EmptyStr then
      begin
        onDetailNamesMalformed(Detalhe.nomeTabela, 'NomeSingular');
        exit;
      end;

      vNode := pNode.selectSingleNode('./' + dasherize(vNomePlural));
      vNodeList := vNode.selectNodes('./' + dasherize(vNomeSingular));

      for j := 0 to vNodeList.length - 1 do
      begin
        vIdRemoto := vNodeList[j].selectSingleNode('./id').text;
        vPkLocal := vNodeList[j].selectSingleNode('./original-id').text;

        if duasVias then
          dmPrincipal.execSQL('UPDATE ' + Detalhe.nomeTabela + ' SET salvouRetaguarda = '
                          + QuotedStr(Self.GetDefaultValueForSalvouRetaguarda) + ', idRemoto = ' + vIdRemoto +
                          ' WHERE salvouRetaguarda = ''N'' and ' + Detalhe.nomePKLocal + ' = ' + vPkLocal) ;
      end;
      if (Detalhe.tabelasDetalhe.Count > 0) and (vNode <> nil) then
        Self.UpdateRecordDetalhe(vNode, Detalhe.tabelasDetalhe);
    end;
  except
    raise;
  end;
end;

procedure TDataIntegradorModuloWeb.updateSingletonRecord(node: IXMLDOMNode);
begin
  if dmPrincipal.getSQLIntegerResult('SELECT count(1) from ' + nomeTabela) < 1 then
    dmPrincipal.execSQL('Insert into ' + nomeTabela + ' DEFAULT VALUES');
  dmPrincipal.execSQL(getUpdateBaseSQL(node, Self));
end;

function TDataIntegradorModuloWeb.getUpdateBaseSQL(node: IXMLDOMNode; Integrador: TDataIntegradorModuloWeb): string;
begin
  result := 'UPDATE ' + Integrador.nomeTabela + getFieldUpdateList(node, Integrador);
end;

function TDataIntegradorModuloWeb.getFieldList(node: IXMLDomNode): string;
var
  i: integer;
  name: string;
begin
  result := '(';
  if duasVias and ((nomeGenerator <> '') or (usePKLocalMethod)) then
    result := result + nomePKLocal + ', ';
  if duasVias then
    result := result + 'salvouRetaguarda, ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i], Self);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmInsert, name, Self) then
        result := result + name + ', ';
  end;
  result := copy(result, 0, length(result)-2);
  result := result + getFieldAdditionalList(node);
  result := result + ')';
end;

function TDataIntegradorModuloWeb.getFieldValues(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
var
  i: integer;
  name: string;
begin
  result := '(';
  if duasVias and ((nomeGenerator <> '') or (usePKLocalMethod)) then
  begin
    if nomeGenerator <> '' then
      result := result + 'gen_id(' + nomeGenerator + ',1), '
    else
      Result := Result + IntToStr(getNewId(Node)) + ', ';
  end;
  if duasVias then
    result := result + QuotedStr('S') + ', ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i], Integrador);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmInsert, name, Integrador) then
        result := result + translateFieldValue(node.childNodes.item[i], Integrador) + ', ';
  end;
  result := copy(result, 0, length(result)-2);
  result := result + getFieldAdditionalValues(node);
  result := result + ')';
end;

function TDataIntegradorModuloWeb.getFieldUpdateList(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
var
  i: integer;
  name: string;
begin
  result := '';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i], Integrador);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmUpdate, name, Integrador) then
        result := result + ' ' + name + ' = :' + name + ',';
  end;
  //Remove a ultima virgula
  result := copy(result, 1, Length(result)-1);
  result := result + getFieldAdditionalUpdateList(node);
end;

function TDataIntegradorModuloWeb.getFieldInsertList(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
var
  i: integer;
  name: string;
  StrInsert: TStringList;
begin
  Result := '';
  StrInsert := TStringList.Create;
  try
    for i := 0 to node.childNodes.length - 1 do
    begin
      name := LowerCase(translateFieldNameServerToPdv(node.childNodes.item[i], Integrador));
      if name <> '*' then
        if Self.getIncludeFieldNameOnList(dmInsert, name, Integrador) and (StrInsert.IndexOf(name) < 0) then
        begin
          StrInsert.Add(name);
          Result := Result + ' :' + name + ',';
        end;
    end;
    Result := copy(Result, 1, Length(Result)-1);
  finally
    StrInsert.Free;
  end;
end;

function TDataIntegradorModuloWeb.getFieldDictionaryList: TFieldDictionaryList;
begin
  Result := FFieldList;
end;

function TDataIntegradorModuloWeb.getIncludeFieldNameOnList(const aDMLOperation: TDMLOperation; const aFieldName: string; Integrador: TDataIntegradorModuloWeb): boolean;
begin
  Result := True;
end;

function TDataIntegradorModuloWeb.getRequestUrlForAction(toSave: boolean; versao: integer = -1): string;
var
  nomeRecurso: string;
begin
  if toSave then
  begin
    nomeRecurso := nomeActionSave;
    Result := buildRequestURL(nomeRecurso, '', haPost);
  end
  else
  begin
    nomeRecurso := nomeActionGet;
    Result := buildRequestURL(nomeRecurso);
  end;

  if versao > -1 then
    result := result + '&version=' + IntToStr(versao);
end;

function TDataIntegradorModuloWeb.ultimaVersao: integer;
begin
  result := dmPrincipal.getSQLIntegerResult('Select max('+self.getVersionFieldName+') from ' + nomeTabela);
end;

function TDataIntegradorModuloWeb.getVersionFieldName: string;
begin
  Result := 'versao';
end;

function TDataIntegradorModuloWeb.translateFieldValue(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
var
  typedTranslate: string;
begin
  if (node.attributes.getNamedItem('nil') <> nil) and (node.attributes.getNamedItem('nil').text = 'true') then
    result := 'NULL'
  else if (node.attributes.getNamedItem('type') <> nil) then
  begin
    typedTranslate := translateTypeValue(node.attributes.getNamedItem('type').text, node.text);
    result := translateValueFromServer(node.nodeName, typedTranslate, Integrador);
  end
  else
    result := translateValueFromServer(node.nodeName, node.text, Integrador);
end;

function TDataIntegradorModuloWeb.translateTypeValue(fieldType, fieldValue: string): string;
begin
  result := QuotedStr(fieldValue);
  if (fieldType = 'integer') or (fieldType = 'float') then
    result := fieldValue
  else if fieldType = 'boolean' then
  begin
    if fieldValue = 'true' then
      result := '1'
    else
      result := '0';
  end;
end;

function TDataIntegradorModuloWeb.translateFieldNameServerToPdv(node: IXMLDomNode; Integrador: TDataIntegradorModuloWeb): string;
begin
  result := Integrador.translations.translateServerToPDV(node.nodeName, duasVias);
  if result = '' then
    result := StringReplace(node.nodeName, '-', '', [rfReplaceAll]);
end;

function TDataIntegradorModuloWeb.translateFieldNamePdvToServer(
  node: IXMLDomNode): string;
begin
  result := translations.translatepdvToServer(node.nodeName);
  if result = '' then
    result := StringReplace(node.nodeName, '-', '', [rfReplaceAll]);
end;


function TDataIntegradorModuloWeb.addTranslatedParams(ds: TDataSet; params: TStringList;
  translations: TTranslationSet; nestedAttribute: string = ''): IXMLDomDocument2;
var
  i: integer;
  nestingText, nomeCampo, nome, valor: string;
begin
  nestingText := '';
  if nestedAttribute <> '' then
    nestingText := '[' + nestedAttribute + '][]';
  for i := 0 to translations.size-1 do
  begin
    nomeCampo := translations.get(i).pdv;
    if ds.FindField(nomeCampo) <> nil then
    begin
      nome := nomeSingularSave + nestingText + '[' + translations.get(i).server + ']';
      valor :=
        translateValueToServer(translations.get(i), translations.get(i).pdv,
          ds.fieldByName(translations.get(i).pdv), nestedAttribute, translations.get(i).fkName);
      //params.Add(nome + '=' + TIdURI.ParamsEncode(valor));
      params.Add(nome + '=' + valor);
    end;
  end;
end;

procedure TDataIntegradorModuloWeb.afterDadosAtualizados;
begin
  //
end;

function TDataIntegradorModuloWeb.getTabelasDetalhe: TTabelaDetalheList;
begin
  Result := Self.tabelasDetalhe;
end;

function TDataIntegradorModuloWeb.getTimeoutValue: integer;
begin
  Result := 30000;
end;

procedure TDataIntegradorModuloWeb.OnWorkHandler(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if (Self.FthreadControl <> nil) and (not Self.FthreadControl.getShouldContinue) then
    Abort;
end;

procedure TDataIntegradorModuloWeb.addDetailsToJsonList(aDetailList: TDetailList; aDs: TDataSet);
var
  Detalhe: TTabelaDetalhe;
begin
  for Detalhe in  Self.tabelasDetalhe do
    Self.SelectDetails(aDetailList, aDs.fieldByName(nomePKLocal).AsInteger, Detalhe);
end;

procedure TDataIntegradorModuloWeb.SelectDetailsIterate(aDetailList: TDetailList; aValorPK: integer);
var
  Detalhe: TTabelaDetalhe;
begin
  for  Detalhe in Self.tabelasDetalhe do
  begin
    SelectDetails(aDetailList, aValorPK, Detalhe);
  end;
end;

procedure TDataIntegradorModuloWeb.SelectDetails(aDetailList: TDetailList; aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe);
var
  jsonArrayDetails: TJSONArrayContainer;
begin
  RunDataSet(aValorPk, aTabelaDetalhe,
             procedure (aDataSet: TDataSet)
             var
               nomeParametro: string;
               Dict: TStringDictionary;
             begin
               if (aDetailList <> nil) and (not aDetailList.ContainsKey(aTabelaDetalhe.nomeParametro)) then
               begin
                 jsonArrayDetails := TJSONArrayContainer.Create;
                 jsonArrayDetails.nomePluralDetalhe := aTabelaDetalhe.nomePlural;
                 jsonArrayDetails.nomeSingularDetalhe := aTabelaDetalhe.FnomeSingular;
                 jsonArrayDetails.nomeTabela := aTabelaDetalhe.nomeTabela;
                 jsonArrayDetails.nomePkLocal := aTabelaDetalhe.nomePKLocal;
                 nomeParametro := aTabelaDetalhe.nomeParametro;
                 aDetailList.Add(nomeParametro, jsonArrayDetails);
               end
               else
                 jsonArrayDetails := aDetailList.Items[aTabelaDetalhe.nomeParametro];
               Dict := Self.DataSetToArray(aDataSet);
               try
                 jsonArrayDetails.getJsonArray.AddElement(Self.getJsonObject(aDataSet, aTabelaDetalhe.translations, Dict, aTabelaDetalhe.nomeParametro));
                 aTabelaDetalhe.SelectDetailsIterate(aDetailList, aDataSet.fieldByName(aTabelaDetalhe.nomePKLocal).AsInteger);
               finally
                 Dict.Free;
               end;
            end);
end;


procedure TDataIntegradorModuloWeb.RunDataSet(const aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe; aProc: TAnonymousMethod);
var
  qry: TSQLDataSet;
begin
  qry := dmPrincipal.getQuery;
  try
    try
      qry.commandText := 'SELECT * FROM ' + aTabelaDetalhe.nomeTabela + ' where ' + aTabelaDetalhe.nomeFK +
        ' = ' + IntToStr(aValorPK) + self.getAdditionalDetailFilter;
      qry.Open;
      while not qry.Eof do
      begin
        aProc(qry);
        qry.Next;
      end;
    except
       on E: Exception do
       begin
         Self.FDataLog.log('Erro no SQL:' + #13#10 + qry.CommandText);
         raise;
       end;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

function TDataIntegradorModuloWeb.JsonObjectHasPair(const aName: string; aJson: TJSONObject): boolean;
var
  jsonPair: TJSONPair;
begin
  Result := False;
  for jsonPair in aJson do
  begin
    if JsonPair.JsonString.Value = aName then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TDataIntegradorModuloWeb.DataSetToArray(aDs: TDataSet) : TStringDictionary;
var
  i: Integer;
  nome: String;
  value: String;
  fieldValue: string;
  BlobStream: TStringStream;
  FieldStream: TStream;
  Input: TMemoryStream;
begin
  Result := TStringDictionary.Create;
  for I := 0 to aDs.FieldCount - 1 do
  begin
    nome := aDs.Fields[i].FieldName;
    if UpperCase(nome) = 'VERSION_ID' then
      continue;
    try
      if VarIsNull(aDs.Fields[i].AsVariant) then
        fieldValue := ''
      else
        fieldValue := aDs.Fields[i].AsVariant;
    except
      fieldValue := aDs.Fields[i].AsString;
    end;

    if (aDs.Fields[i].IsNull) or (fieldValue = '') then
      value := ''
    else if aDS.Fields[i].DataType = ftDate then
      Value := aDS.Fields[i].AsString
    else if aDs.Fields[i].DataType = ftBlob then
    begin
      try
        FieldStream := aDs.CreateBlobStream(aDs.Fields[i], bmRead);
        Input := TBytesStream.Create;
        try
          Input.LoadFromStream(FieldStream);
          Input.Position := 0;
          Value := TIdEncoderMIME.EncodeStream(Input);
        finally
          FreeAndNil(Input);
        end;
      Finally
        FreeAndNil(FieldStream);
      end;
    end
    else
      value := fieldValue;
    Result.Add(nome, value)
  end;
end;

function TDataIntegradorModuloWeb.getJsonObject(aDs: TDataSet;
 aTranslations: TTranslationSet; aDict: TStringDictionary; aNestedAttribute: string = ''): TJsonObject;
var
  i: integer;
  nomeCampo, nome, valor, fieldValue: string;
  StringUTF8: UTF8String;
begin
  Result := TJsonObject.Create;
  Result.Owned := False; // Manter como False
  for i := 0 to aTranslations.size-1 do
  begin
    nomeCampo := aTranslations.get(i).pdv;
    if aDict.ContainsKey(UpperCase(nomeCampo)) then
    begin
      nome := aTranslations.get(i).server;
      fieldValue := aDict.Items[UpperCase(aTranslations.get(i).pdv)];

      valor :=  translateValueToServer(aTranslations.get(i), aTranslations.get(i).pdv,
          aDs.fieldByName(aTranslations.get(i).pdv), aNestedAttribute, aTranslations.get(i).fkName, fieldValue);
      if not JsonObjectHasPair(nome, Result) then
      begin
        StringUTF8 := Trim(valor);
        if Self.encodeJsonValues then
        begin
          if aDs.fieldByName(aTranslations.get(i).pdv).DataType = ftblob then
            Result.AddPair(nome, valor)
          else
            Result.AddPair(nome, TIdEncoderMIME.EncodeString(StringUTF8, IndyTextEncoding_UTF8))
        end
        else
          Result.AddPair(nome, valor);
      end;
    end;
  end;
end;

function TDataIntegradorModuloWeb.getNewId(node: IXMLDomNode): Integer;
begin
  Result := 0;
end;

function TDataIntegradorModuloWeb.GetNomePKLocal: string;
begin
  Result := Self.FnomePKLocal
end;

function TDataIntegradorModuloWeb.GetNomePlural: string;
begin
  Result := Self.FNomePlural
end;

function TDataIntegradorModuloWeb.getNomeSingular: string;
begin
  Result := Self.FnomeSingular
end;

function TDataIntegradorModuloWeb.getNomeTabela: string;
begin
  Result := Self.FNomeTabela
end;

procedure TDataIntegradorModuloWeb.addMasterTableToJson(aDetailList: TDetailList; aDs: TDataSet; apStream: TStringStream);
var
  JMaster, JResponse: TJsonObject;
  Item: TPair<string, TJSONArrayContainer>;
  Dict: TStringDictionary;
begin
  Dict := Self.DataSetToArray(aDs);
  try
    JMaster := Self.getJsonObject(aDs, Self.translations, Dict);
    JResponse := TJSONObject.Create;
    try
      jResponse.AddPair(Self.nomeSingularSave, JMaster);
      for Item in aDetailList do
      begin
        JMaster.Owned := True;
        JMaster.AddPair(item.Key, Item.Value.getJsonArray);
        Item.Value.FJSonArray.Free;
        Item.Value.Free;
      end;
      apStream.WriteString(JResponse.ToString);

    finally
      JResponse.Free;
      JMaster.Free;
    end;
  finally
    Dict.Clear;
    Dict.Free;
  end;
end;

function TDataIntegradorModuloWeb.post(ds: TDataSet; http: TidHTTP; url: string): string;
var
  params: TStringList;
  pStream: TStringStream;
  DetailList: TDetailList;
begin
  result := '';
  if paramsType = ptParam then
  begin
    params := TStringList.Create;
    try
      addTranslatedParams(ds, params, translations);
      addDetails(ds, params);
      addMoreParams(ds, params);
      result := http.Post(url, Params);
    finally
      params.Free;
    end;
  end;
  if paramsType = ptJSON then
  begin
    pStream := TStringStream.Create('', TEncoding.UTF8);
    DetailList := TDetailList.Create;
    try
      Self.addDetailsToJsonList(DetailList, ds);
      Self.addMasterTableToJson(DetailList, ds, pStream);
      result := http.Post(url, pStream);
    finally
      pStream.Free;
      DetailList.Free;
    end;
  end;
end;

function TDataIntegradorModuloWeb.GetIdRemoto(aDoc: IXMLDomDocument2): integer;
begin
  Result := -1;
  try
    if aDoc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id') <> nil then
      Result := strToInt(aDoc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id').text)
    else if aDoc.selectSingleNode('//hash//id') <> nil then
      Result := strToInt(aDoc.selectSingleNode('//hash//id').text)
    else
      Result := StrToInt(aDoc.selectSingleNode('objects').selectSingleNode('object').selectSingleNode('id').text);
  except
    on e: Exception do
    begin
      Self.log('Erro ao ler Campo "ID" no XML de retorno, Tabela: ' + nomeTabela + ' - ' + e.Message, 'Sync');
    end;
  end;
end;

function TDataIntegradorModuloWeb.getXMLContentAsXMLDom(const aXMLContent: string): IXMLDomDocument2;
begin
  Result := nil;
  if aXMLContent <> EmptyStr then
  begin
    CoInitialize(nil);
    try
      Result := CoDOMDocument60.Create;
      Result.loadXML(aXmlContent);
    finally
      CoUninitialize;
    end;
  end;
end;

function TDataIntegradorModuloWeb.saveRecordToRemote(ds: TDataSet;
  var salvou: boolean; http: TidHTTP = nil): IXMLDomDocument2;
var
  multipartParams: TidMultipartFormDataStream;
  xmlContent: string;
  idRemoto: integer;
  txtUpdate: string;
  sucesso: boolean;
  stream: TStringStream;
  url: string;
  criouHttp: boolean;
  log: string;
begin
  Self.log('Iniciando save record para remote. Classe: ' + ClassName, 'Sync');
  salvou := false;
  criouHTTP := false;
  idRemoto := -1;
  if http = nil then
  begin
    criouHTTP := true;
    http := getHTTPInstance;
    http.OnWork := Self.OnWorkHandler;
    http.ConnectTimeout := Self.getTimeoutValue;
    http.ReadTimeout := Self.getTimeoutValue;
  end;

  try
    sucesso := false;
    while (not sucesso) do
    begin
      if (Self.FthreadControl <> nil) and (not Self.FthreadControl.getShouldContinue) then
        break;
      try
        if useMultipartParams then
        begin
          multiPartParams := TIdMultiPartFormDataStream.Create;
          stream := TStringStream.Create('',TEncoding.UTF8);
          try
            prepareMultipartParams(ds, multipartParams );
            http.Post(getRequestUrlForAction(true, -1), multipartParams, stream);
            xmlContent := stream.ToString;
          finally
            Stream.Free;
            MultipartParams.Free;
          end;
        end
        else
        begin
          url := getRequestUrlForAction(true, -1);
          xmlContent := post(ds, http, url);
        end;
        sucesso := true;
        Result := Self.getXMLContentAsXMLDom(xmlContent);
        if duasVias or clientToServer then
        begin
          txtUpdate := 'UPDATE ' + nomeTabela + ' SET salvouRetaguarda = ' + QuotedStr(Self.GetDefaultValueForSalvouRetaguarda);

          if duasVias then
          begin
            idRemoto := Self.GetIdRemoto(Result);
            if idRemoto > 0 then
              txtUpdate := txtUpdate + ', idRemoto = ' + IntToStr(idRemoto);
          end;

          txtUpdate := txtUpdate + ' WHERE ' + nomePKLocal + ' = ' + ds.fieldByName(nomePKLocal).AsString;
          txtUpdate := txtUpdate+ ' AND COALESCE(SALVOURETAGUARDA, ''N'') = ''N'' ';

          //da a chance da classe gerenciar redirecionamentos, por exemplo ao descobrir que este registro já
          //existia no remoto e era outro registro neste banco de dados.
          if not gerenciaRedirecionamentos(ds.fieldByName(nomePKLocal).AsInteger, idRemoto) then
          begin
            dmPrincipal.startTransaction;
            try
              dmPrincipal.execSQL(txtUpdate);
              dmPrincipal.commit;
            except
              on E: Exception do
              begin
                dmPrincipal.rollback;
                raise;
              end;
            end;
          end;

          if (TabelasDetalhe.Count > 0) and (Result.selectSingleNode(dasherize(nomeSingularSave)) <> nil) then
             Self.UpdateRecordDetalhe(Result.selectSingleNode(dasherize(nomeSingularSave)), TabelasDetalhe);

        end;
      except
        on e: EIdHTTPProtocolException do
        begin
          if e.ErrorCode = 422 then
            log := Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d, Erro: %s.',[ClassName, e.ErrorCode, Self.GetErrorMessage(e.ErrorMessage)])
          else if e.ErrorCode = 500 then
            log := Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d. Erro: Erro interno no servidor: %s. ',[ClassName, e.ErrorCode, e.ErrorMessage])
          else
            log :=  Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d. Erro: %s.',[ClassName, e.ErrorCode, e.ErrorMessage]);

          Self.log(log, 'Sync');
          raise EIntegradorException.Create(log) ; //Logou, agorra manda pra cima
        end;
        on E: Exception do
        begin
          log := 'Erro ao tentar salvar registro. Classe: ' + ClassName + '. Erro: ' + e.Message;
          Self.log(log, 'Sync');
          raise EIntegradorException.Create(log) ;
        end;
      end;
    end;
    salvou := sucesso;
  finally
    if criouHttp then
      FreeAndNil(http);
  end;
end;

procedure TDataIntegradorModuloWeb.Log(const aLog: string; aClasse: string = '');
begin
  if (FDataLog <> nil) then
    FDataLog.log(aLog, aClasse);
end;

procedure TDataIntegradorModuloWeb.SetDataLog(const Value: ILog);
begin
  FDataLog := Value;
end;

function TDataIntegradorModuloWeb.GetErrorMessage(const aXML: string): string;
var
  node: IXMLNode;
  list: IXMLNodeList;
  XML: IXMLDocument;
begin
  Result := EmptyStr;
  if Trim(aXML) <> EmptyStr then
  begin
    CoInitialize(nil);
    XML := TXMLDocument.Create(Self);
    try
      XML.LoadFromXML(aXML);
      list := XML.ChildNodes;
      if list.FindNode('errors') <> nil then
      begin
        list := list.FindNode('errors').ChildNodes;
        if list <> nil  then
        begin
          node := list.FindNode('error');
          if node <> nil then
            Result := UTF8ToString(HTTPDecode(node.Text));
        end;
      end;
    finally
      CoUninitialize;
    end;
  end;
end;


procedure TDataIntegradorModuloWeb.addDetails(ds: TDataSet; params: TStringList);
var
  Detalhe : TTabelaDetalhe;
begin
  for Detalhe in tabelasDetalhe do
    addTabelaDetalheParams(ds.fieldByName(nomePKLocal).AsInteger, params, Detalhe);
end;

procedure TDataIntegradorModuloWeb.addTabelaDetalheParamsIterate(valorPK: integer;
  params: TStringList);
var
  Detalhe: TTabelaDetalhe;
begin
  for Detalhe in Self.tabelasDetalhe do
    addTabelaDetalheParams(valorPK, params, Detalhe);
end;

procedure TDataIntegradorModuloWeb.addTabelaDetalheParams(valorPK: integer;
  params: TStringList;
  tabelaDetalhe: TTabelaDetalhe);
begin
  RunDataSet(valorPk,
             TabelaDetalhe,
             procedure (aDataSet: TDataSet)
             begin
               addTranslatedParams(aDataSet, params, tabelaDetalhe.translations, tabelaDetalhe.nomeParametro);
               tabelaDetalhe.addTabelaDetalheParamsIterate(aDataSet.fieldByName(tabelaDetalhe.nomePKLocal).AsInteger, params);

            end);
end;

function TDataIntegradorModuloWeb.getAdditionalDetailFilter: String;
begin
  Result := EmptyStr;
end;

procedure TDataIntegradorModuloWeb.migrateSingletonTableToRemote;
var
  qry: TSQLDataSet;
  salvou: boolean;
begin
  qry := dmPrincipal.getQuery;
  try
    qry.CommandText := 'SELECT * FROM ' + nomeTabela;
    qry.Open;
    saveRecordToRemote(qry, salvou);
  finally
    FreeAndNil(qry);
  end;
end;


procedure TDataIntegradorModuloWeb.postRecordsToRemote(http: TidHTTP = nil);
var
  qry: TSQLDataSet;
  salvou: boolean;
  n, total: integer;
  criouHTTP: boolean;
begin
  criouHTTP := false;
  qry := dmPrincipal.getQuery;
  try
    try
      Self.log('Selecionando registros para sincronização. Classe: ' + ClassName, 'Sync');
      qry.commandText := 'SELECT * from ' + nomeTabela + ' where ((salvouRetaguarda = ' + QuotedStr('N') + ') or (salvouRetaguarda is null)) '
        + getAdditionalSaveConditions;
      {$IFDEF HibridoClientDLL}
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14 OR 4); //FOREGROUND_RED);
      writeln(qry.CommandText);
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
      {$ENDIF}
      qry.Open;
      total := qry.RecordCount;
      n := 1;
      if http = nil then
      begin
        criouHTTP := true;
        http := TIdHTTP.Create(nil);
        http.ProtocolVersion := pv1_1;
        http.HTTPOptions := http.HTTPOptions + [hoKeepOrigProtocol];
        http.Request.Connection := 'keep-alive';
      end;
      qry.First;
      while not qry.Eof do
      begin
        if (not self.shouldContinue) then
          break;

        if notifier <> nil then
        begin
          notifier.setCustomMessage('Salvando ' + getHumanReadableName +
            ' ' + IntToStr(n) + '/' + IntToStr(total));
        end;
        try
          saveRecordToRemote(qry, salvou, http);
          if salvou then
            Self.Log(Format('Registro %d de %d', [n, total]));
          inc(n);
        except
          on e: Exception do
          begin
            Self.log('Erro no processamento do postRecordsToRemote. Classe: ' + ClassName +' | '+ e.Message, 'Sync');
            if stopOnPostRecordError then
              raise;
          end;
        end;
        qry.Next;
      end;
      if notifier <> nil then
        notifier.unflagSalvandoDadosServidor;
      if Total > 0 then
        Self.log(Format('Post de records para remote comitados. Classe: %s. Total de registros: %d.', [ClassName, total]), 'Sync');

    except
      Self.log('Erro no processamento do postRecordsToRemote. Classe: ' + ClassName, 'Sync');
      if stopOnPostRecordError then
        raise;
    end;
  finally
    FreeAndNil(qry);
    if criouHTTP and (http<>nil) then
      FreeAndNil(http);
  end;
end;

procedure TDataIntegradorModuloWeb.redirectRecord(idAntigo, idNovo: integer);
var
  Dependente: TTabelaDependente;
  nomeFK: string;
begin
  beforeRedirectRecord(idAntigo, idNovo);
  //Para cada tabela que referenciava esta devemos dar o update do id antigo para o novo
  for Dependente in tabelasDependentes do
  begin
    nomeFK := Dependente.nomeFK;
    if nomeFK = '' then
      nomeFK := nomePKLocal;
    dmPrincipal.execSQL('UPDATE ' + Dependente.nomeTabela +
    ' set ' + nomeFK + ' = ' + IntToStr(idNovo) +
    ' where ' + nomeFK + ' = ' + IntToStr(idAntigo));
    dmPrincipal.refreshData;    
  end;
  //E então apagar o registro original
  dmPrincipal.execSQL('DELETE FROM ' + nomeTabela + ' where ' +
    nomePKLocal + ' = ' + IntToStr(idAntigo));
end;

{ TTranslationSet }

procedure TTranslationSet.add(serverName, pdvName: string;
  lookupRemoteTable: string = ''; fkName: string = '');
var
  Translation: TNameTranslation;
begin
  Translation := TNameTranslation.Create;
  Translation.server := serverName;
  Translation.pdv := pdvName;
  Translation.lookupRemoteTable := lookupRemoteTable;
  Translation.fkName := fkName;
  Translations.Add(Translation);
end;

procedure TDataIntegradorModuloWeb.beforeRedirectRecord(idAntigo, idNovo: integer);
begin
  //
end;

constructor TTranslationSet.create(owner: TComponent);
begin
  Translations := TNameTranslationsList.Create(True);
end;

destructor TTranslationSet.Destroy;
var
  item: TObject;
begin
  for item in Translations do
    item.Free;
  Translations.Clear;
  Translations.Free;
  inherited;
end;

function TTranslationSet.get(index: integer): TNameTranslation;
begin
  result := translations[index];
end;

function TTranslationSet.size: integer;
begin
  Result := Translations.Count;
end;

function TTranslationSet.translatePDVToServer(pdvName: string): string;
var
  Translation: TNameTranslation;
begin
  Result := '';

  for Translation in Self.Translations do
    if translation.pdv = pdvName then
    begin
      Result := translation.server;
      Break;
    end;
end;

function TTranslationSet.translateServerToPDV(serverName: string; duasVias: boolean): string;
var
  Translation: TNameTranslation;
begin
  result := '';
  if duasVias and (upperCase(serverName) = 'ID') then
    result := 'idRemoto'
  else
    for Translation in Self.Translations do
      if translation.server = underscorize(serverName)  then
      begin
        Result := translation.pdv;
        Break;
      end;
end;

constructor TDataIntegradorModuloWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  verbose := false;
  duasVias := false;
  clientToServer := false;
  translations := TTranslationSet.create(self);
  nomePKLocal := 'id';
  nomePKRemoto := 'id';
  nomeGenerator := '';
  usePKLocalMethod := false;
  useMultipartParams := false;
  paramsType := ptParam;
  FstopOnPostRecordError := true;
  FStopOnGetRecordError := False;
  Self.encodeJsonValues := False;
  translations.add('id', 'idremoto');
  tabelasDetalhe := TTabelaDetalheList.Create(True);
  tabelasDependentes := TTabelaDependenteList.Create(True);
end;


destructor TDataIntegradorModuloWeb.Destroy;
var
  Detalhe: TTabelaDetalhe;
  Dependente: TTabelaDependente;
begin
  for Detalhe in Self.tabelasDetalhe do
     Detalhe.Free;
  for Dependente in Self.tabelasDependentes do
     Dependente.Free;
  if Self.FFieldList <> nil then
    FreeAndNil(Self.FFieldList);
  inherited;
end;

function TDataIntegradorModuloWeb.translateValueToServer(translation: TNameTranslation;
  fieldName: string; field: TField; nestedAttribute: string = ''; fkName: string = ''; fieldValue: String = ''): string;
var
  lookupIdRemoto: integer;
  fk: string;
  ValorCampo: string;
begin
  if fieldValue <> '' then
    ValorCampo := fieldValue
  else
    ValorCampo := field.AsString;
  Result := ValorCampo;
  if translation.lookupRemoteTable <> '' then
  begin
    result := '';
    if (field.asInteger >= 0) and not(field.IsNull) then
    begin
      if fkName = '' then
        fk := translation.pdv
      else
        fk := fkName;
      lookupIdRemoto := dmPrincipal.getSQLIntegerResult('SELECT idRemoto FROM ' +
        translation.lookupRemoteTable +
        ' WHERE ' + fk + ' = ' + ValorCampo);
      if lookupIdRemoto > 0 then
        result := IntToStr(lookupIdRemoto)
    end;
  end
  else
  begin
    if field.DataType in [ftFloat, ftBCD, ftFMTBCD, ftCurrency] then
    begin
      result := StringReplace(ValorCampo, ',','.', [rfReplaceAll]);
    end
    else if field.DataType in [ftDateTime, ftTimeStamp] then
    begin
      if field.IsNull then
        result := 'NULL'
      else
        //result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', field.AsDateTime);
        result := FormatDateTime(Self.getDateFormat , StrToDateTime(ValorCampo));
    end
    else if field.DataType in [ftDate] then
    begin
      if field.IsNull then
        result := 'NULL'
      else
        result := FormatDateTime('yyyy-mm-dd', StrToDate(ValorCampo));
    end
    else if field.DataType = ftBlob then
    begin
      result := ValorCampo;
    end;
  end;
end;

function TDataIntegradorModuloWeb.getDateFormat: String;
begin
  Result := 'dd"/"mm"/"yyyy"T"hh":"nn":"ss'
end;

function TDataIntegradorModuloWeb.translateValueFromServer(fieldName, value: string; Integrador: TDataIntegradorModuloWeb): string;
begin
  result := value;
end;

function TDataIntegradorModuloWeb.getFieldAdditionalList(
  node: IXMLDomNode): string;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.getFieldAdditionalUpdateList(
  node: IXMLDomNode): string;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.getFieldAdditionalValues(
  node: IXMLDomNode): string;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.nomeActionGet: string;
begin
  result := nomePlural;
end;

function TDataIntegradorModuloWeb.nomeActionSave: string;
begin
  result := nomePlural;
end;

function TDataIntegradorModuloWeb.nomeSingularGet: string;
begin
  result := FnomeSingular;
end;

function TDataIntegradorModuloWeb.nomeSingularSave: string;
begin
  result := FnomeSingular;
end;

procedure TDataIntegradorModuloWeb.onDetailNamesMalformed(configName, tableName: string);
begin
  self.Log(Format('Tabela detalhe: %s da Classe: %s não possui configuração de %s',[tableName, Self.ClassName, configName]));
end;

function TDataIntegradorModuloWeb.getOrderBy: string;
begin
  result := nomePKLocal;
end;

procedure TDataIntegradorModuloWeb.addMoreParams(ds: TDataSet;
  params: TStringList);
begin
  //nothing to add here
end;

procedure TDataIntegradorModuloWeb.SetdmPrincipal(
  const Value: IDataPrincipal);
var
  Detalhe: TTabelaDetalhe;
begin
  FdmPrincipal := Value;
  if Value <> nil then
  begin
    if Self.FFieldList = nil then
      Self.FFieldList := TFieldDictionaryList.Create(Self.nomeTabela, Value);
    for Detalhe in Self.tabelasDetalhe do
      TTabelaDetalhe(Detalhe).DmPrincipal := Value;
  end;
end;

procedure TDataIntegradorModuloWeb.setNomePKLocal(const Value: string);
begin
  Self.FnomePKLocal := Value;
end;

procedure TDataIntegradorModuloWeb.setNomePlural(const Value: string);
begin
  Self.FNomePlural := Value;
end;

procedure TDataIntegradorModuloWeb.SetNomeSingular(const Value: string);
begin
  Self.FNomeSingular := Value;
end;

procedure TDataIntegradorModuloWeb.setNomeTabela(const Value: string);
begin
  Self.FnomeTabela := Value;
end;

procedure TDataIntegradorModuloWeb.SetthreadControl(const Value: IThreadControl);
begin
  FthreadControl := Value;
end;

procedure TDataIntegradorModuloWeb.SetTranslateTableNames(aTranslateTableNames: TStringDictionary);
begin
  Self.FTranslateTableNames := aTranslateTableNames;
end;

function TDataIntegradorModuloWeb.getdmPrincipal: IDataPrincipal;
begin
  if FdmPrincipal = nil then
  begin
    FdmPrincipal := getNewDataPrincipal;
  end;
  result := FdmPrincipal;
end;

function TDataIntegradorModuloWeb.getAdditionalSaveConditions: string;
begin
  result := '';
end;

class procedure TDataIntegradorModuloWeb.updateDataSets;
begin
  //nada a atualizar
end;

function TDataIntegradorModuloWeb.gerenciaRedirecionamentos(idLocal,
  idRemoto: integer): boolean;
begin
  result := false;
end;

function TDataIntegradorModuloWeb.BinaryFromBase64(const base64: string): TBytesStream;
var
  Input: TStringStream;
  Output: TBytesStream;
begin
  Input := TStringStream.Create(base64, TEncoding.ASCII);
  try
    Output := TBytesStream.Create;
    try
      Soap.EncdDecd.DecodeStream(Input, Output);
      Output.Position := 0;
      Result := TBytesStream.Create;
      try
        Result.LoadFromStream(Output);
      except
        Result.Free;
        raise;
      end;
    finally
      Output.Free;
    end;
  finally
    Input.Free;
  end;
end;

{ TTabelaDetalhe }

constructor TTabelaDetalhe.Create;
begin
  inherited;
  translations := TTranslationSet.create(nil);
end;

destructor TTabelaDetalhe.Destroy;
begin
  inherited;
end;

function TTabelaDetalhe.GetNomeFK: string;
begin
  Result := FnomeFK;
end;

function TTabelaDetalhe.GetNomeParametro: string;
begin
  Result := FnomeParametro;
end;

procedure TTabelaDetalhe.setNomeFK(const Value: string);
begin
  FnomeFK := Value;
end;

procedure TTabelaDetalhe.setNomeParametro(const Value: string);
begin
  FnomeParametro := Value;
end;

{ TJSONArrayContainer }

destructor TJSONArrayContainer.Destroy;
begin
  Self.FJSonArray.Free;
  inherited;
end;

function TJSONArrayContainer.getJsonArray: TJsonArray;
begin
  if Self.FJSonArray = nil then
    Self.FJSonArray := TJSONArray.Create;
  Result := Self.FJSonArray;
end;

{ TFieldList }

constructor TFieldDictionaryList.Create(const aTableName: string; aDm : IDataPrincipal);
begin
  inherited Create;
  FTableName := UpperCase(aTableName);
  FDm := aDm;
  Self.getTableFields;
end;

destructor TFieldDictionaryList.Destroy;
var
  _item: TPair<string, TFieldDictionary>;
begin
  for _item in Self do
    _item.Value.Free;
  inherited;
end;

procedure TFieldDictionaryList.getTableFields;
var
  lqry: TSQLDataSet;
  lfield: TFieldDictionary;
  lFieldType: TFieldType;
begin
  if (FDm <> nil) and (self.FTableName <> EmptyStr) then
  begin
    lqry := FDm.getQuery;
    try
      Self.FTableName := UpperCase(Self.FTableName);

      lqry.CommandText :=
        '  SELECT ' +
        '    A.RDB$FIELD_NAME FieldName,' +
        '    C.RDB$TYPE AS DataType,' +
        '    C.RDB$TYPE_NAME TIPO,' +
        '    B.RDB$FIELD_SUB_TYPE SUBTIPO,' +
        '    B.RDB$FIELD_LENGTH TAMANHO,' +
        '    B.RDB$SEGMENT_LENGTH SEGMENTO,' +
        '    B.RDB$FIELD_PRECISION PRECISAO,' +
        '    B.RDB$FIELD_SCALE CASAS_DECIMAIS,' +
        '    A.RDB$DEFAULT_SOURCE VALOR_PADRAO,' +
        '    A.RDB$NULL_FLAG OBRIGATORIO' +
        '  FROM' +
        '    RDB$RELATION_FIELDS A,' +
        '    RDB$FIELDS B,' +
        '    RDB$TYPES C' +
        '  WHERE' +
        '    (A.RDB$RELATION_NAME = '+ QuotedStr(Self.FTableName) + ') AND' +
        '    (B.RDB$FIELD_NAME = A.RDB$FIELD_SOURCE)AND' +
        '    (C.RDB$TYPE = B.RDB$FIELD_TYPE) AND' +
        '    (C.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'')' +
        '  ORDER BY' +
        '    RDB$FIELD_POSITION';

      lqry.Open;
      lqry.First;
      while not lqry.Eof do
      begin
        if not self.ContainsKey(Lowercase(Trim(lqry.FieldByName('FieldName').AsString))) then
        begin
          lfield := TFieldDictionary.Create;
          lfield.FieldName := Lowercase(lqry.FieldByName('FieldName').AsString);
          case lqry.FieldByName('DataType').AsInteger of
            7: //Short
              lFieldType := ftSmallInt;
            8: //INTEGER
              begin
                if lqry.FieldByName('SUBTIPO').asInteger = 0 then
                  lFieldType := ftInteger
                else
                  lFieldType := ftFMTBcd;
              end;
            10: //Float
              begin
                if lqry.FieldByName('SUBTIPO').asInteger = 0 then
                  lFieldType := ftSingle
                else
                  lFieldType := ftFloat;
              end;
            12: //Date
              lFieldType := ftDate;
            13: //Time
              lFieldType := ftTime;
            16: //int64
              begin
                if lqry.FieldByName('SUBTIPO').asInteger = 0 then
                  lFieldType := ftLargeint
                else
                  lFieldType := ftCurrency;
              end;
            27: //double
              begin
                if lqry.FieldByName('SUBTIPO').asInteger = 0 then
                  lFieldType := ftFloat
                else
                  lFieldType := ftCurrency;
              end;
            35: //timestamp
              lFieldType := ftTimeStamp;
            14, 37: //varchar
              lFieldType := ftString;
            261: //blob
              if lqry.FieldByName('SUBTIPO').asInteger = 0 then
                lFieldType := ftBlob
              else
                lFieldType := ftMemo;
            else
              lFieldType := ftUnknown;
          end;
          lfield.DataType := lFieldType;
          Self.Add(LowerCase(Trim(lqry.FieldByName('FieldName').asString)), lfield);
        end;
        lqry.Next;
      end;

    finally
      lqry.Free;
    end;
  end;
end;

{ TFieldDictionary }

procedure TFieldDictionary.SetFieldName(const Value: string);
begin
  FFieldName := Trim(Value);
end;


{ TDetailList }

destructor TDetailList.Destroy;
var
  Pair: TPair<String, TJSONArrayContainer>;
  Json: TJsonValue;
begin
  for Pair in Self do
  begin
    for Json in Pair.Value.getJsonArray do
      Json.Free;
  end;
  Self.Clear;
  inherited;
end;

end.
