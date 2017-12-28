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
  TDatasetDictionary = class(TDictionary<String, String>)
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

  TNameTranslation = record
    server: string;
    pdv: string;
    lookupRemoteTable: string;
    fkName: string;
  end;

  TTranslationSet = class
    protected
      translations: array of TNameTranslation;
    public
      constructor create(owner: TComponent);
      procedure add(serverName, pdvName: string;
        lookupRemoteTable: string = ''; fkName: string = '');
      function translateServerToPDV(serverName: string; duasVias: boolean): string;
      function translatePDVToServer(pdvName: string): string;
      function size: integer;
      function get(index: integer): TNameTranslation;
  end;

  TTabelaDependente = record
    nomeTabela: string;
    nomeFK: string;
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
  end;

  TTabelaDetalhe = class
  private
    FNomeTabela: string;
    FnomeFK: string;
    FnomePK: string;
    FnomeParametro: string;
    FnomeSingularDetalhe: string;
    FnomePluralDetalhe: string;
    FFieldList : TFieldDictionaryList;
    FDm : IDataPrincipal;
  protected
    function GetNomeTabela: string; virtual;
    procedure setNomeTabela(const Value: string); virtual;
    function GetNomeFK: string; virtual;
    function GetNomePK: string; virtual;
    procedure setNomeFK(const Value: string); virtual;
    procedure setNomePK(const Value: string); virtual;
    function GetNomeParametro: string; virtual;
    procedure setNomeParametro(const Value: string); virtual;
    function GetNomePluralDetalhe: string; virtual;
    function GetNomeSingularDetalhe: string; virtual;
    procedure setNomePluralDetalhe(const Value: string); virtual;
    procedure setNomeSingularDetalhe(const Value: string); virtual;
    procedure setDmPrincipal(const Value: IDataPrincipal); virtual;
  public
    tabelasDetalhe: array of TTabelaDetalhe;
    translations: TTranslationSet;
    constructor create;
    property nomeTabela: string read GetNomeTabela write setNomeTabela;
    property nomeFK: string read GetNomeFK write setNomeFK;
    property nomePK: string read GetNomePK write setNomePK;
    property nomeParametro: string read GetNomeParametro write setNomeParametro;
    property nomeSingularDetalhe: string read GetNomeSingularDetalhe write setNomeSingularDetalhe;
    property nomePluralDetalhe: string read GetNomePluralDetalhe write setNomePluralDetalhe;
    property DmPrincipal: IDataPrincipal read FDm write setDmPrincipal;
    destructor Destroy; override;
  end;

  TDataIntegradorModuloWeb = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    FdmPrincipal: IDataPrincipal;
    Fnotifier: ISincronizacaoNotifier;
    FthreadControl: IThreadControl;
    FCustomParams: ICustomParams;
    FstopOnPostRecordError: boolean;
    procedure addTabelaDetalheParams(valorPK: integer;
      params: TStringList;
      tabelaDetalhe: TTabelaDetalhe);
    function GetErrorMessage(const aXML: string): string;
    procedure SetDataLog(const Value: ILog);
    procedure UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : array of TTabelaDetalhe);
    procedure SetthreadControl(const Value: IThreadControl);
    procedure OnWorkHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function getFieldInsertList(node: IXMLDomNode): string;
  protected
    FDetailList: TDictionary<String, TJSONArrayContainer>;
    FFieldList : TFieldDictionaryList;
    FDataLog: ILog;
    nomeTabela: string;
    nomeSingular: string;
    nomePlural: string;
    nomePKLocal: string;
    nomePKRemoto: string;
    nomeGenerator: string;
    usePKLocalMethod: Boolean;
    duasVias: boolean;
    useMultipartParams: boolean;
    clientToServer: boolean;
    encodeJsonValues : boolean;
    tabelasDependentes: array of TTabelaDependente;
    tabelasDetalhe: array of TTabelaDetalhe;
    offset: integer;
    paramsType: TParamsType;
    function getVersionFieldName: string; virtual;
    procedure Log(const aLog: string; aClasse: string = ''); virtual;
    function extraGetUrlParams: String; virtual;
    procedure beforeRedirectRecord(idAntigo, idNovo: integer); virtual;
    function ultimaVersao: integer; virtual;
    function getRequestUrlForAction(toSave: boolean; versao: integer = -1): string; virtual;
    procedure importRecord(node: IXMLDomNode);
    procedure updateInsertRecord(node: IXMLDomNode; const id: integer);
    function jaExiste(id: integer): boolean;
    function getFieldList(node: IXMLDomNode): string;
    function getFieldUpdateList(node: IXMLDomNode): string;
    function getFieldValues(node: IXMLDomNode): string;
    function translateFieldValue(node: IXMLDomNode): string; virtual;
    function translateFieldNamePdvToServer(node: IXMLDomNode): string;
    function translateFieldNameServerToPdv(node: IXMLDomNode): string; virtual;
    function translateTypeValue(fieldType, fieldValue: string): string;
    function translateValueToServer(translation: TNameTranslation;
      fieldName: string; field: TField;
      nestedAttribute: string = ''; fkName: string = ''; fieldValue: String = ''): string; virtual;
    function translateValueFromServer(fieldName, value: string): string; virtual;
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
    function getUpdateBaseSQL(node: IXMLDOMNode): string;
    procedure addDetails(ds: TDataSet; params: TStringList);
    function addTranslatedParams(ds: TDataSet;
      params: TStringList;
      translations: TTranslationSet; nestedAttribute: string = ''): IXMLDomDocument2;
    function getAdditionalSaveConditions: string; virtual;
    procedure beforeUpdateRecord(id: integer); virtual;
    function gerenciaRedirecionamentos(idLocal, idRemoto: integer): boolean; virtual;
    function getNewDataPrincipal: IDataPrincipal; virtual; abstract;
    function maxRecords: integer; virtual;
    function getTimeoutValue: integer; virtual;
    function getDateFormat: String; virtual;
    function getAdditionalDetailFilter:String; virtual;
    function shouldContinue: boolean;
    procedure onDetailNamesMalformed(configName, tableName: string); virtual;
    function getIncludeFieldNameOnList(const aDMLOperation: TDMLOperation; const aFieldName: string): boolean; virtual;
    function getObjectsList: string; virtual;
    function getUpdateStatement(node: IXMLDomNode; const id: integer): String; virtual;
    function getInsertStatement(node: IXMLDomNode): String; virtual;
    function getNewId: Integer; virtual;
    function post(ds: TDataSet; http: TidHTTP; url: string): string; virtual;
    procedure addDetailsToJsonList(aDs: TDataSet); virtual;
    procedure SelectDetails(aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe); virtual;
    function getJsonObject(aDs: TDataSet; aTranslations: TTranslationSet; aDict: TDatasetDictionary; aNestedAttribute: string = '') : TJsonObject; virtual;
    procedure addMasterTableToJson(aDs: TDataSet; apStream: TStringStream); virtual;
    procedure RunDataSet(const aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe; aProc: TAnonymousMethod); virtual;
    function GetIdRemoto(aDoc: IXMLDomDocument2): integer;
    function getXMLContentAsXMLDom(const aXMLContent: string): IXMLDomDocument2;
    procedure SetdmPrincipal(const Value: IDataPrincipal); virtual;
    function getdmPrincipal: IDataPrincipal; virtual;
    function JsonObjectHasPair(const aName: string; aJson: TJSONObject): boolean;
    function DataSetToArray(aDs: TDataSet): TDatasetDictionary; virtual;
  public
    translations: TTranslationSet;
    verbose: boolean;
    property notifier: ISincronizacaoNotifier read Fnotifier write Fnotifier;
    property threadControl: IThreadControl read FthreadControl write SetthreadControl;
    property CustomParams: ICustomParams read FCustomParams write FCustomParams;
    property dmPrincipal: IDataPrincipal read getdmPrincipal write SetdmPrincipal;
    property stopOnPostRecordError: boolean read FstopOnPostRecordError write FstopOnPostRecordError;
    function buildRequestURL(nomeRecurso: string; params: string = ''; httpAction: THttpAction = haGet): string; virtual; abstract;
    procedure getDadosAtualizados(http: TIdHTTP = nil);
    function saveRecordToRemote(ds: TDataSet; var salvou: boolean; http: TidHTTP = nil): IXMLDomDocument2;
    procedure migrateSingletonTableToRemote;
    procedure postRecordsToRemote(http: TidHTTP = nil); virtual;
    class procedure updateDataSets; virtual;
    procedure afterDadosAtualizados; virtual;
    function getHumanReadableName: string; virtual;
    property DataLog: ILog read FDataLog write SetDataLog;
    destructor Destroy; override;
  end;

  TDataIntegradorModuloWebClass = class of TDataIntegradorModuloWeb;

var
  DataIntegradorModuloWeb: TDataIntegradorModuloWeb;
implementation

uses AguardeFormUn, ComObj;

{$R *.dfm}

function TDataIntegradorModuloWeb.extraGetUrlParams: String;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.getObjectsList: string;
begin
  Result := '/' + dasherize(nomePlural) + '//' + dasherize(nomeSingular);
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

    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE OR 4 ); //);
    writeln('URL: ' + url);
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
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
          importRecord(node);
      end;
    end;
    keepImporting := (maxRecords > 0) and (numRegistros >= maxRecords);
  end;
  afterDadosAtualizados;
end;

function TDataIntegradorModuloWeb.getHumanReadableName: string;
begin
  result := ClassName;
end;

function TDataIntegradorModuloWeb.maxRecords: integer;
begin
  result := 0;
end;

procedure TDataIntegradorModuloWeb.importRecord(node : IXMLDomNode);
var
  id: integer;
  qry: TSQLDataSet;
begin
  if not singleton then
  begin
    id := strToIntDef(node.selectSingleNode(dasherize(nomePKRemoto)).text, -1);
    if id >= 0 then
    begin
      dmPrincipal.startTransaction;
      try
        try
          qry := dmPrincipal.getQuery;
          Self.updateInsertRecord(node, id);
          dmPrincipal.commit;
        finally
          FreeAndNil(qry);
        end;
      except
        on E:Exception do
        begin
          dmPrincipal.rollBack;
          Self.log(Format('Erro ao importar a tabela "%s":', [self.nomeTabela]));
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_RED or FOREGROUND_INTENSITY);
          Self.log(e.Message);
          SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
        end;
      end;
    end;
  end
  else
    updateSingletonRecord(node);
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

function TDataIntegradorModuloWeb.jaExiste(id: integer): boolean;
var
  qry: string;
begin
  if duasVias then
    qry := 'SELECT count(1) FROM ' + nomeTabela + ' where idRemoto = ' + IntToStr(id)
  else
    qry := 'SELECT count(1) FROM ' + nomeTabela + ' where ' + nomePKLocal + ' = ' + IntToStr(id);
  result := dmPrincipal.getSQLIntegerResult(qry) > 0;
end;

procedure TDataIntegradorModuloWeb.beforeUpdateRecord(id: integer);
begin

end;

function TDataIntegradorModuloWeb.getUpdateStatement(node: IXMLDomNode; const id: integer): String;
begin
  if duasVias then
    Result := getUpdateBaseSQL(node) + ' WHERE idRemoto = ' + IntToStr(id)
  else
    Result := getUpdateBaseSQL(node) + ' WHERE ' + nomePKLocal + ' = ' + IntToStr(id);
end;

function TDataIntegradorModuloWeb.getInsertStatement(node: IXMLDomNode): String;
begin
  Result := 'INSERT INTO ' + nomeTabela + getFieldList(node) + ' values ' + getFieldValues(node);
end;

procedure TDataIntegradorModuloWeb.updateInsertRecord(node: IXMLDomNode; const id: integer);
var
  i: integer;
  name: string;
  qry: TSQLDataSet;
  FieldsListUpdate, FieldsListInsert : string;
  ValorCampo: string;
  BlobStream: TStringStream;
  Existe: Boolean;
  Field: TFieldDictionary;
  NewId: integer;
  Paramlog, vLog: string;
  lFormatSettings: TFormatSettings;
begin
  Existe := jaExiste(id);
  qry := dmPrincipal.getQuery;
  try
    if Existe then
    begin
      FieldsListUpdate := self.getFieldUpdateList(node);
      qry.CommandText := 'UPDATE ' + nomeTabela + ' SET ' + FieldsListUpdate;
      if DuasVias then
        qry.CommandText := qry.CommandText + ' WHERE idRemoto = ' + IntToStr(id)
      else
        qry.CommandText := qry.CommandText + ' WHERE ' + nomePKLocal + ' = ' + IntToStr(id);
    end
    else
    begin
      FieldsListInsert := self.getFieldInsertList(node);
      NewId := Self.getNewId;
      if NewId > 0 then
      begin
        if Pos(':'+Self.nomePKLocal +',', FieldsListInsert) = 0 then
          FieldsListInsert := ':'+ Self.nomePKLocal + ',' + FieldsListInsert;
      end;
      qry.CommandText := 'INSERT INTO ' + nomeTabela + '(' + StringReplace(FieldsListInsert, ':', '', [rfReplaceAll]) + ') values (' + FieldsListInsert + ')';
      if qry.Params.ParamByName(Self.nomePkLocal) <> nil then
        qry.ParamByName(Self.nomePkLocal).AsInteger := NewId;
    end;

    //Preenche os Parametros
    for i := 0 to node.childNodes.length - 1 do
    begin
      name := translateFieldNameServerToPdv(node.childNodes.item[i]);
      ValorCampo := translateFieldValue(node.childNodes.item[i]);
      if name <> '*' then
        if Self.getIncludeFieldNameOnList(dmUpdate, name) then
        begin
          if ValorCampo = 'NULL' then
          begin
            qry.ParamByName(name).Value := unassigned;
            qry.ParamByName(name).DataType := ftString;
          end
          else
          begin
            Field := nil;
            if Self.FFieldList <> nil then
              Field := Self.FFieldList.Items[Lowercase(name)];
            if Field <> nil then
            begin
              case Field.DataType of
                ftString: qry.ParamByName(name).AsString := ValorCampo;
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
                ftCurrency:
                  begin
                    ValorCampo := StringReplace(ValorCampo, '''','', [rfReplaceAll]);
                    qry.ParamByName(name).AsCurrency := StrToCurr(ValorCampo);
                  end;
                ftFloat: qry.ParamByName(name).AsFloat := StrToFloat(ValorCampo);
                ftBlob: begin
                          BlobStream := TStringStream.Create(ValorCampo);
                          try
                            qry.ParamByName(name).LoadFromStream(BlobStream, ftMemo);
                          finally
                            FreeAndNil(BlobStream);
                          end;
                end
              else
                qry.ParamByName(name).AsString := ValorCampo;
              end;
            end;
          end;
        end;
    end;

    if Existe then
      beforeUpdateRecord(id);
    try
      qry.ExecSQL;
    except
      on E:Exception do
      begin
        ParamLog := EmptyStr;
        for i := 0 to qry.Params.Count - 1 do
          ParamLog := ParamLog + qry.Params[i].Name + ' = "' + qry.Params[i].AsString + '"' + #13#10;
        vLog := 'Erro ExecSQL: ' + #13#10 + qry.CommandText + #13#10 + ParamLog + #13#10 + e.Message;
          Raise EIntegradorException.Create(vLog);
      end;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TDataIntegradorModuloWeb.UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : array of TTabelaDetalhe);
var
   i,j : integer;
   vNode : IXMLDomNode;
   vNodeList: IXMLDOMNodeList;
   vIdRemoto, vPkLocal : String;
   vNomePlural, vNomeSingular: string;
begin
  try
    for i := low(pTabelasDetalhe) to high(pTabelasDetalhe) do
    begin
      vNomePlural := pTabelasDetalhe[i].nomePluralDetalhe;
      vNomeSingular := pTabelasDetalhe[i].nomeSingularDetalhe;

      if VNomePlural = EmptyStr then
      begin
        onDetailNamesMalformed(pTabelasDetalhe[i].nomeTabela, 'NomePlural');
        exit;
      end;

      if vNomeSingular = EmptyStr then
      begin
        onDetailNamesMalformed(pTabelasDetalhe[i].nomeTabela, 'NomeSingular');
        exit;
      end;

      vNode := pNode.selectSingleNode('./' + dasherize(vNomePlural));
      vNodeList := vNode.selectNodes('./' + dasherize(vNomeSingular));

      for j := 0 to vNodeList.length - 1 do
      begin
        vIdRemoto := vNodeList[j].selectSingleNode('./id').text;
        vPkLocal := vNodeList[j].selectSingleNode('./original-id').text;

        if duasVias then
          dmPrincipal.execSQL('UPDATE ' + pTabelasDetalhe[i].nomeTabela + ' SET salvouRetaguarda = ' +
                          QuotedStr('S') + ', idRemoto = ' + vIdRemoto +
                          ' WHERE salvouRetaguarda = ''N'' and ' + pTabelasDetalhe[i].nomePK + ' = ' + vPkLocal) ;
      end;
      if (Length(pTabelasDetalhe[i].tabelasDetalhe) > 0) and (vNode <> nil) then
        Self.UpdateRecordDetalhe(vNode, pTabelasDetalhe[i].tabelasDetalhe);
    end;
  except
    raise;
  end;
end;

procedure TDataIntegradorModuloWeb.updateSingletonRecord(node: IXMLDOMNode);
begin
  if dmPrincipal.getSQLIntegerResult('SELECT count(1) from ' + nomeTabela) < 1 then
    dmPrincipal.execSQL('Insert into ' + nomeTabela + ' DEFAULT VALUES');
  dmPrincipal.execSQL(getUpdateBaseSQL(node));
end;

function TDataIntegradorModuloWeb.getUpdateBaseSQL(node: IXMLDOMNode): string;
begin
  result := 'UPDATE ' + nomeTabela + getFieldUpdateList(node);
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
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmInsert, name) then
        result := result + name + ', ';
  end;
  result := copy(result, 0, length(result)-2);
  result := result + getFieldAdditionalList(node);
  result := result + ')';
end;

function TDataIntegradorModuloWeb.getFieldValues(node: IXMLDomNode): string;
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
      Result := Result + IntToStr(getNewId) + ', ';
  end;
  if duasVias then
    result := result + QuotedStr('S') + ', ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmInsert, name) then
        result := result + translateFieldValue(node.childNodes.item[i]) + ', ';
  end;
  result := copy(result, 0, length(result)-2);
  result := result + getFieldAdditionalValues(node);
  result := result + ')';
end;

function TDataIntegradorModuloWeb.getFieldUpdateList(node: IXMLDomNode): string;
var
  i: integer;
  name: string;
begin
  result := '';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmUpdate, name) then
        result := result + ' ' + name + ' = :' + name + ',';
  end;
  //Remove a ultima virgula
  result := copy(result, 1, Length(result)-1);
  result := result + getFieldAdditionalUpdateList(node);
end;

function TDataIntegradorModuloWeb.getFieldInsertList(node: IXMLDomNode): string;
var
  i: integer;
  name: string;
begin
  Result := '';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
      if Self.getIncludeFieldNameOnList(dmInsert, name) then
        Result := Result + ' :' + name + ',';
  end;
  Result := copy(Result, 1, Length(Result)-1);
end;

function TDataIntegradorModuloWeb.getIncludeFieldNameOnList(const aDMLOperation: TDMLOperation; const aFieldName: string): boolean;
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

function TDataIntegradorModuloWeb.translateFieldValue(
  node: IXMLDomNode): string;
var
  typedTranslate: string;
begin
  if (node.attributes.getNamedItem('nil') <> nil) and (node.attributes.getNamedItem('nil').text = 'true') then
    result := 'NULL'
  else if (node.attributes.getNamedItem('type') <> nil) then
  begin
    typedTranslate := translateTypeValue(node.attributes.getNamedItem('type').text, node.text);
    result := translateValueFromServer(node.nodeName, typedTranslate);
  end
  else
    result := translateValueFromServer(node.nodeName, node.text);
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

function TDataIntegradorModuloWeb.translateFieldNameServerToPdv(
  node: IXMLDomNode): string;
begin
  result := translations.translateServerToPDV(node.nodeName, duasVias);
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

procedure TDataIntegradorModuloWeb.addDetailsToJsonList(aDs: TDataSet);
var
  i : integer;
begin
  for i := low(Self.tabelasDetalhe) to high(Self.tabelasDetalhe) do
    Self.SelectDetails(aDs.fieldByName(nomePKLocal).AsInteger, Self.tabelasDetalhe[i]);
end;

procedure TDataIntegradorModuloWeb.SelectDetails(aValorPK: integer; aTabelaDetalhe: TTabelaDetalhe);
var
  jsonArrayDetails: TJSONArrayContainer;
begin
  RunDataSet(aValorPk, aTabelaDetalhe,
             procedure (aDataSet: TDataSet)
             var
               i: integer;
               Dict: TDataSetDictionary;
             begin
               if not Self.FDetailList.ContainsKey(aTabelaDetalhe.nomeParametro) then
               begin
                 jsonArrayDetails := TJSONArrayContainer.Create;
                 jsonArrayDetails.nomePluralDetalhe := aTabelaDetalhe.nomePluralDetalhe;
                 jsonArrayDetails.nomeSingularDetalhe := aTabelaDetalhe.nomeSingularDetalhe;
                 jsonArrayDetails.nomeTabela := aTabelaDetalhe.nomeTabela;
                 jsonArrayDetails.nomePkLocal := aTabelaDetalhe.nomePK;
                 Self.FDetailList.Add(aTabelaDetalhe.nomeParametro, jsonArrayDetails);
               end
               else
                 jsonArrayDetails := Self.FDetailList.Items[aTabelaDetalhe.nomeParametro];
               Dict := Self.DataSetToArray(aDataSet);
               try
                 jsonArrayDetails.getJsonArray.AddElement(Self.getJsonObject(aDataSet, aTabelaDetalhe.translations, Dict, aTabelaDetalhe.nomeParametro));
                 for i := low(aTabelaDetalhe.tabelasDetalhe) to high(aTabelaDetalhe.tabelasDetalhe) do
                   SelectDetails(aDataSet.fieldByName(aTabelaDetalhe.nomePK).AsInteger, aTabelaDetalhe.tabelasDetalhe[i]);
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

function TDataIntegradorModuloWeb.DataSetToArray(aDs: TDataSet) : TDatasetDictionary;
var
  i: Integer;
  nome: String;
  value: String;
  fieldValue: string;
  BlobStream: TStringStream;
  FieldStream: TStream;
begin
  Result := TDatasetDictionary.Create;
  for I := 0 to aDs.FieldCount - 1 do
  begin
    nome := aDs.Fields[i].FieldName;

    try
    begin
      if VarIsNull(aDs.Fields[i].AsVariant) then
        fieldValue := ''
      else
        fieldValue := aDs.Fields[i].AsVariant;
    end;
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
        BlobStream := TStringStream.Create;
        FieldStream := aDs.CreateBlobStream(aDs.Fields[i], bmRead);
        BlobStream.LoadFromStream(FieldStream);
        value := BlobStream.DataString;
      Finally
        FreeAndNil(BlobStream);
        FreeAndNil(FieldStream);
      end;
    end
    else
      value := fieldValue;
    Result.Add(nome, value)
  end;
end;

function TDataIntegradorModuloWeb.getJsonObject(aDs: TDataSet;
 aTranslations: TTranslationSet; aDict: TDatasetDictionary; aNestedAttribute: string = ''): TJsonObject;
var
  i: integer;
  nomeCampo, nome, valor, fieldValue: string;
begin
  Result := TJsonObject.Create;
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
        if Self.encodeJsonValues then
          Result.AddPair(nome,  EncodeString(Trim(valor)))
        else
          Result.AddPair(nome, valor);
    end;
  end;
end;

function TDataIntegradorModuloWeb.getNewId: Integer;
begin
  Result := 0;
end;

procedure TDataIntegradorModuloWeb.addMasterTableToJson(aDs: TDataSet; apStream: TStringStream);
var
  JMaster, JResponse: TJsonObject;
  Item: TPair<string, TJSONArrayContainer>;
  Dict: TDatasetDictionary;
begin
  Dict := Self.DataSetToArray(aDs);
  try
    JMaster := Self.getJsonObject(aDs, Self.translations, Dict);
    JResponse := TJSONObject.Create;
    jResponse.AddPair(Self.nomeSingularSave, JMaster);
    for Item in Self.FDetailList do
      JMaster.AddPair(item.Key, Item.Value.getJsonArray);
    apStream.WriteString(JResponse.ToString);
  finally
    Dict.Free;
  end;
end;

function TDataIntegradorModuloWeb.post(ds: TDataSet; http: TidHTTP; url: string): string;
var
  params: TStringList;
  pStream: TStringStream;
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
    try
      Self.addDetailsToJsonList(ds);
      Self.addMasterTableToJson(ds, pStream);
      result := http.Post(url, pStream);
    finally
      pStream.Free;
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

                                                
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
          try
            stream := TStringStream.Create('');
            prepareMultipartParams(ds, multipartParams );
            http.Post(getRequestUrlForAction(true, -1), multipartParams, stream);
            xmlContent := stream.ToString;
          finally
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
          txtUpdate := 'UPDATE ' + nomeTabela + ' SET salvouRetaguarda = ' + QuotedStr('S');

          if duasVias then
          begin
            idRemoto := Self.GetIdRemoto(Result);
            if idRemoto > 0 then
              txtUpdate := txtUpdate + ', idRemoto = ' + IntToStr(idRemoto);
          end;

          txtUpdate := txtUpdate + ' WHERE ' + nomePKLocal + ' = ' + ds.fieldByName(nomePKLocal).AsString;

          //da a chance da classe gerenciar redirecionamentos, por exemplo ao descobrir que este registro já
          //existia no remoto e era outro registro neste banco de dados.
          if not gerenciaRedirecionamentos(ds.fieldByName(nomePKLocal).AsInteger, idRemoto) then
          begin
            dmPrincipal.startTransaction;
            dmPrincipal.execSQL(txtUpdate);
            dmPrincipal.commit;
          end;

          if (Length(TabelasDetalhe) > 0) and (Result.selectSingleNode(dasherize(nomeSingularSave)) <> nil) then
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
  i : integer;
begin
  for i := low(tabelasDetalhe) to high(tabelasDetalhe) do
    addTabelaDetalheParams(ds.fieldByName(nomePKLocal).AsInteger, params, tabelasDetalhe[i]);
end;

procedure TDataIntegradorModuloWeb.addTabelaDetalheParams(valorPK: integer;
  params: TStringList;
  tabelaDetalhe: TTabelaDetalhe);
begin
  RunDataSet(valorPk,
             TabelaDetalhe,
             procedure (aDataSet: TDataSet)
             var
               i: integer;
             begin
               addTranslatedParams(aDataSet, params, tabelaDetalhe.translations, tabelaDetalhe.nomeParametro);
               for i := low(tabelaDetalhe.tabelasDetalhe) to high(tabelaDetalhe.tabelasDetalhe) do
                 addTabelaDetalheParams(aDataSet.fieldByName(tabelaDetalhe.nomePK).AsInteger, params, tabelaDetalhe.tabelasDetalhe[i]);

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
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 14 OR 4); //FOREGROUND_RED);
      writeln(qry.CommandText);
      SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), 7);
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
        inc(n);

        try
          saveRecordToRemote(qry, salvou, http);
          if salvou then
            Self.Log(Format('Registro %d de %d', [n, total]));
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
        Self.log(Format('Post de records para remote comitados. Classe: %s. Total de registros: %d.', [ClassName, total]), 'Sync')
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
  i: integer;
  nomeFK: string;
begin
  beforeRedirectRecord(idAntigo, idNovo);
  //Para cada tabela que referenciava esta devemos dar o update do id antigo para o novo
  for i:= low(tabelasDependentes) to high(tabelasDependentes) do
  begin
    nomeFK := tabelasDependentes[i].nomeFK;
    if nomeFK = '' then
      nomeFK := nomePKLocal;
    dmPrincipal.execSQL('UPDATE ' + tabelasDependentes[i].nomeTabela +
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
  tam: integer;
begin
  tam := length(translations);
  SetLength(translations, tam + 1);
  translations[tam].server := serverName;
  translations[tam].pdv := pdvName;
  translations[tam].lookupRemoteTable := lookupRemoteTable;
  translations[tam].fkName := fkName;
end;

procedure TDataIntegradorModuloWeb.beforeRedirectRecord(idAntigo, idNovo: integer);
begin
  //
end;

constructor TTranslationSet.create(owner: TComponent);
begin
  SetLength(translations, 0);
end;

function TTranslationSet.get(index: integer): TNameTranslation;
begin
  result := translations[index];
end;

function TTranslationSet.size: integer;
begin
  result := length(translations);
end;

function TTranslationSet.translatePDVToServer(pdvName: string): string;
var
  i: integer;
begin
  result := '';
  for i := low(translations) to high(translations) do
    if translations[i].pdv = pdvName then
      result := translations[i].server;
end;

function TTranslationSet.translateServerToPDV(serverName: string; duasVias: boolean): string;
var
  i: integer;
begin
  result := '';
  if duasVias and (upperCase(serverName) = 'ID') then
    result := 'idRemoto'
  else
    for i := low(translations) to high(translations) do
      if translations[i].server = underscorize(serverName) then
      begin
        result := translations[i].pdv;
        break;
      end;
end;

procedure TDataIntegradorModuloWeb.DataModuleCreate(Sender: TObject);
begin
  verbose := false;
  duasVias := false;
  clientToServer := false;
  translations := TTranslationSet.create(self);
  nomePKLocal := 'id';
  nomePKRemoto := 'id';
  SetLength(tabelasDependentes, 0);
  nomeGenerator := '';
  usePKLocalMethod := false;
  useMultipartParams := false;
  paramsType := ptParam;
  FstopOnPostRecordError := true;
  Self.FDetailList := TDictionary<String, TJSONArrayContainer>.Create;
  Self.encodeJsonValues := False;
  translations.add('id', 'idremoto');
end;


destructor TDataIntegradorModuloWeb.Destroy;
var
  i: integer;
  Item: TPair<string, TJSONArrayContainer>;
begin
  for i := Low(Self.tabelasDetalhe) to High(Self.tabelasDetalhe) do
     Self.tabelasDetalhe[i].Free;
  for Item in Self.FDetailList do
    Item.Value.Free;
  Self.FDetailList.Free;
  if Self.FFieldList <> nil then
    Self.FFieldList.Free;
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

function TDataIntegradorModuloWeb.translateValueFromServer(fieldName,
  value: string): string;
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
  result := nomeSingular;
end;

function TDataIntegradorModuloWeb.nomeSingularSave: string;
begin
  result := nomeSingular;
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
  i: Integer;
begin
  FdmPrincipal := Value;
  if Value <> nil then
  begin
    if Self.FFieldList = nil then
      Self.FFieldList := TFieldDictionaryList.Create(Self.nomeTabela, Value);
    for i := 0 to High(Self.tabelasDetalhe) do
      TTabelaDetalhe(Self.tabelasDetalhe[i]).DmPrincipal := Value;
  end;
end;

procedure TDataIntegradorModuloWeb.SetthreadControl(const Value: IThreadControl);
begin
  FthreadControl := Value;
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

{ TTabelaDetalhe }

constructor TTabelaDetalhe.create;
begin
  translations := TTranslationSet.create(nil);
end;

destructor TTabelaDetalhe.Destroy;
begin
  Self.FFieldList.Free;
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

function TTabelaDetalhe.GetNomePK: string;
begin
  Result := FnomePK;
end;

function TTabelaDetalhe.GetNomePluralDetalhe: string;
begin
  Result := FnomePluralDetalhe;
end;

function TTabelaDetalhe.GetNomeSingularDetalhe: string;
begin
  Result := FnomeSingularDetalhe;
end;

function TTabelaDetalhe.GetNomeTabela: string;
begin
  Result := FNomeTabela;
end;

procedure TTabelaDetalhe.setDmPrincipal(const Value: IDataPrincipal);
begin
  Self.FDm := Value;
  if self.FFieldList = nil then
    Self.FFieldList := TFieldDictionaryList.Create(Self.GetNomeTabela, Value);
end;

procedure TTabelaDetalhe.setNomeFK(const Value: string);
begin
  FnomeFK := Value;
end;

procedure TTabelaDetalhe.setNomeParametro(const Value: string);
begin
  FnomeParametro := Value;
end;

procedure TTabelaDetalhe.setNomePK(const Value: string);
begin
  FnomePK := Value;
end;

procedure TTabelaDetalhe.setNomePluralDetalhe(const Value: string);
begin
  FnomePluralDetalhe := Value;
end;

procedure TTabelaDetalhe.setNomeSingularDetalhe(const Value: string);
begin
  FnomeSingularDetalhe := Value;
end;

procedure TTabelaDetalhe.setNomeTabela(const Value: string);
begin
  FNomeTabela := Value;
end;

{ TJSONArrayContainer }

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
                  lFieldType := ftCurrency;
              end;
            10: //Float
              lFieldType := ftFloat;
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
              lFieldType := ftCurrency;
            35: //timestamp
              lFieldType := ftTimeStamp;
            14, 37: //varchar
              lFieldType := ftString;
            261: //blob
              lFieldType := ftBlob
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


end.
