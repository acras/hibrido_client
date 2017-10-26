unit DIntegradorModuloWeb;

interface

uses
  SysUtils, ExtCtrls, DBClient, idHTTP, MSXML2_TLB, dialogs, acStrUtils, acNetUtils,
  DB, IdMultipartFormData, IdBaseComponent, IdComponent, IdTCPConnection, forms,
  IdTCPClient, IdCoder, IdCoder3to4, IdCoderUUE, IdCoderXXE, Controls,
  IDataPrincipalUnit, idURI, System.Classes, Windows,
  ISincronizacaoNotifierUnit, Data.SqlExpr, ABZipper, ABUtils, AbZipTyp, AbArcTyp, AbZipPrc,
  Xml.XMLIntf, Winapi.ActiveX, XML.XMLDoc, DLog;

type
  EIntegradorException = class(Exception)
  end;

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

  TTabelaDetalhe = class
  public
    nomeTabela: string;
    nomeFK: string;
    nomePK: string;
    nomeParametro: string;
    nomeSingularDetalhe : string;
    nomePluralDetalhe : string;
    tabelasDetalhe: array of TTabelaDetalhe;
    translations: TTranslationSet;
    constructor create;
  end;

  TDataIntegradorModuloWeb = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    FdmPrincipal: IDataPrincipal;
    Fnotifier: ISincronizacaoNotifier;
    FDataLog: TDataLog;
    FthreadControl: IThreadControl;
    procedure SetdmPrincipal(const Value: IDataPrincipal);
    function getdmPrincipal: IDataPrincipal;

    procedure addTabelaDetalheParams(valorPK: integer;
      params: TStringList;
      tabelaDetalhe: TTabelaDetalhe);
    function GetErrorMessage(const aXML: string): string;
    procedure SetDataLog(const Value: TDataLog);
    procedure Log(const aLog, aClasse: string);
    procedure UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : array of TTabelaDetalhe);
    procedure SetthreadControl(const Value: IThreadControl);
  protected
    nomeTabela: string;
    nomeSingular: string;
    nomePlural: string;
    nomePKLocal: string;
    nomePKRemoto: string;
    nomeGenerator: string;
    duasVias: boolean;
    useMultipartParams: boolean;
    clientToServer: boolean;
    tabelasDependentes: array of TTabelaDependente;
    tabelasDetalhe: array of TTabelaDetalhe;
    offset: integer;
    zippedPost: boolean;
    function extraGetUrlParams: String; virtual;
    procedure beforeRedirectRecord(idAntigo, idNovo: integer); virtual;
    function ultimaVersao: integer;
    function getRequestUrlForAction(toSave: boolean; versao: integer = -1): string;
    procedure importRecord(node: IXMLDomNode);
    procedure insertRecord(node: IXMLDomNode);
    procedure updateRecord(node: IXMLDomNode; id: integer);
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
      nestedAttribute: string = ''; fkName: string = ''): string; virtual;
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
      params: TStringList;
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
  public
    translations: TTranslationSet;
    verbose: boolean;
    property notifier: ISincronizacaoNotifier read Fnotifier write Fnotifier;
    property threadControl: IThreadControl read FthreadControl write SetthreadControl;
    property dmPrincipal: IDataPrincipal read getdmPrincipal write SetdmPrincipal;
    function buildRequestURL(nomeRecurso: string; params: string = ''): string; virtual; abstract;
    procedure getDadosAtualizados(http: TIdHTTP = nil);
    function saveRecordToRemote(ds: TDataSet; var salvou: boolean; http: TidHTTP = nil): IXMLDomDocument2;
    procedure migrateSingletonTableToRemote;
    procedure postRecordsToRemote(http: TidHTTP = nil); virtual;
    class procedure updateDataSets; virtual;
    procedure afterDadosAtualizados; virtual;
    function getHumanReadableName: string; virtual;
    property DataLog: TDataLog read FDataLog write SetDataLog;
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

procedure TDataIntegradorModuloWeb.getDadosAtualizados(http: TIdHTTP = nil);
var
  url, xmlContent: string;
  doc: IXMLDomDocument2;
  list : IXMLDomNodeList;
  i, numRegistros: integer;
  node : IXMLDomNode;
  keepImporting: boolean;
begin
  keepImporting := true;
  while keepImporting do
  begin
    if (Self.FThreadControl <> nil) and (not Self.FThreadControl.getShouldContinue) then
      Break;

    url := getRequestUrlForAction(false, ultimaVersao) + extraGetUrlParams;
    if notifier <> nil then
      notifier.setCustomMessage('Buscando ' + getHumanReadableName + '...');
    numRegistros := 0;
    xmlContent := getRemoteXmlContent(url, http);

    if trim(xmlContent) <> '' then
    begin
      doc := CoDOMDocument60.Create;
      doc.loadXML(xmlContent);
      list := doc.selectNodes('/' + dasherize(nomePlural) + '//' + dasherize(nomeSingular));
      numRegistros := list.length;
      if notifier <> nil then
        notifier.setCustomMessage(IntToStr(numRegistros) + ' novos');
      for i := 0 to numRegistros-1 do
      begin
        if (Self.FThreadControl <> nil) and (not Self.FThreadControl.getShouldContinue) then
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
begin
  if not singleton then
  begin
    id := strToInt(node.selectSingleNode(dasherize(nomePKRemoto)).text);
    dmPrincipal.startTransaction;
    try
      if jaExiste(id) then
        updateRecord(node, id)
      else
        insertRecord(node);
      dmPrincipal.commit;
    except
      dmPrincipal.rollBack;
    end;
  end
  else
    updateSingletonRecord(node);
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

procedure TDataIntegradorModuloWeb.updateRecord(node: IXMLDomNode; id: integer);
begin
  beforeUpdateRecord(id);
  if duasVias then
    dmPrincipal.execSQL(getUpdateBaseSQL(node) + ' WHERE idRemoto = ' + IntToStr(id), 3)
  else
    dmPrincipal.execSQL(getUpdateBaseSQL(node) + ' WHERE ' + nomePKLocal + ' = ' + IntToStr(id), 3);
end;

procedure TDataIntegradorModuloWeb.UpdateRecordDetalhe(pNode: IXMLDomNode; pTabelasDetalhe : array of TTabelaDetalhe);
var
   i,j : integer;
   vNode : IXMLDomNode;
   vNodeList, List: IXMLDOMNodeList;
   vIdRemoto, vPkLocal : String;
   vNomePlural, vNomeSingular: string;
begin
  try
    for i := low(pTabelasDetalhe) to high(pTabelasDetalhe) do
    begin
      vNomePlural := pTabelasDetalhe[i].nomePluralDetalhe;
      vNomeSingular := pTabelasDetalhe[i].nomeSingularDetalhe;

      if VNomePlural = EmptyStr then
        raise EIntegradorException.CreateFmt('Tabela detalhe da Classe %s não possui configuração de NomePluralDetalhe',[Self.ClassName]);

      if vNomeSingular = EmptyStr then
        raise EIntegradorException.CreateFmt('Tabela detalhe da Classe %s não possui configuração de NomeSingularDetalhe',[Self.ClassName]);

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
      if Length(pTabelasDetalhe[i].tabelasDetalhe) > 0 then
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

procedure TDataIntegradorModuloWeb.insertRecord(node: IXMLDomNode);
begin
  dmPrincipal.execSQL('INSERT INTO ' + nomeTabela + getFieldList(node) + ' values ' + getFieldValues(node));
end;

function TDataIntegradorModuloWeb.getFieldList(node: IXMLDomNode): string;
var
  i: integer;
  name: string;
begin
  result := '(';
  if duasVias and (nomeGenerator <> '') then
    result := result + nomePKLocal + ', ';
  if duasVias then
    result := result + 'salvouRetaguarda, ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
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
  if duasVias and (nomeGenerator <> '') then
    result := result + 'gen_id(' + nomeGenerator + ',1), ';
  if duasVias then
    result := result + QuotedStr('S') + ', ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
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
  result := ' set ';
  for i := 0 to node.childNodes.length - 1 do
  begin
    name := translateFieldNameServerToPdv(node.childNodes.item[i]);
    if name <> '*' then
      result := result + ' ' + translateFieldNameServerToPdv(node.childNodes.item[i]) + ' = ' +
        translateFieldValue(node.childNodes.item[i]) + ', ';
  end;
  result := copy(result, 0, length(result)-2);
  result := result + getFieldAdditionalUpdateList(node);
end;

function TDataIntegradorModuloWeb.getRequestUrlForAction(toSave: boolean; versao: integer = -1): string;
var
  nomeRecurso: string;
begin
  if toSave then
    nomeRecurso := nomeActionSave
  else
    nomeRecurso := nomeActionGet;
  result := buildRequestURL(nomeRecurso);
  if versao > -1 then
    result := result + '&version=' + IntToStr(versao);
end;

function TDataIntegradorModuloWeb.ultimaVersao: integer;
begin
  result := dmPrincipal.getSQLIntegerResult('Select max(versao) from ' + nomeTabela);
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
    result := QuotedStr(translateValueFromServer(node.nodeName, node.text));
end;

function TDataIntegradorModuloWeb.translateTypeValue(fieldType, fieldValue: string): string;
begin
  result := QuotedStr(fieldValue);
  if fieldType = 'integer' then
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
    {$IFDEF VER150}
    result := FastReplace(node.nodeName, '-', '');
    {$ELSE}
    result := StringReplace(node.nodeName, '-', '', [rfReplaceAll]);
    {$ENDIF}
end;

function TDataIntegradorModuloWeb.translateFieldNamePdvToServer(
  node: IXMLDomNode): string;
begin
  result := translations.translatepdvToServer(node.nodeName);
  if result = '' then
    {$IFDEF VER150}
    result := FastReplace(node.nodeName, '-', '');
    {$ELSE}
    result := StringReplace(node.nodeName, '-', '', [rfReplaceAll]);
    {$ENDIF}
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

function TDataIntegradorModuloWeb.saveRecordToRemote(ds: TDataSet;
  var salvou: boolean; http: TidHTTP = nil): IXMLDomDocument2;
var
  params: TStringList;
  multipartParams: TidMultipartFormDataStream;
  xmlContent: string;
  doc: IXMLDomDocument2;
  idRemoto: integer;
  txtUpdate: string;
  sucesso: boolean;
  strs, stream: TStringStream;
  zippedParams: TMemoryStream;
  zipper: TAbZipper;
  url: string;
  criouHttp: boolean;
  log: string;
begin
  Self.log('Iniciando save record para remote. Classe: ' + ClassName, 'Sync');
  salvou := false;
  criouHTTP := false;
  idRemoto := 0;
  if http = nil then
  begin
    criouHTTP := true;
    http := getHTTPInstance;
    http.ConnectTimeout := Self.getTimeoutValue;
    http.ReadTimeout := Self.getTimeoutValue;
  end;
  params := TStringList.Create;
  try
    addTranslatedParams(ds, params, translations);
    addDetails(ds, params);
    addMoreParams(ds, params);
    sucesso := false;
    while not sucesso do
    begin
      try
        if useMultipartParams then
        begin
          multiPartParams := TIdMultiPartFormDataStream.Create;
          try
            stream := TStringStream.Create('');
            prepareMultipartParams(ds, params, multipartParams);
            http.Post(getRequestUrlForAction(true), multipartParams, stream);
            xmlContent := stream.ToString;
          finally
            MultipartParams.Free;
          end;
        end
        else
        begin
          url := getRequestUrlForAction(true);
          {
            A implementação do zippedPost ainda não está pronta. Ela deve ser mais bem testada em vários casos
            e precisa ser garantido que o post está de fato indo zipado.
          }

          if zippedPost then
          begin
            http.Request.ContentEncoding := 'gzip';
            zippedParams := TMemoryStream.Create;
            zipper := TAbZipper.Create(nil);
            try
              strs := TStringStream.Create(utf8Encode(params.ToString), TEncoding.UTF8);
              zipper.ArchiveType := atGzip;
              zipper.ForceType := true;
              zipper.Stream := zippedParams;
              zipper.AddFromStream('', strs);
              Self.log('Pré post do registro', 'TDataIntegradorModuloWeb');
              xmlContent := http.Post(url, zippedParams);
              Self.log('Pós post do registro', 'TDataIntegradorModuloWeb');
            finally
              freeAndNil(zipper);
              freeAndNil(zippedParams);
            end;
          end
          else
          begin
            xmlContent := http.Post(url, Params);
          end;
        end;
        sucesso := true;
        CoInitialize(nil);
        try
          {$IFDEF VER150}
          doc := CoDOMDocument.Create;
          {$ELSE}
          doc := CoDOMDocument60.Create;
          {$ENDIF}
          doc.loadXML(xmlContent);
          result := doc;
        finally
          CoUninitialize;
        end;
        if duasVias or clientToServer then
        begin
          txtUpdate := 'UPDATE ' + nomeTabela + ' SET salvouRetaguarda = ' + QuotedStr('S');

          if duasVias then
          begin
            idRemoto := strToInt(doc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id').text);
            txtUpdate := txtUpdate + ', idRemoto = ' + IntToStr(idRemoto);
          end;

          txtUpdate := txtUpdate + ' WHERE salvouRetaguarda = ''N'' and ' + nomePKLocal + ' = ' + ds.fieldByName(nomePKLocal).AsString;

          //da a chance da classe gerenciar redirecionamentos, por exemplo ao descobrir que este registro já
          //existia no remoto e era outro registro neste banco de dados.
          if not gerenciaRedirecionamentos(ds.fieldByName(nomePKLocal).AsInteger, idRemoto) then
          begin
            dmPrincipal.startTransaction;
            dmPrincipal.execSQL(txtUpdate);
            dmPrincipal.commit;
          end;

          if Length(TabelasDetalhe) > 0 then
             Self.UpdateRecordDetalhe(doc.selectSingleNode(dasherize(nomeSingularSave)), TabelasDetalhe);

        end;
      except
        on e: EIdHTTPProtocolException do
        begin
          if e.ErrorCode = 422 then
            log := Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d, Erro: %s.',[ClassName, e.ErrorCode, Self.GetErrorMessage(e.ErrorMessage)])
          else if e.ErrorCode = 500 then
            log := Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d. Erro: Erro interno no servidor. ',[ClassName, e.ErrorCode])
          else
            log :=  Format('Erro ao tentar salvar registro. Classe: %s, Código de erro: %d. Erro: %s.',[ClassName, e.ErrorCode, e.ErrorMessage]);

          Self.log(log, 'Sync');
          raise EIntegradorException.Create(log) ; //Logou, agora manda pra cima
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
    FreeAndNil(params);
  end;
end;

procedure TDataIntegradorModuloWeb.Log(const aLog, aClasse: string);
begin
  if (FDataLog <> nil) then
    FDataLog.log(aLog, aClasse);
end;

procedure TDataIntegradorModuloWeb.SetDataLog(const Value: TDataLog);
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
          Result := node.Text;
      end;
    end;
  finally
    CoUninitialize;
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
var
  qry: TSQLDataSet;
  i: integer;
begin
  qry := dmPrincipal.getQuery;
  try
    qry.commandText := 'SELECT * FROM ' + tabelaDetalhe.nomeTabela + ' where ' + tabelaDetalhe.nomeFK +
      ' = ' + IntToStr(valorPK) + self.getAdditionalDetailFilter;
    qry.Open;
    while not qry.Eof do
    begin
      addTranslatedParams(qry, params, tabelaDetalhe.translations, tabelaDetalhe.nomeParametro);
      for i := low(tabelaDetalhe.tabelasDetalhe) to high(tabelaDetalhe.tabelasDetalhe) do
        addTabelaDetalheParams(qry.fieldByName(tabelaDetalhe.nomePK).AsInteger, params, tabelaDetalhe.tabelasDetalhe[i]);
      qry.Next;
    end;
  finally
    FreeAndNil(qry);
  end;
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
        if (Self.FthreadControl <> nil) and (not Self.FthreadControl.getShouldContinue) then
          break;

        if notifier <> nil then
        begin
          notifier.setCustomMessage('Salvando ' + getHumanReadableName +
            ' ' + IntToStr(n) + '/' + IntToStr(total));
        end;
        inc(n);
        saveRecordToRemote(qry, salvou, http);
        qry.Next;
      end;
      if notifier <> nil then
        notifier.unflagSalvandoDadosServidor;
      Self.log('Commitando post de records para remote. Classe: ' + ClassName, 'Sync')
    except
      Self.log('Erro no processamento do postRecordsToRemote. Classe: ' + ClassName, 'Sync');
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
        result := translations[i].pdv;
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
  useMultipartParams := false;
  zippedPost := true;
end;


destructor TDataIntegradorModuloWeb.Destroy;
var
  i: integer;
begin
  for i := Low(Self.tabelasDetalhe) to High(Self.tabelasDetalhe) do
     Self.tabelasDetalhe[i].Free;
  inherited;
end;

function TDataIntegradorModuloWeb.translateValueToServer(translation: TNameTranslation;
  fieldName: string; field: TField; nestedAttribute: string = ''; fkName: string = ''): string;
var
  lookupIdRemoto: integer;
  fk: string;
begin
  if translation.lookupRemoteTable <> '' then
  begin
    if Trim(field.asString) <> EmptyStr then
    begin
      if fkName = '' then
        fk := translation.pdv
      else
        fk := fkName;
      lookupIdRemoto := dmPrincipal.getSQLIntegerResult('SELECT idRemoto FROM ' +
        translation.lookupRemoteTable +
        ' WHERE ' + fk + ' = ' + field.AsString);
      if lookupIdRemoto > 0 then
        result := IntToStr(lookupIdRemoto)
      else
        result := '';
    end
    else
      result := '';
  end
  else
  begin
    if field.DataType in [ftFloat, ftBCD, ftFMTBCD, ftCurrency] then
    begin
      try
        {$IFDEF VER150}
        DecimalSeparator := '.';
        ThousandSeparator := #0;
        {$ELSE}
        FormatSettings.DecimalSeparator := '.';
        FormatSettings.ThousandSeparator := #0;
        {$ENDIF}
        result := field.AsString;
      finally

        {$IFDEF VER150}
        DecimalSeparator := ',';
        ThousandSeparator := '.';
        {$ELSE}
        FormatSettings.DecimalSeparator := ',';
        FormatSettings.ThousandSeparator := '.';
        {$ENDIF}
      end;
    end
    else if field.DataType in [ftDateTime, ftTimeStamp] then
    begin
      if field.IsNull then
        result := 'NULL'
      else
        //result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', field.AsDateTime);
        result := FormatDateTime(Self.getDateFormat , field.AsDateTime);
    end
    else if field.DataType in [ftDate] then
    begin
      if field.IsNull then
        result := 'NULL'
      else
        result := FormatDateTime('yyyy-mm-dd', field.AsDateTime);
    end
    else
      result := field.asString;
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
begin
  FdmPrincipal := Value;
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

end.
