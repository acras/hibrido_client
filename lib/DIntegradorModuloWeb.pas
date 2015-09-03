unit DIntegradorModuloWeb;

interface

uses
  SysUtils, ExtCtrls, DBClient, idHTTP, MSXML2_TLB, dialogs, acStrUtils, acNetUtils,
  DB, IdMultipartFormData, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdCoder, IdCoder3to4, IdCoderUUE, IdCoderXXE, Controls,
  IDataPrincipalUnit, idURI, System.Classes, Windows,
  ISincronizacaoNotifierUnit, Data.SqlExpr, ABZipper, ABUtils, AbZipTyp, AbArcTyp, AbZipPrc;

type
  TNameTranslation = record
    server: string;
    pdv: string;
    lookupRemoteTable: string;
  end;

  TTranslationSet = class
    protected
      translations: array of TNameTranslation;
    public
      constructor create(owner: TComponent);
      procedure add(serverName, pdvName: string;
        lookupRemoteTable: string = '');
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
    tabelasDetalhe: array of TTabelaDetalhe;
    translations: TTranslationSet;
    constructor create;
  end;

  TDataIntegradorModuloWeb = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    FdmPrincipal: IDataPrincipal;
    Fnotifier: ISincronizacaoNotifier;
    procedure SetdmPrincipal(const Value: IDataPrincipal);
    function getdmPrincipal: IDataPrincipal;

    procedure addTabelaDetalheParams(valorPK: integer;
      params: TStringList;
      tabelaDetalhe: TTabelaDetalhe);
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
      nestedAttribute: string = ''): string; virtual;
    function translateValueFromServer(fieldName, value: string): string; virtual;
    procedure duplicarRegistroSemOffset(ds: TDataSet);
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
    function getHumanReadableName: string; virtual;
  public
    translations: TTranslationSet;
    verbose: boolean;
    property notifier: ISincronizacaoNotifier read Fnotifier write Fnotifier;
    property dmPrincipal: IDataPrincipal read getdmPrincipal write SetdmPrincipal;
    function buildRequestURL(nomeRecurso: string; params: string = ''): string; virtual; abstract;
    function getDadosAtualizados: TClientDataset;
    function saveRecordToRemote(ds: TDataSet; var salvou: boolean): IXMLDomDocument2;
    procedure migrateTableToRemote(where: string = '');
    procedure migrateSingletonTableToRemote;
    procedure postRecordsToRemote;
    class procedure updateDataSets; virtual;
  end;

  TDataIntegradorModuloWebClass = class of TDataIntegradorModuloWeb;

var
  DataIntegradorModuloWeb: TDataIntegradorModuloWeb;
implementation

uses AguardeFormUn, ComObj, DLog;

{$R *.dfm}

function TDataIntegradorModuloWeb.extraGetUrlParams: String;
begin
  result := '';
end;

function TDataIntegradorModuloWeb.getDadosAtualizados: TClientDataset;
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
    url := getRequestUrlForAction(false, ultimaVersao) + extraGetUrlParams;
    notifier.setCustomMessage('Buscando ' + getHumanReadableName + '...');
    numRegistros := 0;
    xmlContent := getRemoteXmlContent(url);

    if trim(xmlContent) <> '' then
    begin
      doc := CoDOMDocument60.Create;
      doc.loadXML(xmlContent);
      list := doc.selectNodes('/' + dasherize(nomePlural) + '//' + dasherize(nomeSingular));
      numRegistros := list.length;
      notifier.setCustomMessage(IntToStr(numRegistros) + ' novos');
      for i := 0 to numRegistros-1 do
      begin
        notifier.setCustomMessage('Importando ' + getHumanReadableName + ': ' + IntToStr(i+1) +
          '/' + IntToStr(numRegistros));
        node := list.item[i];
        if node<>nil then
          importRecord(node);
      end;
    end;
    keepImporting := (maxRecords > 0) and (numRegistros >= maxRecords);
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
          ds.fieldByName(translations.get(i).pdv), nestedAttribute);
      //params.Add(nome + '=' + TIdURI.ParamsEncode(valor));
      params.Add(nome + '=' + valor);
    end;
  end;
end;

function TDataIntegradorModuloWeb.saveRecordToRemote(ds: TDataSet; var salvou: boolean): IXMLDomDocument2;
var
  http: TIdHTTP;
  params: TStringList;
  multipartParams: TidMultipartFormDataStream;
  xmlContent: string;
  doc: IXMLDomDocument2;
  i, idRemoto: integer;
  nome, nomeCampo, valor, txtUpdate: string;
  sucesso: boolean;
  stream: TStringStream;
  zippedParams: TMemoryStream;
  zipper: TAbZipper;
  url, s: string;
  a,b: cardinal;
begin
  DataLog.log('Iniciando save record para remote. Classe: ' + ClassName, 'Sync');
  salvou := false;
  http := TIdHTTP.Create(nil);
  params := TStringList.Create;
  multiPartParams := TIdMultiPartFormDataStream.Create;
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
          stream := TStringStream.Create('');
          prepareMultipartParams(ds, params, multipartParams);
          http.Post(getRequestUrlForAction(true), multipartParams, stream);
          xmlContent := stream.ToString;
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
            params.Delimiter := '&';
            params.QuoteChar := '&';
            s := params.DelimitedText;
            stream := TStringStream.Create(utf8Encode(s));
            try
              zippedParams := TMemoryStream.Create;
              zipper := TAbZipper.Create(nil);
              zipper.ArchiveType := atGzip;
              zipper.ForceType := true;
              zipper.Stream := zippedParams;
              zipper.AddFromStream('', stream);
              http.Request.contentEncoding := 'gzip';
              xmlContent := http.Post(url, zippedParams);
            finally
              if zipper <> nil then
                freeAndNil(zipper);
              if zippedParams <> nil then
                freeAndNil(zippedParams);
            end;
          end
          else
          begin
            xmlContent := http.Post(url, Params);
          end;
        end;
        sucesso := true;
        {$IFDEF VER150}
        doc := CoDOMDocument.Create;
        {$ELSE}
        doc := CoDOMDocument60.Create;
        {$ENDIF}
        doc.loadXML(xmlContent);
        result := doc;
        if duasVias or clientToServer then
        begin
          txtUpdate := 'UPDATE ' + nomeTabela + ' SET salvouRetaguarda = ' + QuotedStr('S');

          if duasVias then
          begin
            idRemoto := strToInt(doc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id').text);
            txtUpdate := txtUpdate + ', idRemoto = ' + IntToStr(idRemoto);
          end;

          txtUpdate := txtUpdate + ' WHERE ' + nomePKLocal + ' = ' + ds.fieldByName(nomePKLocal).AsString;

          //da a chance da classe gerenciar redirecionamentos, por exemplo ao descobrir que este registro já
          //existia no remoto e era outro registro neste banco de dados.
          if not gerenciaRedirecionamentos(ds.fieldByName(nomePKLocal).AsInteger, idRemoto) then
            dmPrincipal.execSQL(txtUpdate);

          dmPrincipal.refreshData;
        end;
      except
        on e: Exception do
        begin
          DataLog.log('Erro ao tentar salvar registro. Classe: ' + ClassName +
            '. Erro: ' + e.Message, 'Sync');
          raise; //Logou, agora manda pra cima
        end;
      end;
    end;
    salvou := sucesso;
  finally
    FreeAndNil(http);
    FreeAndNil(params);
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
      ' = ' + IntToStr(valorPK);
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


procedure TDataIntegradorModuloWeb.postRecordsToRemote;
var
  qry: TSQLDataSet;
  salvou: boolean;
begin
  qry := dmPrincipal.getQuery;
  dmPrincipal.startTransaction;
  try try
    DataLog.log('Selecionando registros para sincronização. Classe: ' + ClassName, 'Sync');
    qry.commandText := 'SELECT * from ' + nomeTabela + ' where ((salvouRetaguarda = ' + QuotedStr('N') + ') or (salvouRetaguarda is null)) '
      + getAdditionalSaveConditions;
    qry.Open;
    qry.First;
    while not qry.Eof do
    begin
      saveRecordToRemote(qry, salvou);
      qry.Next;
    end;
    dmPrincipal.commit;
    DataLog.log('Commitando post de records para remote. Classe: ' + ClassName, 'Sync')
  except
    DataLog.log('Erro no processamento do postRecordsToRemote. Classe: ' + ClassName, 'Sync');
    raise;
  end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TDataIntegradorModuloWeb.migrateTableToRemote(where: string = '');
var
  qry: TSQLDataSet;
  doc: IXMLDomDocument2;
  list : IXMLDomNodeList;
  node : IXMLDomNode;
  idRemoto: integer;
  salvou: boolean;
  log: TextFile;
begin
  offset := dmPrincipal.getSQLIntegerResult('SELECT max(' + nomePKLocal + ' + 1) from ' +
    nomeTabela + ' ');
  qry := dmPrincipal.getQuery;
  qry.CommandText := 'SELECT * from ' + nomeTabela + ' where ' + nomePKLocal + ' = :' + nomePKLocal;;
  dmPrincipal.startTransaction;
  try
    qry.commandText := 'SELECT * from ' + nomeTabela + ' ' + where + ' order by ' + getOrderBy;
    qry.Open;
    qry.First;
    AguardeForm.total := qry.RecordCount;
    AguardeForm.current := 1;
    AguardeForm.mostrar('Migrando para a web ' + nomeTabela,0,0,true);
    ReWrite(log);

    while not qry.Eof do
    begin
      qry.Refresh;
      try
        doc := saveRecordToRemote(qry, salvou);
        if salvou and not(duasVias) then
        begin
          //no resp virá o id e os dados, devemos salvar porém com o id somado
          idRemoto := strToInt(doc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id').text);
          doc.selectSingleNode('//' + dasherize(nomeSingularSave) + '//id').text :=
            IntToStr(idRemoto + offset);
          importRecord(doc.selectSingleNode('//' + dasherize(nomeSingularSave)));
          redirectRecord(qry.FieldByName(nomePKLocal).AsInteger, idRemoto + offset);
          Writeln(log, 'Redirecionando. De ' + qry.FieldByName(nomePKLocal).asString + ' -> ' + intToStr(idRemoto + offset));
        end;
      except
        //se der erro ao salvar um registro eu vou redirecionar para outro id, por exemplo
        //  em um produto eu posso redirecionar para um produto padrão, genérico, que será
        //  usado para este fim.
        write(log, 'Erro no item ' + qry.fieldByName(nomePKLocal).asString);

      end;

      qry.Next;
      AguardeForm.current := AguardeForm.current + 1;
    end;
    dmPrincipal.commit;
    AguardeForm.esconder;
    if not(duasVias) then
    begin
      //Segunda passada. Agora com o espaço liberado e os registros já semi-integrados
      //ao remoto, faltando apenas o ajuste do id
      qry.close;
      qry.commandText := 'SELECT * from ' + nomeTabela + ' ' + where + ' order by ' + nomePKLocal;
      qry.Open;
      AguardeForm.total := qry.RecordCount;
      AguardeForm.current := 0;
      AguardeForm.mostrar('Corrigindo ids. Esta operação poder ser bastante demorada.', 0, 0, true);
      qry.First;
      Writeln(log, '--Iniciando duplicação de volta, sem offset. Offset: ' + inttostr(offset));
      while not qry.Eof do
      begin
        qry.Refresh;
        //tira o offset e insere
        try
          Write(log, 'id: ' + qry.fieldByName(nomePKLocal).asString);
          duplicarRegistroSemOffset(qry);
          AguardeForm.current := AguardeForm.current + 1;
          WriteLn(log, '... ok');
        except
          WriteLn(log, '... erro');
        end;
        Flush(log);
        qry.Next;
      end;
    end;
  finally
    dmPrincipal.commit;
    FreeAndNil(qry);
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

procedure TDataIntegradorModuloWeb.duplicarRegistroSemOffset(ds: TDataSet);
var
  qry: TSQLDataSet;
  i: integer;
begin
  qry := dmPrincipal.getQuery;
  try
    qry.commandText := 'INSERT INTO ' + nomeTabela + '(';
    for i := 0 to ds.fieldCount -1 do
    begin
      qry.commandText := qry.commandText + ds.Fields[i].FieldName;
      if i < ds.fieldCount -1 then
        qry.commandText := qry.commandText + ', ';
    end;
    qry.commandText := qry.commandText + ') values (';
    for i := 0 to ds.fieldCount -1 do
    begin
      qry.commandText := qry.commandText + ':' + ds.Fields[i].FieldName;
      if i < ds.fieldCount -1 then
        qry.commandText := qry.commandText + ', ';
    end;
    qry.commandText := qry.commandText + ')';
    for i := 0 to ds.fieldCount -1 do
    begin
      if uppercase(ds.Fields[i].FieldName) = uppercase(nomePKLocal) then
        qry.ParamByName(nomePKLocal).Value := ds.Fields[i].AsInteger - offset
      else
        qry.ParamByName(ds.Fields[i].FieldName).Value := ds.Fields[i].Value;
    end;
    qry.ExecSQL;
    redirectRecord(ds.FieldByName(nomePKLocal).AsInteger, ds.FieldByName(nomePKLocal).AsInteger - offset);
    dmPrincipal.commit;
  finally
    FreeAndNil(qry);
  end;
end;

{ TTranslationSet }

procedure TTranslationSet.add(serverName, pdvName: string;
  lookupRemoteTable: string = '');
var
  tam: integer;
begin
  tam := length(translations);
  SetLength(translations, tam + 1);
  translations[tam].server := serverName;
  translations[tam].pdv := pdvName;
  translations[tam].lookupRemoteTable := lookupRemoteTable;
end;

procedure TDataIntegradorModuloWeb.beforeRedirectRecord(idAntigo, idNovo: integer);
begin
  //
end;

constructor TTranslationSet.create(owner: TComponent);
begin
  SetLength(translations, 0);
  add('version', 'versao');
  add('active', 'ativo');
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


function TDataIntegradorModuloWeb.translateValueToServer(translation: TNameTranslation;
  fieldName: string; field: TField; nestedAttribute: string = ''): string;
var
  lookupIdRemoto: integer;
begin
  if translation.lookupRemoteTable <> '' then
  begin
    if field.AsInteger > 0 then
    begin
      lookupIdRemoto := dmPrincipal.getSQLIntegerResult('SELECT idRemoto FROM ' +
        translation.lookupRemoteTable +
        ' WHERE ' + translation.pdv + ' = ' + field.AsString);
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
        result := FormatDateTime('dd"/"mm"/"yyyy"T"hh":"nn":"ss', field.AsDateTime);
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

{ TTabelaDetalhe }

constructor TTabelaDetalhe.create;
begin
  translations := TTranslationSet.create(nil);
end;

procedure TDataIntegradorModuloWeb.SetdmPrincipal(
  const Value: IDataPrincipal);
begin
  FdmPrincipal := Value;
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

end.
