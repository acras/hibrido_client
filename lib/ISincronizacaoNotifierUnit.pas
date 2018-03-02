unit ISincronizacaoNotifierUnit;

interface

Uses System.Generics.Collections;

type
  TCustomParams = class(TDictionary<string, string>)
  public
    function getDefaultParams: string; virtual; abstract;
  end;

  ISincronizacaoNotifier = interface
    procedure flagSalvandoDadosServidor;
    procedure unflagSalvandoDadosServidor;
    procedure flagBuscandoDadosServidor;
    procedure unflagBuscandoDadosServidor;
    procedure setCustomMessage(content: string);
  end;

  IThreadControl = interface
    function getShouldContinue: boolean;
  end;

  ILog = interface
    procedure log(const mensagem: string; const classe: string = ''; newLine: boolean = true; timestamp: boolean = true);
  end;

  ICustomParams = interface
    function getCustomParams: TCustomParams;
    function getJsonFromServer(var aRestrictPosters: boolean): string;
  end;

implementation

end.
