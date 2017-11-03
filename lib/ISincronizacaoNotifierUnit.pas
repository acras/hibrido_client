unit ISincronizacaoNotifierUnit;

interface

Uses System.Generics.Collections;

type
  TCustomParams = TDictionary<string, string>;

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
  end;

implementation

end.
