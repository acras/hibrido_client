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

  ICustomParams = interface
    function getCustomParams: TCustomParams;
  end;

implementation

end.
