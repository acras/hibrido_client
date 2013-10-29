unit ISincronizacaoNotifierUnit;

interface

type

ISincronizacaoNotifier = interface
  procedure flagSalvandoDadosServidor;
  procedure unflagSalvandoDadosServidor;
  procedure flagBuscandoDadosServidor;
  procedure unflagBuscandoDadosServidor;
  procedure setCustomMessage(content: string);
end;

implementation

end.
