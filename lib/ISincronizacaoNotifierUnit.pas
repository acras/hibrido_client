unit ISincronizacaoNotifierUnit;

interface

type

ISincronizacaoNotifier = interface
  procedure flagSalvandoDadosServidor;
  procedure unflagSalvandoDadosServidor;
  procedure flagBuscandoDadosServidor;
  procedure unflagBuscandoDadosServidor;
end;

implementation

end.
