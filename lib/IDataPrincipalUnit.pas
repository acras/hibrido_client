unit IDataPrincipalUnit;

interface

uses IBQuery;

type

IDataPrincipal = interface
  procedure startTransaction; overload;
  procedure startTransaction(name: string); overload;
  procedure commit; overload;
  procedure commit(name: string); overload;
  function getSQLResult(sqlText: string): variant;
  function getSQLIntegerResult(sqlText: string): integer;
  procedure execSQL(sqlText: string);
  procedure refreshData;
  function getQuery: TIBQuery;
end;

implementation

end.
