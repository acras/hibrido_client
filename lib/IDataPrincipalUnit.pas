unit IDataPrincipalUnit;

interface

uses Data.SqlExpr;

type

IDataPrincipal = interface
  procedure startTransaction; overload;                     //Inicia uma transa��o no banco de dados

  procedure commit; overload;                               //faz commit dos dados da transa��o
                                                            //gen�rica

  procedure rollback; overload;                             //faz o rollback da transa��o ativa

  function getSQLResult(sqlText: string): variant;          //retorna o resultado de uma query
                                                            //ele pode ser de qualquer tipo

  function getSQLIntegerResult(sqlText: string): integer;   //retorna um resultado inteiro
                                                            //de uma query. �til em queries de count e max

  procedure execSQL(sqlText: string; retries: integer = 0);                       //executa um comando SQL sem resultado

  procedure refreshData;                                    //faz refresh dos dados do client
                                                            //em firebird por exemplo � um Commit Work

  function getQuery: TSQLDataSet;                              //retorna um TSQLDataSet conectado na base
  function sincronizar: boolean;                            //indica se deve-se ou n�o sincronizar
                                                            //com a parte web. �til quando queremos ter
                                                            //clientes que sincronizam e outros que n�o sincronizam

end;

implementation

end.
