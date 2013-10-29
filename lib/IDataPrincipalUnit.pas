unit IDataPrincipalUnit;

interface

uses Data.SqlExpr;

type

IDataPrincipal = interface
  procedure startTransaction; overload;                     //Inicia uma transação no banco de dados

  procedure startTransaction(name: string); overload;       //Inicia uma transação nomeada no banco
                                                            //de dados. Geralmente pode chamar a
                                                            //não nomeada simplesmente.

  procedure commit; overload;                               //faz commit dos dados da transação
                                                            //genérica

  procedure commit(name: string); overload;                 //faz o commit de transação nomeada

  procedure rollback; overload;                             //faz o rollback da transação ativa

  procedure rollback(name: string); overload;               //faz o rollback da transação nomeada


  function getSQLResult(sqlText: string): variant;          //retorna o resultado de uma query
                                                            //ele pode ser de qualquer tipo

  function getSQLIntegerResult(sqlText: string): integer;   //retorna um resultado inteiro
                                                            //de uma query. Útil em queries de count e max

  procedure execSQL(sqlText: string; retries: integer = 0);                       //executa um comando SQL sem resultado

  procedure refreshData;                                    //faz refresh dos dados do client
                                                            //em firebird por exemplo é um Commit Work

  function getQuery: TSQLQuery;                              //retorna um TSQLquery conectado na base
  function sincronizar: boolean;                            //indica se deve-se ou não sincronizar
                                                            //com a parte web. Útil quando queremos ter
                                                            //clientes que sincronizam e outros que não sincronizam

end;

implementation

end.
