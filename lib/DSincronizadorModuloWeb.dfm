object DataSincronizadorModuloWeb: TDataSincronizadorModuloWeb
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 139
  Top = 95
  Height = 255
  Width = 443
  object sincronizaRetaguardaTimer: TTimer
    Interval = 900000
    OnTimer = sincronizaRetaguardaTimerTimer
    Left = 144
    Top = 24
  end
end
