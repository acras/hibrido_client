object DataSincronizadorModuloWeb: TDataSincronizadorModuloWeb
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 255
  Width = 443
  object sincronizaRetaguardaTimer: TTimer
    Interval = 900000
    OnTimer = sincronizaRetaguardaTimerTimer
    Left = 144
    Top = 24
  end
end
