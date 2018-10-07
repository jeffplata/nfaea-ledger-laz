unit ResourceDM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

const
  iindBtnFilterCancel = 0;
  iindBtnPrint = 1;
  iindBtnDummy = 99;

type

  { TdmResources }

  TdmResources = class(TDataModule)
    imlButtonGlyphs: TImageList;
  private

  public

  end;

var
  dmResources: TdmResources;

implementation

{$R *.lfm}

end.

