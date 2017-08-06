object Form1: TForm1
  Left = 749
  Top = 242
  Width = 913
  Height = 702
  Caption = 'FARG Framework: NUMBO'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 656
    Top = 448
    Width = 179
    Height = 140
    Brush.Color = clYellow
    Shape = stCircle
  end
  object Shape2: TShape
    Left = 624
    Top = 120
    Width = 0
    Height = 65
    Shape = stCircle
  end
  object Shape3: TShape
    Left = 688
    Top = 240
    Width = 0
    Height = 65
    Shape = stCircle
  end
  object Shape4: TShape
    Left = 592
    Top = 352
    Width = 0
    Height = 65
    Shape = stCircle
  end
  object Shape5: TShape
    Left = 464
    Top = 264
    Width = 0
    Height = 65
    Shape = stCircle
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 905
    Height = 668
    ActivePage = TabSheet5
    Align = alClient
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = 'Activation1Unit Test'
      ImageIndex = 2
      object Image2: TImage
        Left = 424
        Top = 80
        Width = 400
        Height = 400
      end
      object Memo3: TMemo
        Left = 0
        Top = 0
        Width = 385
        Height = 640
        Align = alLeft
        Lines.Strings = (
          
            'Activation tester:  Select a number of increase steps, and then ' +
            'press the button '
          'to test activation values (Sigmoid function)'
          ''
          ''
          '--'
          '')
        TabOrder = 0
      end
      object Button9: TButton
        Left = 424
        Top = 16
        Width = 145
        Height = 25
        Caption = 'Test activation'
        TabOrder = 1
        OnClick = TestActivation
      end
      object SpinEdit7: TSpinEdit
        Left = 424
        Top = 48
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 400
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Node1Unit Test'
      object Shape11: TShape
        Left = 544
        Top = 104
        Width = 65
        Height = 65
        Brush.Color = clActiveBorder
        Pen.Color = clMoneyGreen
        Pen.Width = 2
        Shape = stCircle
      end
      object Shape12: TShape
        Left = 640
        Top = 184
        Width = 65
        Height = 65
        Brush.Color = clActiveBorder
        Pen.Color = clMoneyGreen
        Pen.Width = 2
        Shape = stCircle
      end
      object Shape13: TShape
        Left = 600
        Top = 296
        Width = 65
        Height = 65
        Brush.Color = clActiveBorder
        Pen.Color = clMoneyGreen
        Pen.Width = 2
        Shape = stCircle
      end
      object Shape14: TShape
        Left = 480
        Top = 296
        Width = 65
        Height = 65
        Brush.Color = clActiveBorder
        Pen.Color = clMoneyGreen
        Pen.Width = 2
        Shape = stCircle
      end
      object Shape15: TShape
        Left = 432
        Top = 184
        Width = 65
        Height = 65
        Brush.Color = clActiveBorder
        Pen.Color = clMoneyGreen
        Pen.Width = 2
        Shape = stCircle
      end
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 377
        Height = 640
        Align = alLeft
        Lines.Strings = (
          'Pre-Slipnet NUMBO tester')
        TabOrder = 0
      end
      object Button3: TButton
        Left = 392
        Top = 0
        Width = 129
        Height = 25
        Caption = 'Node 1 max activation'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 392
        Top = 24
        Width = 129
        Height = 25
        Caption = 'Nodes 1 and 4 activated'
        Enabled = False
        TabOrder = 2
        OnClick = Button4Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'NumboSlipnet1Unit Test'
      ImageIndex = 1
      object Image1: TImage
        Left = 0
        Top = 502
        Width = 897
        Height = 138
        Align = alBottom
        OnClick = Image1Click
        OnMouseMove = Image1MouseMove
      end
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 345
        Height = 489
        Lines.Strings = (
          'NUMBO Slipnet tester'
          ''
          
            'Creates the Numbo slipnet and lets you see how activation spread' +
            's, '
          'given a specific number'
          '--')
        TabOrder = 0
      end
      object Button1: TButton
        Left = 368
        Top = 0
        Width = 185
        Height = 25
        Caption = 'Create Slipnet Numbers'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 368
        Top = 24
        Width = 185
        Height = 25
        Caption = 'Create Proximity Links'
        Enabled = False
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button5: TButton
        Left = 368
        Top = 48
        Width = 185
        Height = 25
        Caption = 'Create Links to Salient Numbers'
        Enabled = False
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 624
        Top = 0
        Width = 145
        Height = 25
        Caption = 'Show activations'
        Enabled = False
        TabOrder = 4
        OnClick = Button6Click
      end
      object SpinEdit1: TSpinEdit
        Left = 648
        Top = 32
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 11
      end
      object SpinEdit2: TSpinEdit
        Left = 648
        Top = 56
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 20
      end
      object SpinEdit3: TSpinEdit
        Left = 648
        Top = 80
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 7
      end
      object SpinEdit4: TSpinEdit
        Left = 648
        Top = 104
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 8
        Value = 1
      end
      object Button8: TButton
        Left = 368
        Top = 72
        Width = 185
        Height = 25
        Caption = 'Create Multiplication Nodes and Links'
        Enabled = False
        TabOrder = 10
        OnClick = Button8Click
      end
      object Button7: TButton
        Left = 368
        Top = 96
        Width = 185
        Height = 25
        Caption = 'Create Addition Nodes and Links'
        Enabled = False
        TabOrder = 9
        OnClick = Button8Click
      end
      object SpinEdit5: TSpinEdit
        Left = 648
        Top = 128
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 11
        Value = 6
      end
      object SpinEdit6: TSpinEdit
        Left = 648
        Top = 152
        Width = 60
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 12
        Value = 114
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Chunk1Unit Test'
      ImageIndex = 3
      object Memo4: TMemo
        Left = 0
        Top = 0
        Width = 297
        Height = 640
        Align = alLeft
        Lines.Strings = (
          'Memo4')
        TabOrder = 0
      end
      object Button11: TButton
        Left = 320
        Top = 64
        Width = 137
        Height = 25
        Caption = 'Create Working Memory'
        TabOrder = 1
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 320
        Top = 88
        Width = 137
        Height = 25
        Caption = 'Button12'
        TabOrder = 2
      end
      object Button13: TButton
        Left = 320
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Button13'
        TabOrder = 3
        OnClick = TestChunk
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Hedonic feedback'
      ImageIndex = 4
      DesignSize = (
        897
        640)
      object Image3: TImage
        Left = 424
        Top = 280
        Width = 473
        Height = 361
      end
      object Memo5: TMemo
        Left = 0
        Top = 0
        Width = 417
        Height = 639
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'Memo5')
        TabOrder = 0
      end
      object Button10: TButton
        Left = 512
        Top = 48
        Width = 153
        Height = 25
        Caption = 'Get Value in N1'
        Enabled = False
        TabOrder = 1
        OnClick = Command_GET_A
      end
      object Button14: TButton
        Left = 512
        Top = 72
        Width = 153
        Height = 25
        Caption = 'Get Value in N2'
        Enabled = False
        TabOrder = 2
        OnClick = Command_GET_B
      end
      object Button15: TButton
        Left = 512
        Top = 96
        Width = 153
        Height = 25
        Caption = 'N1>N2?'
        Enabled = False
        TabOrder = 3
        OnClick = Command_compare
      end
      object Button16: TButton
        Left = 512
        Top = 120
        Width = 153
        Height = 25
        Caption = 'Switch (N1,N2)!!!'
        Enabled = False
        TabOrder = 4
        OnClick = Switch
      end
      object Button17: TButton
        Left = 512
        Top = 144
        Width = 153
        Height = 25
        Caption = 'Yes?'
        Enabled = False
        TabOrder = 5
        OnClick = yes
      end
      object Button18: TButton
        Left = 512
        Top = 168
        Width = 153
        Height = 25
        Caption = 'Do nothing'
        Enabled = False
        TabOrder = 6
        OnClick = Nothing1
      end
      object Button19: TButton
        Left = 512
        Top = 192
        Width = 153
        Height = 25
        Caption = 'Do nothing'
        Enabled = False
        TabOrder = 7
        OnClick = Nothing2
      end
      object Button20: TButton
        Left = 512
        Top = 216
        Width = 153
        Height = 25
        Caption = 'No'
        Enabled = False
        TabOrder = 8
        OnClick = No
      end
      object Button21: TButton
        Left = 520
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Init'
        TabOrder = 9
        OnClick = Init
      end
      object Button22: TButton
        Left = 512
        Top = 248
        Width = 153
        Height = 25
        Caption = 'Optimum Command'
        Enabled = False
        TabOrder = 10
        OnClick = OptimumCommand
      end
      object Button25: TButton
        Left = 736
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Coderack run!'
        Enabled = False
        TabOrder = 11
        OnClick = Button25Click
      end
      object Button26: TButton
        Left = 736
        Top = 128
        Width = 153
        Height = 25
        Caption = 'Print Commands'
        TabOrder = 12
        OnClick = Button26Click
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Relative Probability'
      ImageIndex = 5
      object Memo6: TMemo
        Left = 0
        Top = 0
        Width = 369
        Height = 640
        Align = alLeft
        Lines.Strings = (
          'Memo6')
        TabOrder = 0
      end
      object Button23: TButton
        Left = 464
        Top = 88
        Width = 129
        Height = 25
        Caption = 'feedback'
        TabOrder = 1
        OnClick = Button23Click
      end
      object Button24: TButton
        Left = 464
        Top = 112
        Width = 129
        Height = 25
        Caption = 'Decay'
        Enabled = False
        TabOrder = 2
        OnClick = Button24Click
      end
      object SpinEdit8: TSpinEdit
        Left = 592
        Top = 88
        Width = 121
        Height = 22
        Increment = 15
        MaxValue = 100
        MinValue = 0
        TabOrder = 3
        Value = 50
      end
    end
  end
end
