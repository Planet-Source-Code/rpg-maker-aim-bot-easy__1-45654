VERSION 5.00
Begin VB.Form frmSplash 
   BackColor       =   &H00000000&
   BorderStyle     =   3  'Fixed Dialog
   ClientHeight    =   4245
   ClientLeft      =   255
   ClientTop       =   1410
   ClientWidth     =   7380
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   Icon            =   "frmSplash.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4245
   ScaleWidth      =   7380
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer2 
      Interval        =   10
      Left            =   960
      Top             =   240
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   360
      Top             =   120
   End
   Begin VB.Line Line11 
      BorderColor     =   &H00FFFFFF&
      Index           =   2
      X1              =   6240
      X2              =   2040
      Y1              =   3120
      Y2              =   3120
   End
   Begin VB.Line Line11 
      BorderColor     =   &H00FFFFFF&
      Index           =   1
      X1              =   5520
      X2              =   2040
      Y1              =   1680
      Y2              =   1680
   End
   Begin VB.Line Line4 
      BorderColor     =   &H00FFFFFF&
      Index           =   1
      X1              =   6240
      X2              =   6240
      Y1              =   2760
      Y2              =   3120
   End
   Begin VB.Line Line5 
      BorderColor     =   &H00FFFFFF&
      BorderStyle     =   6  'Inside Solid
      X1              =   5520
      X2              =   6240
      Y1              =   2760
      Y2              =   2760
   End
   Begin VB.Label Label44 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "% Loaded"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3000
      TabIndex        =   38
      Top             =   1440
      Width           =   705
   End
   Begin VB.Line Line16 
      BorderColor     =   &H00FFFFFF&
      X1              =   2880
      X2              =   4200
      Y1              =   1320
      Y2              =   1320
   End
   Begin VB.Line Line15 
      BorderColor     =   &H00FFFFFF&
      X1              =   4200
      X2              =   4200
      Y1              =   1320
      Y2              =   1680
   End
   Begin VB.Line Line14 
      BorderColor     =   &H00FFFFFF&
      X1              =   2880
      X2              =   2880
      Y1              =   1320
      Y2              =   1680
   End
   Begin VB.Label Label43 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3720
      TabIndex        =   37
      Top             =   1440
      Width           =   90
   End
   Begin VB.Label Label42 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "100 %"
      ForeColor       =   &H000000FF&
      Height          =   195
      Left            =   5640
      TabIndex        =   36
      Top             =   2880
      Visible         =   0   'False
      Width           =   435
   End
   Begin VB.Label Label36 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4800
      TabIndex        =   35
      Top             =   2880
      Width           =   90
   End
   Begin VB.Label Label35 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3480
      TabIndex        =   34
      Top             =   2880
      Width           =   90
   End
   Begin VB.Label Label34 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2160
      TabIndex        =   33
      Top             =   2880
      Width           =   90
   End
   Begin VB.Line Line11 
      BorderColor     =   &H00FFFFFF&
      Index           =   0
      X1              =   5520
      X2              =   2040
      Y1              =   2760
      Y2              =   2760
   End
   Begin VB.Line Line4 
      BorderColor     =   &H00FFFFFF&
      Index           =   0
      X1              =   5520
      X2              =   5520
      Y1              =   1680
      Y2              =   3120
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00FFFFFF&
      X1              =   4680
      X2              =   4680
      Y1              =   1680
      Y2              =   3120
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      X1              =   3360
      X2              =   3360
      Y1              =   1680
      Y2              =   3120
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      X1              =   2040
      X2              =   2040
      Y1              =   1680
      Y2              =   3120
   End
   Begin VB.Label Label33 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4200
      TabIndex        =   32
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label32 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4800
      TabIndex        =   31
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label31 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   5160
      TabIndex        =   30
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label30 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3840
      TabIndex        =   29
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label29 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3480
      TabIndex        =   28
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label28 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2880
      TabIndex        =   27
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label27 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2520
      TabIndex        =   26
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label26 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2160
      TabIndex        =   25
      Top             =   2520
      Width           =   90
   End
   Begin VB.Label Label25 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4200
      TabIndex        =   24
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label24 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4800
      TabIndex        =   23
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label23 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   5160
      TabIndex        =   22
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label22 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3840
      TabIndex        =   21
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label21 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3480
      TabIndex        =   20
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label20 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2880
      TabIndex        =   19
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label19 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2520
      TabIndex        =   18
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label18 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2160
      TabIndex        =   17
      Top             =   2280
      Width           =   90
   End
   Begin VB.Label Label17 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4200
      TabIndex        =   16
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label16 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4800
      TabIndex        =   15
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label15 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   5160
      TabIndex        =   14
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label14 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3840
      TabIndex        =   13
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label13 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3480
      TabIndex        =   12
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label12 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2880
      TabIndex        =   11
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label11 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2520
      TabIndex        =   10
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label10 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2160
      TabIndex        =   9
      Top             =   2040
      Width           =   90
   End
   Begin VB.Label Label9 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4200
      TabIndex        =   8
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label8 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   4800
      TabIndex        =   7
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label7 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   5160
      TabIndex        =   6
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label6 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3840
      TabIndex        =   5
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   3480
      TabIndex        =   4
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2880
      TabIndex        =   3
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2520
      TabIndex        =   2
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   2160
      TabIndex        =   1
      Top             =   1800
      Width           =   90
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackColor       =   &H00000000&
      Caption         =   "Loading"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   41.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   975
      Left            =   2280
      TabIndex        =   0
      Top             =   120
      Width           =   2850
   End
End
Attribute VB_Name = "frmSplash"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Timer1_Timer()
Label2.Caption = Label2 + 1
Label3.Caption = Label2 + 12
Label4.Caption = Label2 + 13
Label5.Caption = Label2 + 54
Label6.Caption = Label2 + 56
Label7.Caption = Label2 + 17
Label8.Caption = Label2 + 435
Label9.Caption = Label2 + 324
Label10.Caption = Label2 + 657
Label11.Caption = Label2 + 455
Label12.Caption = Label2 + 65
Label13.Caption = Label2 + 312
Label14.Caption = Label2 + 123
Label15.Caption = Label2 + 423
Label16.Caption = Label2 + 654
Label17.Caption = Label2 + 76
Label18.Caption = Label2 + 52
Label19.Caption = Label2 + 23
Label20.Caption = Label2 + 14
Label21.Caption = Label2 + 435
Label22.Caption = Label2 + 14
Label23.Caption = Label2 + 53
Label24.Caption = Label2 + 11
Label25.Caption = Label2 + 13
Label26.Caption = Label2 + 543
Label27.Caption = Label2 + 76
Label28.Caption = Label2 + 123
Label29.Caption = Label2 + 123
Label30.Caption = Label2 + 543
Label31.Caption = Label2 + 213
Label32.Caption = Label2 + 543
Label33.Caption = Label2 + 123
Label34.Caption = Label2 + Label10 + Label18 + Label26
Label35.Caption = Label3 + Label11 + Label19 + Label27
Label36.Caption = Label3 + Label11



If Label43.Caption = 100 Then
Label2.Caption = "100"
Label3.Caption = "100"
Label4.Caption = "100"
Label5.Caption = "100"
Label6.Caption = "100"
Label7.Caption = "100"
Label8.Caption = "100"
Label9.Caption = "100"
Label10.Caption = "100"
Label11.Caption = "100"
Label12.Caption = "100"
Label13.Caption = "100"
Label14.Caption = "100"
Label15.Caption = "100"
Label16.Caption = "100"
Label17.Caption = "100"
Label18.Caption = "100"
Label19.Caption = "100"
Label20.Caption = "100"
Label21.Caption = "100"
Label22.Caption = "100"
Label23.Caption = "100"
Label24.Caption = "100"
Label25.Caption = "100"
Label26.Caption = "100"
Label27.Caption = "100"
Label28.Caption = "100"
Label29.Caption = "100"
Label30.Caption = "100"
Label31.Caption = "100"
Label32.Caption = "100"
Label33.Caption = "100"
Label34.Caption = "10000000000"
Label35.Caption = "10000000000"
Label36.Caption = "1000000"
Timer1.Enabled = False
Label42.Visible = True
Timer2.Interval = 500
End If
End Sub


Private Sub Timer2_Timer()
Label43.Caption = Label43 + 1
If Label43 = "105" Then
Unload Me
form1.Show
End If
End Sub
