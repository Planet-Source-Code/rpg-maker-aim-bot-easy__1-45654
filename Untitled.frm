VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.0#0"; "RICHTX32.OCX"
Begin VB.Form form1
   BackColor       =   &H80000004&
   BorderStyle     =   1
   Caption         =   "sonic 10 bot booter beta"
   ClientHeight    =   3855
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5760
   FillColor       =   &H00C0C0C0&
   Icon            =   "FORM01.FRX":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0
   ScaleHeight     =   3855
   ScaleWidth      =   5760
   StartUpPosition =   3
   Begin VB.CommandButton h1
      Caption         =   "Hide bots"
      Height          =   315
      Left            =   2350
      TabIndex        =   70
      Top             =   25
      Width           =   975
   End
   Begin SevOscar.SevOsc SevOsc10
      Height          =   435
      Left            =   0
      TabIndex        =   69
      Top             =   4680
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc9
      Height          =   435
      Left            =   3120
      TabIndex        =   68
      Top             =   4560
      Width           =   1515
   End
   Begin VB.CommandButton s1
      Caption         =   "Show Bots"
      Height          =   315
      Left            =   2350
      TabIndex        =   66
      Top             =   25
      Width           =   975
   End
   Begin SevOscar.SevOsc SevOsc8
      Height          =   435
      Left            =   1560
      TabIndex        =   57
      Top             =   4560
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc7
      Height          =   435
      Left            =   0
      TabIndex        =   52
      Top             =   4560
      Width           =   1515
   End
   Begin RichTextLib.RichTextBox RTB6
      Height          =   375
      Left            =   2880
      TabIndex        =   47
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox RTB5
      Height          =   375
      Left            =   2640
      TabIndex        =   46
      Top             =   3960
      Width           =   255
   End
   Begin VB.CommandButton Command1
      Caption         =   "rtb"
      Height          =   375
      Left            =   3960
      TabIndex        =   45
      Top             =   3960
      Width           =   615
   End
   Begin RichTextLib.RichTextBox RTB4
      Height          =   375
      Left            =   2400
      TabIndex        =   44
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox RTB3
      Height          =   375
      Left            =   2160
      TabIndex        =   43
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox invite6
      Height          =   375
      Left            =   1200
      TabIndex        =   42
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox invite5
      Height          =   375
      Left            =   960
      TabIndex        =   41
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox invite4
      Height          =   375
      Left            =   720
      TabIndex        =   40
      Top             =   3960
      Width           =   255
   End
   Begin RichTextLib.RichTextBox invite3
      Height          =   375
      Left            =   480
      TabIndex        =   39
      Top             =   3960
      Width           =   255
   End
   Begin SevOscar.SevOsc SevOsc6
      Height          =   435
      Left            =   3120
      TabIndex        =   38
      Top             =   4320
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc5
      Height          =   435
      Left            =   1560
      TabIndex        =   33
      Top             =   4320
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc4
      Height          =   435
      Left            =   0
      TabIndex        =   28
      Top             =   4320
      Width           =   1515
   End
   Begin VB.CommandButton help
      BackColor       =   &H00008080&
      Caption         =   "Help"
      Height          =   255
      Left            =   4080
      TabIndex        =   23
      Top             =   120
      Width           =   615
   End
   Begin SevOscar.SevOsc SevOsc3
      Height          =   435
      Left            =   3120
      TabIndex        =   19
      Top             =   4080
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc2
      Height          =   435
      Left            =   1560
      TabIndex        =   15
      Top             =   4080
      Width           =   1515
   End
   Begin SevOscar.SevOsc SevOsc1
      Height          =   435
      Left            =   0
      TabIndex        =   14
      Top             =   4080
      Width           =   1515
   End
   Begin VB.CommandButton Boot
      BeginProperty Font
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0
         Italic          =   0
         Strikethrough   =   0
      EndProperty
      Appearance      =   0
      BackColor       =   &H00C0C0C0&
      Caption         =   "Boot  !"
      Height          =   315
      Left            =   1660
      TabIndex        =   9
      Top             =   25
      Width           =   615
   End
   Begin VB.TextBox Text4
      Height          =   285
      Left            =   3240
      TabIndex        =   5
      Text            =   "im text"
      Top             =   3960
      Width           =   615
   End
   Begin VB.TextBox Text3
      BeginProperty Font
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0
         Italic          =   0
         Strikethrough   =   0
      EndProperty
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FFFF&
      Height          =   315
      Left            =   50
      TabIndex        =   4
      Text            =   " Who to boot"
      Top             =   25
      Width           =   1575
   End
   Begin VB.Frame bots
      BeginProperty Font
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0
         Italic          =   0
         Strikethrough   =   0
      EndProperty
      Caption         =   "Booting  Bots"
      Height          =   3255
      Left            =   0
      TabIndex        =   0
      Top             =   360
      Width           =   4905
      Begin VB.CommandButton O10
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   80
         Top             =   2880
         Width           =   375
      End
      Begin VB.CommandButton O9
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   79
         Top             =   2575
         Width           =   375
      End
      Begin VB.CommandButton O8
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   78
         Top             =   2280
         Width           =   375
      End
      Begin VB.CommandButton O7
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   77
         Top             =   1975
         Width           =   375
      End
      Begin VB.CommandButton O6
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   76
         Top             =   1700
         Width           =   375
      End
      Begin VB.CommandButton O5
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   75
         Top             =   1400
         Width           =   375
      End
      Begin VB.CommandButton O4
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   74
         Top             =   1125
         Width           =   375
      End
      Begin VB.CommandButton O3
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   73
         Top             =   840
         Width           =   375
      End
      Begin VB.CommandButton O2
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   72
         Top             =   525
         Width           =   375
      End
      Begin VB.CommandButton O1
         Caption         =   "out"
         Height          =   255
         Left            =   4475
         TabIndex        =   71
         Top             =   240
         Width           =   375
      End
      Begin VB.CommandButton L10
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   65
         Top             =   2880
         Width           =   615
      End
      Begin VB.CommandButton L9
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   64
         Top             =   2575
         Width           =   615
      End
      Begin VB.TextBox T22
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   285
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   63
         Text            =   "asdfgh"
         Top             =   2880
         Width           =   1095
      End
      Begin VB.TextBox T21
         Height          =   285
         Left            =   1525
         TabIndex        =   62
         Text            =   "your bot 10"
         Top             =   2880
         Width           =   1215
      End
      Begin VB.TextBox T20
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   285
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   61
         Text            =   "ddsdss"
         Top             =   2575
         Width           =   1095
      End
      Begin VB.TextBox T19
         Height          =   285
         Left            =   1525
         TabIndex        =   60
         Text            =   "your bot 9"
         Top             =   2575
         Width           =   1215
      End
      Begin VB.CommandButton L8
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   56
         Top             =   2280
         Width           =   615
      End
      Begin VB.TextBox T18
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   285
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   55
         Text            =   "asdfgh"
         Top             =   2280
         Width           =   1095
      End
      Begin VB.TextBox T17
         Height          =   285
         Left            =   1525
         TabIndex        =   54
         Text            =   "your bot 8"
         Top             =   2280
         Width           =   1215
      End
      Begin VB.CommandButton L7
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   51
         Top             =   1975
         Width           =   615
      End
      Begin VB.TextBox T16
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   285
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   50
         Text            =   "asdfgh"
         Top             =   1975
         Width           =   1095
      End
      Begin VB.TextBox T15
         Height          =   285
         Left            =   1525
         TabIndex        =   49
         Text            =   "your bot 7"
         Top             =   1975
         Width           =   1215
      End
      Begin VB.CommandButton L6
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   37
         Top             =   1700
         Width           =   615
      End
      Begin VB.TextBox T14
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   300
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   36
         Text            =   "asdfgh"
         Top             =   1680
         Width           =   1095
      End
      Begin VB.TextBox T13
         Height          =   285
         Left            =   1525
         TabIndex        =   35
         Text            =   "your bot 6"
         Top             =   1680
         Width           =   1215
      End
      Begin VB.CommandButton L5
         Caption         =   "Login"
         Height          =   255
         Left            =   3840
         TabIndex        =   32
         Top             =   1400
         Width           =   615
      End
      Begin VB.TextBox T12
         BeginProperty Font
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   400
            Underline       =   0
            Italic          =   0
            Strikethrough   =   0
         EndProperty
         Height          =   300
         IMEMode         =   3
         Left            =   2760
         PasswordChar    =   "*"
         TabIndex        =   31
         Text            =   "asdfgh"
         Top             =   1400
         Width           =   1095
      End
      Begin VB.TextBox T11
         Height          =   285
         Left            =   1525
         TabIndex        =   30
         Text            =   "your bot 5"
         Top             =   1400
         Width           =   1215
      End
   Begin VB.Label sonicn
      Caption         =   "sonic Bot beta boota 2003 test version"
      Height          =   255
      Left            =   360
      TabIndex        =   67
      Top             =   3645
      Width           =   3135
   End
End
Attribute VB_Name = "form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
 
' This form rebuilt by VBRezQDemo v2.3.  18-May-2003 / 16:23:21
Option Explicit 
 
' Declare Function GetActiveWindow Lib "user32" ()
 
' Declare Function SetWindowPos Lib "user32" ()
 
Sub Form_Load()
End Sub
 
Sub Command1_Click()
End Sub
 
Sub h1_Click()
End Sub
 
Sub help_Click()
End Sub
 
Sub L1_Click()
End Sub
 
Sub L2_Click()
End Sub
 
Sub L3_Click()
End Sub
 
Sub L4_Click()
End Sub
 
Sub L5_Click()
End Sub
 
Sub L6_Click()
End Sub
 
Sub L7_Click()
End Sub
 
Sub L8_Click()
End Sub
 
Sub L9_Click()
End Sub
 
Sub L10_Click()
End Sub
 
Sub O1_Click()
End Sub
 
Sub O10_Click()
End Sub
 
Sub O2_Click()
End Sub
 
Sub O3_Click()
End Sub
 
Sub O4_Click()
End Sub
 
Sub O5_Click()
End Sub
 
Sub O6_Click()
End Sub
 
Sub O7_Click()
End Sub
 
Sub O8_Click()
End Sub
 
Sub O9_Click()
End Sub
 
Sub s1_Click()
End Sub
 
'Sub SevOsc1_Event9()
'End Sub
 
'Sub SevOsc2_Event9()
'End Sub
 
'Sub SevOsc3_Event9()
'End Sub
 
'Sub SevOsc4_Event9()
'End Sub
 
'Sub SevOsc5_Event9()
'End Sub
 
'Sub SevOsc6_Event9()
'End Sub
 
'Sub SevOsc7_Event9()
'End Sub
 
'Sub SevOsc8_Event9()
'End Sub
 
'Sub SevOsc9_Event9()
'End Sub
 
'Sub SevOsc10_Event9()
'End Sub
 
'Sub SevOsc1_Event10()
'End Sub
 
'Sub SevOsc2_Event10()
'End Sub
 
'Sub SevOsc3_Event10()
'End Sub
 
'Sub SevOsc4_Event10()
'End Sub
 
'Sub SevOsc5_Event10()
'End Sub
 
'Sub SevOsc6_Event10()
'End Sub
 
'Sub SevOsc7_Event10()
'End Sub
 
'Sub SevOsc8_Event10()
'End Sub
 
'Sub SevOsc9_Event10()
'End Sub
 
'Sub SevOsc10_Event10()
'End Sub
 
Sub Boot_Click()
End Sub
 
