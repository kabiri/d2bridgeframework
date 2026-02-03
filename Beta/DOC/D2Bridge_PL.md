# 🌐 D2Bridge Framework - Nieoficjalna Encyklopedia

<div align="center">

![D2Bridge Logo](/assets/LogoD2BridgeTransp.png)

[![License: LGPL 2.1](https://img.shields.io/badge/License-LGPL%202.1-blue.svg)](https://opensource.org/licenses/LGPL-2.1)
[![Delphi](https://img.shields.io/badge/Delphi-10.0--13.0-orange.svg)](https://www.embarcadero.com/products/delphi)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.4--4.4-green.svg)](https://www.lazarus-ide.org/)

[GitHub](https://github.com/d2bridge/d2bridgeframework) • [Discord](https://discord.gg/WvHaWP6h9t) • [Strona](https://d2bridge.com.br)

</div>

---

## 📑 Spis Treści

- [Informacje Podstawowe](#-informacje-podstawowe)
- [Instalacja i Konfiguracja](#-instalacja-i-konfiguracja)
- [Architektura Framework'a](#-architektura-frameworka)
- [System Tłumaczeń](#-system-tłumaczeń)
- [Callbacki i Zdarzenia](#-callbacki-i-zdarzenia)
- [Szablony (Templates)](#-szablony-templates)
- [Komponenty](#-komponenty)
- [Formularze](#-formularze)
- [CRUD System](#-crud-system)
- [Baza Danych](#-baza-danych)
- [API E-mail](#-api-e-mail)
- [System Tokenów](#-system-tokenów)
- [Popup i Nested Forms](#-popup-i-nested-forms)
- [Upload Plików](#-upload-plików)
- [Walidacja](#-walidacja)
- [Sesje i Bezpieczeństwo](#-sesje-i-bezpieczeństwo)
- [Cykl Życia Aplikacji](#-cykl-życia-aplikacji)
- [Dobre Praktyki](#-dobre-praktyki)
- [Rozwiązywanie Problemów](#-rozwiązywanie-problemów)
- [Przykładowy Projekt](#-przykładowy-projekt)

---

## 📋 Informacje Podstawowe

### Czym jest D2Bridge?

D2Bridge to framework open-source umożliwiający konwersję aplikacji napisanych w Delphi lub Lazarus na aplikacje webowe **bez konieczności pisania kodu JavaScript**. Framework zachowuje natywne podejście Delphi do programowania, jednocześnie generując nowoczesne aplikacje działające w przeglądarce.

### Dane Techniczne

| Parametr | Wartość |
|----------|---------|
| **Autor** | Talis Jonatas Gomes |
| **Licencja** | LGPL 2.1 |
| **Wersja Beta** | 2.5.76+ |
| **Wersja Stable** | 2.0.8 |
| **Delphi** | 10.0 - 13.0 |
| **Lazarus** | 3.4 - 4.4 (Windows) |
| **Email** | talisjonatas@me.com |

### Linki

| Zasób | URL |
|-------|-----|
| **Repozytorium GitHub** | https://github.com/d2bridge/d2bridgeframework |
| **Serwer Discord** | https://discord.gg/WvHaWP6h9t |
| **Strona Oficjalna** | https://d2bridge.com.br |

### Obsługiwane Języki (Tłumaczenia)

D2Bridge natywnie obsługuje 18 języków:

| Kod | Język |
|-----|-------|
| `AR_SA` | Arabski (Arabia Saudyjska) |
| `CS_CZ` | Czeski |
| `DE_DE` | Niemiecki |
| `EN_US` | Angielski (USA) |
| `ES_ES` | Hiszpański |
| `FA_IR` | Perski (Iran) |
| `FR_FR` | Francuski |
| `IT_IT` | Włoski |
| `JA_JP` | Japoński |
| `KO_KR` | Koreański |
| `PL_PL` | Polski |
| `PT_BR` | Portugalski (Brazylia) |
| `RO_RO` | Rumuński |
| `RU_RU` | Rosyjski |
| `TH_TH` | Tajski |
| `TR_TR` | Turecki |
| `UK_UA` | Ukraiński |
| `ZH_CN` | Chiński (Uproszczony) |

---

## 🚀 Instalacja i Konfiguracja

### Tworzenie Nowego Projektu

```
File → New → Other → D2Bridge Framework → D2Bridge Framework Delphi Project
```

### Parametry Projektu

| Parametr | Przykład | Opis |
|----------|----------|------|
| **Nazwa** | `D2Checkin` | Nazwa projektu |
| **Port** | `8888` | Port serwera HTTP |
| **Platforma** | `Web+VCL` | `Web`, `Web+VCL`, `Web+LCL`, `Web+FMX` |
| **Typ** | `Server Console` | Typ aplikacji |

### Struktura Katalogów

```
Projekt/
├── wwwroot/                 ← Pliki publiczne (HTML, CSS, JS)
│   ├── css/
│   ├── js/
│   │   └── prismpage.js     ← Plugin PrismPage
│   ├── mail/                ← Szablony e-mail
│   └── temp/                ← Pliki tymczasowe sesji
├── language/                ← Pliki tłumaczeń JSON
│   ├── PT_BR.json
│   └── EN_US.json
├── langue_export/           ← Eksport tłumaczeń (DEBUG)
├── files/                   ← Pliki aplikacji (POZA wwwroot!)
│   └── images/
└── Units/
    ├── D2Checkin_Session.pas     ← Klasa projektu
    ├── Unit_DM.pas               ← DataModule
    ├── Unit_ServerController.pas ← Kontroler serwera
    └── [formularze...]
```

### Konfiguracja Połączenia z Bazą (Config.ini)

```ini
[Database]
Host=localhost
Database=D2Checkin
User=SA
Password=YourPassword
```

---

## 🏗 Architektura Framework'a

### Model Wątków

```
┌─────────────────────────────────────────────────────────────┐
│                    ARCHITEKTURA WĄTKÓW                      │
├─────────────────────────────────────────────────────────────┤
│  [Żądanie HTTP]                                             │
│        │                                                    │
│        ▼                                                    │
│  ┌─────────────────────┐                                    │
│  │    MAIN THREAD      │  ← Tworzenie formularzy            │
│  │  (instancja visual) │    (wymóg Windows API)             │
│  └─────────────────────┘                                    │
│        │                                                    │
│        ▼                                                    │
│  ┌─────────────────────┐                                    │
│  │   MULTI-THREAD      │  ← Obsługa zdarzeń                 │
│  │   (paralelizm)      │    Każdy przycisk = osobny wątek   │
│  └─────────────────────┘                                    │
└─────────────────────────────────────────────────────────────┘
```

> **Kluczowe:** Sesja ≠ Wątek. Obiekty są powiązane z `TPrismSession`, nie z wątkiem.

### Klasa Projektu (Session)

```pascal
type
  TD2Checkin = class(TPrismSessionBase)
  public
    const FILE_PATH = 'files\';
    const FILE_PATH_IMAGES = 'files\images\';
    const DEFAULT_USER_IMAGE = 'files\images\default.jpeg';
    
    // Zmienne sesji użytkownika
    UserID: Integer;
    UserName: string;
    UserEmail: string;
    UserPassword: string;
    UserFotoPath: string;
    UserFoto: string;      // Base64
    AccountID: Integer;
    IsLogged: Boolean;
    AutoJoinEventID: Integer;
  end;
```

### Dostęp do Klasy Projektu

```pascal
// Globalna funkcja dostępu
function D2Checkin: TD2Checkin;
begin
  Result := TD2Checkin(D2BridgeInstance.PrismSession.Data);
end;
```

---

## 🌍 System Tłumaczeń

### Składnia Tagów HTML

| Typ | Składnia | Przykład | Użycie |
|-----|----------|----------|--------|
| **Tłumaczenie** | `{{_nazwa_}}` | `{{_menu_dashboard_}}` | Tekst wielojęzyczny |
| **Zmienna** | `{{nazwa}}` | `{{username}}` | Dane dynamiczne |
| **Tag systemowy** | `{{language}}` | - | Aktualny język |

### Definicja Terminów

```pascal
// D2Bridge_Lingua_App_Term.pas
type
  TD2BridgeLinguaAppTerm = record
    // Menu
    menu_dashboard: string;
    menu_users: string;
    menu_logout: string;
    
    // Przyciski
    btn_save: string;
    btn_cancel: string;
    
    // Komunikaty
    MSG_SaveSuccess: string;
    MSG_InvalidEmail: string;
  end;
```

### Pliki JSON

```json
{
  "menu_dashboard": "Panel główny",
  "menu_users": "Użytkownicy",
  "menu_logout": "Wyloguj",
  "btn_save": "Zapisz",
  "btn_cancel": "Anuluj",
  "MSG_SaveSuccess": "Rekord został zapisany",
  "MSG_InvalidEmail": "Nieprawidłowy adres e-mail"
}
```

### Proces Generowania Tłumaczeń

```
1. Dodaj tagi {{_tag_}} w HTML
2. Zdefiniuj w D2Bridge_Lingua_App_Term.pas
3. ⚠️ KOMPILUJ W TRYBIE DEBUG (generuje JSON)
4. Skopiuj JSON z langue_export/ do language/
5. Edytuj tłumaczenia - działają w RUNTIME!
```

### Użycie w Kodzie

```pascal
// Dostęp do tłumaczeń
ShowMessage(D2Bridge.LinguaApp.MSG_SaveSuccess);

// W eksportacji
.FormGroup(D2Bridge.LApp.user_name, CSSClass.Col.ColMD6)
```

---

## 🔗 Callbacki i Zdarzenia

### Podstawowe Callbacki (HTML)

```html
<!-- Prosty callback -->
<a onclick="{{callback=open_dashboard}}">Dashboard</a>

<!-- Callback z parametrem -->
<button onclick="{{callback=open_new_account(free)}}">Darmowy</button>

<!-- Callback z wieloma parametrami -->
<button onclick="{{callback=action(param1=val1&param2=val2)}}">Akcja</button>
```

### Obsługa w Delphi

```pascal
procedure TAdminPageTemplate.Callback(const ACallbackName: string;
  AEventParsing: ID2BridgeEventParsing);
begin
  // Prosty callback
  if SameText(ACallbackName, 'open_dashboard') then
  begin
    if FormDashboard = nil then
      FormDashboard := TFormDashboard.Create(nil);
    FormDashboard.Show;
  end
  
  // Callback z parametrem
  else if SameText(ACallbackName, 'open_new_account') then
  begin
    SubscriptionType := AEventParsing.Values['subscription'];
    // lub pierwszy parametr bez nazwy:
    SubscriptionType := AEventParsing.Values[0];
  end;
end;
```

### Zamykanie Sesji

```pascal
// Callback zamknięcia
if SameText(ACallbackName, 'close_session') then
  Session.Close;
  // lub Session.Close(false/true);
```

---

## 🎨 Szablony (Templates)

### Architektura Master/Page Template

```
┌─────────────────────────────────────────┐
│           MASTER TEMPLATE               │
│  ┌─────────┐  ┌──────────────────────┐  │
│  │ SIDEBAR │  │    PAGE TEMPLATE     │  │
│  │  MENU   │  │   ($prismpage)       │  │
│  │         │  │   ┌──────────────┐   │  │
│  │         │  │   │ PRISM BODY   │   │  │
│  │         │  │   │ ($prismbody) │   │  │
│  └─────────┘  └──────────────────────┘  │
└─────────────────────────────────────────┘
```

### Plugin PrismPage

```bash
# Skopiuj plugin
D2Bridge/Beta/HTML/plugin/prismpage.js → wwwroot/js/prismpage.js
```

```html
<!-- master_template.html -->
<script src="js/prismpage.js"></script>
<PrismPage view-page="dashboard.html"></PrismPage>
```

> **Korzyść:** Edycja podstron w kontekście master template nawet bez uruchomionego serwera!

### Konfiguracja Szablonu

```pascal
procedure TFormTemplate.ExportD2Bridge;
begin
  inherited;
  Title := 'D2 Checkin';
  TemplateClassForm := TAdminPageTemplate;
  
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := 'master_template.html';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := 'page_template.html';
end;
```

### Klasa Template (Prism Form)

```pascal
type
  TAdminPageTemplate = class(TD2BridgePrismForm)
  protected
    procedure Callback(const ACallbackName: string;
      AEventParsing: ID2BridgeEventParsing); override;
    procedure TagHTML(const ATag: string; var ATagValue: string); override;
  end;

procedure TAdminPageTemplate.TagHTML(const ATag: string; var ATagValue: string);
begin
  if SameText(ATag, 'username') then
    ATagValue := D2Checkin.UserName
  else if SameText(ATag, 'user_foto') then
    ATagValue := D2Checkin.UserFoto;  // Base64
end;
```

---

## 🧩 Komponenty

### Eksportacja Komponentów

#### Podstawowa Składnia

```pascal
procedure TForm_Checkout.ExportD2Bridge;
begin
  inherited;

  Title := 'My D2Bridge Form';

  //TemplateClassForm := TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.Add do
  begin
    with Row.Items.Add do
      with ColAuto.Items.Add do
      begin
        VCLObj(Label_TagName);
        VCLObj(DBText_TagName);
      end;

    with Row.Items.Add do
      Col8.Add.LCLObj(Image_Product);
  end;
end;
```

#### Układy (Rows i Columns)

```pascal
// Wiersz z kolumnami
with D2Bridge.Items.Add do
begin
  Row.Items.Add.VCLObj(EditFirstName, CSSClass.Col.ColMD6);
  Row.Items.Add.VCLObj(EditLastName, CSSClass.Col.ColMD6);
end;

// Wiersz z marginesami Bootstrap
with D2Bridge.Items.Add.Row('mt-2 mb-2') do  // margin-top, margin-bottom
begin
  Row.Items.Add.Image(ImageFoto, CSSClass.Col.ColMD4);
  Row.Items.Add.FileUpload(UploadFoto, CSSClass.Col.ColMD8);
end;
```

### Klasy CSS Bootstrap (Kolumny)

| Klasa | Opis |
|-------|------|
| `ColMD1` - `ColMD12` | 1-12 jednostek na medium+ |
| `ColLG1` - `ColLG12` | 1-12 jednostek na large+ |
| `ColSize1` - `ColSize12` | Stała szerokość |

### Panel Group

```pascal
with D2Bridge.Items.Add.PanelGroup('{{_personal_data_}}', 'PanelUserData', 
                                    CSSClass.Col.ColLG12) do
begin
  AddFormGroup('{{_name_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditName);
  AddFormGroup('{{_email_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditEmail);
  AddFormGroup('{{_phone_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditPhone);
end;
```

### Card

```pascal
with Card do
begin
  CSSClasses := CSSClass.Card.Card_Center_ExtraLarge;

  Header('My Text');

  with BodyItems.Add do
  begin
    with Row.Items.Add do
    begin
      Col6.Add.FormGroup(Label_FullName).AddLCLObj(DBEdit_FullName, 'ValidationAccount', true);
      Col6.Add.FormGroup(Label_Doc).AddLCLObj(DBEdit_Doc);
    end;
    // ...
  end;

  with Footer.Items.Add do
    with Row.Items.Add do
    begin
      ColAuto.Add.LCLObj(Button_Save, 'ValidationAccount', false, CSSClass.Button.save);
    end;
end;
```

### Div

```pascal
with D2Bridge.Items.Add do
begin
  with HTMLDIV('myClass').Items.Add do
  begin
    // Zawartość diva
    VCLObj(Component);
  end;
end;
```

### Accordion

```pascal
with D2Bridge.Items.Add.Accordion do
begin
  // Sekcja domyślnie rozwinięta
  with Items.Add('Zdjęcia', True) do
    Items.Add.Image(Image1);
  
  // Sekcja domyślnie zwinięta
  with Items.Add('Opcje zaawansowane', False) do
    Items.Add.VCLObj(EditAdvanced);
end;
```

### Karuzela (Carousel)

```pascal
// Karuzela z DataSource - JEDNA LINIA!
with Row.Items.Add.Div(CSSClass.Col.ColMD4) do
begin
  Carousel
    .DataSource(DSEventPhoto)
    .FieldName('foto_path');
end;

// Konfiguracja
with Carousel do
begin
  DataSource(DSEventPhoto);
  FieldName('foto_path');
  Interval := 3000;        // ms między slajdami
  MaxRecords := 25;        // max obrazów
  ShowIndicators := True;  // kropki wskaźników
  ShowControls := True;    // przyciski prev/next
  AutoPlay := True;        // auto-przewijanie
end;
```

### Komponent Link

Zamienia dowolny komponent VCL w klikalny link:

```pascal
// Zwykły komponent
.AddVCLObj(DBTextAdminName);

// Komponent jako LINK
.AddVCLObjLink(DBTextAdminName);
```

```pascal
// Obsługa kliknięcia
procedure TFormAccount.DBTextAdminNameClick(Sender: TObject);
begin
  // Otwórz popup
  D2D.ShowPopup('PopupUserAdmin');
end;
```

---

## 📝 Formularze

### Tworzenie Formularza

```
File → New → Other → D2Bridge Framework → D2Bridge Form
```

### Dziedziczenie Formularzy

```
File → New → Other → D2Bridge Framework → D2Bridge Inherited Form
→ Select Ancestor: TFormTemplate
```

### Hierarchia Formularzy

```
TFormTemplate (Master/Page)
       │
       ├── TFormDashboard
       │
       └── TFormCrudTemplate
                │
                ├── TFormUsers
                ├── TFormEvents
                └── TFormAccount
```

### Eksportacja w Formularzach Dziedziczonych

```pascal
procedure TFormDashboard.ExportD2Bridge;
begin
  // ⚠️ W formularzach dziedziczonych ZAKOMENTUJ inherited!
  // inherited;  // Master i Page już w rodzicu!
  
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := 'dashboard.html';
end;
```

### Cykl Życia Formularza

```
1. Create         → Tworzenie instancji
2. ExportD2Bridge → Eksportacja komponentów
3. OnShow         → Przed renderowaniem (otwieranie Query)
4. BeginRender    → Początek renderowania
5. [Renderowanie komponentów]
6. EndRender      → Koniec renderowania (PrismControlFromID!)
7. [Wysłanie HTML do przeglądarki]
8. OnActivate     → Po załadowaniu w przeglądarce
```

| Zdarzenie | Kiedy używać | PrismControl dostępny? |
|-----------|--------------|------------------------|
| `OnShow` | Otwieranie Query, dane | ❌ NIE |
| `BeginRender` | Przed renderem | ❌ NIE |
| `EndRender` | **Manipulacja PrismControl** | ✅ TAK |
| `OnActivate` | Po załadowaniu strony | ✅ TAK |

---

## 📊 CRUD System

### Struktura CRUD

```
┌─────────────────────────────────────────┐
│ CARD SEARCH (Wyszukiwanie)              │
│  ├── Crud_RowSearch (przyciski)         │
│  └── Crud_RowGrid (tabela)              │
├─────────────────────────────────────────┤
│ CARD DATA (Edycja)                      │
│  └── Pola formularza                    │
└─────────────────────────────────────────┘
```

### Zdarzenia CRUD

```pascal
type
  TFormUsers = class(TFormCrudTemplate)
  protected
    procedure OnCrudOpen(Sender: TObject); override;
    procedure OnCrudSearch(Sender: TObject); override;
    procedure OnCrudEdit(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudInsert(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudSave(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudDelete(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudBack(Sender: TObject); override;
    procedure OnCrudClose(Sender: TObject); override;
  end;
```

### Przykład Implementacji

```pascal
procedure TFormUsers.OnCrudOpen(Sender: TObject);
begin
  CrudOperation(coSearch);
end;

procedure TFormUsers.OnCrudSearch(Sender: TObject);
begin
  DM.QueryUsers.Close;
  DM.QueryUsers.SQL.Text := 
    'SELECT * FROM users WHERE id_account = ' + D2Checkin.AccountID.ToString;
  DM.QueryUsers.Open;
end;

procedure TFormUsers.OnCrudSave(Sender: TObject; var AAllow: Boolean);
begin
  AAllow := True;
  
  // Walidacja
  if not TD2BridgePoni.IsValidAddress(DBEditEmail.Text) then
  begin
    D2D.Validation(DBEditEmail, False, D2Bridge.LApp.MSG_InvalidEmail);
    AAllow := False;
    Exit;
  end;
end;
```

### Operacje CRUD

```pascal
CrudOperation(coSearch);  // Wyszukaj
CrudOperation(coEdit);    // Edytuj
CrudOperation(coInsert);  // Wstaw
CrudOperation(coSave);    // Zapisz
CrudOperation(coDelete);  // Usuń
CrudOperation(coBack);    // Wróć
```

### Start Card

```pascal
CrudStartCard := csSearch;  // Start od listy
CrudStartCard := csData;    // Start od edycji (np. Profil)
```

### Uprawnienia

```pascal
type
  TCrudPermission = set of (cpSearch, cpEdit, cpInsert, cpSave, cpDelete);

CrudPermissions := [cpSearch, cpEdit, cpInsert, cpSave, cpDelete];  // Pełne
CrudPermissions := [cpSearch];  // Tylko podgląd
```

### Dodawanie Przycisków do GRID

```pascal
procedure TFormEvents.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  
  if PrismControl.VCLComponent = DBGridSearch then
  begin
    // Dodaj kolumnę z przyciskami
    with PrismControl.Columns.Add do
    begin
      Index := 0;
      Width := 78;  // ~26px na przycisk
      
      // Przycisk View
      with Buttons.Add do
      begin
        ButtonModel := bmView;
        Caption := '';
        OnClick := ShowEvent;
      end;
      
      // Przycisk Edit
      with Buttons.Add do
      begin
        ButtonModel := bmEdit;
        Caption := '';
        OnClick := procedure begin CrudOperation(coEdit); end;
      end;
      
      // Przycisk Delete
      with Buttons.Add do
      begin
        ButtonModel := bmDelete;
        Caption := '';
        OnClick := procedure begin CrudOperation(coDelete); end;
      end;
    end;
  end;
end;
```

### Formatowanie Kolumn GRID

```pascal
procedure TFormUsers.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  
  if PrismControl.VCLComponent is TDBGrid then
  begin
    if PrismControl.FieldName = 'is_confirmed' then
    begin
      PrismControl.HTML := 
        '<span class="badge ' +
        '${value == "yes" ? "bg-success" : "bg-danger"}"' +
        ' style="border-radius: 50px; padding: 5px 10px;">' +
        '${value == "yes" ? "' + D2Bridge.LApp.yes + '" : "' + D2Bridge.LApp.no + '"}' +
        '</span>';
    end;
  end;
end;
```

---

## 🗄 Baza Danych

### DataModule dla Sesji

```pascal
unit Unit_DM;

uses D2Bridge.Instance;

type
  TDM = class(TDataModule)
    ADOConnection: TADOConnection;
    QueryUser: TADOQuery;
    QueryAccount: TADOQuery;
    DSUser: TDataSource;
  end;

function DM: TDM;

implementation

function DM: TDM;
begin
  Result := TDM(D2BridgeInstance.GetInstance(TDM));
end;
```

### Tworzenie Instancji DM

```pascal
uses
  Unit_DM,
  Winapi.ActiveX;  // Wymagane dla SQL Server!

procedure TD2Checkin.OpenDM;
begin
  CoInitializeEx(0, COINIT_MULTITHREADED);  // Tylko SQL Server!

  if DM = nil then
    D2BridgeInstance.CreateInstance(TDM);
end;
```

### Dynamiczna Konfiguracja Połączenia

```pascal
procedure TDM.ADOConnectionWillConnect(Connection: TADOConnection;
  var ConnectionString, UserID, Password: WideString;
  var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Config.ini');
  try
    ConnectionString := 
      'Provider=SQLOLEDB.1;' +
      'Password=' + IniFile.ReadString('Database', 'Password', '') + ';' +
      'User ID=' + IniFile.ReadString('Database', 'User', 'SA') + ';' +
      'Initial Catalog=' + IniFile.ReadString('Database', 'Database', 'D2Checkin') + ';' +
      'Data Source=' + IniFile.ReadString('Database', 'Host', 'localhost');
  finally
    IniFile.Free;
  end;
end;
```

### Ustawienia ADO dla SQL Server

```
Location: clUseServer     (Cursor Location = Server)
CursorType: ctKeySet      (Cursor Type = KeySet)
```

---

## 📧 API E-mail

### Konfiguracja

```pascal
procedure TServerController.ConfigEmail;
begin
  if D2BridgeManager.API.Mail.Config then
  begin
    Config.Host := 'smtp.gmail.com';
    Config.Port := 587;
    Config.Username := 'email@gmail.com';
    Config.Password := 'xxxx xxxx xxxx xxxx';  // App Password!
    Config.UseSSL := True;
    Config.UseTLS := True;
    Config.UseThread := True;
    Config.FromName := 'D2 Checkin';
    Config.FromMailAddress := 'noreply@d2checkin.com';
  end;
end;
```

### Wymagane DLL

```
libeay32.dll
ssleay32.dll
```

### Wysyłanie E-maila

```pascal
procedure TD2Checkin.SendEmail(const ATo, ASubject, ABody: string);
begin
  with D2BridgeServerController.API.Mail do
  begin
    Addresses.Clear;
    Addresses.Add(ATo);
    Subject := ASubject;
    Body.Text := ABody;
    
    if not Send then
      raise Exception.Create('Błąd wysyłania e-mail');
  end;
end;
```

### E-mail HTML z Base64

```pascal
procedure TD2Checkin.SendHTMLEmail(AUserID: Integer);
var
  EmailBody: TStringList;
begin
  EmailBody := TStringList.Create;
  try
    EmailBody.LoadFromFile(
      D2BridgeServerController.PrismOptions.RootDirectory + 
      'mail\email_template.html',
      TEncoding.UTF8  // ⚠️ WAŻNE dla polskich znaków!
    );
    
    // Zamień tagi
    EmailBody.Text := StringReplace(EmailBody.Text, 
      '{{_welcome_}}', D2Bridge.LinguaApp.mail_welcome, [rfReplaceAll]);
    
    with D2BridgeServerController.API.Mail do
    begin
      Body.HTML := EmailBody.Text;
      Send;
    end;
  finally
    EmailBody.Free;
  end;
end;
```

---

## 🔑 System Tokenów

### Typy Tokenów

```pascal
type
  TTokenType = (
    ttNewUser,      // Nowy użytkownik (weryfikacja email)
    ttRecoverPass,  // Odzyskiwanie hasła
    ttNewEvent,     // Nowe wydarzenie
    ttCheckin       // Check-in
  );
  
  TTokenMode = (
    tmLink,    // Walidacja przez link (64 znaki)
    tmCode     // Walidacja przez kod (6 cyfr)
  );
```

### Stałe

```pascal
const
  TOKEN_LINK_SIZE = 64;
  TOKEN_CODE_SIZE = 6;
  TOKEN_LINK_EXPIRY_HOURS = 24;
  TOKEN_CODE_EXPIRY_SECONDS = 300;
```

### Tworzenie Tokena

```pascal
function TD2Checkin.CreateToken(AType: TTokenType; AMode: TTokenMode; 
  AUserID: Integer; AEventID: Integer = 0): Integer;
var
  TokenValue: string;
begin
  repeat
    if AMode = tmLink then
      TokenValue := GenerateRandomString(TOKEN_LINK_SIZE)
    else
      TokenValue := GenerateRandomNumber(TOKEN_CODE_SIZE);
  until not TokenExists(TokenValue);
  
  with DM.QueryToken do
  begin
    Append;
    FieldByName('user_id').AsInteger := AUserID;
    FieldByName('id_event').AsInteger := AEventID;
    FieldByName('type').AsString := TokenTypeToStr(AType);
    FieldByName('token').AsString := TokenValue;
    FieldByName('created_at').AsDateTime := Now;
    FieldByName('expires_at').AsDateTime := Now + (TOKEN_LINK_EXPIRY_HOURS / 24);
    Post;
    Result := FieldByName('id').AsInteger;
  end;
end;
```

### Walidacja Tokena z URL

```pascal
procedure TFormLogin.FormActivate(Sender: TObject);
var
  TokenValue: string;
begin
  if D2BridgeServerController.PrismSession.URI.QueryParams.Count > 0 then
  begin
    TokenValue := D2BridgeServerController.PrismSession.URI.QueryParams.Values['token'];
    
    if TokenValue <> '' then
    begin
      D2Checkin.OpenDM;
      
      if OpenToken(TokenValue) then
      begin
        case DM.QueryToken.FieldByName('type').AsString of
          'new_user': begin
            // Potwierdź konto
            DM.QueryUser.Edit;
            DM.QueryUser.FieldByName('is_confirmed').AsBoolean := True;
            DM.QueryUser.Post;
            ShowMessage(D2Bridge.LApp.MSG_AccountConfirmed);
          end;
          
          'new_event': begin
            // Pokaż wydarzenie
            TFormShowEvent(FormShowEvent).IDEvent := 
              DM.QueryToken.FieldByName('id_event').AsInteger;
            D2D.ShowPopup('PopupShowEvent');
          end;
        end;
      end;
      
      // Wyczyść parametry URL
      D2BridgeServerController.PrismSession.URI.QueryParams.Clear;
    end;
  end;
end;
```

---

## 🪟 Popup i Nested Forms

### Tworzenie Popup

```pascal
// W eksportacji
with D2Bridge.Items.Add.Popup('PopupName', 'Tytuł', False) do
begin
  // False = pokaż przycisk X
  // True = ukryj przycisk X
  
  Items.Add.VCLObj(Component1);
  Items.Add.VCLObj(Component2);
end;
```

### Pokazywanie/Ukrywanie Popup

```pascal
// Pokaż
D2D.ShowPopup('PopupName');

// Ukryj
D2D.ClosePopup('PopupName');
```

### Nested Forms

Nested Form to formularz osadzony w innym formularzu lub popup.

```pascal
type
  TFormAccount = class(TFormTemplate)
  private
    FormUsers: TD2BridgeForm;  // ⚠️ Typ bazowy (unikaj circular reference!)
  end;

procedure TFormAccount.ExportD2Bridge;
begin
  inherited;
  
  // Tworzenie Nested
  if FormUsers = nil then
  begin
    FormUsers := TFormUsers.Create(Self);
    TFormUsers(FormUsers).CrudStartCard := csData;
    D2D.AddNested(FormUsers);
  end;
  
  // Popup z Nested
  with D2Bridge.Items.Add.Popup('PopupUserAdmin', D2Bridge.LApp.edit_user, False) do
  begin
    Items.Add.AddNested(FormUsers);
  end;
end;
```

### Sprawdzanie Kontekstu Nested

```pascal
procedure TFormUsers.FormShow(Sender: TObject);
begin
  inherited;
  
  if not D2D.IsNested then
  begin
    // Standalone
    D2Bridge.FrameworkExportType.TemplatePageColSize := 8;
  end
  else
  begin
    // W popup - pełna szerokość
    D2Bridge.FrameworkExportType.TemplatePageColSize := 12;
  end;
end;
```

### Tag Popup w HTML (Landing Page)

```html
<!-- Przed </body> -->
<PrismPopup name="PopupShowEvent"></PrismPopup>
```

### Zdarzenie OnShow Popup

```pascal
procedure TFormEvents.PopupImagePreviewShow(Sender: TObject; var ACanShow: Boolean);
begin
  // Sprawdź nazwę popup (gdy jest wiele)
  if SenderName = 'PopupImagePreview' then
  begin
    ACanShow := True;
    ImagePreview.Picture.LoadFromFile(FImages[FSelectedIndex]);
  end;
end;
```

---

## 📤 Upload Plików

### Pojedynczy Upload

```pascal
// Eksportacja
Row.Items.Add.FileUpload(UploadFoto, CSSClass.Col.ColMD8);

// Zdarzenie
procedure TFormUsers.UploadFotoUpload(Sender: TObject);
var
  UploadedFile, NewFileName: string;
begin
  UploadedFile := UploadFoto.UploadFiles[0];
  
  // Generuj unikalną nazwę (cache busting!)
  NewFileName := D2Checkin.FILE_PATH_IMAGES + 
                 'user_' + UserID.ToString + '_' +
                 GenerateRandomString(8) + 
                 ExtractFileExt(UploadedFile);
  
  // Usuń starą fotkę
  if FileExists(OldPath) then
    DeleteFile(OldPath);
  
  // Skopiuj nową
  CopyFile(PChar(UploadedFile), PChar(NewFileName), False);
  
  // Załaduj podgląd
  ImageFoto.Picture.LoadFromFile(NewFileName);
  
  // Zapisz w bazie
  DM.QueryUser.Edit;
  DM.QueryUser.FieldByName('foto_path').AsString := NewFileName;
  DM.QueryUser.Post;
end;
```

### Upload Wielu Plików

```pascal
// Eksportacja z limitem
with Row.Items.Add.FileUpload(UploadImages, CSSClass.Col.ColMD12) do
begin
  MaxFiles := 5;         // Max 5 plików
  MaxFileSize := 20;     // Max 20 MB na plik
  Accept := 'image/*';   // Tylko obrazy
end;

// Zdarzenie
procedure TFormEvents.UploadImagesUpload(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to UploadImages.UploadFiles.Count - 1 do
  begin
    ProcessImage(UploadImages.UploadFiles[I]);
  end;
end;
```

### Problem z Cache Przeglądarki

**Problem:** Przeglądarka cache'uje obrazy po nazwie.

**Rozwiązanie:** Dodaj losowy ciąg do nazwy:

```pascal
NewFileName := 'user_' + UserID.ToString + '_' + 
               GenerateRandomString(8) +  // ← Losowy ciąg!
               ExtractFileExt(UploadedFile);
```

---

## ✅ Walidacja

### Walidacja E-mail

```pascal
uses D2Bridge.Poni;

if not TD2BridgePoni.IsValidAddress(EditEmail.Text) then
begin
  D2D.Validation(EditEmail, False, D2Bridge.LApp.MSG_InvalidEmail);
  Abort;
end;
```

### Grupy Walidacji

```pascal
with AddFormGroup('{{_name_}}', CSSClass.Col.ColLG12) do
begin
  AddVCLObj(DBEditName);
  ValidationGroup := Crud_ValidationGroup;  // Grupa CRUD
end;
```

### Maski Pól

```pascal
procedure TFormAccount.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  
  // Maska CNPJ (Brazylia)
  if PrismControl.VCLComponent = DBEditDocument then
    TDBEdit(PrismControl.VCLComponent).EditMask := TD2BridgePrismTMask.BrasilCNPJ;
  
  // Maska CEP (Brazylia)
  if PrismControl.VCLComponent = DBEditPostalCode then
    TDBEdit(PrismControl.VCLComponent).EditMask := TD2BridgePrismTMask.BrasilCEP;
  
  // Własna maska
  if PrismControl.VCLComponent = DBEditPhone then
    TDBEdit(PrismControl.VCLComponent).EditMask := 
      '{"mask": "(99) 99999-9999", "autoUnmask": true}';
end;
```

### Predefiniowane Maski

| Maska | Format |
|-------|--------|
| `BrasilCNPJ` | `00.000.000/0000-00` |
| `BrasilCPF` | `000.000.000-00` |
| `BrasilCEP` | `00000-000` |
| `BrasilPhone` | `(00) 00000-0000` |

---

## 🔒 Sesje i Bezpieczeństwo

### Inicjalizacja Sesji

```pascal
// ServerController.pas
procedure TServerController.NewSession(Sender: TObject);
begin
  D2Checkin.IsLogged := False;
  D2Checkin.AutoJoinEventID := 0;
end;
```

### Logowanie

```pascal
procedure TFormLogin.ButtonLoginClick(Sender: TObject);
begin
  // Walidacja...
  
  // Po sukcesie:
  D2Checkin.IsLogged := True;
  D2Checkin.UserID := DM.QueryUser.FieldByName('id').AsInteger;
  D2Checkin.UserName := DM.QueryUser.FieldByName('name').AsString;
  // ...
  
  FormDashboard.Show;
end;
```

### Zamykanie Sesji

```pascal
// Kończy WSZYSTKO: wątki, formularze, DM, zmienne
Session.Close;
// lub Session.Close(false/true);
```

### Bezpieczeństwo Plików

```
wwwroot/        ← Dostępne z przeglądarki
files/          ← NIEDOSTĘPNE z przeglądarki!
```

> Pliki użytkowników (zdjęcia) przechowuj POZA `wwwroot/`!

---

## 🔄 Cykl Życia Aplikacji

### Zdarzenia Formularza

```
Create → ExportD2Bridge → OnShow → BeginRender → 
[Renderowanie] → EndRender → InitControlsD2Bridge → 
[Wysłanie do przeglądarki] → OnActivate
```

### Kiedy Używać Którego Zdarzenia?

| Zdarzenie | Użycie |
|-----------|--------|
| `OnShow` | Otwieranie Query, ustawianie danych |
| `BeginRender` | Przygotowanie przed renderem |
| `EndRender` | `D2D.PrismControlFromID()` |
| `InitControlsD2Bridge` | Maski, formatowanie GRID |
| `OnActivate` | Pytania do użytkownika, auto-akcje |

### ⚠️ Ważne Ograniczenia

```pascal
// ❌ NIGDY w OnShow/BeginRender/EndRender:
ShowMessage('Pytanie?', mtConfirmation, [mbYes, mbNo]);
// Zablokuje sesję!

// ✅ Tylko w OnActivate:
procedure TForm.FormActivate(Sender: TObject);
begin
  if ShowMessage('Kontynuować?', mtConfirmation, [mbYes, mbNo]) = mrYes then
    // ...
end;
```

---

## 💡 Dobre Praktyki

### 1. Kompilacja

```
✅ DEBUG      → Generuje JSON tłumaczeń
✅ RELEASE    → Produkcja
```

### 2. Kolejność Units

```pascal
uses
  // ...inne unity...
  D2Bridge.Forms;  // ← ZAWSZE NA KOŃCU!
```

### 3. Połączenie z Bazą

```pascal
// Używaj WillConnect zamiast AfterConnect
procedure TDM.ADOConnectionWillConnect(...);
```

### 4. UTF-8 dla E-maili

```pascal
EmailBody.LoadFromFile(Path, TEncoding.UTF8);
```

### 5. OnActivate dla Query z DBLookupComboBox

```pascal
// ŹLE - komponenty jeszcze nie wyrenderowane
procedure TForm.FormShow(Sender: TObject);
begin
  DM.Query.Open;  // DBLookup nie zadziała!
end;

// DOBRZE
procedure TForm.FormActivate(Sender: TObject);
begin
  DM.Query.Open;  // Teraz działa!
end;
```

### 6. Unikanie Circular Reference

```pascal
// ŹLE
FormUsers: TFormUsers;  // Konkretny typ

// DOBRZE
FormUsers: TD2BridgeForm;  // Typ bazowy
TFormUsers(FormUsers).Property := Value;  // Rzutowanie
```

### 7. Auto-Fill w Debug

```pascal
procedure TFormLogin.FormActivate(Sender: TObject);
begin
  if DebugHook <> 0 then
  begin
    EditEmail.Text := 'test@test.com';
    EditPassword.Text := 'password';
  end;
end;
```

### 8. Szyfrowane Połączenia TLS/SSL

D2Bridge obsługuje szyfrowane połączenia TLS. Aby włączyć TLS, należy:

1. Dodać `IdSSLOpenSSLHeaders` do klauzuli `uses`.
2. Wskazać OpenSSL katalog z wymaganymi bibliotekami DLL (`ssleay32.dll` i `libeay32.dll`):

```pascal
uses
  IdSSLOpenSSLHeaders;

// Ustaw ścieżkę do bibliotek SSL (musi odpowiadać docelowej architekturze)
IdOpenSSLSetLibPath('C:\Sciezka\Do\SSL\DLLs');
```

> **Uwaga:** TLS nie będzie działać, jeśli `IdSSLOpenSSLHeaders` nie znajduje się w klauzuli `uses` lub ścieżka do bibliotek SSL nie jest poprawnie ustawiona.

### 9. Auto-Tłumaczenie Elementów Nawigacji GUI

Aby włączyć automatyczne tłumaczenie wbudowanych elementów nawigacji GUI (np. podpisów przycisków), użyj tagów tłumaczeń z kontekstem w podpisach:

```
{{_Button,CaptionOpen_}}
{{_Button,CaptionRefresh_}}
```

Następnie dodaj `D2Bridge.Lang.Core` do klauzuli `uses` i nadpisz `TagTranslate`:

```pascal
uses
  D2Bridge.Lang.Core;

procedure TForm1.TagTranslate(const Language: TD2BridgeLang;
  const AContext: string; const ATerm: string; var ATranslated: string);
begin
  inherited;
  ATranslated := D2BridgeLangCore.LangByTD2BridgeLang(Language)
    .Language.Translate(AContext, ATerm);
end;
```

---

## 🔧 Rozwiązywanie Problemów

### "Pointer is not valid" przy zamykaniu sesji

**Przyczyna:** Normalne w trybie debug przy zamykaniu wątków.

**Rozwiązanie:** Ignoruj w debug, nie pojawia się w release.

### DBLookupComboBox nie pokazuje danych

**Przyczyna:** Query otwarte w `OnShow` zamiast `OnActivate`.

**Rozwiązanie:** Przenieś `Query.Open` do `OnActivate`.

### Obrazy nie odświeżają się po upload

**Przyczyna:** Cache przeglądarki.

**Rozwiązanie:** Dodaj losowy ciąg do nazwy pliku.

### PrismControlFromID generuje wyjątek

**Przyczyna:** Wywołanie w `OnShow` zamiast `EndRender`.

**Rozwiązanie:** Przenieś do `EndRender` lub `OnActivate`.

### Popup nie pojawia się w Landing Page

**Przyczyna:** Brak tagu `<PrismPopup>` w HTML.

**Rozwiązanie:** Dodaj przed `</body>`:
```html
<PrismPopup name="PopupName"></PrismPopup>
```

### Przeglądarka przekierowuje na d2bridge.com.br

**Przyczyna:** Brak pliku `favicon.ico` w katalogu `wwwroot/`. Framework wykonuje przekierowanie 301 na `https://d2bridge.com.br/favicon.ico`.

**Rozwiązanie:** Umieść własny plik `favicon.ico` w katalogu `wwwroot/`.

### Formularz nie dziedziczy stylu

**Przyczyna:** Brak `inherited` lub błędna konfiguracja Template.

**Rozwiązanie:** Sprawdź `TemplateClassForm` i `TemplateMasterHTMLFile`.

---

## 📚 Przykładowy Projekt: D2Checkin

### Opis

System zarządzania wydarzeniami z check-inem uczestników.

### Funkcjonalności

| Moduł | Funkcje |
|-------|---------|
| **Landing Page** | Wielojęzyczność, formularz kontaktowy |
| **Logowanie** | Walidacja, tokeny e-mail |
| **Dashboard** | Master/Page Template, menu |
| **Użytkownicy** | CRUD, upload zdjęć, profil |
| **Konto** | Edycja firmy, Link do admina |
| **Wydarzenia** | CRUD, karuzela, tokeny |
| **Podgląd** | Popup, Join, Share |

### Struktura Bazy Danych

```sql
-- Konta
CREATE TABLE account (
    id INT IDENTITY(1,1) PRIMARY KEY,
    company_name VARCHAR(200),
    document VARCHAR(50),
    id_user INT,  -- Administrator
    created_at DATETIME
);

-- Użytkownicy
CREATE TABLE users (
    id INT IDENTITY(1,1) PRIMARY KEY,
    id_account INT,
    name VARCHAR(200),
    email VARCHAR(200) UNIQUE,
    password VARCHAR(100),
    foto_path VARCHAR(500),
    is_confirmed BIT,
    is_admin BIT,
    created_at DATETIME
);

-- Tokeny
CREATE TABLE token (
    id INT IDENTITY(1,1) PRIMARY KEY,
    id_user INT,
    id_account INT,
    id_event INT,
    type VARCHAR(20),
    token VARCHAR(100) UNIQUE,
    is_used BIT,
    created_at DATETIME,
    expires_at DATETIME
);

-- Wydarzenia
CREATE TABLE event (
    id INT IDENTITY(1,1) PRIMARY KEY,
    id_user INT,
    id_account INT,
    id_token INT,
    name VARCHAR(200),
    location VARCHAR(200),
    start_date DATETIME,
    end_date DATETIME,
    is_active BIT
);

-- Zdjęcia wydarzeń
CREATE TABLE event_photo (
    id INT IDENTITY(1,1) PRIMARY KEY,
    id_event INT,
    foto_path VARCHAR(500),
    created_at DATETIME
);

-- Uczestnicy
CREATE TABLE participant (
    id INT IDENTITY(1,1) PRIMARY KEY,
    id_event INT,
    id_user INT,
    id_account INT,
    id_token INT,
    joined_at DATETIME,
    checked_in_at DATETIME,
    is_checked_in BIT
);
```

### Użyte Mechaniki D2Bridge

- ✅ System tłumaczeń (18 języków)
- ✅ Callbacki z parametrami
- ✅ Master/Page Template
- ✅ CRUD z dziedziczeniem
- ✅ Upload pojedynczy i wielokrotny
- ✅ Maski pól
- ✅ Komponent Link
- ✅ Nested Forms
- ✅ Popup
- ✅ Karuzela
- ✅ Tokeny
- ✅ Toast
- ✅ Clipboard
- ✅ Formatowanie GRID
- ✅ Walidacja
- ✅ API E-mail


---

## 📖 Zasoby Dodatkowe

### Oficjalne

- [GitHub Repository](https://github.com/d2bridge/d2bridgeframework)
- [Discord Server](https://discord.gg/WvHaWP6h9t)
- [Oficjalna Strona](https://d2bridge.com.br)

---

<div align="center">

**"Rozwijaj web w sposób Delphi"**

*D2Bridge Framework - LGPL 2.1*

</div>