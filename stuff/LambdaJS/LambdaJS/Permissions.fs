module LambdaJS.Permissions
open Text.PrettyPrint

(* 
  http://developer.chrome.com/extensions/declare_permissions
*)

type CPermission = 
  /// No permission
  | Bottom 
  /// "[scheme]:[host]/*"	 Specifies a host permission. Required if the extension or app wants to interact with the code running on pages. Many capabilities, such as cross-origin XMLHttpRequests, programmatically injected content scripts, and the extension's cookies API require host permissions. For details on the syntax, see Match Patterns. A path is allowed but treated as /*.
  | Host of string * string * string 
  /// "activeTab"	 Requests that the extension be granted permissions according to the activeTab specification.
  | ActiveTab
  /// "alarms"	 Gives your extension access to the chrome.alarms API.
  | Alarms
  /// <summary>"background"	
  /// Makes Chrome start up early and and shut down late, so that apps and extensions can have a longer life.
  /// When any installed hosted app, packaged app, or extension has "background" permission, Chrome runs (invisibly) as soon as the user logs into their computer—before the user launches Chrome. The "background" permission also makes Chrome continue running (even after its last window is closed) until the user explicitly quits Chrome.
  /// Note: Disabled apps and extensions are treated as if they aren't installed.
  /// You typically use the "background" permission with a background page, event page or (for hosted apps) a background window.</summary>
  | Background
  /// "bookmarks"	 Gives your extension access to the chrome.bookmarks API.
  | Bookmarks
  /// "browsingData"	 Gives your extension access to the chrome.browsingData API.
  | BrowsingData
  /// "clipboardRead"	 Required if the extension or app uses document.execCommand('paste').
  | ClipboardRead
  /// "clipboardWrite"	 Indicates the extension or app uses document.execCommand('copy') or document.execCommand('cut'). This permission is required for hosted apps; it's recommended for extensions and packaged apps.
  | ClipboardWrite
  /// "contentSettings"	 Gives your extension access to the chrome.contentSettings API.
  | ContentSettings
  /// "contextMenus"   Gives your extension access to the chrome.contextMenus API.
  | ContextMenus
  /// "cookies"   Gives your extension access to the chrome.cookies API.
  | Cookies
  /// "debugger"   Gives your extension access to the chrome.debugger API.
  | Debugger
  /// "declarativeContent"   Gives your extension access to the chrome.declarativeContent API.
  | DeclarativeContent
  /// "declarativeWebRequest"   Gives your extension access to the chrome.declarativeWebRequest API.
  | DeclarativeWebRequest
  /// "desktopCapture"   Gives your extension access to the chrome.desktopCapture API.
  | DesktopCapture
  /// "dns"   Gives your extension access to the chrome.dns API.
  | DNS
  /// "downloads"   Gives your extension access to the chrome.downloads API.
  | Downloads
  /// "experimental"   Required if the extension or app uses any chrome.experimental.* APIs.
  | Experimental
  /// "fileBrowserHandler"   Gives your extension access to the chrome.fileBrowserHandler API.
  | FileBrowserHandler
  /// "fontSettings"   Gives your extension access to the chrome.fontSettings API.
  | FontSettings
  /// "gcm"   Gives your extension access to the chrome.gcm API.
  | GCM
  /// "geolocation"   Allows the extension or app to use the proposed HTML5 geolocation API without prompting the user for permission.
  | Geolocation
  /// "history"   Gives your extension access to the chrome.history API.
  | History
  /// "identity"   Gives your extension access to the chrome.identity API.
  | Identity
  /// "idle"   Gives your extension access to the chrome.idle API.
  | Idle
  /// "idltest"   Gives your extension access to the chrome.idltest API.
  | Idltest
  /// "infobars"   Gives your extension access to the chrome.infobars API.
  | Infobars
  /// "location"   Gives your extension access to the chrome.location API.
  | Location
  /// "management"   Gives your extension access to the chrome.management API.
  | Management
  /// "notifications"   Gives your extension access to the chrome.notifications API.
  | Notifications
  /// "pageCapture"   Gives your extension access to the chrome.pageCapture API.
  | PageCapture
  /// "power"   Gives your extension access to the chrome.power API.
  | Power
  /// "privacy"   Gives your extension access to the chrome.privacy API.
  | Privacy
  /// "processes"   Gives your extension access to the chrome.processes API.
  | Processes
  /// "proxy"   Gives your extension access to the chrome.proxy API.
  | Proxy
  /// "pushMessaging"   Gives your extension access to the chrome.pushMessaging API.
  | PushMessaging
  /// "Runtime" NOT REAL. Used to find send message utilities.
  | Runtime
  /// "sessions"   Gives your extension access to the chrome.sessions API.
  | Sessions
  /// "signedInDevices"   Gives your extension access to the chrome.signedInDevices API.
  | SignedInDevices
  /// "storage"   Gives your extension access to the chrome.storage API.
  | Storage
  /// "system.cpu"   Gives your extension access to the chrome.system.cpu API.
  | System_CPU
  /// "system.display"   Gives your extension access to the chrome.system.display API.
  | System_display
  /// "system.memory"   Gives your extension access to the chrome.system.memory API.
  | System_memory
  /// "system.storage"   Gives your extension access to the chrome.system.storage API.
  | System_storage
  /// "tabCapture"   Gives your extension access to the chrome.tabCapture API.
  | TabCapture
  /// "tabs"   Gives your extension access to privileged fields of the $ref:[tabs.Tab Tab] objects used by several APIs including $ref:[tabs chrome.tabs] and $ref:[windows chrome.windows]. In many circumstances your extension will not need to declare the "tabs" permission to make use of these APIs.
  | Tabs
  /// "topSites"   Gives your extension access to the chrome.topSites API.
  | TopSites
  /// "tts"   Gives your extension access to the chrome.tts API.
  | TTS
  /// "ttsEngine"   Gives your extension access to the chrome.ttsEngine API.
  | TTSEngine
  /// "unlimitedStorage"   Provides an unlimited quota for storing HTML5 client-side data, such as databases and local storage files. Without this permission, the extension or app is limited to 5 MB of local storage.
  /// Note: This permission applies only to Web SQL Database and application cache (see issue 58985). Also, it doesn't currently work with wildcard subdomains such as http://*.example.com.
  | UnlimitedStorage
  /// "webNavigation"   Gives your extension access to the chrome.webNavigation API.
  | WebNavigation
  /// "webRequest"   Gives your extension access to the chrome.webRequest API.
  | WebRequest
  /// "webRequestBlocking"   Required if the extension uses the chrome.webRequest API in a blocking fashion.
  | WebRequestBlocking
  with
    member self.pretty = 
      match self with
      | Host (scheme, url, path) -> text <| sprintf "HOST   %s:%s/%s" scheme url path
      | ActiveTab
      | Alarms
      | Bottom
      | Background
      | Bookmarks
      | BrowsingData
      | ClipboardRead
      | ClipboardWrite
      | ContentSettings
      | ContextMenus
      | Cookies
      | Debugger
      | DeclarativeContent
      | DeclarativeWebRequest
      | DesktopCapture
      | DNS
      | Downloads
      | Experimental
      | FileBrowserHandler
      | FontSettings
      | GCM
      | Geolocation
      | History
      | Identity
      | Idle
      | Idltest
      | Infobars
      | Location
      | Management
      | Notifications
      | PageCapture
      | Power
      | Privacy
      | Processes
      | Proxy
      | PushMessaging
      | Runtime
      | Sessions
      | SignedInDevices
      | Storage
      | System_CPU
      | System_display
      | System_memory
      | System_storage
      | TabCapture
      | Tabs
      | TopSites
      | TTS
      | TTSEngine
      | UnlimitedStorage
      | WebNavigation
      | WebRequest
      | WebRequestBlocking -> text <| sprintf "%A" self
    override self.ToString () = 
      self.pretty.pretty
    static member parse s = 
      match s with
      | "ActiveTab" -> ActiveTab
      | "Alarms" -> Alarms
      | "Bottom" -> Bottom
      | "Background" -> Background
      | "Bookmarks" -> Bookmarks
      | "BrowsingData" -> BrowsingData
      | "ClipboardRead" -> ClipboardRead
      | "ClipboardWrite" -> ClipboardWrite
      | "ContentSettings" -> ContentSettings
      | "ContextMenus" -> ContextMenus
      | "Cookies" -> Cookies
      | "Debugger" -> Debugger
      | "DeclarativeContent" -> DeclarativeContent
      | "DeclarativeWebRequest" -> DeclarativeWebRequest
      | "DesktopCapture" -> DesktopCapture
      | "DNS" -> DNS
      | "Downloads" -> Downloads
      | "Experimental" -> Experimental
      | "FileBrowserHandler" -> FileBrowserHandler
      | "FontSettings" -> FontSettings
      | "GCM" -> GCM
      | "Geolocation" -> Geolocation
      | "History" -> History
      | "Identity" -> Identity
      | "Idle" -> Idle
      | "Idltest" -> Idltest
      | "Infobars" -> Infobars
      | "Location" -> Location
      | "Management" -> Management
      | "Notifications" -> Notifications
      | "PageCapture" -> PageCapture
      | "Power" -> Power
      | "Privacy" -> Privacy
      | "Processes" -> Processes
      | "Proxy" -> Proxy
      | "PushMessaging" -> PushMessaging
      | "Runtime" -> Runtime
      | "Sessions" -> Sessions
      | "SignedInDevices" -> SignedInDevices
      | "Storage" -> Storage
      | "System_CPU" -> System_CPU
      | "System_display" -> System_display
      | "System_memory" -> System_memory
      | "System_storage" -> System_storage
      | "TabCapture" -> TabCapture
      | "Tabs" -> Tabs
      | "TopSites" -> TopSites
      | "TTS" -> TTS
      | "TTSEngine" -> TTSEngine
      | "UnlimitedStorage" -> UnlimitedStorage
      | "WebNavigation" -> WebNavigation
      | "WebRequest" -> WebRequest
      | "WebRequestBlocking" -> WebRequestBlocking 
      | s when s.StartsWith "HOST" -> Host ("unknown", "unknown", "unknown")
      | _ -> failwith "not known permission execution"

type PermissionBox = 
    private P of Set<CPermission>
  with 
    member self.Content = let (P c) = self in c
    member self.pretty = 
      Text.PrettySeqs.pretty_seq prettifier ";" self.Content
    member self.contains (c : CPermission) = Set.contains c self.Content
    member self.subseq (c : PermissionBox) = Set.isSubset c.Content self.Content
    member self.AsSet = let (P p) = self in p
    static member Bottom = P Set.empty
    static member (+) (P x, P y) = P (x + y)

type Permission = private P of Set<PermissionBox>
  with    
    member self.Content = let (P c) = self in c
    member self.pretty = 
      Text.PrettySeqs.pretty_seq prettifier ";" self.Content
    member self.contains (c : PermissionBox) = Set.contains c self.Content
    member self.contains (c : CPermission) = 
      self.Content |> Set.exists (fun ps -> Set.contains c ps.Content) 
    member self.subseq (c : Permission) = Set.isSubset c.Content self.Content
    static member Bottom = P <| Set [PermissionBox.Bottom]
    static member (+) (P x, P y) = P (x + y)
